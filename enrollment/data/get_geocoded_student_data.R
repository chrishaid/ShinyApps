# This file downloads geocoded student location data from 
# KIPP Silo and then gets distance data from google.  Finally
# the script writes a csv file that is lodded into memory by shiny with all 
# data used in plotting of enrollment map.

message("Instantiating logger")
require(log4r)

logger<-create.logger(logfile="../logs/get_gis_data.log", level="INFO")
info(logger, "logger instantiated")

info(logger, "Loading packages")
require(RJDBC)
require(ggmap)
require(dplyr)
require(stringr)
require(lubridate)
require(RColorBrewer)


info(logger, "Reading Silo configuration")
silo<-as.data.frame(read.dcf('../config//silo_dw.dcf'))

info(logger, "Connection to Silo")
drvr <- JDBC(driverClass = 'com.microsoft.sqlserver.jdbc.SQLServerDriver', 
             classPath = '/var/lib/jdbc/sqljdbc_4.1/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)


info(logger, "Querying student data")
qry<-paste("SELECT student_number as StudentID,
                   first_name,
                   last_name,
                   grade_level,
                   street,
                   city, 
                   state,
                   zip,
                   schoolid,
                   geocode
           FROM PS_mirror..students 
           WHERE enroll_status=0")

students<-dbGetQuery(conn, qry)


info(logger, "Extracting latitude and longitude from geocode field")
lat<-str_extract_all(students$geocode, perl("\\S?\\d{2}\\S\\d+(?=\\S\\s)"))

lon<-str_extract_all(students$geocode, perl("\\S?\\d{2}\\S\\d+$"))

stu_lat_lon <- students %>%
  mutate(lat=as.numeric(cbind(lat)),
         lon=as.numeric(cbind(lon)), 
         Address=paste(street, city, state, zip))

info(logger, "Reading KIPP Chicago Schools Locations")
# Uncomment the following lines if you need to re-geocode the schools
# locations otherwise these will be read in via csv
# kcs_address <- data.frame(SchoolID=c(78102, 7810, 400146, 400163),
#                          School=c("KAP", "KAMS", "KCCP", "KBCP"),
#                          Address=c("1440 S Christiana Ave, Chicago, IL 60623",
#                                    "1616 S Avers Ave, Chicago, IL 60642",
#                                    "4818 W. Ohio Street Chicago, IL 60644",
#                                    "5515 S. Lowe Ave, Chicago, IL 60621")
#                          )
# 
# kcs_latlon<-geocode(as.character(kcs_address$Address))
# kcs<-cbind(kcs_address, kcs_latlon)
#write.csv(kcs, file="kcs_locations.csv")

kcs<-read.csv(file="kcs_locations.csv")
kcs<-kcs %>% mutate(schoolid=SchoolID)

info(logger, "Getting student distances form home to school from Google.")
stu_school<-stu_lat_lon %>% left_join(kcs, by="schoolid") %>%
  mutate(Address.x = gsub("#", "", Address.x))

distances<-mapdist(from=as.character(stu_school$Address.x), to = as.character(stu_school$Address.y))

distances<-filter(distances, !duplicated(distances))

stu_school_distances<-left_join(stu_school, distances, by=c("Address.x" = "from"))

info(logger, "Munging student data in prep for final save")

# This function matches schoolid's to school initials
schools<-function(x){
  lookup<-function(y){
    switch(y,
           "78102" = "KAP",
           "7810" = "KAMS",
           "400146" = "KCCP",
           "400163" = "KBCP")
  }
  
  out<-mapply(lookup, x, SIMPLIFY = TRUE)
  out
}

stus <- stu_school_distances %>% 
  mutate(lat=lat.x,
         lon=lon.x,
         School = schools(as.character(SchoolID)),
         School=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP")))

colors<-c("purple", "#439539", "#60A2D7", "#C49A6C")

stus2 <- stus %>% 
  mutate(fillColor=colors[match(School, c("KAP", "KAMS", "KCCP", "KBCP"))],
         popup=paste0("<p>Student: ", paste(first_name, last_name),
                      "<br>School: ", School,
                      "<br>Grade: ", grade_level,
                      "<br>Miles form school: ", round(miles),
                      "<br>Minutes from school: ", round(minutes),
                      "</p>")
  ) %>%
  #select(lat, lon, fillColor=color, popup) %>%
  filter(!(is.na(lat)|is.na(lon)))


save(stus2, stus, stu_school_distances, kcs, colors, file="gis_students.Rda")

info("Telling Shiny-server to restart.")
system("touch ../restart.txt")
