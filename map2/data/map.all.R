#### Get MAP Data ####


message('Reading configuration')
sconfig <- as.list(read.dcf("../config/map2.dcf", all=TRUE))
#### Attach to Testing Server
mapsrc<-src_mysql(dbname=sconfig$DB, 
                  host=sconfig$HOST, 
                  user=sconfig$UID, 
                  password=sconfig$PWD)

# pull data
message('Contacting server and querying results for all years')

# get viewAllAssessments
map.all<-collect(tbl(mapsrc, "viewAllAssessments"))
map.all <- filter(map.all, GrowthMeasureYN=="TRUE")



