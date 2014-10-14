#all students enrolled on October 3, 2012
message("Loading required libraries . . . ")
#library(RJDBC)
#library(lubridate)

# Connect to server using JDBC Connection (note: requires a VPN connection to be open to psshostingvpn.poowerschool.com)

message("Logging into server . . . ")
drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/var//lib/jdbc//ojdbc6.jar","") # define driver

#pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

#pscon <- dbConnect(drvr,pspw$SERVER,pspw$UID,pspw$PWD) # connect to server

message('Getting 10/03/12 attendance')

sql.statment<-"SELECT
m.SchoolID,
                            s.student_number as StudentID,
s.first_name,
s.middle_name,
s.last_name,
m.grade_level,
s.ethnicity AS Race_ID,
s.gender,
s.dob,
s.entrydate,
s.SCHOOLENTRYDATE,
s.DISTRICTENTRYDATE,
s.EXITDATE,
s.exitcode,
s.EXITCOMMENT
FROM PS_Membership_Defaults m
JOIN STUDENTS s
ON m.studentid = s.id
WHERE m.calendardate = '03-OCT-12'
ORDER BY schoolid, grade_level
"

Enrolled.121003<-read.jdbc.ffdf(query=sql.statement,
               dbConnect.args = list( 
                 drv=drvr,
                 url=pspw$SERVER,
                 user=pspw$UID,
                 password=pspw$PWD
               ),
               first.rows=100,
               next.rows=1000,
               VERBOSE=TRUE
               )


message("Writing attendance data to data/Enrolled.121003.csv")
write.csv.ffdf(Enrolled.121003, 'data/Enrolled.121003.csv')
