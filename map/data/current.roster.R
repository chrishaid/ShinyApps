message('Reading configuration')
#sconfig <- as.list(read.dcf("../config/map2.dcf", all=TRUE))
#### Attach to Testing Server
#mapsrc<-dplyr::src_mysql(dbname=sconfig$DB, 
#                   host=sconfig$HOST, 
#                   user=sconfig$UID, 
#                   password=sconfig$PWD)

silo<-as.data.frame(read.dcf('../config//silo_dw.dcf'))


drvr <- JDBC(driverClass = 'com.microsoft.sqlserver.jdbc.SQLServerDriver', 
             classPath = '/var/lib/jdbc/sqljdbc_4.1/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)

# pull data
#message('Contacting server and querying results for all years')
message('Contacting server and querying results for current roster')

# get current roster
qry<-"SELECT * FROM [PS_mirror].[dbo].[students] WHERE enroll_status=0"
current.roster<-dbGetQuery(conn, qry)


# pull data
#message('Contacting server and querying results for current roster')

#current.roster<-collect(tbl(mapsrc, "tbl_PS_Roster_All"))




