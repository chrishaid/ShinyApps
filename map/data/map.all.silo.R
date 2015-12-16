message('Reading silo configuration')
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
message('Contacting server and querying results for all years')

# get viewAllAssessments
qry<-"SELECT * FROM NWEA..MAP$comprehensive#plus_cps WHERE GrowthMeasureYN='TRUE'"
map.all.silo<-dbGetQuery(conn, qry)


