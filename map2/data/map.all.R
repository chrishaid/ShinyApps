#### Get MAP Data ####


message('Reading configuration')
sconfig <- as.list(read.dcf("../config/map.dcf", all=TRUE))
#### Attach to Testing Server
drvr <- JDBC("com.mysql.jdbc.Driver",sconfig$JAR,"")

con <- dbConnect(drvr,sconfig$SERVER, sconfig$UID, sconfig$PWD)

# pull data
message('Contacting server and querying results for all years')

qry<-'SELECT * FROM viewAllAssessments WHERE GrowthMeasureYN="TRUE"'

tryCatch(map.all<-dbGetQuery(con, qry),
           error = function(w) {message(paste("You need to have a JDBC conncetion to the database.  Original error is:", w))}
         )

# cache
#cache("map.F13W14")