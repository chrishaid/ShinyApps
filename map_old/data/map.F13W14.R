#### Get MAP Data ####


message('Reading configuration')
sconfig <- as.list(read.dcf("../config/map.dcf", all=TRUE))
#### Attach to Testing Server
drvr <- JDBC("com.mysql.jdbc.Driver",sconfig$JAR,"")

con <- dbConnect(drvr,sconfig$SERVER, sconfig$UID, sconfig$PWD)

# pull data
message('Contacting server and querying results')
map.F13W14<-get_MAPResults(con, "Fall13", "Winter14")

# cache
#cache("map.F13W14")