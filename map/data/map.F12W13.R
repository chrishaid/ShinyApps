#### Get MAP Data ####


message('Reading configuration')
sconfig <- as.list(read.dcf("../config/map.dcf", all=TRUE))
#### Attach to Testing Server
drvr <- JDBC("com.mysql.jdbc.Driver",sconfig$JAR,"")

con <- dbConnect(drvr,sconfig$SERVER, sconfig$UID, sconfig$PWD)

# pull data
message('Contacting server and querying results')
map.F12W13<-get_MAPResults(con, "Fall12", "Winter13")

# cache
#cache("map.F13W14")