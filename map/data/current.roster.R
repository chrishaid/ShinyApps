message('Reading configuration')
sconfig <- as.list(read.dcf("../config/map2.dcf", all=TRUE))
#### Attach to Testing Server
mapsrc<-src_mysql(dbname=sconfig$DB, 
                  host=sconfig$HOST, 
                  user=sconfig$UID, 
                  password=sconfig$PWD)

# pull data
message('Contacting server and querying results for current roster')

current.roster<-collect(tbl(mapsrc, "tbl_PS_Roster_All"))




