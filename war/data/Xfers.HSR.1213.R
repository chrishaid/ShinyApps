#all Students enrolled on Oct 31 who have exit dates prior to 6/14/2013

#Xfers.HSR.1213<-subset(Enrolled.121003, ymd_hms(Enrolled.121003$EXITDATE)<ymd("13-06-14"))
Enrolled.121003<-data.table(Enrolled.121003)
Xfers.HSR.1213<-Enrolled.121003[!is.na(EXITCODE) & EXITCODE!="GR" & ymd_hms(EXITDATE)<=ymd("13-09-30")]
