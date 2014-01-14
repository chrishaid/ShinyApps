
message('Sourcing daily attendence data')
source('data/Attendance.R')
message('Sourcing students enrolled on 3 Oct 2012')
source('data/Enrolled.121003.R')
message('Sourcing students enrolled on 1 Oct 2013')
source('data/Enrolled.131001.R')

message('Telling Shiny Server to reload')
system('touch restart.txt')