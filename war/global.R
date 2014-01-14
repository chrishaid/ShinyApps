# global.R for session
message("Loading libraries for global environment")
library(shiny)
library(data.table)
library(lubridate)
library(ggplot2)
library(reshape)
library(knitr)
library(RCurl)
library(xtable)

# source helper functions:
message("Loading helper libraries")
source('lib/attendence_helpers.R', local=TRUE)
source('lib/transfer_helpers.R', local=TRUE)

#### Attendance Data ####
message('Reading and munging attendance data')
# Load and munge attendance data (this can be lengthy) once for all sessions 
#  and create ancillary data tables
Attendance <- as.data.table(read.csv("data/Attendance.csv"))
source('munge/01-Attendance.R', local=TRUE)

message('Creating attendance output tables')
source('src/attendence_tables.R', local=TRUE)

message('Reading and munging transfer data')

Enrolled.121003 <- as.data.table(read.csv("data/Enrolled.121003.csv"))
Enrolled.131001 <- as.data.table(read.csv("data/Enrolled.131001.csv"))

source('data/Xfers.HSR.1213.R', local=TRUE)
source('data/Xfers.HSR.1314.R', local=TRUE)

source('munge/02-Tranfers_Prep.R', local=TRUE)

message('Creating transfer and suspension tables')





#### Date information ####
message("Getting first day of school and current day information")
date.first  <- "2013-08-19" # first day of school year
date.second <- lubridate::today() # 

title.date<-paste(lubridate::month(date.second, 
                                   label=TRUE, 
                                   abbr=FALSE),
                  " ",
                  lubridate::day(date.second), ", ", 
                  lubridate::year(date.second), sep=""
)

