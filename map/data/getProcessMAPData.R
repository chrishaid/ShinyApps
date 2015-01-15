# R Script to load and process MAP data for Idea reporting.

args<-commandArgs(TRUE)

message(print(args[1]))
message(print(args[2]))
message(print(args[3]))

message("Loading packages")
library(data.table)
library(RJDBC)
library(stringr)
library(mapvisuals)
library(dplyr)

message("Loading helper functions")

source('../lib/helpers.R')
source('../lib/MAP_helper_functions.R')

message('Getting Intial MAP Data')

if("no" %in% args[1]){
  message("Not importing map.F13W14.R, map.F13W14.R, or map.F13W14.R")
} else {
  source('map.F13W14.R')
  source('map.F12W13.R')
  source('map.F11W12.R')  
  
  
  message('Prep Data')
  map.F13W14<-PrepMAP(map.F13W14, season1="Fall13", season2="Winter14")
  map.F12W13<-PrepMAP(map.F12W13, season1="Fall12", season2="Winter13")
  map.F11W12<-PrepMAP(map.F11W12, season1="Fall11", season2="Winter12")
  
  map.F13W14[,SY:="2013-2014"]
  map.F12W13[,SY:="2012-2013"]
  map.F11W12[,SY:="2011-2012"]
  
  map.data<-rbind(map.F13W14, map.F12W13, map.F11W12, use.names=FALSE)
  
  setnames(map.data,
           old=names(map.data),
           new=gsub("([a-zA-z]+)[0-9]+(_\\w+)",
                    "\\1\\2", 
                    names(map.data)
           )
  )
  
  message('Writing files to disk (as .Rdata binaries)')
  save(map.data, file="map_FW.Rdata")

  
}


if("no" %in% args[2]){
  message("Not importing map.all")
  
} else {
  source('map.all.silo.R')
  message('Preprocess Data')
  #source('../munge/01-All_Schools.R')
  source('../munge/03-map_all.R')
  
  
  message('Writing files to disk (as .Rdata binaries)')
  
  save(map.all, file="map_all.Rdata")
  save(map.all.growth, file="map_all_growth.Rdata")
  save(map.all.growth.sum, file="map_all_growth_sum.Rdata")
  save(map.all.growth.sum.p, file="map_all_growth_sum_p.Rdata")
  

}

if("no" %in% args[3]){
  message("Not importing current roster.")
} else {
  source('current.roster.R')
  if("no" %in% args[2]) load("map_all.Rdata")
  source('../munge/04-current_roster.R')
  save(tested.summary, file="tested_summary.Rdata")
}
  
system('touch ../restart.txt')