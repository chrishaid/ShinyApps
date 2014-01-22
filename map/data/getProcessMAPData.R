# R Script to load and process MAP data for Idea reporting.

message("Loading packages")
library(data.table)
library(RJDBC)

message("Loading helper functions")

source('../lib/helpers.R')
source('../lib/MAP_helper_functions.R')

message('Getting Intial MAP Data')

source('map.F13W14.R')

message('Preprocess Data')
#source('../munge/01-All_Schools.R')
#source('../munge//02-kaps.R')

message('Prep Data')
FW.dt<-PrepMAP(map.F13W14, season1="Fall13", season2="Winter14")

message('Writing file to csv')
write.csv(FW.dt, file="map_F13W14.csv")

system('touch ../restart.txt')