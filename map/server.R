log <- function(text, file="log.txt"){
  tryCatch({
    log <- file(file, "at")
    writeLines(text, con=log)
    close(log)
  }, error= function(e){}
  )
  print(text)
}


log("Loading requried packages")
library(shiny)
require(data.table)
require(grid)
require(ggplot2)

#load MAP Helper functions
log("Loading MAP graphing functions")

source("lib/MAP_helper_functions.R")

#need to load one-time per session.
log("Loading dataset...")

map.data <- read.csv("data/map.scores.by.grade.csv")
map.data <- data.table(map.data)


shinyServer(function(input, output) {
  
  log("Server init.")
  
  getData <- reactive(function(){
    log("Subsetting dataset...")
    
    map.data[Fall12_Grade     == input$grade & 
               Subject        == input$subject & 
               SchoolInitials == input$school
             ]
    
  })
  
  dataName <- reactive(function(){
    log("Setting data name...")
    
    if (input$dataset == "Geyeser"){
      return("Eruption Duration (min)")
    }
    if (input$dataset == "NYC Wind"){
      return("Wind Speed (mph)")
    }
  })
  
  output$main_plot <- renderPlot(function() {
    log("Plotting MAP Results...")
  
    ptitle <- paste0(input$school, " 2012-13 MAP Scores ",input$grade," ",input$subject, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
   
    p<-plot_MAP_Results_and_Goals(getData(), ptitle, labxpos=100, minx=95,alp=.6) 

    print(p)
  })
  
  output$main_table <- renderDataTable({
   tbData <- getData()
   tbData[,list(Fall12_Grade, StudentLastName, StudentFirstName, Subject, Fall12_RIT, Fall12_Pctl, 
                ProjectedGrowth, CollegeReadyGrowth, Spring13_RIT, Spring13_Pctl)] 
   }, options = list(bSortClasses=TRUE))
   

})
