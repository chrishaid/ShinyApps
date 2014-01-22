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
require(stringr)
require(shinysky)

#load MAP Helper functions
log("Loading MAP graphing functions")

source("lib/MAP_helper_functions.R")
source("lib/helpers.R")

#need to load one-time per session.
log("Loading dataset...")

map.data <- read.csv("data/map_F13W14.csv")
map.data <- data.table(map.data)

# Tabular summary for Winter 14 (should be abstracted and moved to liv)
tabSummaryMAP <- function(.data, school="KAMS"){
  dt<-copy(.data)
  dt.sum<-dt[SchoolInitials %in% school,
             list("Total Tested"= .N, 
                  "# >= Typical" = sum(Winter14_RIT>=ProjectedGrowth),  
                  "% >= Typical" = round(sum(Winter14_RIT>=ProjectedGrowth)/.N,2), 
                  "# >= College Ready" = sum(Winter14_RIT>=CollegeReadyGrowth),
                  "% >= College Ready" = round(sum(Winter14_RIT>=CollegeReadyGrowth)/.N,2)), 
             by=list(SchoolInitials, 
                     Winter14_Grade, 
                     Subject)]
  setnames(dt.sum, c("SchoolInitials", "Winter14_Grade"),
           c("School", "Grade"))
  
  dt.sum[order(School, Subject, Grade)]
}


shinyServer(function(input, output, session) {
  # Trick SO on delaying rendering:
  # http://stackoverflow.com/questions/20490619/delayed-execution-in-r-shiny-app
  # and here:
  # https://github.com/rstudio/shiny-examples/blob/master/014-onflushed/server.R
  values <- reactiveValues(starting = TRUE)
  
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  log("Server init.")
  
  
  output$schools<-renderUI({
    schls<-map.data[,unique(SchoolInitials)]
    
    selectInput("school", 
                "Select School", 
                schls, 
                selected=schls[1])
    }
    )
  
  getSubjects <- reactive({dt<-map.data[SchoolInitials==input$school, 
                                    unique(Subject)]
                           return(dt)}
                          )
  

  
  output$subjects <- renderUI({
    selectInput("subject",
                "Select Subject",
                choices = getSubjects(),
                selected="Reading")
  }
  )
  
  getGrades <- reactive({dt<-map.data[SchoolInitials==input$school & 
                                        Subject==input$subject, 
                                        unique(Fall13_Grade)]
                           return(dt)}
  )
  output$grades <- renderUI({
    selectInput("grade",
                "Select Grade",
                choices = getGrades(),
                selected=getGrades()[1]
                )
  }
  )

  getData <- reactive(function(){
    log("Subsetting dataset...")
    map.data[Fall13_Grade     == input$grade & 
               Subject        == input$subject & 
               SchoolInitials == input$school
             ]
    
  })
  
   
  
  output$main_plot <- renderPlot({
   
    
    log("Plotting MAP Results...")
    
    
    ptitle <- paste0(input$school, 
                     " 2013-14 Fall to Winter MAP Scores ",
                     input$grade,
                     " ",
                     input$subject, 
                     "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
   
    # using try() catch an error because renderPlot trys to execute plot_waterfall
    #  before the observer recieves all data from the renderUIs for school, grade, subjectx
    p<-try(plot_waterfall(getData(), 
                      ptitle, 
                      season1="Fall13", 
                      season2="Winter14",
                      labxpos=100, 
                      minx=95,
                      alp=.6
                      ),
           silent=TRUE)
           

    print(p)
    
  }
)
  
output$main_table <- renderDataTable({
   tbData <- map.data
   tbData[,list(SchoolInitials, Fall13_Grade, StudentLastName, StudentFirstName, Subject, Fall13_RIT, Fall13_Pctl, 
                ProjectedGrowth, CollegeReadyGrowth, Winter14_RIT, Winter14_Pctl)] 
   }, 
   options = list(bSortClasses=TRUE)
   )

output$sum_table <-renderDataTable({
  tabSummaryMAP(map.data, map.data[,unique(SchoolInitials)])       
  },
  options = list(bSortClasses=TRUE)
)

output$reg_sum_table <-renderDataTable({
  tbData<-map.data[,
     list("Total Tested"= .N, 
          "# >= Typical" = sum(Winter14_RIT>=ProjectedGrowth),  
          "% >= Typical" = round(sum(Winter14_RIT>=ProjectedGrowth)/.N,2), 
          "# >= College Ready" = sum(Winter14_RIT>=CollegeReadyGrowth),
          "% >= College Ready" = round(sum(Winter14_RIT>=CollegeReadyGrowth)/.N,2)), 
     by=list( Winter14_Grade, 
             Subject)]
  setnames(tbData, "Winter14_Grade", "Grade")
  tbData[order(Subject, Grade)]
})

})
