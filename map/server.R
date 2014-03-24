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

load("data/map_FW.Rdata")
load("data/map_all.Rdata")


# Tabular summary for Winter 14 (should be abstracted and moved to lib)
tabSummaryMAP <- function(.data, school="KAMS"){
  dt<-copy(.data)
  dt.sum<-dt[SchoolInitials %in% school,
             list("Total Tested F & W"= .N, 
                  "# >= Typical NWEA" = sum(Winter_RIT>=ProjectedGrowth),  
                  "% >= Typical NWEA" = round(sum(Winter_RIT>=ProjectedGrowth)/.N,2), 
                  "# >= Typical Tracker" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth/2)),  
                  "% >= Typical Tracker" = round(sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth/2))/.N,2), 
                  "# >= College Ready NWEA" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToWinterGrowth*KIPPTieredGrowth)),
                  "% >= College Ready NWEA" = round(sum(Winter_RIT>=Fall_RIT+(ReportedFallToWinterGrowth*KIPPTieredGrowth))/.N,2),
                  "# >= College Ready Tracker" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth*KIPPTieredGrowth/2)),
                  "% >= College Ready Tracker" = round(sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth*KIPPTieredGrowth/2))/.N,2),
                  "# >= 50th Percentile Fall" = sum(Fall_Pctl>=50),
                  "% >= 50th Percentile Fall" = round(sum(Fall_Pctl>=50)/.N,2),
                  "# >= 50th Percentile Winter" = sum(Winter_Pctl>=50),
                  "% >= 50th Percentile Winter" = round(sum(Winter_Pctl>=50)/.N,2),
                  "# >= 75th Percentile Fall" = sum(Fall_Pctl>=75),
                  "% >= 75th Percentile Fall" = round(sum(Fall_Pctl>=75)/.N,2),
                  "# >= 75th Percentile Winter" = sum(Winter_Pctl>=75),
                  "% >= 75th Percentile Winter" = round(sum(Winter_Pctl>=75)/.N,2)
                  ), 
             by=list(SY, SchoolInitials, 
                     Winter_Grade, 
                     Subject)]
  setnames(dt.sum, c("SchoolInitials", "Winter_Grade"),
           c("School", "Grade"))
  
  dt.sum[order(School, Subject, Grade)]
}

map.F13W14 <- map.data[SY=="2013-2014"]

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
    schls<-map.F13W14[,unique(SchoolInitials)]
    
    selectInput("school", 
                "Select School", 
                schls, 
                selected=schls[1])
    }
    )
  
  getSubjects <- reactive({dt<-map.F13W14[SchoolInitials==input$school, 
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
  
  getGrades <- reactive({dt<-map.F13W14[SchoolInitials==input$school & 
                                        Subject==input$subject, 
                                        unique(Fall_Grade)]
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
    map.F13W14[Fall_Grade     == input$grade & 
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
                      season1="Fall", 
                      season2="Winter",
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
   tbData[,list(SY,
                SchoolInitials, 
                Fall_Grade, 
                StudentLastName, 
                StudentFirstName, 
                Subject,
                Fall_RIT, 
                Fall_Pctl, 
                ProjectedGrowth, 
                CollegeReadyGrowth, 
                Winter_RIT, 
                Winter_Pctl)] 
   }, 
   options = list(bSortClasses=TRUE)
   )

getSummaryTable <- reactive({
  tbSchools<-tabSummaryMAP(map.data, map.data[,unique(SchoolInitials)]) 
  tbSchools[,School:=as.character(School)]
  tbRegion<-map.data[,
                     list("Total Tested F & W"= .N, 
                          "# >= Typical NWEA" = sum(Winter_RIT>=ProjectedGrowth),  
                          "% >= Typical NWEA" = round(sum(Winter_RIT>=ProjectedGrowth)/.N,2), 
                          "# >= Typical Tracker" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth/2)),  
                          "% >= Typical Tracker" = round(sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth/2))/.N,2), 
                          "# >= College Ready NWEA" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToWinterGrowth*KIPPTieredGrowth)),
                          "% >= College Ready NWEA" = round(sum(Winter_RIT>=CollegeReadyGrowth)/.N,2),
                          "# >= College Ready Tracker" = sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth*KIPPTieredGrowth/2)),
                          "% >= College Ready Tracker" = round(sum(Winter_RIT>=Fall_RIT+(ReportedFallToSpringGrowth*KIPPTieredGrowth/2))/.N,2),
                          "# >= 50th Percentile Fall" = sum(Fall_Pctl>=50),
                          "% >= 50th Percentile Fall" = round(sum(Fall_Pctl>=50)/.N,2),
                          "# >= 50th Percentile Winter" = sum(Winter_Pctl>=50),
                          "% >= 50th Percentile Winter" = round(sum(Winter_Pctl>=50)/.N,2),
                          "# >= 75th Percentile Fall" = sum(Fall_Pctl>=75),
                          "% >= 75th Percentile Fall" = round(sum(Fall_Pctl>=75)/.N,2),
                          "# >= 75th Percentile Winter" = sum(Winter_Pctl>=75),
                          "% >= 75th Percentile Winter" = round(sum(Winter_Pctl>=75)/.N,2)
                     ), 
                     by=list(SY, Winter_Grade, 
                             Subject)]
  setnames(tbRegion, "Winter_Grade", "Grade")
  tbRegion[,School:="KIPP Chicago"]
  
  tbData<-rbind(tbSchools, tbRegion)
  
  tbData[,Grade:=factor(Grade, levels=c("K", 1:8))]
  
  tbData[,Subject:=factor(Subject, levels=c("Mathematics",
                                            "Reading",
                                            "Language Usage",
                                            "General Science",
                                            "Science - Concepts and Process")
                          )
         ]
  
  tbData[,School:=factor(School, 
                         levels=c("KIPP Chicago",
                                  "KAP",
                                  "KAMS",
                                  "KCCP",
                                  "KBCP"
                                  )
                         )
         ]
  
  tbData<-tbData[School %in% input$selectSummSchool & 
                   SY %in% input$selectSummSY &
                   Subject %in% input$selectSummSubj &
                   Grade %in% input$selectSummGrades
                 ][order(SY, Subject, Grade, School)]
  
  setnames(tbData, "SY", "School Year")
  if(nrow(tbData)==0) return()
  tbData[,c("School Year", "Subject", "School", "Grade", input$selectSummCols), with=F]
  
})





output$sum_table <-renderDataTable({
  getSummaryTable()
  },
  options = list(bSortClasses=TRUE)
)

output$reg_sum_table <-renderDataTable({
  tbData<-map.data[,
     list("Total Tested F & W"= .N, 
          "# >= Typical" = sum(Winter_RIT>=ProjectedGrowth),  
          "% >= Typical" = round(sum(Winter_RIT>=ProjectedGrowth)/.N,2), 
          "# >= College Ready" = sum(Winter_RIT>=CollegeReadyGrowth),
          "% >= College Ready" = round(sum(Winter_RIT>=CollegeReadyGrowth)/.N,2)), 
     by=list(SY, Winter_Grade, 
             Subject)]
  setnames(tbData, "Winter_Grade", "Grade")
  tbData[,School:="KIPP Chicago"]
  
  tbData[order(SY, Subject, Grade)]
})

})
