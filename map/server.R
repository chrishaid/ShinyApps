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
require(DT)
library(mapvisuals)

#load MAP Helper functions
log("Loading MAP graphing functions")

source("lib/MAP_helper_functions.R")
source("lib/helpers.R")

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

#need to load one-time per session.
log("Loading dataset...")

#load("data/map_FW.Rdata")
load("data/map_all.Rdata")
load("data/map_all_growth.Rdata")
load("data/map_all_growth_sum.Rdata")
load("data/map_all_growth_sum_p.Rdata")
load("data/tested_summary.Rdata")


# Tabular summary for Winter 14 (should be abstracted and moved to lib)
tabSummaryMAP <- function(.data, school="KAMS"){
  dt<-copy(.data)
  dt.sum<-dt[School %in% school]
  dt.sum[order(School, Subject, Grade)]
}

#map.F13W14 <- map.data[SY=="2013-2014"]

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
  
  

# Main Table ####
output$main_table <- DT::renderDataTable(
  map.all.growth %>% 
    arrange(desc(SY), SchoolInitials, Grade, MeasurementScale, GrowthSeason) %>%
    select("School Year" = SY,
                "Growth Season (Season 1 - Season 2)"=GrowthSeason,
                "School" = SchoolInitials, 
                Grade, 
                "Last Name" = StudentLastName, 
                "First Name" = StudentFirstName, 
                "Subject" = MeasurementScale,
                "Seasone 1 RIT" = TestRITScore, 
                "Season 1 Percentile"=TestPercentile, 
                "Typical Target"=TypicalTarget,
                "College Ready Target"= CollegeReadyTarget, 
                "Season 2 RIT" = TestRITScore.2, 
                "Season 2 Percentile"=TestPercentile.2),
  filter = "top",
  rownames = FALSE,
  options = list(autoWidth = TRUE)
   )
  
  output$hist_scores <- DT::renderDataTable(
    map.all %>%
      arrange(desc(SY), SchoolInitials, Grade, desc(MeasurementScale), Season) %>%
      select("School Year" = SY,
             "School" = SchoolInitials,
             Grade, 
             Season,
             "Subject" = MeasurementScale,
             "Student ID" = StudentID,
             "First Name" = StudentFirstName, 
             "Last Name" = StudentLastName, 
             "RIT Score" = TestRITScore, 
             "Percentile"=TestPercentile, 
             "Date Taken" = TestStartDate,
              "Taken @ KIPP?" = Tested_at_KIPP
      ),
    filter = "top",
    rownames = FALSE,
    options = list(autoWidth = TRUE)
  )
  


# Summary Table ####  
getSummaryTable <- reactive({
  tbData<-tabSummaryMAP(map.all.growth.sum, map.all.growth.sum[,unique(School)]) 
  
  #tbData[,Grade:=factor(Grade, levels=c("K", 1:8))]
  
  tbData[,Subject:=factor(Subject, levels=c("Mathematics",
                                            "Reading",
                                            "Language Usage",
                                            "General Science",
                                            "Science - Concepts and Process")
                          )
         ]
  
  tbData[,School:=factor(as.character(School), 
                         levels=c("Region",
                                  "KAP",
                                  "KAMS",
                                  "KCCP",
                                  "KBCP"
                                  )
                         )
         ]
  
  tbData<-tbData[School %in% input$selectSummSchool & 
                   SY %in% input$selectSummSY &
                   GrowthSeason %in% input$selectSummSeason &
                   Subject %in% input$selectSummSubj &
                   Grade %in% input$selectSummGrades
                 ][order(SY, Subject, Grade, School)]
  
  setnames(tbData, 
           c("SY", "GrowthSeason"), 
           c("School Year", "S1-S2")
           )
  if(nrow(tbData)==0) return()
  tbData[,c("School Year", "S1-S2", "Subject", "School", "Grade", input$selectSummCols), with=F]
  
})





output$sum_table <-renderDataTable({
  getSummaryTable()
  },
  options = list(orderClasses=TRUE,
                 lengthMenu = list(c(5,25, 50, 100, -1), 
                                    list(5,25,50,100,'All')
                                    ),
                 pageLength = 50,
                 "dom"='T<"clear">lfrtip',
                 "oTableTools"=list(
                   "sSwfPath"="static/swf/copy_csv_xls_pdf.swf"
                   )
                 )
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

# Dashboard ####
# Dashboard numbers tested plot ####
output$dashboard_tested <- DT::renderDataTable(
  datatable(tested.summary %>%
    arrange(`% Tested`),
  filter = "top",
  rownames = FALSE,
  options = list(pageLength = 100)
  ) %>%
  formatStyle(
    '% Tested',
    background = styleColorBar(tested.summary$`% Tested`, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )
)
# Dashboard panel plot ####
output$dashboard_panel <- renderPlot({
  
  withProgress(min=1, max=10, {
       setProgress(message = 'Loading MAP data',
                      detail = 'This may take a while...')
   
       setProgress(value = 3, message="Filtering metric...")
  
       y.actual<-input$selectDBStat
        
       y.label<-switch(y.actual,
                     "Pct.Typical" = "Percent M/E Typical Growth" ,
                     "Pct.CR" ="Percent M/E College Ready Growth",
                     "Pct.50.S2" = "Percent >= 50th %ile (season 2)",
                      "Pct.75.S2"="Percent >= 50th %ile (season 2)" 
                      )
       
       setProgress(value = 5, message="Building visualization",
                   detail="This can take a minute...")
       
       
       if(input$selectDBCohort=="Grade") {
         p<-ggplot(map.all.growth.sum.p[GrowthSeason==input$selectDBSeason & 
                                          Subject %in% input$selectDBSubject & 
                                          School!="Region" & 
                                          N.S1.S2>=10], 
                   aes_string(x='gsub("20","",SY)', 
                              y=paste0(y.actual,'*100')
                   )
         ) + 
           geom_line(aes(group=School, color=School)) +
           geom_point(color="white", size=10) +
           geom_hline(aes(yintercept=80), color="lightgray") +
           geom_text(aes_string(label=paste0('paste(',y.actual,'*100,"%",sep="")'), 
                                color="School"),
                     size=3) +
           scale_color_manual(values = c("#439539", "purple", "#C49A6C", "#60A2D7")) +
           facet_grid(Subject~Grade) +
           theme_bw() + 
           theme(legend.position="bottom") +
           xlab("School Year") +
           ylab(y.label) 
       } 
       if(input$selectDBCohort=="Class"){
         p<-ggplot(map.all.growth.sum.p[GrowthSeason==input$selectDBSeason & 
                                          Subject %in% input$selectDBSubject & 
                                          School!="Region" & 
                                          N.S1.S2>=10 &
                                        Class>=2019], 
                   aes_string(x='Grade', 
                              y=paste0(y.actual,'*100')
                   )
         ) + 
           geom_line(aes(group=School, color=School)) +
           geom_point(color="white", size=10) +
           geom_hline(aes(yintercept=80), color="lightgray") +
           geom_text(aes_string(label=paste0('paste(',y.actual,'*100,"%",sep="")'), 
                                color="School"),
                     size=3) +
           scale_color_manual(values = c("#439539", "purple", "#C49A6C", "#60A2D7" )) +
           scale_x_continuous(breaks=-1:9, labels=c(NA, "K", 1:8, NA)) +
           facet_grid(Subject~Class2) +
           theme_bw() + 
           theme(legend.position="bottom") +
           xlab("Grade") +
           ylab(y.label)   
       }
       
  # flush session if starting because data load is slower than visualization load 
  # (TO DO: check depenencies)
  if (values$starting) invalidateLater(1000, session)
  
  #Trysuppresses need atomic vector or data frame error on initial load from 
  # data not being loaded up yet
  try(print(p), silent = TRUE)
  
  setProgress(value = 9, detail="Drawing visualization!!!")
  
  })
  
  })

})
