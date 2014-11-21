# server.R for rCharts based implementation of Openstreet maps and 
# enrolled student information

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

####  Sessionwide Data ####
message('Starting logger')
require(log4r)
logger<-create.logger(logfile = "logs/enroll_server.log")

info(logger, "Loading packages")
require(shiny)
require(rCharts)
require(dplyr)
require(ggplot2)


info(logger, "Loading data")
load('data//gis_students.Rda')

info(logger, "Sourcing helper functions")
source("lib//gis_helpers.R")

#### Shiny Server output code ####
shinyServer(function(input, output, session) {

  # Student histogram ####
  output$geo_histogram <- renderPlot({
    stu_hist<-stu_school_distances %>%
      mutate(School=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP")),
             School2=ifelse(grepl("A", as.character(School)), "KACP", as.character(School)
             )
      )
    
    ggplot(stu_hist %>% filter(miles<10, grade_level>=5),
           aes(x=miles)
    ) + 
      geom_histogram(aes(y = ..density.., 
                         fill=School),
                     color="white", 
                     binwidth=1) + 
      facet_grid(School2~grade_level) + 
      scale_fill_manual(values = c("#7FC97F",
                                   "#BEAED4",
                                   "#FDC086",
                                   "#FFFF99"
      )
      ) +
      theme_bw() + 
      theme(legend.position="bottom")
    
    
  })
  
  # The MAP ####
  output$map_test <- renderMap({
    withProgress(message = 'Creating Map', value = 0, {
      incProgress(.33, detail = "Subsetting data")
      the_map<-plot_enroll_map(stus2 %>% filter(grade_level %in% input$grade,
                                                School %in% input$school), 
                               zoom_level=12,
                               center=data.frame(lat=41.8, lon=-87.6))
      incProgress(.66, detail = "Rendering map")
      the_map
      #incProgress(.99, detail = "Plotting students")
      
      })
    })

  
  }
)
