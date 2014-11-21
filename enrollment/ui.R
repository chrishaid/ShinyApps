# ui.R for Enorllment map page. 
require(shiny)
library(rCharts)
require(lubridate)

schools<-c("KAP", "KAMS", "KCCP", "KBCP")
grades<-c(0:8)


#getting last time data was updated
restart_time<-file.info('restart.txt')$mtime
update_time_stamp<-lubridate::stamp("Map shows students with geocodable addresses as of Tuesday, September 14, 2001 at 4:41 pm.")(restart_time)


shinyUI(bootstrapPage( 
  
  
  h4("The spatial distribution of our students"),
  h6(update_time_stamp),
  p(em("The map takes about 10 seconds to load; your patience will be rewarded!")),
  br(), 
  #tags$link(href='style.css', rel='stylesheet'),
  #tags$script(src='app.js'),
  #istyle the abosoulte panel
  tags$style('#controls {
  /* Appearance */
  background-color: white;
  padding: 0 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.65;
  zoom: 0.9;
  transition: opacity 500ms 1s;
  }
  #controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
  }'),
  absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 400, height = "auto",
                
                h2("School explorer"),
                
                selectInput("school", 
                            "School", 
                            schools,
                            selected = schools,
                            multiple = TRUE),
                selectInput("grade", 
                            "Grade", 
                            grades,
                            selected=c(5:8),
                            multiple=TRUE),
                plotOutput("geo_histogram", height = 250)
                #plotOutput("scatterCollegeIncome", height = 250)
                ),
  chartOutput('map_test', 'leaflet')
  )
)