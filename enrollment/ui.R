# ui.R for Enorllment map page. 
require(shiny)
library(rCharts)

schools<-c("KAP", "KAMS", "KCCP", "KBCP")
grades<-c(0:8)

shinyUI(bootstrapPage( 
  
  
  h4("The spatial distribution our our students"),
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
                            selected=grades,
                            multiple=TRUE),
                plotOutput("geo_histogram", height = 250)
                #plotOutput("scatterCollegeIncome", height = 250)
                ),
  chartOutput('map_test', 'leaflet')
  )
)