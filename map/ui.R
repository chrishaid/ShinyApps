library(shiny)
require(shinysky)

shinyUI(bootstrapPage(
  
  tabsetPanel(
    
    tabPanel("Waterfalls",
             h3("Waterfall Charts", span(class="label label-default","New")),
             sidebarPanel(uiOutput("schools"),
             uiOutput("subjects"),
             uiOutput("grades")),
             mainPanel(
               busyIndicator("Chasing waterfalls! Please be patient.", wait = 1000),
               plotOutput(outputId = "main_plot", height = "1000px")
             )),
    tabPanel("KIPPChi Summary Stats",
             h3("Regional Summary", span(class="label label-default","New")),
             dataTableOutput("reg_sum_table")
             ),
    tabPanel("School Summary Stats",
             h3("School-level Summary", span(class="label label-default","New")),
             dataTableOutput("sum_table")
             ),
    tabPanel("Student Data",
             h3("Student-level Summary", span(class="label label-default","New")),
             dataTableOutput("main_table")
             )
    )
  )
)
