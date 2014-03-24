library(shiny)
require(shinysky)
schools <- list("KIPP Chicago", "KAP", "KAMS", "KCCP", "KBCP")
sys <- list("2013-2014", "2012-2013", "2011-2012")
subjs <-  list("Mathematics", 
               "Reading", 
               "Language Usage", 
               "General Science", 
               "Science - Concepts and Process")
grades <- c("K", 1:8)

cols <- c("Total Tested F & W", 
             "# >= Typical NWEA",  
             "% >= Typical NWEA", 
             "# >= Typical Tracker",  
             "% >= Typical Tracker", 
             "# >= College Ready NWEA",
             "% >= College Ready NWEA",
             "# >= College Ready Tracker",
             "% >= College Ready Tracker",
             "# >= 50th Percentile Fall",
             "% >= 50th Percentile Fall",
             "# >= 50th Percentile Winter",
             "% >= 50th Percentile Winter",
             "# >= 75th Percentile Fall",
             "% >= 75th Percentile Fall",
             "# >= 75th Percentile Winter",
             "% >= 75th Percentile Winter"
          )

cols.selected <- c("Total Tested F & W", 
         #"# >= Typical NWEA",  
          "% >= Typical NWEA", 
          #"# >= Typical Tracker",  
          #"% >= Typical Tracker", 
          #"# >= College Ready NWEA",
          "% >= College Ready NWEA",
          #"# >= College Ready Tracker",
          #"% >= College Ready Tracker",
          #"# >= 50th Percentile Fall",
          "% >= 50th Percentile Fall",
          #"# >= 50th Percentile Winter",
          "% >= 50th Percentile Winter",
          #"# >= 75th Percentile Fall",
          "% >= 75th Percentile Fall",
          #"# >= 75th Percentile Winter",
          "% >= 75th Percentile Winter"
)

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
    tabPanel("School Summary Stats",
             h3("School-level Summary", span(class="label label-default","New")),
             div(class="row-fluid ",
                 div(class="well container-fluid",
                     div(class="container span3",
                         selectInput(inputId="selectSummSchool", 
                                      label="School(s) Selected:",
                                      choices=schools,
                                      selected=schools,
                                     # type="select",
                                      multiple=TRUE
                                  )
                         ),
                     div(class="container span3",
                         selectInput(inputId="selectSummSY", 
                                      label="Year(s) Selected:",
                                      choices=sys,
                                      selected="2013-2014",
                                      #type="select",
                                      multiple=TRUE
                                      )
                         ),
                     div(class="container span3",
                         selectInput(inputId="selectSummSubj", 
                                      label="Subject(s) Selected:",
                                      choices=subjs,
                                      selected=list("Mathematics", "Reading"),
                                      #type="select",
                                      multiple=TRUE
                                      )
                         ),
                     div(class="container span3",
                         selectInput(inputId="selectSummGrades", 
                                      label="Grade(s) Selected:",
                                      choices=grades,
                                      selected=grades,
                                      #type="select",
                                      multiple=TRUE
                                      )
                         ),
                     p(),
                     div(class="container span3",
                         p(),p(),
                         helpText("You can select different columns in this table using Command+clicking (Mac) or Control+clicking (PC):")
                         ),
                     div(class="container span3",
                         selectInput(inputId="selectSummCols", 
                                      label="Columns Selected:",
                                      choices=cols,
                                      selected=cols.selected,
                                      multiple=TRUE
                         )
                     )
                     )
                 ),
             dataTableOutput("sum_table")
             ),
    tabPanel("Student Data",
             h3("Student-level Summary", span(class="label label-default","New")),
             dataTableOutput("main_table")
             )
    )
  )
)
