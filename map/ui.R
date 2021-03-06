library(shiny)
require(DT)


schools <- list("Region", "KAP", "KAMS", "KCCP", "KBCP")
sys <- list("2015-2016", "2014-2015","2013-2014", "2012-2013", "2011-2012")
subjs <-  list("Mathematics", 
               "Reading",  
               "General Science"
               )
grades <- c(0:8)

seasons<-c("Spring - Spring",
           "Spring - Winter",
           "Fall - Spring",
           "Fall - Winter",
           "Winter - Spring",
           "Fall - Fall")

cols <- c("N (both seasons)", 
             "# >= Typical",  
             "% >= Typical", 
             "# >= College Ready",
             "% >= College Ready",
             "# >= 50th Pctl S1",
             "% >= 50th Pctl S1",
             "# >= 50th Pctl S2",
             "% >= 50th Pctl S2",
             "# >= 75th Pctl S1",
             "% >= 75th Pctl S1",
             "# >= 75th Pctl S2",
             "% >= 75th Pctl S2"
          )

cols.selected <- c("N (both seasons)", 
                   #"# >= Typical",  
                   "% >= Typical", 
                   #"# >= College Ready",
                   "% >= College Ready",
                   #"# >= 50th Pctl S1",
                   "% >= 50th Pctl S1",
                   #"# >= 50th Pctl S2",
                   "% >= 50th Pctl S2",
                   #"# >= 75th Pctl S1",
                   "% >= 75th Pctl S1",
                   #"# >= 75th Pctl S2",
                   "% >= 75th Pctl S2"
)

restart_time<-file.info('restart.txt')$mtime

update_time_stamp<-lubridate::stamp("NWEA MAP data last updated on Tuesday, September 14, 2001 at 4:41 pm")(restart_time)



shinyUI(fluidPage(
  tags$head( 
    tags$link(href='static/css/shinyprogress.css', rel="stylesheet", type="text/css")
    #tags$link(href='static/css/dataTables.tableTools.css', rel="stylesheet", type="text/css"), 
    #tags$script(src='static/js/jquery.dataTables.js'),
    #tags$script(src='static/js/dataTables.tableTools.js')
    ), 
  br(),
  p(em(update_time_stamp)),
  br(),
  tabsetPanel(
    tabPanel("Visualization",
             h3("MAP Performance over Time"),
             fluidRow(
               column(2,
                      selectInput(inputId="selectDBSeason",
                                  label="Growth Season:",
                                  choices=seasons,
                                  selected = "Spring - Winter"
                                  )
                      ),
               column(4,
                      selectInput(inputId="selectDBSubject",
                                  label="Subjects",
                                  choices=subjs,
                                  selected=c("Reading",
                                             "Mathematics"),
                                  multiple=TRUE
                                  )
                      ),
               column(4,
                      selectInput(inputId="selectDBStat",
                                  label="Statistic:",
                                  choices=c("Percent M/E Typical Growth" ="Pct.Typical",
                                            "Percent M/E College Ready Growth" = "Pct.CR",
                                            "Percent >= 50th %ile (season 2)"="Pct.50.S2",
                                            "Percent >= 75th %ile (season 2)"= "Pct.75.S2" 
                                            ),
                                  selected="Pct.Typical"
                                  )
                      ),
               column(2,
                      selectInput(inputId="selectDBCohort",
                                  label="View by:",
                                  choices=c("Grade",
                                            "Cohort (Class of...)" = "Class"
                                            ),
                                  selected="Grade"
                                  )
                      )
               ),
             fluidRow(
               column(12,
                      plotOutput("dashboard_panel")
                      )
               )
      ),
    tabPanel("Students Tested",
      h3("Winter 2015-2016 Students Rostered and Tested", span(class="label label-default","New")),
      fluidRow(column(12,
        dataTableOutput("dashboard_tested")
          )
        )
    ),
    tabPanel("School Summary Stats",
             h3("School-level Summary", span(class="label label-default","New")),
             fluidRow(
               column(4,
                      selectInput(inputId="selectSummSY", 
                                  label="Year(s) Selected:",
                                  choices=sys,
                                  selected="2015-2016",
                                  #type="select",
                                  multiple=TRUE
                                  )
                      ),
               column(4,
                      selectInput(inputId="selectSummSeason", 
                                  label="Growth Season(s) Selected:",
                                  choices=seasons,
                                  selected="Spring - Winter",
                                  #type="select",
                                  multiple=TRUE
                                  )
                      ),
               column(4,
                      selectInput(inputId="selectSummSubj", 
                                  label="Subject(s) Selected:",
                                  choices=subjs,
                                  selected=list("Mathematics", "Reading"),
                                  #type="select",
                                  multiple=TRUE
                                  )
                      )
               ),
      fluidRow(
        column(3,
               selectInput(inputId="selectSummSchool", 
                           label="School(s) Selected:",
                           choices=schools,
                           selected=c("KAP", "KAMS", "KCCP", "KBCP"),
                           #type="select",
                           multiple=TRUE
                           )
               ),
        column(3,
               selectInput(inputId="selectSummGrades", 
                           label="Grade(s) Selected:",
                           choices=grades,
                           selected=grades,
                           #type="select",
                           multiple=TRUE
                           )
               ),
        column(6,
               selectInput(inputId="selectSummCols", 
                           label="Columns Selected:",
                           choices=cols,
                           selected=cols.selected,
                           multiple=TRUE
                           )
               )
        ),
      fluidRow(
               column(12,
                      dataTableOutput("sum_table")
                      )
             )
      ),
    tabPanel("Historical Scores",
             h3("Historical Score Look-up", span(class="label label-default","New")),
             DT::dataTableOutput("hist_scores")
             ),
    tabPanel("Student Growth Data",
             h3("Student-level Summary", span(class="label label-default","New")),
             DT::dataTableOutput("main_table")
    )
    )
  )
)
