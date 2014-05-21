# Test ui.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. This is derived from the war (weekly attendance
# report) data analysis project

library(shiny)
library(shinysky)


lastXweeks<-ymd(as.character(floor_date(today() -weeks(6), 
                                        unit="week")+1))
last6weeks <- as.character(lastXweeks)
thisweek <- as.character(today())
firstweek <- as.character(floor_date(DailyEnrollAttend[,min(WeekOfDate)]))




shinyUI(
  bootstrapPage(
    tags$head( 
      tags$link(href='static/css/shinyprogress.css', rel="stylesheet", type="text/css"), 
      tags$link(href='static/css/dataTables.tableTools.css', rel="stylesheet", type="text/css"), 
      tags$script(src='static/js/jquery.dataTables.js'),
      tags$script(src='static/js/dataTables.tableTools.js')
    ), 
    div(class="container-fluid",
        h3(paste("as of", title.date)),
        #busyIndicator("Counting students! Please be patient.", wait = 1000),
        tabsetPanel(
          tabPanel(title="Attendance",
                   tabsetPanel(
                     tabPanel(title="Daily",
                              h4("Daily Enrollment and Attendance by School"),
                              tabsetPanel(
                                tabPanel(title="Visualization", 
                                         div(class="alert alert-info",
                                             "The light gray lines shows total enrollment. ",
                                             "The green line demarcates 96% of enrollment ",
                                             "(i.e., our regional daily attendance goal). ",
                                             "The black line with dots represents actual attendance, ",
                                             "where the dot marks the given weekdays attendance. ",
                                             br(),
                                             span(class="label label-default", "New"),
                                             strong("You can now select the date range for the visualization!")
                                             ),
                                         dateRangeInput("attDates", 
                                                        "Select Dates:",
                                                        start=last6weeks,
                                                        end=thisweek,
                                                        min=firstweek,
                                                        max=thisweek
                                                        ),
                                         progressInit(),
                                         plotOutput("plotAttendEnroll",
                                                    height="600px")
                                         ),
                                tabPanel(title="Table",
                                         selectInput('schools', 
                                                            'Select School:', 
                                                            DailyEnrollAttend[,unique(School)],
                                                            selected = "KAMS", 
                                                            multiple=TRUE),
                                         div(class="table-condensed", 
                                             dataTableOutput("daily_attend")
                                             )
                                         )
                                )
                              ),
                     tabPanel(title = "YTD PowerSchool vs IMPACT",
                              h4("Powerschool vs. IMPACT* YTD Attendance"),
                              htmlOutput("impact"),
                              p(tags$small("*IMPACT data is only updated on the first day of each school week. ",  
                                           "Consequently, the ",
                                           strong("Difference"),
                                           "column will show wider discrepencies as the week progresses"
                              )
                              )
                     ),
                     tabPanel(title="Weekly & YTD",
                              h4("YTD and Weekly Average Daily Attendance"),
                              br(),
                              htmlOutput("WeeklyYTDAtt")
                              ),
                     tabPanel(title="Student Laggard Boards",
                              h4("Student Absence Leaders"),
                              br(),
                              dataTableOutput("StudentAtt")
                              )
                     )
                   ),
          tabPanel(title="HSR Transfers",
                   h3("Transfers"),
                   h4("Healthy Schools & Regions transfer totals"),
                   
                   div(class="alert alert-info", p("Cumulative transfers of students enrolled on October 1st of each year and transferred prior to September 20th of the next year. Students who enrolled ", strong("after October 1st"), " and transferred during the school year are ", strong("not included"), " in in these tables and figures.")),
                   tabsetPanel(
                     tabPanel(title="Visualization",
                              div(class="well", busyIndicator("Tracking students! Please be patient.", wait = 1000),
                              plotOutput("plotTransfers"))
                     ),
                     tabPanel(title="Summary Table",
                              dataTableOutput("xfersSummary")
                     ),
                     tabPanel(title="Student Details",
                              dataTableOutput("xfersStudents")
                              )
                   )
          ),
          tabPanel(title="Suspension Events",
                   h3("Suspension Events"),
                   #fluidRow(
                   #column(4,
                  #          selectInput('susp_input', 'Options', c("A","B","C"), multiple=TRUE, selectize=TRUE)
                   #         )
                   #),
                   dataTableOutput("suspensions")
                   ),
          tabPanel(title="Suspension Days",
                   h3("Suspensions Days"),
                   div(class="alert alert-info", p(strong('Note:'),'In this report the metric ', strong('suspensions'),  ' represents days absent due to suspensions and not the number of suspension events. That is, a student recieving a 3-day suspension would appear in this report as having 3 "suspensions" (i.e., three absenses due to being out of school serving a suspension).')),
                   tabsetPanel(
                     tabPanel(title="Weekly",
                              htmlOutput("suspWeeklyBySchool")
                     ),
                     tabPanel(title="Year-to-date",
                              htmlOutput("suspYTDByGrade")
                     ),
                     tabPanel(title="Suspension Leaders",
                              dataTableOutput("suspLeaders")
                     )
                   )
          )
        )
    )
  )
)