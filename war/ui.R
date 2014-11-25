# Test ui.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. This is derived from the war (weekly attendance
# report) data analysis project

library(shiny)
#library(shinysky)


lastXweeks<-ymd(as.character(floor_date(today() -weeks(6), 
                                        unit="week")+1))
last6weeks <- as.character(lastXweeks)
thisweek <- as.character(today())
firstweek <- as.character(floor_date(min(DailyEnrollAttend$WeekOfDate)))


restart_time<-file.info('restart.txt')$mtime

update_time_stamp<-lubridate::stamp("Attendance data last updated on Tuesday, September 14, 2001 at 4:41 pm")(restart_time)


shinyUI(
  bootstrapPage(
    tags$head( 
      #tags$link(href='static/css/shinyprogress.css', rel="stylesheet", type="text/css"), 
      #tags$link(href='static/css/dataTables.tableTools.css', rel="stylesheet", type="text/css"), 
      #tags$script(src='static/js/jquery.dataTables.js'),
      #tags$script(src='static/js/dataTables.tableTools.js'),
      # Following tags needed to surpress errors that are caused be reactives
      # loading too quickly.  During debugging it should be commented out
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }") #
    ), 
    div(class="container-fluid",
        br(),
        p(em(update_time_stamp)),
        br(),
        #busyIndicator("Counting students! Please be patient.", wait = 1000),
        navlistPanel(
          tabPanel(title="Attendance",
                   tabsetPanel(
                     tabPanel(title="Daily",
                              h4("Daily Enrollment and Attendance by School"),
                              fluidRow(
                                column(4,
                                       dateRangeInput("attDates", 
                                                      "Select Dates:",
                                                      start=last6weeks,
                                                      end=thisweek,
                                                      min=firstweek,
                                                      max=thisweek
                                       )
                                ),
                                column(2,
                                       radioButtons("attSchoolvsHR",
                                                    "Show attendance by:",
                                                    c("School"="school",
                                                      "Home Room" = "hr"),
                                                    select="school"
                                       )
                                ),
                                column(2, 
                                       conditionalPanel(
                                         condition="input.attSchoolvsHR == 'hr'",
                                         selectInput("school_hr",
                                                     "Select School",
                                                     choices = c("KAP",
                                                                 "KAMS",
                                                                 "KCCP",
                                                                 "KBCP"),
                                                     multiple=FALSE,
                                                     selected="KAP"
                                         )
                                       )
                                ),
                                column(2,uiOutput("grades_hrs")),
                                column(2, uiOutput("home_rooms"))
                              ),
                              tabsetPanel(
                                tabPanel(title="Enrollment & Attendance", 
                                         fluidRow(
                                           column(12, 
                                                  plotOutput("plotAttendEnroll",
                                                            height="600px")
                                                  )
                                           )
                                         ),
                                tabPanel("ADA Trace Graph",
                                         h4("YTD ADA"),
                                         em("Black traces regional average daily attendance."),
                                         conditionalPanel(
                                           condition="input.attSchoolvsHR == 'school'",
                                           selectInput(inputId = "traceSchools",
                                                       label =  "Choose Schools:",
                                                       choices = c("KAP",
                                                                   "KAMS",
                                                                   "KCCP",
                                                                   "KBCP"),
                                                       multiple=TRUE,
                                                       selected=c("KAP",
                                                                  "KAMS",
                                                                  "KCCP",
                                                                  "KBCP")
                                                       )
                                                    ),
                                         plotOutput("plotYTDAttend")
                                         ),
                                        
                                tabPanel(title="Table", 
                                         dataTableOutput("daily_attend")    
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
          tabPanel(title="Transfers",
                   h3("Transfers"),
                   h4("Healthy Schools & Regions transfer totals"),
                   div(class="alert alert-info", 
                       p("Cumulative transfers of students enrolled on",
                            "October 1st of each year and transferred prior to ",
                            "September 20th of the next year. Students who enrolled ", 
                            strong("after October 1st"), 
                            " and transferred during the school year are ", 
                            strong("not included"), " in in these tables and figures."),
                       p(strong("Darker colors"), " indicate the cumulative number of transfers for ",
                         "reasons other than moving. ", strong("Lighter colors"),  " indicate ",
                         "the additional transfers for students who moved. ",
                         "The ", strong("angled orange lines"), " indicate the rate ",
                         "at which a 10 percent attrition rate accumulates; it is an indicator ", 
                         "if we are on track (below the line) or off track (above the line) in a given month.")
                       ),
                   tabsetPanel(
                     tabPanel(title="Visualization",
                              plotOutput("plotTransfers")
                     ),
                     tabPanel(title="Summary Table",
                              htmlOutput("xfersSummary")
                     ),
                     tabPanel(title="Student Details",
                              dataTableOutput("xfersStudents")
                              )
                   )
          ),
          tabPanel(title="Suspensions",
                   h3("Suspensions"),
                   #fluidRow(
                   #column(4,
                  #          selectInput('susp_input', 'Options', c("A","B","C"), multiple=TRUE, selectize=TRUE)
                   #         )
                   #),
                  plotOutput("suspensions_viz"),
                  hr(),
                   dataTableOutput("suspensions")
                   ),
          widths = c(2, 10),
          well=FALSE
        )
    )
  )
)