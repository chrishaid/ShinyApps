# Test ui.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. This is derived from the war (weekly attendance
# report) data analysis project

library(shiny)





shinyUI(
  bootstrapPage(
   # includeCSS("/var/www/css//bootstrap.min.css"),
    #includeScript("/var/www/js/bootstrap.min.js"),
    div(class="container-fluid",
        h3(paste("as of", title.date)),
       div(class="alert alert-warning alert-dismissable", 
           strong("Note well"), 
           " that this page can take some time to load.  Please be patient.",
        tag("button", 
            list("class"="close", 
                 "data-dismiss"="alert", 
                 "aria-hidden"="true",
                 HTML("&times;")
                )
           )
        ),
        tabsetPanel(
          tabPanel(title="Attendance",
                   h4("Powerschool vs. IMPACT* YTD Attendance"),
                   htmlOutput("impact"),
                   p(tags$small("*IMPACT data is only updated on the first day of each school week. ",  
                                "Consequently, the ",
                                strong("Difference"),
                                "column will show wider discrepencies as the week progresses"
                               )
                    ),
                   br(),
                   tabsetPanel(
                     tabPanel(title="Daily",
                              h4("Daily Enrollment and Attendance by School"),
                              tabsetPanel(
                                tabPanel(title="Visualization", 
                                         plotOutput("plotAttendEnroll",
                                                    height="600px")
                                         ),
                                tabPanel(title="Table",
                                         checkboxGroupInput('schools', 
                                                            'Select School:', 
                                                            DailyEnrollAttend[,unique(School)],
                                                            selected = "KAMS"),
                                         div(class="table-condensed", 
                                             dataTableOutput("daily_attend")
                                             )
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
                   
                   div(class="alert alert-info", p("Cumulative transfers of students enrolled on October 1st of each year and transferred prior to September 20th of the next year. Students who enrolled ", strong("after October 1st"), " and transferred during the school year are ", strong("not included"), " in in these tables and figures.")),
                   tabsetPanel(
                     tabPanel(title="Visualization",
                              plotOutput("plotTransfers")
                     ),
                     tabPanel(title="Summary Table",
                              dataTableOutput("xfersSummary")
                     ),
                     tabPanel(title="Student Details",
                              dataTableOutput("xfersStudents")
                              )
                   )
          ),
          tabPanel(title="Suspension Days",
                   h3("Suspensions"),
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