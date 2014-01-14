# Test server.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. 

####  Sessionwide Data ####
# CPS Impact ####
message('Get IMPACT data from google spreadsheet')
googurl <- getURL(read.dcf('config//ps.dcf', fields='GOOG')[1])
impact <- read.csv(textConnection(googurl))

# PowerSchool/Impact Summary Table ####
message('Create PowerSchool/Impact summary table(s)')
message('Summarizing PowerSchool data')
ps.schools<- Attendance[,list(PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1)), by=list(School)]
ps.pm<-Attendance[School=="KAMS"|School=="KAP",list(School="KAP/KAMS",PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1))]
ps.region<-Attendance[,list(School="Region",PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1))]

ps.all<-rbind(ps.schools, ps.pm, ps.region)

message("Appending Impact Data")
impact<-cbind(ps.all, round(impact[2],1))

impact$Difference<-impact$Impact - impact$PowerSchool



#### Shiny Server output code ####
shinyServer(function(input, output) {
  #Impact and PowerSchool Comparison
  output$impact <- renderPrint(kable(impact, 
                                      format='html', 
                                      table.attr='class="table table-responsive table-hover table-condensed table-striped shiny-html-output"', 
                                      output=TRUE
                                      )
                                )
  # Daily Enrollment & Attendance Plot ####
  lastXweeks<-ymd(as.character(floor_date(today() -weeks(6), 
                                          unit="week")+1
                               )
                  )
  
  DailyEnrollAttend.plotdata<-subset(DailyEnrollAttend, Date>=lastXweeks)
  DailyEnrollAttend.plotdata$Day <- wday(DailyEnrollAttend.plotdata$Date)
  DailyEnrollAttend.plotdata$Enrolled96Pct <- DailyEnrollAttend.plotdata$Enrolled*.96
  DailyEnrollAttend.plotdata.melt<-melt(DailyEnrollAttend.plotdata, 
                                        id=c("Date", "Day", "School", "WeekOfShortDateLabel"), 
                                        measure.vars=c("Enrolled", "Enrolled96Pct", "Present"))
  
  DailyEnrollAttend.plotdata.melt$variable<-factor(DailyEnrollAttend.plotdata.melt$variable, 
                                                   labels=c("Enrolled", 
                                                            "96% of Enrolled", 
                                                            "Attended")
                                                   )
  
  DAE.dt<-as.data.table(DailyEnrollAttend.plotdata.melt)
  
  # can't plot a line with only one point (so need)
  if(DAE.dt[max(as.numeric(WeekOfShortDateLabel))==as.numeric(WeekOfShortDateLabel), 
            length(unique(Date))]==1){
    p <- ggplot(DAE.dt[Date<max(Date),], aes(x=Day, y=value))
  }
  else p <- ggplot(DAE.dt, aes(x=Day, y=value))
  p <- p + 
    geom_step(direction="hv", 
              aes(color=variable),
              size=1) + 
    geom_point(data=DAE.dt[variable=="Attended"], 
               color="black",
               size=3) +
    scale_x_continuous(breaks = c(2,3,4,5,6), labels=c("M","T","W","R","F")) + #Change numberd week days to lettered
    scale_y_continuous("# of Students") + 
    scale_colour_manual("", values=c("#8D8685", "#439539", "black")) +
    facet_grid(School~WeekOfShortDateLabel, scales="free_y") +
    theme_bw() + 
    theme(legend.position="bottom", 
          strip.text.x=element_text(size=12),
          strip.text.y=element_text(size=12),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text=element_text(size=12)
    )
  output$plotAttendEnroll <- renderPlot(print(p))
  
  # Daily Attend Table ####
  DailyEnrollAttend.dt <- copy(DailyEnrollAttend[,list(
                                              School,
                                              Date,
                                              Enrolled,
                                              Present,
                                              Absent,
                                              PctAtt=round(PctPresent*100,1))]
                               )
  setnames(DailyEnrollAttend.dt, "PctAtt", "% Attending")
  output$daily_attend <- renderDataTable({DailyEnrollAttend.dt[School %in% input$schools]
                                          }, 
                                         options = list(bSortClasses = TRUE,
                                                        aLengthMenu = c(5,25, 50, 200), 
                                                        iDisplayLength = 5)
                                         )
  
  # Weekly & YTD ADA ####
  AttRateByWeekBySchool.table<-xtable(arrange(AttRateByWeekBySchool.table, 
                                              desc(AttRateByWeekBySchool.table[["Week of"]]
                                                   )
                                              )
                                      )
  
  output$WeeklyYTDAtt <- renderPrint(kable(AttRateByWeekBySchool.table, 
                                                  format='html', 
                                                  table.attr='class="table table-responsive table-hover table-condensed table-striped shiny-html-output"', 
                                                  output=TRUE
                                           )
                                     )
  output$StudentAtt<-renderDataTable({AttByStudentBySchool[,c("School", 
                                                               "Grade", 
                                                               "Student", 
                                                               "ADA", 
                                                               "ADA (prior month)", 
                                                               "Absences"), 
                                                            with=FALSE]
                                      },
                                     options = list(bSortClasses = TRUE,
                                                    aLengthMenu = c(10,20, 50), 
                                                    iDisplayLength = 10)
                                     )
  
  
  #### Student Transfers ####
  source('src//transfer_tables.R', local=TRUE)
  
  TransferPlot <- ggplot(data=subset(xferplot, Variable=="Ceiling"), 
                         aes(x=Month, y=Value)) + 
    geom_line(aes(group=Variable), color="#E27425") + 
    geom_bar(data=subset(xferplot, Variable!="Ceiling"), 
             aes(x=Month, y=Value, fill=School), 
             stat='identity',
             #fill="#439539", 
             width=.5) + 
    geom_text(data=subset(xferplot, Variable!="Ceiling"), 
              aes(x=Month, y=Value-.5, group=Variable, label=Value), 
              size=3,
              vjust=1) +
    facet_grid(Year~School, scale="free_y") +
    scale_fill_manual(values = c("purple",  #KCCP 
                                 "#439539", #KAMS
                                 "#60A2D7", #KCCP
                                 "#A7CFEE"  #KBCP
                                 )
                      ) + 
    theme_bw() + 
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size=10,angle=45, hjust=1),
          axis.text.y = element_text(size=10),
          strip.text = element_text(size=12),
          legend.position="none"
          )     
  
  output$plotTransfers<-renderPlot(print(TransferPlot))
  
  output$xfersSummary <- renderDataTable({Xfer.table},
                                         options = list(bSortClasses = TRUE)
  )
  
  output$xfersStudents <- renderDataTable({Xfer.students.table},
                                          options = list(bSortClasses = TRUE)
                                          )
  
  ### Suspensions ####
  source('src/suspension_tables.R', local=TRUE)
  
  output$suspWeeklyBySchool <- renderPrint(kable(WeeklySuspensionsBySchool.xtable, 
                                                     format='html', 
                                                     table.attr='class="table table-responsive table-hover table-condensed table-striped"', 
                                                     output=TRUE,  
                                                     row.names=FALSE)
                                               )
  
  output$suspLeaders <- renderDataTable(Sups.leaders)
  
  output$suspYTDByGrade <- renderPrint(kable(YTDSuspensionsByGradeBySchool.xtable, 
                                                     format='html', 
                                                     table.attr='class="table table-responsive table-hover table-condensed table-striped"', 
                                                     output=TRUE,  
                                                     row.names=FALSE))
  
    
}
)