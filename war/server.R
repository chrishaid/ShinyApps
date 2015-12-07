# Test server.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. 

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

att_goal = .97

####  Sessionwide Data ####
# Illuminate suspension

#load('data/discipline.Rdata')

# CPS Impact ####
message('Get IMPACT data from google spreadsheet')
googurl <- getURL(read.dcf('config//ps.dcf', fields='GOOG')[1])
impact <- read.csv(textConnection(googurl)) %>%
  dplyr::filter(School != "KAP (K)")

# PowerSchool/Impact Summary Table ####
message('Create PowerSchool/Impact summary table(s)')
message('Summarizing PowerSchool data')
message(' . . . by school')
ps.schools <- Attendance %>%
  group_by(School) %>%
  dplyr::summarize(PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1)) %>%
  dplyr::arrange(School)
  
#ps.schools<- Attendance[,list(PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1)), by=list(School)][order(School)]
message(' . . . by combining Ascend campuses')
ps.pm<-Attendance %>%
  filter(School=="KAMS"|School=="KAP") %>%
  summarize(PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1)) %>%
  mutate(School="KACP")
#ps.pm<-Attendance[School=="KAMS"|School=="KAP",list(School="KAP/KAMS",PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1))]

message(' . . . by rolling up the region')
ps.region<-Attendance %>%
  summarize(PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1)) %>%
  mutate(School="Region")
  
#ps.region<-Attendance[,list(School="Region",PowerSchool=round((1-(sum(Absent)/sum(Enrolled)))*100,1))]
message("Combining School, Ascend-only, and regional data")
ps.all<-rbind(ps.schools, ps.pm, ps.region)

message("Appending Impact Data")
impact<-cbind(ps.all, round(impact[2],1))

message("Calculating diff between impact data and PS data")
impact$Difference<-impact$IMPACT - impact$PowerSchool

message("Staring shiny server portion of code")
#### Shiny Server output code ####
shinyServer(function(input, output, session) {
  #Impact and PowerSchool Comparison
  message('Render impact table')
  
  output$impact <- renderPrint(kable(impact, 
                                      format='html', 
                                      table.attr='class="table table-responsive table-hover table-condensed table-striped shiny-html-output"', 
                                      output=TRUE
                                      )
                                )
  message('Begin building Daily Enrollement and Attendance plot')
  
  # Daily Enrollment & Attendance Plot ####
  lastXweeks<-ymd(as.character(floor_date(today() -weeks(6), 
                                          unit="week"
                                          )+1
                               )
                  )
  
  message("Render date text")
  output$last6weeks <- renderText(as.character(lastXweeks))
  output$thisweek <- renderText(as.character(floor_date(today(),unit="week")+1))
  output$firstweek <- renderText(as.character(floor_date(min(DailyEnrollAttend$WeekOfDate))))
                                   
  message("Munge DailyEnrollAttend.plotdata")                               
  DailyEnrollAttend.plotdata<-DailyEnrollAttend %>%
    mutate(Day=wday(Date),
           Enrolled96Pct=Enrolled*att_goal
           ) %>%
    as.data.frame
  #DailyEnrollAttend.plotdata$Day <- wday(DailyEnrollAttend.plotdata$Date)
  #DailyEnrollAttend.plotdata$Enrolled96Pct <- DailyEnrollAttend.plotdata$Enrolled*.96
  DailyEnrollAttend.plotdata.melt<-melt(DailyEnrollAttend.plotdata, 
                                        id=c("Date", "Day", "School", "WeekOfShortDateLabel"), 
                                      measure.vars=c("Enrolled", "Enrolled96Pct", "Present"))
  
  message("Munge DailyEnrollAttend.plotdata: factor lables")                               
  
  DailyEnrollAttend.plotdata.melt$variable<-factor(DailyEnrollAttend.plotdata.melt$variable, 
                                                   labels=c("Enrolled", 
                                                            sprintf("%s%% of Enrolled", round(att_goal*100)), 
                                                            "Attended")
                                                   )
  # HR only
  message("Munge DailyEnrollAttend_HR.plotdata")
  DailyEnrollAttend_HR.plotdata<-DailyEnrollAttendByHR %>%
    mutate(Day=wday(Date),
           Enrolled96Pct=Enrolled*att_goal
    ) %>%
    as.data.frame
  
  DailyEnrollAttend_HR.plotdata.melt<-melt(DailyEnrollAttend_HR.plotdata, 
                                        id=c("Date", "Day", "School", "Grade", "Home_Room", "WeekOfShortDateLabel"), 
                                        measure.vars=c("Enrolled", "Enrolled96Pct", "Present"))
  
  message("Munge DailyEnrollAttend._HRplotdata: factor labels")                               
  
  DailyEnrollAttend_HR.plotdata.melt$variable<-factor(DailyEnrollAttend_HR.plotdata.melt$variable, 
                                                   labels=c("Enrolled", 
                                                            sprintf("%s%% of Enrolled", round(att_goal*100)),
                                                            "Attended")
                                                   )  
 
  message("Ascertaining homerooms")
  
  output$grades_hrs <- renderUI({
    if(input$attSchoolvsHR=='hr') {
      if(is.null(input$school_hr)) return()
      homerooms<-DailyEnrollAttendByHR %>%
        filter(School==input$school_hr) %>%
        select(Grade) 
      
      grades <- unique(homerooms$Grade)
      
      selectInput("grades_hrs", 
                  "Choose Grade", 
                  choices=as.character(grades),
                  multiple=FALSE
      )
    }
  })
  
  
  output$home_rooms <- renderUI({
    if(input$attSchoolvsHR=='hr') {

      if(is.null(input$grades_hrs)) return()
      if(is.null(input$school_hr)) return()
      

      
      homerooms<-DailyEnrollAttendByHR %>%
        filter(School==input$school_hr,
               Grade==input$grades_hrs) %>%
        select(Home_Room) 
      
      homerooms <- unique(homerooms$Home_Room)
      
      selectInput("homerooms", 
                  "Choose Home Rooms", 
                  choices=as.character(homerooms),
                  selected=as.character(homerooms),
                  multiple=TRUE
                  )
    }
  })
  
  
  getDAE <- reactive({
    
    if(input$attSchoolvsHR!="hr"){
      DAE<-DailyEnrollAttend.plotdata.melt %>%
        filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2]))
      
      DAE.ytd.plot<-DailyEnrollAttend.plotdata.melt %>% as.data.frame %>%
        cast(Date + Day + School + WeekOfShortDateLabel ~ variable) %>% 
        as.data.frame %>%
        group_by(School) %>% 
        dplyr::mutate(Cum_Enrolled=order_by(Date, cumsum(Enrolled)), 
                      Cum_Attended=order_by(Date, cumsum(Attended)),
                      YTD_ADA = round(Cum_Attended/Cum_Enrolled*100,1),
                      ADA=round(Attended/Enrolled*100,1)
        )  %>%
        filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2]))
      
      DAE.ytd.plot.filtered <- DAE.ytd.plot %>%
        filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2])) %>%
        arrange(School, Date) 
      
      DAE.weekly.ada <- DAE.ytd.plot.filtered  %>%
        group_by(School, WeekOfShortDateLabel) %>%
        dplyr::summarise(Weekly_ADA=round(sum(Attended)/sum(Enrolled)*100,1),
                         y_pos=min(Attended)
        ) 
      
      DAE.weekly.ytd.plot<- DAE.ytd.plot.filtered %>%
        group_by(School, WeekOfShortDateLabel) %>%
        filter(Day==max(Day)) %>%
        select(School, WeekOfShortDateLabel, Day, YTD_ADA) %>%
        inner_join(DAE.weekly.ada, by=c("School", "WeekOfShortDateLabel")) %>%
        group_by(School) %>%
        dplyr::mutate(y_pos=min(y_pos),
                      x_pos=max(Day),
                      threshold=ifelse(Weekly_ADA>=att_goal*100, 
                                       sprintf("Weekly ≥ %s%%", round(att_goal*100)), 
                                       sprintf("Weekly < %s%%",  round(att_goal*100))
                                       )
                      )
      
      
      DAE.ytd.region<-DAE.ytd.plot %>% group_by(Date) %>%
        dplyr::summarize(Enrolled=sum(Enrolled),
                         Attended=sum(Attended),
                         Cum_Enrolled=sum(Cum_Enrolled),
                         Cum_Attended=sum(Cum_Attended)) %>%
        dplyr::mutate(ADA=Attended/Enrolled*100,
                      YTD_ADA=Cum_Attended/Cum_Enrolled*100) %>%
        filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2]))
      
      
      
      dt<-list(DAE=DAE,
               DAE.weekly.ytd.plot=DAE.weekly.ytd.plot,
               DAE.ytd.plot=DAE.ytd.plot,
               DAE.ytd.region=DAE.ytd.region
      )
      
      dt
    } else {
            
        if(is.null(input$homerooms)) return()
        if(is.null(input$grades_hrs)) return()
        if(is.null(input$school_hr)) return()
        if(is.null(input$attDates)) return()
        DAE<-DailyEnrollAttend_HR.plotdata.melt %>% 
          filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2]),
                 School==input$school_hr,
                 Grade==input$grades_hrs,
                 Home_Room %in% input$homerooms)
        
        DAE.ytd.plot<-DailyEnrollAttend_HR.plotdata.melt %>% 
          as.data.frame %>%
          cast(Date + Day + Grade + Home_Room + School + WeekOfShortDateLabel ~ variable) %>% 
          as.data.frame %>%
          group_by(School, Grade, Home_Room) %>% 
          dplyr::mutate(Cum_Enrolled=order_by(Date, cumsum(Enrolled)), 
                        Cum_Attended=order_by(Date, cumsum(Attended)),
                        YTD_ADA = round(Cum_Attended/Cum_Enrolled*100,1),
                        ADA=round(Attended/Enrolled*100,1)
          ) 
        
        DAE.ytd.plot.filtered <- DAE.ytd.plot %>%
          filter(Date >= ymd(input$attDates[1]) & Date <= ymd(input$attDates[2]),
                 School==input$school_hr,
                 Grade==input$grades_hrs,
                 Home_Room %in% input$homerooms) %>%
          arrange(School, Grade, Home_Room, Date) 
        
        DAE.weekly.ada <- DAE.ytd.plot.filtered  %>%
          group_by(School, Grade, Home_Room, WeekOfShortDateLabel) %>%
          dplyr::summarise(Weekly_ADA=round(sum(Attended)/sum(Enrolled)*100,1),
                           y_pos=min(Attended)
          ) 
        
        DAE.weekly.ytd.plot<- DAE.ytd.plot.filtered %>%
          group_by(School, WeekOfShortDateLabel) %>%
          filter(Day==max(Day)) %>%
          select(School, Grade, Home_Room, WeekOfShortDateLabel, Day, YTD_ADA) %>%
          inner_join(DAE.weekly.ada, by=c("School", "Grade", "Home_Room", "WeekOfShortDateLabel")) %>%
          group_by(School, Grade, Home_Room) %>%
          dplyr::mutate(y_pos=min(y_pos),
                        x_pos=max(Day),
                        threshold=ifelse(Weekly_ADA>=att_goal*100, 
                                         sprintf("Weekly ≥ %s%%", round(att_goal*100)), 
                                         sprintf("Weekly < %s%%",  round(att_goal*100))
                                         )
          )
        
        DAE.ytd.region<-DAE.ytd.plot %>% group_by(Date) %>%
          dplyr::summarize(Enrolled=sum(Enrolled),
                           Attended=sum(Attended),
                           Cum_Enrolled=sum(Cum_Enrolled),
                           Cum_Attended=sum(Cum_Attended)) %>%
          dplyr::mutate(ADA=Attended/Enrolled*100,
                        YTD_ADA=Cum_Attended/Cum_Enrolled*100)
        
        
        
        
        dt<-list(DAE=DAE,
                 DAE.weekly.ytd.plot=DAE.weekly.ytd.plot,
                 DAE.ytd.plot=DAE.ytd.plot,
                 DAE.ytd.region=DAE.ytd.region
        )
        
        dt
      
    }
    
    

   
   
  })
  
 # output$test_table <- renderDataTable({getDAE()$DAE})
  
 # can't plot a line with only one point (so need)
  
  message("Build DAE plot")
  ggAttend <- eventReactive(input$hrs_button, {
    # Test that input's have populatied with data
    # otherwise errors are displayed to user while shiny works through
    # all the reactive functions.
    
    withProgress(message = 'Graphing attendance  & enrollment', value = 0, {
      # Number of times we'll go through the loop
      #incProgress(.1, detail = "Counting Students") 
    
    message("get DAE")
    incProgress(.3, detail = "Getting data")
    DAE.list <- getDAE()
    #if(is.null(DAE.list$DAE)) return()
    DAE.weekly.ytd.plot<-DAE.list$DAE.weekly.ytd.plot 
    DAE<-DAE.list$DAE
    
    incProgress(.4, detail = "Calculting ADA")
    
    
    
    
    # Determine if there is only one day worth of data
    
    if(input$attSchoolvsHR=="hr"){
      
#       if(is.null(input$homerooms)) return()
#       if(is.null(input$grades_hrs)) return()
#       if(is.null(input$school_hr)) return()
      
      
    
    DAE_max_days<- DAE %>%
      filter(max(as.numeric(WeekOfShortDateLabel))==as.numeric(WeekOfShortDateLabel)
             )
    
    a_unqiue_school <- unique(DAE$School)[1]
    
    DAE_single_day_week <- 
      DAE %>% 
    #  dplyr::filter(variable=="Enrolled", School==a_unqiue_school) %>%
      dplyr::select(Date, WeekOfShortDateLabel) %>% 
      unique %>%
      dplyr::count(WeekOfShortDateLabel) %>%
      filter(n<2)
    
    DAE_multi_day_weeks <- anti_join(DAE, 
                                     DAE_single_day_week, 
                                     by="WeekOfShortDateLabel")
    
    } else {
      DAE_max_days<- DAE %>%
        filter(max(as.numeric(WeekOfShortDateLabel))==as.numeric(WeekOfShortDateLabel)
        )
      
      a_unqiue_school <- unique(DAE$School)[1]
      
      DAE_single_day_week <- 
        DAE %>% 
        #dplyr::filter(variable=="Enrolled", School==a_unqiue_school) %>%
        dplyr::select(Date, WeekOfShortDateLabel) %>%
        unique %>%
        dplyr::count(WeekOfShortDateLabel) %>%
        filter(n<2)
      
      DAE_multi_day_weeks <- anti_join(DAE, 
                                       DAE_single_day_week, 
                                       by="WeekOfShortDateLabel")
      
    }
    

    
    
    single_day_weeks <- nrow(DAE_single_day_week)>0
    #only_one_day <- length(unique(DAE_max_days$Date))==1
    
    if(single_day_weeks){
      p <- ggplot(DAE_multi_day_weeks, aes(x=Day, y=value))
      } else {
        p <- ggplot(DAE, aes(x=Day, y=value))
      }
    incProgress(.6, detail = "Constructing graph")
  p <- p + 
    geom_step(direction="hv", 
              aes(color=variable),
              size=1) + 
    geom_point(data=filter(DAE, variable=="Attended"), 
               color="black",
               size=3) +
     geom_text(data=DAE.weekly.ytd.plot,
               aes(x=x_pos, 
                   y=y_pos, 
                   label=paste0("YTD ADA: ",
                                YTD_ADA,
                                "\nWeekly ADA: ",
                                Weekly_ADA),
                   #alpha=Weekly_ADA>=96,
                   color=threshold
                   ),
               #color="orange",
               #alpha=.7,
              hjust=1,
              vjust=0,
              size=4,
              inherits.aes=FALSE
               ) +
    scale_x_continuous(breaks = c(2,3,4,5,6), labels=c("M","T","W","R","F")) + #Change numberd week days to lettered
    scale_y_continuous("# of Students") + 
    scale_colour_manual("", values=c("#439539", "black","#8D8685", "#E27425", "#439539")) 
    #scale_alpha_manual("Weekly ADA >= 96%", values=c(.4,1)) +
    if(input$attSchoolvsHR!="hr"){
      p <- p + facet_grid(School~WeekOfShortDateLabel, scales="free_y")   
    } else {
      
#       if(is.null(input$homerooms)) return()
#       if(is.null(input$grades_hrs)) return()
#       if(is.null(input$school_hr)) return()
      
      p <- p + facet_grid(Home_Room~WeekOfShortDateLabel, scales="free_y")
    }
    p <- p + theme_bw() + 
    theme(legend.position="bottom", 
          strip.text.x=element_text(size=12),
          strip.text.y=element_text(size=12),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text=element_text(size=12)
    )
  
  incProgress(.8, detail = "Constructing YTD graph")
  
  if(input$attSchoolvsHR!="hr"){ 
    DAE.ytd.plot<-DAE.list$DAE.ytd.plot %>% filter(School %in% input$traceSchools)
    p2 <-
      ggplot(DAE.ytd.plot,
             aes(x=Date, y=ADA)) + 
      geom_hline(data=as.data.frame(att_goal),
                 aes(yintercept=att_goal*100), color="darkblue") +
      geom_point(aes(color=School)) + 
      geom_smooth(aes(y=YTD_ADA, color=School), se=F, span=1) + 
      geom_smooth(data=DAE.list$DAE.ytd.region, 
                  aes(y=YTD_ADA, color=School, size=Enrolled),
                  se=F, 
                  span=1, 
                  color="black",
                  size=1) + 
      geom_text(data=DAE.ytd.plot %>% filter(Date==max(Date)), 
                aes(y=YTD_ADA, color=School, label=round(YTD_ADA,1)), 
                size=4,
                hjust=0, 
                alpha=.6) +
      geom_text(data=DAE.list$DAE.ytd.region %>% filter(Date==max(Date)), 
                aes(y=YTD_ADA, label=round(YTD_ADA,1)), 
                size=4,
                hjust=0, 
                alpha=.2) + 
      theme_bw()
  } else {

#     if(is.null(input$homerooms)) return()
#     if(is.null(input$grades_hrs)) return()
#     if(is.null(input$school_hr)) return()
    
    DAE.ytd.plot<-DAE.list$DAE.ytd.plot %>% 
      filter(School %in% input$school_hr,
             Grade %in% input$grades_hrs,
             Home_Room %in% input$homerooms)
    p2 <-
      ggplot(DAE.ytd.plot,
             aes(x=Date, y=ADA)) + 
      geom_hline(data=as.data.frame(att_goal),
                 aes(yintercept=att_goal*100), 
                 color="darkblue") +
      geom_point(aes(color=Home_Room)) + 
      geom_smooth(aes(y=YTD_ADA, color=Home_Room), se=F, span=1) + 
      geom_smooth(data=DAE.list$DAE.ytd.region, 
                  aes(y=YTD_ADA,  size=Enrolled),
                  se=F, 
                  span=1, 
                  color="black",
                  size=1) + 
      geom_text(data=DAE.ytd.plot %>% filter(Date==max(Date)), 
                aes(y=YTD_ADA, color=Home_Room, label=round(YTD_ADA,1)), 
                size=4,
                hjust=0, 
                alpha=.6) +
      geom_text(data=DAE.list$DAE.ytd.region %>% filter(Date==max(Date)), 
                aes(y=YTD_ADA, label=round(YTD_ADA,1)), 
                size=4,
                hjust=0, 
                alpha=.2) + 
      theme_bw()
    
  }
 
  
  
  incProgress(1, detail = "Drawing graph")
    })
  
  
  
  
  
  x<-list(p=p,
          p2=p2
       )
  x
  })
  
  message("Render DEA plot")
  
  output$plotAttendEnroll <- renderPlot({
       ggAttend()$p
    })
  


  output$plotYTDAttend <- renderPlot({
   ggAttend()$p2 
    }) 
  
  
  
  
  
  # Daily Attend Table ####
  message('Munge  daily enrollement/attendence data table')
 DAE_dt <- eventReactive(input$hrs_button, {
   DAE.list<-getDAE()
   DAE<-DAE.list$DAE.ytd.plot
   if(input$attSchoolvsHR=="hr"){
     x<-dplyr::filter(DAE, 
               School==input$school_hr,
               Grade==input$grades_hrs,
               Home_Room %in% input$homerooms)  %>%
       select(Date, 
               Week = WeekOfShortDateLabel,
               Grade,
               "Home Room" = Home_Room,
               Attended,
               Enrolled,
               ADA,
               "YTD ADA"= YTD_ADA
     )
   } else {
     x <-  select(DAE,
                  Date, 
                  Week = WeekOfShortDateLabel,
                  Attended,
                  Enrolled,
                  ADA,
                  "YTD ADA"= YTD_ADA
     ) 
   }
   x
   
 })
         
  #setnames(DailyEnrollAttend.dt, "PctAtt", "% Attending")
  message('Render  daily enrollement/attendence data table')
  output$daily_attend <- renderDataTable(as.data.frame(DAE_dt()),
                                         options = list(
                                           lengthMenu = list(c(5, 15, -1), 
                                                             c('5', '15', '50', 'All')
                                                             ),
                                           pageLength = 20)
                                         )
                                                 
                                                
                                           #DAE# %>%
                                            #mutate(PctAtt=round(PctPresent*100,1)) %>%
                                            #select(School,
                                            #       Date,
                                            #       Enrolled,
                                            #       Present,
                                            #       Absent,
                                            #       "% Attending" = PctAtt)
                                              #filter(DailyEnrollAttend.dt, 
                                              #   School %in% input$schools)
                                          #}, 
                                         #options = list(bSortClasses = TRUE,
                                        #                aLengthMenu = list(c(5,25, 50, 100, -1), 
                                         #                                  list(5,25,50,100,'All')
                                          #                                 ),
                                           #             iDisplayLength = 50,
                                            #            "sDom"='T<"clear">lfrtip',
                                             #           "oTableTools"=list(
                                              #            "sSwfPath"="static/swf/copy_csv_xls_pdf.swf"
                                                          #)
                                                        #)
                                         #)
  
  
 
 
 
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
  output$StudentAtt<-renderDataTable({select(AttByStudentBySchool,
                                             School, 
                                             Grade, 
                                             Student, 
                                             starts_with("ADA"), 
                                             Absences)
                                      },
                                     options = list(bSortClasses = TRUE,
                                                    aLengthMenu = list(c(10,20, 50, -1), 
                                                                       list(10,20,50,'All')
                                                                       ), 
                                                    iDisplayLength = 10,
                                                    "sDom"='T<"clear">lfrtip',
                                                    "oTableTools"=list(
                                                      "sSwfPath"="static/swf/copy_csv_xls_pdf.swf"
                                                      )
                                                    )
                                     )
  
  
  #### Student Transfers ####
  message("Student Transfers section")
  source('src//transfer_tables.R', local=TRUE)
  
  message("Binding Enrolled students tables")
  enrolled<-rbind(group_by(Enrolled.151001, SCHOOLID) %>%
                    dplyr::summarise(N=n()) %>% 
                    mutate(Year="SY15-16"), 
                  group_by(Enrolled.131001, SCHOOLID) %>% 
                    dplyr::summarise(N=n()) %>% 
                    mutate(Year="SY13-14"),
                  group_by(Enrolled.141001, SCHOOLID) %>% 
                    dplyr::summarise(N=n()) %>% 
                    mutate(Year="SY14-15")  
  ) %>% mutate(School=school_abbrev(SCHOOLID),
               School=factor(School, 
                             levels=c("KAP", "KAMS", "KCCP", "KBCP")
                             ),
               Year=as.factor(Year)
               )
  
#   enrolled$School<-mapply(function(x){ switch(as.character(x),
#                                               "7810" = "KAMS",
#                                               "78102" = "KAP",
#                                               "400146"= "KCCP",
#                                               "400163" = "KBCP"
#   )
#   },
#   enrolled$SCHOOLID
#   )
  
#   enrolled<-mutate(enrolled, 
#                    School=factor(School, 
#                                  levels=c("KAP", "KAMS", "KCCP", "KBCP")
#                    )
#   )
  
message("Combining transerfed and enrolled tables into xferplot")
  xferplot<-left_join(xferplot, 
                      select(enrolled, -SCHOOLID), 
                      by=c("School", "Year")
  ) %>% mutate(Year=as.factor(Year))
  
  xferplot <- mutate(xferplot, Pct=round(Value/HSR_Enrolled*100), Month=factor(Month, ordered=T))
  xferplot.nm <- mutate(xferplot.nm,  Month=factor(Month, ordered=T), Year=as.factor(Year))
  
  todays_month<-lubridate::month(today(), label = TRUE, abbr = TRUE)
  todays_month <- factor(as.character(todays_month), 
                                      levels=c("Oct", 
                                               "Nov", 
                                               "Dec", 
                                               "Jan", 
                                               "Feb", 
                                               "Mar",
                                               "Apr", 
                                               "May", 
                                               "Jun", 
                                               "Jul", 
                                               "Aug", 
                                               "Sep"),
                                      ordered=T)
  
                         
  #remove cumulative transfers passed this month
  message("Remove cumulative transfers passed this month")
  #xferplot2<-filter(xferplot,
  #                  !(Year=="SY14-15" & 
  #                        Month > todays_month & 
  #                        Variable=="Cumulative Transfers"
  #                    )
  #                  )
  
  #xferplot2.nm<-filter(xferplot.nm, 
  #                     !(Year=="SY14-15" & 
  #                        Month > todays_month & 
  #                        Variable=="Cumulative Transfers"
  #                       )
  #                     )

xferplot2<-filter(xferplot, !(is.na(Value) & Year=="SY15-16"))
xferplot2.nm<-filter(xferplot.nm,  !(is.na(Value) & Year=="SY15-16"))

  message("Creating Transfer plot")
  
  TransferPlot <- ggplot(data=filter(xferplot2, Variable=="Ceiling"), 
         aes(x=Month, y=Value)) + 
    geom_area(data=filter(xferplot2, Variable!="Ceiling"), 
              aes(x=Month, y=CumVal, fill=School, group=School), 
              stat='identity',
              #fill="#439539", 
              width=.5, 
              alpha=.4) + 
    geom_area(data=filter(xferplot2.nm, Variable!="Ceiling"), 
              aes(x=Month, y=CumVal, fill=School, group=School), 
              stat='identity',
              #fill="#439539", 
              width=.5, 
              alpha=1) + 
    geom_line(aes(group=Variable), color="#E27425") + 
    geom_text(data=filter(xferplot2, Variable!="Ceiling"), 
              aes(x=Month, 
                  y=Value, 
                  group=Variable, 
                  label=paste0(Value,"\n(",Pct,"%)")), 
              size=3,
              vjust=0) +
    geom_text(data=filter(xferplot2.nm, Variable!="Ceiling"), 
              aes(x=Month, 
                  y=Value, 
                  group=Variable, 
                  label=Value), 
              size=3,
              vjust=1) +
    facet_grid(Year~School, scale="free_y", as.table=FALSE) +
    scale_fill_manual(values = c("purple",  #KCCP 
                                 "#439539", #KAMS
                                 "#60A2D7", #KCCP
                                 "#C49A6C"  #KBCP
    )
    ) + theme_bw() + 
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size=10,angle=45, hjust=1),
          axis.text.y = element_text(size=10),
          strip.text = element_text(size=12),
          legend.position="none"
    )     
  
  
#   TransferPlot <- ggplot(data=subset(xferplot, Variable=="Ceiling"), 
#                          aes(x=Month, y=Value)) + 
#     geom_line(aes(group=Variable), color="#E27425") + 
#     geom_bar(data=subset(xferplot, Variable!="Ceiling"), 
#              aes(x=Month, y=Value, fill=School), 
#              stat='identity',
#              #fill="#439539", 
#              width=.5) + 
#     geom_text(data=subset(xferplot, Variable!="Ceiling"), 
#               aes(x=Month, y=Value-.5, group=Variable, label=Value), 
#               size=3,
#               vjust=1) +
#     facet_grid(Year~School, scale="free_y") +
#     scale_fill_manual(values = c("purple",  #KCCP 
#                                  "#439539", #KAMS
#                                  "#60A2D7", #KCCP
#                                  "#A7CFEE"  #KBCP
#                                  )
#                       ) + 
#     theme_bw() + 
#     theme(axis.title = element_blank(),
#           axis.text.x = element_text(size=10,angle=45, hjust=1),
#           axis.text.y = element_text(size=10),
#           strip.text = element_text(size=12),
#           legend.position="none"
#           )     
#   
  
  
  
  output$plotTransfers<-renderPlot(print(TransferPlot))
  
  output$xfersSummary <- renderPrint(kable(Xfer.table,
                                     format='html', 
                                     table.attr='class="table table-responsive table-hover table-condensed table-striped shiny-html-output"', 
                                     output=TRUE)
  )
  #renderDataTable(Xfer.table) #,
                                         #options = list(#ordering = TRUE,
                                          #              lengthMenu = list(c(10,20, 50, -1), 
                                           #                                list(10,20,50,'All')), 
                                            #           pageLength = 10
                                             #           )
  #)                                         
  
  output$xfersStudents <- renderDataTable(Xfer.students.table,
                                          options = list(ordering = TRUE,
                                                         lengthMenu = list(c(10,20, 50, -1), 
                                                                           list(10,20,50,'All')
                                                         ), 
                                                         pageLength = 10,
                                                         "dom"='T<"clear">lfrtip',
                                                         "tableTools"=list("sSwfPath"="static/swf/copy_csv_xls_pdf.swf"
                                                                           )
                                                         )
                                          )
  
  ### Suspensions ####
 load("data/suspensions.Rdata")

message("Loading Illuminate suspensions data")

SY1314 <- interval(ymd("130801"), ymd("140731"))
SY1415 <- interval(ymd("140801"), ymd("150731"))

susp.dt <-susp %>%
  mutate(Referrer = paste(staff_last_name, 
                          staff_first_name),
         SY_1 = "SY15-16",
         SY_2 = ifelse(date_assigned %within% SY1415, "SY14-15", SY_1),
         SY = ifelse(date_assigned %within% SY1314, "SY13-14", SY_2),
         grade_level_1 = ifelse(SY=="SY13-14", as.integer(grade_level)-2, as.integer(grade_level)),
         grade_level=ifelse(SY=="SY14-15", as.integer(grade_level)-1, as.integer(grade_level_1))
         ) %>%
  select(-SY_1, -SY_2, -grade_level_1) %>%
  arrange(desc(date_assigned)) %>%
  select("School Year" = SY,
         School,
        "First Name" = stu_first_name,
         "Last Name" = stu_last_name,
         "Grade" = grade_level,
         "Violation" = description,
         "Date Assigned" = date_assigned,
         "Consequence" = code_translation,
         "Duration" = length,
         Referrer) %>%
  as.data.frame 
  

output$suspensions_viz <- renderPlot({
  p<-ggplot(as.data.frame(susp_plot_data), aes(x=Month, y=Cum_N)) + 
    geom_step(data=susp_plot_data %>% 
                group_by(SY, School, Type) %>% 
                filter(n()>=2), 
              aes(color=SY, group=SY), direction="hv") + 
    geom_point(aes(color=SY)) +
    geom_text(aes(y=Cum_N+1.5,
                  color=SY,
                  label=Cum_N), 
              vjust=0,
              size=4) +
    facet_grid(Type~School) +
    scale_color_manual(values = c("#CFCCC1",
                                  "#C49A6C", 
                                  "#17345B")) +
    theme_bw()
  
  p
  })


 output$suspensions<-renderDataTable(susp.dt,
                                     options = list(
                                       lengthMenu = list(c(5, 15, -1), 
                                                         c('5', '15', 'All')
                                                         ),
                                       pageLength = 20)
                                      )

  
  
    
}
)