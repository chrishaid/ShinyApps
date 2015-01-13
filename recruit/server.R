# Test server.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. 

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

####  Sessionwide Data ####
# CPS Impact ####
message('Get recruitement and application data from google spreadsheet')
apps.googurl <- getURL(read.dcf('config//ps.dcf', fields='APPS')[1])
apps <- read.csv(textConnection(apps.googurl))

apps<-apps %>% mutate(Focus = ((Schl=="KAP" & Gr =="K") |
                        (Schl=="KAMS" & Gr== "6") | 
                        (Schl=="KAMS" & Gr== "6") |
                        (Schl=="KCCP" & Gr %in% c("5","6")) |
                        (Schl=="KBCP" & Gr %in% c("5","6"))) 
                        ) 

message('Munchge recruitment /  applications table')
#apps<-data.table(apps)
colnames(apps) <- c("School", "Grade", "AppsYTD.1516","AppsYTD.1415", "AppsYTD.1314", "Seats.available", "Seats.filled", "Focus")

regs<-apps %>% select(School, Grade, Seats.available, Seats.filled)


# Applications ####
apps<-apps %>% select(School, Grade, AppsYTD.1516, AppsYTD.1415, AppsYTD.1314, Focus) 
  
apps_grouped <- group_by(apps, School) 
apps_schools <- apps_grouped %>%
  summarize(AppsYTD.1516=sum(AppsYTD.1516),
            AppsYTD.1415=sum(AppsYTD.1415), 
            AppsYTD.1314=sum(AppsYTD.1314)) %>%
  mutate(Grade="All",
         Focus=FALSE)

app_chi <- apps_schools  %>%
  summarize(AppsYTD.1516=sum(AppsYTD.1516),
            AppsYTD.1415=sum(AppsYTD.1415), 
            AppsYTD.1314=sum(AppsYTD.1314)) %>%
  mutate(School="KIPP Chicago", Grade="All", Focus=FALSE)

apps<-rbind(apps, app_chi, apps_schools) %>%
  mutate(Diff_1516_1415=AppsYTD.1516 - AppsYTD.1415,
         Diff_1516_1314=AppsYTD.1516 - AppsYTD.1314,
         School=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP", "KIPP Chicago")),
         Grade=factor(Grade, levels=c("K", "1", "2", "3", "4", "5", "6", "7", "8", "All"))
         )

apps_tbl <- dplyr::rename_(apps, 
                       "YTD 2015-16"="AppsYTD.1516",
                       "YTD 2014-15"="AppsYTD.1415",
                       "YTD 2013-14"="AppsYTD.1314",
                       "Diff (1 yr)" = "Diff_1516_1415",
                       "Diff (2 yr)" = "Diff_1516_1314") %>%
  arrange(School, Grade)

ggApps<-ggplot(apps %>% filter(Grade!="All"), 
               aes(x=Grade, y=Diff_1516_1314)) + 
  geom_bar(aes(fill=Diff_1516_1314>=0), 
           stat="identity") + 
  scale_fill_manual(values=c("firebrick", "#439539")) + 
  facet_grid(.~School, 
             scales="free_x", 
             space="free_x") + 
  theme_bw() + 
  ylab("Differnece (2015-16 and 2013-14)")


regs_on<-FALSE
if (regs_on){
  # Registration ####
  message("Getting regs data")
  
  regs_grouped <- group_by(regs, School) 
  
  regs_schools <- regs_grouped %>%
    summarize(Seats.available=sum(Seats.available),
              Seats.filled=sum(Seats.filled)) %>%
    mutate(Grade="All")
  
  regs_chi <- regs_schools %>%
    summarize(Seats.available=sum(Seats.available),
              Seats.filled=sum(Seats.filled)) %>%
    mutate(School="KIPP Chicago", Grade="All")
  
  regs<-rbind(regs, regs_schools, regs_chi)
  
  regs <- regs %>%
    mutate(Pct.filled=round(Seats.filled/Seats.available*100,1),
           School=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP", "KIPP Chicago")),
           Grade=factor(Grade, levels=c("K", "1", "2", "3", "4", "5", "6", "7", "8", "All"))
    ) %>%
    dplyr::rename_("Seats Filled" = "Seats.filled",
                   "Seats Available"  = "Seats.available",
                   "Percent Filled" = "Pct.filled") %>%
    arrange(School, Grade)
  
  regs2<-regs
  
  colnames(regs2)[3:5] <- c("Available", "Filled", "Pct")
  
  ggRegs <-
    ggplot(regs2 %>% filter(Grade %in% c("K", "5")), 
           aes(x=Grade, y=Available)) + 
    geom_bar(fill=NA, 
             color="black", 
             stat="identity") +
    geom_bar(aes(y=Filled),
             fill="black", 
             color="black", 
             stat="identity") +
    geom_text(aes(x=Grade, y=Available-3, 
                  label=paste0("Percent Filled\n= ", Pct, "%")
    ),
    vjust=1
    ) +
    #scale_fill_manual(values=c("firebrick", "#439539")) + 
    facet_grid(.~School, 
               scales="free_x", 
               space="free_x") + 
    theme_bw() + 
    ylab("Seats")
  
}


#### Shiny Server output code ####
shinyServer(function(input, output) {
  
  apps_table <- reactive(
    if (input$focusGrades){
      apps_tbl %>% 
        filter(Focus==TRUE) %>%
        select(-Focus)
      } else {
        apps_tbl %>% 
          select(-Focus)
        })
  
  
  apps_plot_data <- reactive(
    if (input$focusGrades) {
      apps %>% 
      filter(Focus==TRUE)
      } else {
        apps %>% 
          select(-Focus)
        }
    )
  
  
  
  output$tblApps<-renderDataTable(apps_table(),
                           options = list(bSortClasses=TRUE,
                                          "sDom"='T<"clear">lfrtip',
                                          "oTableTools"=list(
                                          "sSwfPath"="static/swf/copy_csv_xls_pdf.swf")
                                          )
                           )
  
  output$plotApps<-renderPlot(ggApps %+% apps_plot_data())
  
  if (regs_on){
    output$tblRegs<-renderDataTable(regs,
                                    options = list(bSortClasses=TRUE,
                                                   "sDom"='T<"clear">lfrtip',
                                                   "oTableTools"=list(
                                                     "sSwfPath"="static/swf/copy_csv_xls_pdf.swf")
                                                   )
                                    )
    
    output$plotRegs<-renderPlot(print(ggRegs))
  }
  
  
  
  
  }
)
