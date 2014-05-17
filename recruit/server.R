# Test server.R Script for tabbed attendance data usuing the DataTable javascript package 
# implemented with Shiny Server. 

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

####  Sessionwide Data ####
# CPS Impact ####
message('Get recruitement and application data from google spreadsheet')
apps.googurl <- getURL(read.dcf('config//ps.dcf', fields='APPS')[1])
apps <- read.csv(textConnection(apps.googurl))

message('Munchge recruitment /  applications table')
apps<-data.table(apps)
setnames(apps, c("School", "Grade", "AppsYTD.1415", "AppsYTD.1314", "Seats.available", "Seats.filled"))

regs<-apps[,list(School, Grade, Seats.available, Seats.filled)]


# Applications ####
apps<-apps[, list(School, Grade, AppsYTD.1415, AppsYTD.1314)]
apps<-rbind(apps, apps[,list(School="KIPP Chicago", 
                             Grade="All", 
                             AppsYTD.1415=sum(AppsYTD.1415), 
                             AppsYTD.1314=sum(AppsYTD.1314)
)
]
)

apps<-rbind(apps, 
            apps[,list(Grade="All",  
                       AppsYTD.1415=sum(AppsYTD.1415), 
                       AppsYTD.1314=sum(AppsYTD.1314)),
                 by=list(School)][School!="KIPP Chicago"])


apps[,Difference:=AppsYTD.1415 - AppsYTD.1314]
            
apps[,School:=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP", "KIPP Chicago"))]
apps[, Grade:=factor(Grade, levels=c("K", "1", "2", "3", "4", "5", "6", "7", "8", "All"))]

setnames(apps, c("AppsYTD.1415", "AppsYTD.1314"), c("YTD 2014-15", "YTD 2013-14"))

apps<-apps[order(School, Grade)]

# Registration ####
message("Getting regs data")
regs<-data.table(regs)


regs<-rbind(regs, regs[,list(School="KIPP Chicago", 
                             Grade="All", 
                             Seats.available=sum(Seats.available), 
                             Seats.filled=sum(Seats.filled)
                             )
                       ]
            )

regs<-rbind(regs, 
            regs[,list(Grade="All",  
                       Seats.available=sum(Seats.available), 
                       Seats.filled=sum(Seats.filled)
                       ),
                 by=list(School)][School!="KIPP Chicago"])


regs[,Pct.filled:=round(Seats.filled/Seats.available*100,1)]

regs[, School:=factor(School, levels = c("KAP", "KAMS", "KCCP", "KBCP", "KIPP Chicago"))]
regs[, Grade:=factor(Grade, levels=c("K", "1", "2", "3", "4", "5", "6", "7", "8", "All"))]

setnames(regs, 
         c("Seats.filled", "Seats.available", "Pct.filled"), 
         c("Seats Filled", "Seats Available", "Percent Filled"))

regs<-regs[order(School, Grade)]

ggApps<-ggplot(apps[Grade!="All"], 
       aes(x=Grade, y=Difference)) + 
  geom_bar(aes(fill=Difference>=0), 
           stat="identity") + 
  scale_fill_manual(values=c("firebrick", "#439539")) + 
  facet_grid(.~School, 
             scales="free_x", 
             space="free_x") + 
  theme_bw() + 
  ylab("Differnece (2014-15 and 2013-14)")


regs2<-copy(regs)

setnames(regs2, 
         c("Seats Available", "Seats Filled",  "Percent Filled"), 
         c("Available", "Filled", "Pct")
        )

ggRegs <-
  ggplot(regs2[Grade %in% c("K", "5")], 
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




#### Shiny Server output code ####
shinyServer(function(input, output) {
  output$tblApps<-renderDataTable(apps,
                           options = list(bSortClasses=TRUE,
                                          "sDom"='T<"clear">lfrtip',
                                          "oTableTools"=list(
                                          "sSwfPath"="static/swf/copy_csv_xls_pdf.swf")
                                          )
                           )
  
  output$tblRegs<-renderDataTable(regs,
                           options = list(bSortClasses=TRUE,
					                                "sDom"='T<"clear">lfrtip',
                                          "oTableTools"=list(
                                          "sSwfPath"="static/swf/copy_csv_xls_pdf.swf")
					                                )
                           )
  
  output$plotApps<-renderPlot(print(ggApps))
  
  output$plotRegs<-renderPlot(print(ggRegs))
  }
)
