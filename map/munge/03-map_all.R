# Munging scricpt for map.all

#fix 2010-11 data which has kindergarten at KAMS

map.all.silo[map.all.silo$SchoolName=="KIPP Ascend Middle School" & (map.all.silo$Grade<5|map.all.silo$Grade=="K"),"SchoolName"]<-"KIPP Ascend Primary School"


map.all<-map.all.silo %>% 
  mutate(Season=str_extract(TermName, 
                            "[[:alpha:]]+"), 
         Year1=as.integer(str_extract(TermName, 
                           "[[:digit:]]+")), 
         Year2=as.integer(gsub("([a-zA-Z]+[[:space:]][[:digit:]]+-)([[:digit:]]+)",
                    "\\2", 
                    TermName)),
         SY=paste(Year1, Year2, sep="-"),
         Grade=ifelse(Grade=="K", 0, as.integer(Grade)),
         Grade=as.integer(Grade),
         CohortYear=Year2+(12-Grade),
         MeasurementScale = ifelse(grepl("General Science", MeasurementScale), 
                                   "General Science",
                                   MeasurementScale)
  ) %>%
  filter(Year1 >= 2010 & GrowthMeasureYN=='TRUE') %>%
  mutate(SchoolInitials   = abbrev(SchoolName, list(old="KAPS", new="KAP")), 
         TestQuartile     = kipp_quartile(TestPercentile),
         KIPPTieredGrowth = tiered_growth(TestQuartile, Grade)
         )

map.all<-cbind(map.all,
               mapvisuals::nwea_growth(map.all$Grade, 
                           map.all$TestRITScore, 
                           map.all$MeasurementScale
                           )
               )

          
# Create Seaason to Season Numbers

years<-unique(map.all$Year2)

map.SS<-rbind_all(lapply(years, 
                         mapvisuals::s2s_match, 
                         .data=map.all, 
                         season1="Spring", 
                         season2="Spring", 
                         typical.growth=T,
                         college.ready=T
                         )
                  )
map.FS<-rbind_all(lapply(years, 
                         mapvisuals::s2s_match,
                         .data=map.all, 
                         season1="Fall", 
                         season2="Spring", 
                         typical.growth=T,
                          college.ready=T
                                 )
                          )
map.FW<-rbind_all(lapply(years, 
                         mapvisuals::s2s_match, 
                        .data=map.all, 
                         season1="Fall", 
                         season2="Winter", 
                         typical.growth=T,
                         college.ready=T
                               )
                        )
map.WS<-rbind_all(lapply(years,
                         mapvisuals::s2s_match, 
                         .data=map.all, 
                         season1="Winter", 
                         season2="Spring", 
                         typical.growth=T,
                         college.ready=T
                         )
                  )
map.FF<-rbind_all(lapply(years, 
                         mapvisuals::s2s_match, 
                         .data=map.all, 
                         season1="Fall", 
                         season2="Fall", 
                         typical.growth=T,
                         college.ready=T
                         )
                  )

map.SW<-rbind_all(lapply(years, 
                         mapvisuals::s2s_match, 
                         .data=map.all, 
                         season1="Spring", 
                         season2="Winter", 
                         typical.growth=T,
                         college.ready=T
                         )
                  )

map.all.growth<-rbind_all(list(map.SS, map.FS, map.FW, map.WS, map.FF, map.SW))

rm(map.SS, map.FS, map.FW, map.WS, map.FF, map.SW)

map.all.growth.sum<-data.table(map.all.growth)[,list("N (both seasons)"= .N, 
                    "# >= Typical" = sum(MetTypical),  
                    "% >= Typical" = round(sum(MetTypical)/.N,2), 
                    "# >= College Ready" = sum(MetCollegeReady),
                    "% >= College Ready" = round(sum(MetCollegeReady)/.N,2),
                    "# >= 50th Pctl S1" = sum(TestPercentile>=50),
                    "% >= 50th Pctl S1" = round(sum(TestPercentile>=50)/.N,2),
                    "# >= 50th Pctl S2" = sum(TestPercentile.2>=50),
                    "% >= 50th Pctl S2" = round(sum(TestPercentile.2>=50)/.N,2),
                    "# >= 75th Pctl S1" = sum(TestPercentile>=75),
                    "% >= 75th Pctl S1" = round(sum(TestPercentile>=75)/.N,2),
                    "# >= 75th Pctl S2" = sum(TestPercentile.2>=75),
                    "% >= 75th Pctl S2" = round(sum(TestPercentile.2>=75)/.N,2)
                    ),
                    by=list(SY.2, 
                            GrowthSeason, 
                            SchoolInitials.2, 
                            Grade.2, 
                            CohortYear.2,
                            MeasurementScale)
                    ]

setnames(map.all.growth.sum, 
         c("SchoolInitials.2", "Grade.2", "MeasurementScale", "SY.2", "CohortYear.2"),
         c("School", "Grade", "Subject", "SY", "Class")
         )

map.all.growth.sum.reg<-data.table(map.all.growth)[,list("School"="Region",
                                             "N (both seasons)"= .N, 
                                         "# >= Typical" = sum(MetTypical),  
                                         "% >= Typical" = round(sum(MetTypical)/.N,2), 
                                         "# >= College Ready" = sum(MetCollegeReady),
                                         "% >= College Ready" = round(sum(MetCollegeReady)/.N,2),
                                         "# >= 50th Pctl S1" = sum(TestPercentile>=50),
                                         "% >= 50th Pctl S1" = round(sum(TestPercentile>=50)/.N,2),
                                         "# >= 50th Pctl S2" = sum(TestPercentile.2>=50),
                                         "% >= 50th Pctl S2" = round(sum(TestPercentile.2>=50)/.N,2),
                                         "# >= 75th Pctl S1" = sum(TestPercentile>=75),
                                         "% >= 75th Pctl S1" = round(sum(TestPercentile>=75)/.N,2),
                                         "# >= 75th Pctl S2" = sum(TestPercentile.2>=75),
                                         "% >= 75th Pctl S2" = round(sum(TestPercentile.2>=75)/.N,2)
                                         ),
                                       by=list(SY.2, 
                                               GrowthSeason, 
                                               Grade.2, 
                                               CohortYear.2,
                                               MeasurementScale)
                                       ]

setnames(map.all.growth.sum.reg, 
         c("Grade.2", "MeasurementScale", "SY.2", "CohortYear.2"),
         c("Grade", "Subject", "SY", "Class")
         )

map.all.growth.sum<-rbind(map.all.growth.sum,map.all.growth.sum.reg)
rm(map.all.growth.sum.reg)

map.all.growth.sum.p<-copy(map.all.growth.sum) # for plotting
setnames(map.all.growth.sum.p, 
         names(map.all.growth.sum.p),
         c("SY",
           "GrowthSeason",
           "School",
           "Grade",
           "Class",
           "Subject",
           "N.S1.S2",
           "N.Typical",
           "Pct.Typical",
           "N.CR",
           "Pct.CR",
           "N.50.S1",
           "Pct.50.S1",
           "N.50.S2",
           "Pct.50.S2",
           "N.75.S1",
           "Pct.75.S1",
           "N.75.S2",
           "Pct.75.S2"
           )
         )

map.all.growth.sum.p<-na.omit(map.all.growth.sum.p)

require(dplyr)
message("Class by current Grade")
class_current_grade<-map.all.growth.sum.p%>%
  group_by(Class) %>% 
  dplyr::summarize(Grade=max(Grade), N=n()) %>% 
  mutate(Class2=paste0(Class, 
                       "\n(Current grade: ", 
                       Grade, ")")
         ) %>%
  filter(N>20) %>%
  select(Class, Class2)

map.all.growth.sum.p<-left_join(map.all.growth.sum.p, 
                                 class_current_grade, 
                                 by="Class")

map.all.growth.sum.p<-as.data.table(map.all.growth.sum.p)
