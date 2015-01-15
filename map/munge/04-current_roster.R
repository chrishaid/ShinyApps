# munge current roster to get %  complete

id_to_initials <- function(schoolid){
  inner<-function(x) {switch(as.character(x),
            "78102"   = "KAP",
            "7810"    = "KAMS",
            "400146"  = "KCCP",
            "400163"  = "KBCP")
  }
  out<-sapply(schoolid, inner)
  out
}
current.roster<-filter(current.roster, Enroll_Status==0) %.%
  mutate(School=id_to_initials(SchoolID)) %.%
  group_by(School, Grade) 

grade.summary<-dplyr::summarise(current.roster, N=n())


current.term<-
  dplyr::filter(map.all, TermName=="Winter 2014-2015") %.% 
  mutate(School=abbrev(SchoolName, exceptions=list(old="KAPS", new="KAP"))
         ) %>% 
  group_by(School, Grade, MeasurementScale)

current.tests<-dplyr::summarise(current.term, N.tested=n())

test.summary<-left_join(grade.summary, current.tests, by=c("School", "Grade")) %.%
  mutate(Pct.tested=round(N.tested/N*100,1))
        
tested.summary<-ungroup(test.summary) %.%  
  mutate(School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))) %.%
  arrange(School, Grade, MeasurementScale) %.% 
  select(School, Grade, MeasurementScale, N, N.tested, Pct.tested)

setnames(tested.summary, c("MeasurementScale","N", "N.tested", "Pct.tested"),
         c("Subject", "# Rostered", "# Tested", "% Tested")
)





