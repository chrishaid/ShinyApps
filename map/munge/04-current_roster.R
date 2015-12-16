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

names(current.roster)<-tolower(names(current.roster))

current.roster<-current.roster %>%
  dplyr::mutate(School=id_to_initials(schoolid),
                Grade=grade_level) %>%
  dplyr::group_by(School, Grade)

grade.summary<-dplyr::summarise(current.roster, N=n())


current.term<-
  dplyr::filter(map.all, TermName=="Winter 2015-2016") %>% 
  dplyr::mutate(School=mapvisuals::abbrev(SchoolName, 
                                          exceptions=list(old="KAPS", 
                                                          new="KAP"))
         ) %>% 
  dplyr::group_by(School, Grade, MeasurementScale)

current.tests<-dplyr::summarise(current.term, N.tested=n())

test.summary<-dplyr::left_join(grade.summary, current.tests, by=c("School", "Grade")) %>%
  dplyr::mutate(Pct.tested=round(N.tested/N*100,1))
        
tested.summary<-dplyr::ungroup(test.summary) %>%  
  dplyr::mutate(School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))) %>%
  dplyr::arrange(School, Grade, MeasurementScale) %>% 
  dplyr::select(School, Grade, MeasurementScale, N, N.tested, Pct.tested)

setnames(tested.summary, c("MeasurementScale","N", "N.tested", "Pct.tested"),
         c("Subject", "# Rostered", "# Tested", "% Tested")
)





