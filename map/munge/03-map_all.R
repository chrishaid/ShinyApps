# Munging scricpt for map.all


# quick function to extract school initials
schoolInitials<-function(x){
  x.out<- gsub("(K)IPP|\\s([A-Z])[a-z]+", 
               "\\1\\2", 
               x)
  x.out[x.out=="KAPS"]<-"KAP"
  x.out
}


map.all<-map.all %.% 
  mutate(Season=str_extract(TermName, 
                            "[[:alpha:]]+"), 
         Year1=str_extract(TermName, 
                           "[[:digit:]]+"), 
         Year2=gsub("([a-zA-Z]+[[:space:]][[:digit:]]+-)([[:digit:]]+)",
                    "\\2", 
                    TermName),
         CohortYear=(12-Grade)+as.numeric(Year2)
  ) %.%
  filter(Year1 >= 2007 & GrowthMeasureYN=='TRUE') %.%
  mutate(SchoolInitials=schoolInitials(SchoolName), SY=paste(Year1, Year2, sep="-"))