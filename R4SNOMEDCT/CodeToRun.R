library(tidyverse)
load('snomedct.rda')

#works only for the most recent release
getNewConcepts <- function(release){
  
  c2<-c %>% dplyr::filter(active==1,effectiveTime==release)
  c2
  
}
  
#getNewConcepts('20180131')
