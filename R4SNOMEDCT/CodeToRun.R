library(tidyverse)
export_path<-'n:/snomed/'

load(file.path(export_path,'snomedct.rda'))



#-------------------definition of functions -------------------------


#' works only for the most recent release
getNewConcepts <- function(release){
  
  c2<-c %>% dplyr::filter(active==1,effectiveTime==release)
  names(c2)
  names(d2)
  c2 %>% left_join(d2,by=c('id'='conceptId'))
}



sct_search_text <-function(pattern){
  d2 %>% dplyr::filter(grepl(pattern=pattern,term,ignore.case = TRUE))
}

sct_search_conceptId <-function(whichConceptId){
  d2 %>% dplyr::filter(conceptId==whichConceptId)
}




#--------------------end of funcion definitions --------------------------------------------







#try some of these commands

#release='20180731'  
#aa<-getNewConcepts(release)
#aa %>% count(tag) %>% arrange(desc(n))
