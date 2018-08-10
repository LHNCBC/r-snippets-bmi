library(tidyverse)
export_path<-'n:/snomed/'
options(scipen=999)

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

#' List all active concepts with effective time and semantic tag
sct_concept_list <-function(){
  c2<-c %>% dplyr::filter(active==1) %>%  left_join(d2,by=c('id'='conceptId'))
  c2
}

#' List all active concepts with a text definition
sct_get_definitions <- function(){
  c2<-c %>% dplyr::filter(active==1) %>%  left_join(d2,by=c('id'='conceptId'))
  names(textDefinition)
  names(c2)
  out<-textDefinition %>% dplyr::inner_join(c2,by=c('conceptId'='id')) %>% dplyr::arrange(tag)
  out
}

#--------------------end of funcion definitions --------------------------------------------






#try some of these commands

#options(scipen=999) #to display long codes as long numbers without scientific notation nonsense
#release='20180731'  
#aa<-getNewConcepts(release)
#aa %>% count(tag) %>% arrange(desc(n)) %>% write_csv('new.csv')
#aa %>% write_csv('new_concepts.csv')

#ab<-sct_concept_list()
#ab %>% write_csv('sct_pragmatic_concept_list.csv')
#ab %>% count(tag) %>% arrange(desc(n))
#ab<- filter(!tag %in% c('disorder','procedure','finding') )

#out <- sct_get_definitions()
#out %>% write_csv('TextDefinition2.csv')

#aa<-out %>% count(tag) %>% arrange(desc(n)) %>% as.data.frame()
#print(aa,row.names = FALSE)
