load('o:/athena/concept.rda')


lkup2<-concept %>% filter(vocabulary_id %in% c('ICD9Proc','HCPCS','ICD9CM','ICD10CM','ICD10PCS'))

lkup2  %<>% mutate(concept_code=str_replace(concept_code,'\\.',''))

#-------------------------------------------------------

lkup2 %>% write_csv('lkupALL.csv')
lkup2 %>% sample_n(200) %>% write_csv('lkupALL-preview.csv')
