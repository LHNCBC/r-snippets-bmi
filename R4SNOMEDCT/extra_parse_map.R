fname='C:/snomed/SnomedCT_Icd11Map_DRAFT_20200930T120000Z/Full/Refset/Map/der2_iisssccRefset_Icd11MapExtendedMapFull_INT_20200930.txt'
#d=read_delim(fname,delim = '/t',quote='')
d=read.delim(fname)
nrow(d)
str(d)
names(d)
cl=sct_concept_list()
names(cl)
m=d %>% left_join(cl,by=c('referencedComponentId'='id'))
m %>% write_csv('c:/snomed/icd11map.csv')


#how many sct terms are mapped to same icd11 concept
aa=d %>% group_by(mapTarget) %>% summarise(n=n())
aa %>% count(n)

#all same correlation
d %>% count(correlationId)

sct_search_conceptId(447561005)
#not specified