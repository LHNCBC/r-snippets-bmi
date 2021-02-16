library(tidyverse)

#specify the release substring
#release='20180131'
release='20180731'
release='20180731'
release='20210131'

#modify the start of the path  point to downloaded SNOMED CT release files
base_folder<-'c:/snomed/SnomedCT_InternationalRF2_PRODUCTION_' #release goes here
suffix<-'T120000Z/Snapshot/Terminology'

#note how the path is constructed
folder<-paste0(base_folder,release,suffix)

#check if it is correct
folder


export_folder<-'c:/snomed/'
#---------------end of specifying user entered parameters ----------------









#END OF CUSTOMIZATION
#run code below without any modification

library(tidyverse)
options(scipen=999)
FSN_CID=900000000000003001 #fsn

#concepts
c<-read_delim(file.path(folder,paste0('sct2_Concept_Snapshot_INT_',release,'.txt')),delim = '\t',col_types = 'dcidc')

#descriptions

d<-read_delim(file.path(folder,
                        paste0('sct2_Description_Snapshot-en_INT_',release,'.txt'))
              ,delim='\t',col_types = 'dciddcdcd',quote = "")

#definitions
textDefinition<-read_delim(file.path(folder,
                        paste0('sct2_TextDefinition_Snapshot-en_INT_',release,'.txt'))
              ,delim='\t',quote = "")




#another view of descriptions
d2<-d %>% filter(typeId==FSN_CID) %>% filter(active==1) %>% select(conceptId,term) 

#IMPORTANT extract semantic tag from concepts using RegEx 
d2$tag<-sub(".*\\((.*)\\).*", "\\1", d2$term)



#load relationship

r<-read_delim(file.path(folder,
                        paste0('sct2_Relationship_Snapshot_INT_',release,'.txt'))
              ,delim='\t',quote = "",col_types = 'dcidddcddd')



rm(folder)
#rm(release)

save.image(file.path(export_folder,'snomedct.rda'))
