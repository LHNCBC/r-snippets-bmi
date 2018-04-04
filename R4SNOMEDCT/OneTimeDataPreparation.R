

#modify to point to downloaded SNOMED CT release files
folder<-'n:/snomed/SnomedCT_InternationalRF2_PRODUCTION_20180131T120000Z/Snapshot/Terminology'
library(tidyverse)

#specify again the release substring
release='20180131'



#END OF CUSTOMIZATION
#run code below without any modification

library(tidyverse)
options(scipen=999)
FSN_CID=900000000000003001 #fsn

#concepts
c<-read_delim(file.path(folder,'sct2_Concept_Snapshot_INT_20180131.txt'),delim = '\t',col_types = 'dcidc')

#descriptions

d<-read_delim(file.path(folder,
                        paste0('sct2_Description_Snapshot-en_INT_',release,'.txt'))
              ,delim='\t',col_types = 'dciddcdcd',quote = "")


#another view of descriptions
d2<-d %>% filter(typeId==FSN_CID) %>% filter(active==1) %>% select(conceptId,term) 

#IMPORTANT extract semantic tag from concepts using RegEx 
d2$tag<-sub(".*\\((.*)\\).*", "\\1", d2$term)



#load relationship

r<-read_delim(file.path(folder,
                        paste0('sct2_Relationship_Snapshot_INT_',release,'.txt'))
              ,delim='\t',quote = "",col_types = 'dcidddcddd')



rm(folder)
rm(release)
save.image('snomedct.rda')