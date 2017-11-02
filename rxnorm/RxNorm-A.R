

library(tidyverse)



fname='RxNorm_full_10022017/rrf/RXNCONSO.RRF'
folder='RxNorm_full_10022017/rrf'

conso<-read_delim(fname,delim = '|',col_names = F)
conso[3:7] <- list(NULL)  #not used in this distribution, only in full UMLS

# column names are taken from http://www.nlm.nih.gov/research/umls/rxnorm/docs/2014/rxnorm_doco_full_2014-2.html#s12_4
#https://www.nlm.nih.gov/research/umls/rxnorm/docs/2016/rxnorm_doco_full_2016-1.html#s12_0

names(conso)[1] <- 'rxcui'
names(conso)[3] <- 'rxaui'
names(conso)[7] <- 'sab'  #12
names(conso)[8] <- 'tty'  #13
names(conso)[10] <- 'str' #15
names(conso)[12] <- 'suppress' #15 #obsolete items have flag O here, current items have flag N


nrow(conso)

#filter only RxNorm concepts (beta code)
rxnNames<-conso %>% filter(sab=='RXNORM') %>% filter(tty!='SY') %>%  filter(tty!='TMSY') %>% filter(tty!='PSN')
