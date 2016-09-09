library(pander)
library(dplyr)
library(tidyr)

conso<-read.delim(file = 'RXNCONSO.RRF', sep='|',header = F, stringsAsFactors=F)
conso[3:7] <- list(NULL)  #not used in this distribution, only in full UMLS

# column names are taken from http://www.nlm.nih.gov/research/umls/rxnorm/docs/2014/rxnorm_doco_full_2014-2.html#s12_4

names(conso)[1] <- 'rxcui'
names(conso)[3] <- 'rxaui'
names(conso)[7] <- 'sab'  #12
names(conso)[8] <- 'tty'  #13
names(conso)[10] <- 'str' #15
names(conso)[10] <- 'str' #15
names(conso)[12] <- 'suppress' #15 #obsolete items have flag O here, current items have flag N

#how many rows
nrow(conso)

#what are the types?
table(conso$tty)

#restrict to   rxnorm terms
sconso<-subset(conso,sab=='RXNORM')

#suppress column is very important
table(sconso$suppress)
