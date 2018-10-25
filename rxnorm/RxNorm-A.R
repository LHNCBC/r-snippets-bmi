library(tidyverse)

#local copy of zip file
zipfile<-'c:/q/net/RxNorm_full_10012018.zip'

#after UMLS login, it can be obtained from here
url<-'https://download.nlm.nih.gov/umls/kss/rxnorm/RxNorm_full_10012018.zip'


#where we will unzip it
folder='local/temp'

#prepare target folder
fullfolder=file.path(getwd(),folder);dir.create(fullfolder);print(fullfolder)

#unzip the file (and get listing of them)
filenames <- unzip(zipfile, exdir=fullfolder,list=TRUE)
filenames







filename<-file.path(fullfolder, 'rrf','RXNCONSO.RRF')

conso<-read_delim(filename,delim = '|',col_names = F)
conso[3:7] <- list(NULL)  #per documentation - (no value provided) - 5 columns removed  - not used in this distribution, only in full UMLS

# column names are taken from http://www.nlm.nih.gov/research/umls/rxnorm/docs/2014/rxnorm_doco_full_2014-2.html#s12_4
#https://www.nlm.nih.gov/research/umls/rxnorm/docs/2016/rxnorm_doco_full_2016-1.html#s12_0

names(conso)[1] <- 'rxcui'
names(conso)[3] <- 'rxaui'
names(conso)[7] <- 'sab'  #12 source abbreviation
names(conso)[8] <- 'tty'  #13
names(conso)[10] <- 'str' #15
names(conso)[12] <- 'suppress' #15 
#Suppressible flag. Values = N, O, Y, or E. 
#N - not suppressible. O - Specific individual names (atoms) set as Obsolete because the name is no longer provided by the original source. Y - Suppressed by RxNorm editor. E - unquantified, non-prescribable drug with related quantified, prescribable drugs. NLM strongly recommends that users not alter editor-assigned suppressibility.

#how many concepts we see (number of rows)
nrow(conso)

#which sources we see
conso %>% count(sab) %>% arrange(desc(n))


# A tibble: 15 x 2
# sab              n
# <chr>        <int>
#  1 RXNORM      318717
#  2 MTHSPL      166630
#  3 NDFRT       142246
#  4 NDDF        111712
#  5 SNOMEDCT_US  98101
#  6 MMSL         82288
#  7 MMX          72851
#  8 VANDF        62427
#  9 GS           36731
# 10 MSH          23307
# 11 DRUGBANK     23185
# 12 ATC           6816
# 13 USP           3828
# 14 CVX            642
# 15 MTHCMSFRF        8


#filter only RxNorm concepts (beta code)
rxnNames<-conso %>% filter(sab=='RXNORM') %>% filter(tty!='SY') %>%  filter(tty!='TMSY') %>% filter(tty!='PSN')
nrow(rxnNames)
