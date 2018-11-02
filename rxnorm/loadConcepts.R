#this file loads RxNorm conceps only
#it uses a prepared file, not all columns were renamed nicely


library(tidyverse)
rxnorm<-read_rds('rxnorm.rds')

names(rxnorm)
nrow(rxnorm)

rxnorm %>% count(sab)
