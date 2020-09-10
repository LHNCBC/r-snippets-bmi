regCovid Tutorial
================

*Craig Mayer, Vojtech Huser*  
*Lister Hill National Center for Biomedical Communications, National
Library of Medicine, NIH, Bethesda, MD*

# Using regCovid\_code\_for\_analysis.R

## Connecting to AACT Database

To connect to the AACT data base first create a username and password at
<https://aact.ctti-clinicaltrials.org/connect>

library(RPostgreSQL)  
library(tidyverse)  
drv \<- dbDriver(‘PostgreSQL’)  
con \<- dbConnect(drv,
dbname=“aact”,host=“aact-db.ctti-clinicaltrials.org”, port=5432,
user= user, password= psw)

## Query AACT for COVID-19 Studies

co\_studies\_q\<-“select \* FROM studies where
study\_first\_submitted\_date \> ‘2019-12-27’ and
(lower(official\_title) like ‘%covid%’ or lower(official\_title) like
‘%sars-cov%’or lower(official\_title) like’%2019-ncov%’ or
lower(official\_title) like ‘%cov2%’or lower(official\_title)
like’%cov-2%’ or lower(official\_title) like ‘%coronavirus%’or
lower(official\_title) like’%corona virus%’ )”

co\_studies\<-dbGetQuery(con, co\_studies\_q)

# Using individual regCOVID repository static data files

Reading in the list of studies by type:

covid9\<-read\_csv(url(‘<https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_all_studies-a.csv>’))  
covid9\_int\<-read\_csv(url(‘<https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_int_a.csv>’))  
covid9\_obs\<-read\_csv(url(‘<https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_obs_a.csv>’))  
covid9\_reg\<-read\_csv(url(‘<https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_registry_a.csv>’))
