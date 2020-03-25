user= username
psw= password

library(RPostgreSQL)
library(tidyverse)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)
tbls<-RPostgreSQL::dbListTables(con)
tbls

library(dplyr)

co_studies_q<-"select * FROM studies
where study_first_submitted_date > '2020-01-01' and (lower(official_title) like '%covid%' 
or lower(official_title) like '%sars-cov%'or lower(official_title) like '%2019-ncov%'or lower(official_title) like '%coronavirus%' )" 

co_studies<-dbGetQuery(con, co_studies_q)
intervent_q<-"select* from interventions"
intervent<-dbGetQuery(con, intervent_q)

facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)
count<-count(intervent$nct_id)
intervent2<-intervent %>% aggregate(  list(intervent$nct_id), paste, collapse="|")
facil2<-aggregate(facil, list(facil$nct_id), paste, collapse="|")
internctcount<-intervent%>% group_by(nct_id)%>%count()
intervent3<-left_join(intervent2,internctcount,by= c('Group.1'='nct_id'))
intervent4<-intervent3[,c(1,4:7)]
facilcount<-facil%>% group_by(nct_id)%>%count()
facil3<-left_join(facil2,facilcount,by= c('Group.1'='nct_id'))
facil4<-facil3[,c(1,4:10)]
colnames(facil4)[3] <- "facility_name"
colnames(intervent4)[3] <- "intervention_name"

covid8<-left_join(co_studies, intervent4, by = c('nct_id'= 'Group.1'))
covid9<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))

covid_int<-covid9%>%filter(study_type == 'Interventional')
covid_int %>%write_csv('covid_int_a.csv')

covid_obs<-covid9%>%filter(study_type == 'Observational')
covid_obs %>%write_csv('covid_obs_a.csv')

covid_registry<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid_registry %>%write_csv('covid_registry_a.csv')


