user=username
psw=password

library(RPostgreSQL)
library(tidyverse)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)
tbls<-RPostgreSQL::dbListTables(con)
tbls

library(dplyr)
#-Book A: Search Method A (in title of study)
co_studies_q<-"select * FROM studies
where study_first_submitted_date > '2020-01-01' and (lower(official_title) like '%covid%' 
or lower(official_title) like '%sars-cov%'or lower(official_title) like '%2019-ncov%'
or lower(official_title) like '%coronavirus%'or lower(official_title) like '%corona virus%' )" 

co_studies<-dbGetQuery(con, co_studies_q)

intervent_q<-"select* from interventions"
intervent<-dbGetQuery(con, intervent_q)
colnames(intervent)[4] <- "intervention_name"
intervent2<-intervent %>% aggregate(  list(intervent$nct_id), paste, collapse="|")
internctcount<-intervent%>% group_by(nct_id)%>%count()
intervent3<-left_join(intervent2,internctcount,by= c('Group.1'='nct_id'))
intervent4<-intervent3[,c(1,4:7)]
colnames(intervent4)[5] <- "intervention_count"


facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)
facil2<-aggregate(facil, list(facil$nct_id), paste, collapse="|")
facilcount<-facil%>% group_by(nct_id)%>%count()
facil3<-left_join(facil2,facilcount,by= c('Group.1'='nct_id'))
facil4<-facil3[,c(1,4:10)]
colnames(facil4)[3] <- "facility_name"
colnames(facil4)[8] <- "site_count"


covid8<-left_join(co_studies, intervent4, by = c('nct_id'= 'Group.1'))
covid9<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))
covid9 %>% write_csv('regCovid_all_studies-a.csv')

#----Chapter 1: Covid studies by type
covid_int<-covid9%>%filter(study_type == 'Interventional')
covid_int %>%write_csv('regCovid_int_a.csv')

covid_obs<-covid9%>%filter(study_type == 'Observational')
covid_obs %>%write_csv('regCovid_obs_a.csv')

covid_registry<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid_registry %>%write_csv('regCovid_registry_a.csv')


#---- Chapter 2a: Covid interventions

covid8_int_per_row<-left_join(co_studies, intervent, by = 'nct_id')
covid9_int_per_row<-left_join(covid8_int_per_row, facil4, by = c('nct_id'= 'Group.1'))

covid_int_row<-covid9_int_per_row %>% filter(study_type == 'Interventional')
covid_int_row %>%write_csv('regCovid_int_per_row-a.csv')

#intervention by comp methods
covid_int_row$downcase_intervention <- tolower(covid_int_row$intervention_name)
intervention_count<-covid_int_row %>% group_by(downcase_intervention )%>%count()
colnames(intervention_count)<- c("intervention", "Study_count")
intervention_count <- intervention_count[with(intervention_count, order(-Study_count)), ]

intervention_count %>% write_csv('regCovid_interventions_raw_cnts-a.csv')

#normalizing interventions
covid_int_row$interventions_normalized<-covid_int_row$downcase_intervention
covid_int_row$interventions_normalized<- gsub("\\+", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" or ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" and ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" combined ", "/", covid_int_row$interventions_normalized)




intervention_count_normal1<-covid_int_row %>% group_by(interventions_normalized  )%>%count()


covid_int_row2 <- covid_int_row %>% separate_rows(interventions_normalized, sep ='/')
covid_int_row2$interventions_normalized<- trimws(covid_int_row2$interventions_normalized)


#Changing to normalized intervention
intervention_map2 <- read_excel("intervention_map2.xlsx")
covid_int_row_total1<- rbind(covid_int_row,covid_int_row2)

covid_int_studies_normal<-left_join(covid_int_row_total1, intervention_map2, by = c('interventions_normalized' ='intervention') )
covid_int_studies_normal$mapped_intervention <- ifelse(is.na(covid_int_studies_normal$mapped_intervention), covid_int_studies_normal$interventions_normalized, covid_int_studies_normal$mapped_intervention)
covid_int_studies_normal2 <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,78)]),]


intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]

intervention_count_mapped %>% write_csv('regCovid_interventions_mapped_cnts-a.csv')



#intervention type
studies_int_type <-covid_int_row [!duplicated(covid_int_row[c(1,66)]),]


studies_int_type2<-aggregate(studies_int_type, list(studies_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  data.frame(table(studies_int_type2$intervention_type))
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("regCovid_intervention-type_cnts.csv")

#Changing to normalized intervention
intervention_map2 <- read_excel("intervention_map2.xlsx")
covid_int_row_total1<- rbind(covid_int_row,covid_int_row2)

covid_int_studies_normal<-left_join(covid_int_row_total1, intervention_map2, by = c('interventions_normalized' ='intervention') )
covid_int_studies_normal$mapped_intervention <- ifelse(is.na(covid_int_studies_normal$mapped_intervention), covid_int_studies_normal$interventions_normalized, covid_int_studies_normal$mapped_intervention)
covid_int_studies_normal2 <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,78)]),]


intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(mapped_intervention )%>%count()
colnames(intervention_count)<- c("intervention", "Study_count")




#browse ineterventions
browse_inter_q<-"select * from browse_interventions"
browse_inter<-dbGetQuery(con, browse_inter_q)

covid_bint_per_row<-left_join(co_studies, browse_inter, by = 'nct_id')
browse_inter2<-covid_bint_per_row %>% filter (study_type == 'Interventional')
mesh_itnervent_count<-browse_inter2%>% group_by(downcase_mesh_term)%>%count()
# 49 interventional with no mesh intervention as of 3/30/20







#----Chapter 3: studies over time
#All studies
studies_dates<- table(co_studies$study_first_submitted_date)
studies_dates<-data.frame(studies_dates)
colnames(studies_dates)[1] <- "Date"
colnames(studies_dates)[2] <- "Count"

study_date2<-data.frame()
study_count = 0
for (i in seq_along(studies_dates$Date)){
  study_count<- study_count + studies_dates$Count[i]
  study_date<-data.frame(studies_dates$Date[i],study_count)
  study_date2<-rbind(study_date2,study_date)
}
colnames(study_date2)[1] <- "Date"

# Studies over time by type
#-interventional
interventional_studies<-co_studies%>%filter(study_type =='Interventional')
studies_dates_int2<- table(interventional_studies$study_first_submitted_date)
studies_dates_int<-data.frame(studies_dates_int2)
colnames(studies_dates_int)[1] <- "Date"
colnames(studies_dates_int)[2] <- "Count"

study_date2_int<-data.frame()
study_count_int = 0

for (b in seq_along(studies_dates_int$Date)){
  study_count_int<- study_count_int + studies_dates_int$Count[b]
  study_date_int<-data.frame(studies_dates_int$Date[b],study_count_int)
  study_date2_int<-rbind(study_date2_int,study_date_int)
}
colnames(study_date2_int)[1] <- "Date"

#-Observational
observational_studies<-co_studies%>%filter(study_type =='Observational')
studies_dates_obs2<- table(observational_studies$study_first_submitted_date)
studies_dates_obs<-data.frame(studies_dates_obs2)
colnames(studies_dates_obs)[1] <- "Date"
colnames(studies_dates_obs)[2] <- "Count"

study_date2_obs<-data.frame()
study_count_obs = 0

for (b in seq_along(studies_dates_obs$Date)){
  study_count_obs<- study_count_obs + studies_dates_obs$Count[b]
  study_date_obs<-data.frame(studies_dates_obs$Date[b],study_count_obs)
  study_date2_obs<-rbind(study_date2_obs,study_date_obs)
}
colnames(study_date2_obs)[1] <- "Date"


#-Registry
registry_studies<-co_studies%>%filter(study_type =='Observational [Patient Registry]')
studies_dates_reg2<- table(registry_studies$study_first_submitted_date)
studies_dates_reg<-data.frame(studies_dates_reg2)
colnames(studies_dates_reg)[1] <- "Date"
colnames(studies_dates_reg)[2] <- "Count"

study_date2_reg<-data.frame()
study_count_reg = 0

for (b in seq_along(studies_dates_reg$Date)){
  study_count_reg<- study_count_reg + studies_dates_reg$Count[b]
  study_date_reg<-data.frame(studies_dates_reg$Date[b],study_count_reg)
  study_date2_reg<-rbind(study_date2_reg,study_date_reg)
}
colnames(study_date2_reg)[1] <- "Date"

library(ggplot2)

png("regCovid_total_studies_time-a.png")


ggplot() + 
  geom_path(data = study_date2_int, aes(x = Date, y = study_count_int, group =1, color = 'Interventional')) +
  geom_path(data = study_date2_obs, aes(x = Date, y = study_count_obs, group =1, color= 'Observational')) +
  geom_path(data = study_date2_reg, aes(x = Date, y = study_count_reg, group =1,color = 'Registry')) +
  xlab('Date') +
  ylab('Study Count')+
  scale_x_discrete(breaks = study_date2_int$Date[seq(1, length(study_date2_int$Date), by = 7)])

dev.off() 

#----Chapter 4: Publications
pub_q<-"select* from study_references"
pub<-dbGetQuery(con, pub_q)

pub2<- left_join(co_studies,pub, by = 'nct_id')


pub3 <- pub2 %>% filter(reference_type == 'results_reference')


pub3 %>% write_csv('regCOvid_publications.csv')


#---- CHapter 5 Number of Sites
site_counts<-  data.frame(table(covid9$site_count))
colnames(site_counts)<-c("Site_count","Study_Count")
site_counts %>% write_csv("regCovid_site_cnts.csv")

countries_q<-"select * from countries"
countries<-dbGetQuery(con, countries_q)

countries1<-left_join(co_studies, countries, by ='nct_id')
countries2<-aggregate(countries1, list(countries1$nct_id), paste, collapse="|")
country_counts<-  data.frame(table(countries2$name))
colnames(country_counts)<-c("Country","Study_Count")
country_counts <- country_counts[with(country_counts, order(-Study_Count)), ]
country_counts %>% write_csv("regCovid_country_cnts.csv")







#-Book B: Search method B (in condition)
# a to inform conditions for b
browse_con_q<-"select * from browse_conditions"
browse_con<-dbGetQuery(con, browse_con_q)
covid_browse_con<-left_join(co_studies, browse_con, by = 'nct_id')
mesh_cond_count<-covid_browse_con%>% group_by(downcase_mesh_term)%>%count()
#mesh coronavirus infections. 104 of 199 in search method a
mesh_co_cond<-browse_con %>% filter(downcase_mesh_term == 'coronavirus infections')

studies_q<-"select * from studies where study_first_submitted_date > '2020-01-01'"
studies<-dbGetQuery(con, studies_q)
mesh_studies<- merge(mesh_co_cond, studies, by = 'nct_id')


cond_q<-"select * from conditions"
cond<-dbGetQuery(con, cond_q)
covid_cond<-left_join(co_studies, cond, by = 'nct_id')
cond_count<-covid_cond%>% group_by(downcase_name)%>%count()



co_studies_qb<-"select * FROM studies s join conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2020-01-01' and (downcase_name like '%covid%' 
or downcase_name like '%sars-cov%'or downcase_name like '%2019-ncov%'
or downcase_name like '%coronavirus%'or downcase_name like '%corona virus%' )" 

co_studies_b<-dbGetQuery(con, co_studies_qb)
co_studies_b$nct_id = NULL
co_studies_b2<-co_studies_b[!duplicated(co_studies_b$nct_id),]


covid8<-left_join(co_studies_b2, intervent4, by = c('nct_id'= 'Group.1'))
covid9<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))
covid9 %>% write_csv('regCovid_all_studies-b.csv')

#----Chapter 1: Covid studies by type
covid_int<-covid9%>%filter(study_type == 'Interventional')
covid_int %>%write_csv('regCovid_int_b.csv')

covid_obs<-covid9%>%filter(study_type == 'Observational')
covid_obs %>%write_csv('regCovid_obs_b.csv')

covid_registry<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid_registry %>%write_csv('regCovid_registry_b.csv')

# Book AB: Method a and b comparision
co_studies_ab <- merge(co_studies, co_studies_b, by = 'nct_id', all= TRUE)
co_studies_ab %>% write_csv('co_studies_ab.csv')
co_studies_ab2 <- merge(co_studies_ab, mesh_studies, by = 'nct_id', all= TRUE)
co_studies_ab2 %>% write_csv('co_studies_ab2.csv')

#---- Chapter 2: Covid interventions
covid8_int_per_row<-left_join(co_studies_b2, intervent, by = 'nct_id')
covid9_int_per_row<-left_join(covid8_int_per_row, facil4, by = c('nct_id'= 'Group.1'))

covid_int_row<-covid9_int_per_row %>% filter(study_type == 'Interventional')
covid_int_row %>%write_csv('regCovid_int_per_row-b.csv')



