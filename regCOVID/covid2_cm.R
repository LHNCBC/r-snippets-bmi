user=username
psw=password

library(RPostgreSQL)
library(tidyverse)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)
tbls<-RPostgreSQL::dbListTables(con)
tbls

library(dplyr)
library(tidyquant)
library(xts)

#-Book A: Search Method A (in title of study)
co_studies_q<-"select * FROM studies
where study_first_submitted_date > '2019-12-27' and (lower(official_title) like '%covid%' 
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

covid7<- co_studies %>% filter(overall_status != 'Withdrawn')
withdrawn_a<-length(co_studies$nct_id)- length(covid7$nct_id)
covid8<-left_join(covid7, intervent4, by = c('nct_id'= 'Group.1'))
covid11<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))
covid10<-covid11%>% filter(overall_status == 'Not yet recruiting')
covid9<-covid11%>% filter(country != 'NA')
covid9 %>% write_csv('regCovid_all_studies-a.csv')
no_location<-covid11%>% filter(is.na(country))

#----Chapter 1: Covid studies by type
covid9_int<-covid9%>%filter(study_type == 'Interventional')
covid9_int %>%write_csv('regCovid_int_a.csv')

covid9_obs<-covid9%>%filter(study_type == 'Observational')
covid9_obs %>%write_csv('regCovid_obs_a.csv')

covid9_reg<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid9_reg %>%write_csv('regCovid_registry_a.csv')


#---- Chapter 2a: Covid interventions

covid8_int_per_row<-left_join(co_studies, intervent, by = 'nct_id')
covid9_int_per_row<-left_join(covid8_int_per_row, facil4, by = c('nct_id'= 'Group.1'))

covid_int_row<-covid9_int_per_row %>% filter(study_type == 'Interventional')
covid_int_row %>%write_csv('regCovid_int_per_row-a.csv')

#intervention by comp methods
covid_int_row$downcase_intervention <- tolower(covid_int_row$intervention_name)
intervention_count<-covid_int_row %>% group_by(intervention_type, downcase_intervention )%>%count()
colnames(intervention_count)<- c("Intervention_type","intervention", "Study_count")
intervention_count <- intervention_count[with(intervention_count, order(-Study_count)), ]

intervention_count %>% write_csv('regCovid_interventions_raw_cnts-a.csv')

#normalizing interventions
covid_int_row$interventions_normalized<-covid_int_row$downcase_intervention
covid_int_row$interventions_normalized<- gsub("\\+", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" or ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" and ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" combined ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" plus ", "/", covid_int_row$interventions_normalized)
covid_int_row$interventions_normalized<- gsub(" & ", "/", covid_int_row$interventions_normalized)



intervention_count_normal1<-covid_int_row %>% group_by(interventions_normalized  )%>%count()


covid_int_row2 <- covid_int_row %>% separate_rows(interventions_normalized, sep ='/')
covid_int_row2$interventions_normalized<- trimws(covid_int_row2$interventions_normalized)


#Changing to normalized intervention
library(readxl)
intervention_map2 <- read_excel("intervention_map2.xlsx")
covid_int_row_total1<- rbind(covid_int_row,covid_int_row2)

covid_int_studies_normal<-left_join(covid_int_row_total1, intervention_map2, by = c('interventions_normalized' ='intervention') )
covid_int_studies_normal$mapped_intervention <- ifelse(is.na(covid_int_studies_normal$mapped_intervention), covid_int_studies_normal$interventions_normalized, covid_int_studies_normal$mapped_intervention)
covid_int_studies_normal2 <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,78)]),]


intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]

intervention_count_mapped %>% write_csv('regCovid_interventions_mapped_cnts-a.csv')



#intervention type
studies_int_type <-covid_int_row [!duplicated(covid_int_row[c(1,66)]),]

studies_int_type<-studies_int_type[order(studies_int_type$intervention_type),]
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

for (c in seq_along(studies_dates_reg$Date)){
  study_count_reg<- study_count_reg + studies_dates_reg$Count[c]
  study_date_reg<-data.frame(studies_dates_reg$Date[c],study_count_reg)
  study_date2_reg<-rbind(study_date2_reg,study_date_reg)
}
colnames(study_date2_reg)[1] <- "Date"

#Expanded Access
expanded_access<-co_studies%>%filter(study_type =='Expanded Access')
studies_dates_ea2<- table(expanded_access$study_first_submitted_date)
studies_dates_ea<-data.frame(studies_dates_ea2)
colnames(studies_dates_ea)[1] <- "Date"
colnames(studies_dates_ea)[2] <- "Count"

study_date2_ea<-data.frame()
study_count_ea = 0

for (d in seq_along(studies_dates_ea$Date)){
  study_count_ea<- study_count_ea + studies_dates_ea$Count[d]
  study_date_ea<-data.frame(studies_dates_ea$Date[d],study_count_ea)
  study_date2_ea<-rbind(study_date2_ea,study_date_ea)
}
colnames(study_date2_ea)[1] <- "Date"




library(ggplot2)

png("regCovid_total_studies_time-a.png")


ggplot() + 
  geom_path(data = study_date2_int, aes(x = Date, y = study_count_int, group =1, color = 'Interventional')) +
  geom_path(data = study_date2_obs, aes(x = Date, y = study_count_obs, group =1, color= 'Observational')) +
  geom_path(data = study_date2_reg, aes(x = Date, y = study_count_reg, group =1,color = 'Registry')) +
  geom_path(data = study_date2_ea, aes(x = Date, y = study_count_ea, group =1,color = 'Expanded Access'))+
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


site_counts<-  data.frame(table(covid9_int$site_count))
colnames(site_counts)<-c("Site_count","Study_Count")
site_counts %>% write_csv("regCovid_site_cnts_int.csv")

site_counts_obs<-  data.frame(table(covid9_obs$site_count))
colnames(site_counts_obs)<-c("Site_count","Study_Count")
site_counts_obs %>% write_csv("regCovid_site_cnts_obs.csv")

site_counts_reg<-  data.frame(table(covid9_reg$site_count))
colnames(site_counts_reg)<-c("Site_count","Study_Count")
site_counts_reg %>% write_csv("regCovid_site_cnts_reg.csv")

facil_uniq <- facil [!duplicated(facil[c(2,8)]),]
facil_per_row<-left_join(co_studies, facil_uniq, by = 'nct_id')

facil_per_row<-facil_per_row[order(facil_per_row$country),]

facil_per_row_all<-aggregate(facil_per_row, list(facil_per_row$nct_id), paste, collapse="|")
facil_per_row2<-facil_per_row_all %>%filter(country !='NA')

facil_per_row2_int<-facil_per_row2%>%filter(study_type == 'Interventional')
facil_per_row2_obs<-facil_per_row2%>%filter(study_type == 'Observational')
facil_per_row2_reg<-facil_per_row2%>%filter(study_type == 'Observational [Patient Registry]')

fac_country_count<-facil_per_row2 %>% group_by(country )%>%count()
fac_country_count_int<-facil_per_row2_int %>% group_by(country )%>%count()
fac_country_count_obs<-facil_per_row2_obs %>% group_by(country )%>%count()
fac_country_count_reg<-facil_per_row2_reg %>% group_by(country )%>%count()
colnames(fac_country_count)<-c("Country","Study_Count")
fac_country_count <- fac_country_count[with(fac_country_count, order(-Study_Count)), ]
fac_country_count %>% write_csv("regCovid_facil_country_cnts.csv")
colnames(fac_country_count_int)<-c("Country","Study_Count")
fac_country_count_int <- fac_country_count_int[with(fac_country_count_int, order(-Study_Count)), ]
fac_country_count_int %>% write_csv("regCovid_facil_country_cnts_int.csv")

colnames(fac_country_count_obs)<-c("Country","Study_Count")
fac_country_count_obs <- fac_country_count_obs[with(fac_country_count_obs, order(-Study_Count)), ]
fac_country_count_obs %>% write_csv("regCovid_facil_country_cnts_obs.csv")

colnames(fac_country_count_reg)<-c("Country","Study_Count")
fac_country_count_reg <- fac_country_count_reg[with(fac_country_count_reg, order(-Study_Count)), ]
fac_country_count_reg %>% write_csv("regCovid_facil_country_cnts_reg.csv")


countries_q<-"select * from countries"
countries<-dbGetQuery(con, countries_q)

countries1<-left_join(co_studies, countries, by ='nct_id')
countries2<-aggregate(countries1, list(countries1$nct_id), paste, collapse="|")
countries3<- countries2%>%filter(study_type == 'Interventional')
country_counts<-  data.frame(table(countries3$name))
colnames(country_counts)<-c("Country","Study_Count")
country_counts <- country_counts[with(country_counts, order(-Study_Count)), ]
country_counts %>% write_csv("regCovid_country_cnts_int.csv")



# Chapter 6 History
library(htmltab)
total_history<-data.frame()
for(i in seq_len(nrow(covid9))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid9$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid9$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count of SStudies")

Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]

Change_type2 %>%write_csv('regCovid_change_type_cnt.csv')

history_change_per_row %>%write_csv('regCovid_updates_per_row.csv')

update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('regCOvid_update_counts.csv')

covid9$days_since_update<-Sys.Date()-covid9$last_update_submitted_date

Days_since_update<-covid9 %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('regCOvid_Days_since_update.csv')

#Interventional
total_history<-data.frame()
for(i in seq_len(nrow(covid9_int))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid9_int$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid9_int$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('regCovid_updates_per_row_int.csv')
history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count of SStudies")

Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)),] 

Change_type2 %>%write_csv('regCovid_change_type_cnt_int.csv')

update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('regCOvid_update_counts_int.csv')

covid9_int$days_since_update<-Sys.Date()-covid9_int$last_update_submitted_date

Days_since_update<-covid9_int %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('regCOvid_Days_since_update_int.csv')



#observational
total_history<-data.frame()
for(i in seq_len(nrow(covid9_obs))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid9_obs$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid9_obs$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('regCovid_updates_per_row_obs.csv')

history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count of SStudies")

Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]


Change_type2 %>%write_csv('regCovid_change_type_cnt_obs.csv')


update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('regCOvid_update_counts_obs.csv')


covid9_obs$days_since_update<-Sys.Date()-covid9_obs$last_update_submitted_date

Days_since_update<-covid9_obs %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('regCOvid_Days_since_update_obs.csv')


#Registry
total_history<-data.frame()
for(i in seq_len(nrow(covid9_reg))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid9_reg$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid9_reg$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)

history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('regCovid_updates_per_row_reg.csv')

history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count of SStudies")

Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)),]

Change_type2 %>%write_csv('regCovid_change_type_cnt_reg.csv')

update_counts<-total_history %>% group_by(nct_id)%>%count()
update_counts2<-update_counts %>% group_by(n)%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('regCOvid_update_counts_reg.csv')

covid9_reg$days_since_update<-Sys.Date()-covid9_reg$last_update_submitted_date

Days_since_update<-covid9_reg %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('regCOvid_Days_since_update_reg.csv')


#Chapter 7 Phase
phase<-covid9_int %>% group_by(phase) %>%count()
colnames(phase)<-c("Phase", "Study_Count")
phase <- phase[with(phase, order(-Study_Count)), ]

phase %>% write_csv('regCOvid_phase.csv')

#CHapter 8 Arms
arms_q<-"select * from design_groups"
arms<-dbGetQuery(con, arms_q)
covid_arms<-left_join(covid9, arms, by = 'nct_id')
covid_arms_nct<-covid_arms %>% group_by(nct_id) %>% count()
covid_arms_cnt<-covid_arms_nct %>% group_by(n) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('regCovid_arm_cnts.csv')

covid_arms<-left_join(covid9_int, arms, by = 'nct_id')
covid_arms_nct<-covid_arms %>% group_by(nct_id) %>% count()
covid_arms_cnt<-covid_arms_nct %>% group_by(n) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('regCovid_arm_cnts_int.csv')

covid_arms<-left_join(covid9_obs, arms, by = 'nct_id')
covid_arms_nct<-covid_arms %>% group_by(nct_id) %>% count()
covid_arms_cnt<-covid_arms_nct %>% group_by(n) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('regCovid_arm_cnts_obs.csv')

covid_arms<-left_join(covid9_reg, arms, by = 'nct_id')
covid_arms_nct<-covid_arms %>% group_by(nct_id) %>% count()
covid_arms_cnt<-covid_arms_nct %>% group_by(n) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('regCovid_arm_cnts_reg.csv')

#Chapter 9 Interventions over time

Dates<-data.frame(seq(as.Date(min(covid_int_studies_normal2$study_first_submitted_date)), as.Date(max(covid_int_studies_normal2$study_first_submitted_date)), by="days"))
colnames(Dates)<-c("Date")

weekly_intervent2<-Dates
weekly_intervent2<- weekly_intervent2 %>%
  tq_transmute(select=Date,
    mutate_fun = apply.weekly, FUN =sum)
weekly_intervent2<-data.frame(weekly_intervent2[,1])

intervention_count_mapped_short<-intervention_count_mapped[1:10,]
all_intervent_date<-data.frame(unique(covid_int_studies_normal2$study_first_submitted_date))
all_intervent_date <- data.frame(all_intervent_date [!duplicated(all_intervent_date[c(1)]),])
colnames(all_intervent_date)<-c("Date")
for (x in seq_along(intervention_count_mapped_short$intervention)){
  
  intervent_time =intervention_count_mapped$intervention[x]
  intervent_time2<-covid_int_studies_normal2%>%filter(mapped_intervention== intervent_time)
  intervent_time3<- table(intervent_time2$study_first_submitted_date)
  intervent_time4<-data.frame(intervent_time3)
  colnames(intervent_time4)[1] <- "Date"
  colnames(intervent_time4)[2] <- "Count"

  intervent_time4$Date<-as.Date(intervent_time4$Date)
  
  
  weekly_intervent<-left_join(Dates, intervent_time4, by = 'Date')
  weekly_intervent[is.na(weekly_intervent)] <- 0
  colnames(weekly_intervent) <- c("Date", intervent_time)
  weekly_intervent1<- weekly_intervent %>%
    tq_transmute(
      mutate_fun = apply.weekly,
      FUN        = sum)
  weekly_intervent2<-merge(weekly_intervent2,weekly_intervent1, by = 'Date', all = TRUE)
  
  
 
  
  
  
  intervent_date<-data.frame()
  intervent_date_count = 0
  
  for (d in seq_along(intervent_time4$Date)){
    intervent_date_count<- intervent_date_count + intervent_time4$Count[d]
    int_date<-data.frame(intervent_time4$Date[d],intervent_date_count)
    intervent_date<-rbind(intervent_date,int_date)
  }
  colnames(intervent_date) <- c("Date", intervent_time)
  all_intervent_date<-merge(all_intervent_date,intervent_date, by = 'Date', all = TRUE)
}
library(reshape2)
all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("regCovid_interventions_time-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()




intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("regCovid_new_interventions_time-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')
dev.off()


#Chapter 10 ENrollment
#Interventional
Phases<-sort(unique(covid9_int$phase))

enrollment_phase <- function(Phases) {
  covid9_phase<- covid9_int %>% filter(phase == Phases[1])
  enroll_sum<-summary(covid9_phase$enrollment)
  enroll_sum<- data.frame(unclass(enroll_sum), check.names = TRUE)
  colnames(enroll_sum)<-c(Phases[1])
  return(enroll_sum)}
regCovid_enroll<-data.frame(lapply(Phases,FUN =enrollment_phase))
regCovid_enroll2<- data.frame( row.names(regCovid_enroll), regCovid_enroll)
colnames(regCovid_enroll2)[1]<-"Measure"

regCovid_enroll2 %>% write_csv('regCovid_enroll_int.csv')


#Observational
Measures<-data.frame(c('Min','1st Qu','Median', 'Mean', '3rd Qu','Max'))
enroll_sum<-summary(covid9_obs$enrollment)
enroll_sum<-left_join(Measures, enroll_sum)
enroll_sum<- data.frame(unclass(enroll_sum), check.names = TRUE)
regCovid_enroll2<- data.frame( row.names(enroll_sum), enroll_sum)
colnames(regCovid_enroll2)<-c("Measure", "Observational")

regCovid_enroll2 %>% write_csv('regCovid_enroll_obs.csv')

#Registries


enroll_sum<-summary(covid9_reg$enrollment)
enroll_sum<-left_join(Measures, enroll_sum)
enroll_sum<- data.frame(unclass(enroll_sum), check.names = TRUE)
regCovid_enroll2<- data.frame( row.names(enroll_sum), enroll_sum)
colnames(regCovid_enroll2)<-c("Measure", "Registries")

regCovid_enroll2 %>% write_csv('regCovid_enroll_reg.csv')




#-Book B: Search method B (in condition) Free Text

cond_q<-"select * from conditions"
cond<-dbGetQuery(con, cond_q)
covid_cond<-left_join(co_studies, cond, by = 'nct_id')
cond_count<-covid_cond%>% group_by(downcase_name)%>%count()



co_studies_qb<-"select * FROM studies s join conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2019-12-27' and (downcase_name like '%covid%' 
or downcase_name like '%sars-cov%'or downcase_name like '%2019-ncov%'
or downcase_name like '%coronavirus%'or downcase_name like '%corona virus%' )" 

co_studies_b<-dbGetQuery(con, co_studies_qb)
co_studies_b$nct_id = NULL
co_studies_b2<-co_studies_b[!duplicated(co_studies_b$nct_id),]

covid7<- co_studies_b2 %>% filter(overall_status != 'Withdrawn')
withdrawn_c<-length(co_studies_b2$nct_id)- length(covid7$nct_id)
covid8<-left_join(covid7, intervent4, by = c('nct_id'= 'Group.1'))
covid9<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))
covid9 %>% write_csv('regCovid_all_studies-b.csv')

#----Chapter 1: Covid studies by type
covid_int<-covid9%>%filter(study_type == 'Interventional')
covid_int %>%write_csv('regCovid_int_b.csv')

covid_obs<-covid9%>%filter(study_type == 'Observational')
covid_obs %>%write_csv('regCovid_obs_b.csv')

covid_registry<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid_registry %>%write_csv('regCovid_registry_b.csv')


#---- Chapter 2: Covid interventions
covid8_int_per_row<-left_join(co_studies_b2, intervent, by = 'nct_id')
covid9_int_per_row<-left_join(covid8_int_per_row, facil4, by = c('nct_id'= 'Group.1'))

covid_int_row<-covid9_int_per_row %>% filter(study_type == 'Interventional')
covid_int_row %>%write_csv('regCovid_int_per_row-b.csv')




# Book C MeSH term

covid_qc<-"select * from studies s join browse_conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2019-12-27' and downcase_mesh_term = 'coronavirus infections'"
co_studies_c<-dbGetQuery(con, covid_qc)
co_studies_c$nct_id = NULL
covid7<- co_studies_c %>% filter(overall_status != 'Withdrawn')
withdrawn_c<-length(covid7$nct_id)- length(covid7$nct_id)

covid8<-left_join(covid7, intervent4, by = c('nct_id'= 'Group.1'))
covid9<-left_join(covid8, facil4, by = c('nct_id'= 'Group.1'))
covid9 %>% write_csv('regCovid_all_studies-c.csv')

#----Chapter 1: Covid studies by type
covid_int<-covid9%>%filter(study_type == 'Interventional')
covid_int %>%write_csv('regCovid_int_c.csv')

covid_obs<-covid9%>%filter(study_type == 'Observational')
covid_obs %>%write_csv('regCovid_obs_b.csv')

covid_registry<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid_registry %>%write_csv('regCovid_registry_c.csv')


#---- Chapter 2: Covid interventions
covid8_int_per_row<-left_join(co_studies_c, intervent, by = 'nct_id')
covid9_int_per_row<-left_join(covid8_int_per_row, facil4, by = c('nct_id'= 'Group.1'))

covid_int_row<-covid9_int_per_row %>% filter(study_type == 'Interventional')
covid_int_row %>%write_csv('regCovid_int_per_row-c.csv')

