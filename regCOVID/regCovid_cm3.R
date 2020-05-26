#regCOVID

#CTG Connection
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
library(readxl)
library(reshape2)
library(htmltab)
library(data.table)

#-Book A: Search Method A (in title of study)
co_studies_q<-"select * FROM studies
where study_first_submitted_date > '2019-12-27'  and (lower(official_title) like '%covid%' 
or lower(official_title) like '%sars-cov%'or lower(official_title) like '%2019-ncov%'
or lower(official_title) like '%cov2%'or lower(official_title) like '%cov-2%'
or lower(official_title) like '%coronavirus%'or lower(official_title) like '%corona virus%' )" 

co_studies<-dbGetQuery(con, co_studies_q)


Counts_by_status<-co_studies %>% group_by(overall_status)%>% count()
colnames(Counts_by_status)[2]<-"Study_Count"
Counts_by_status %>% write_csv('finaloutput/regCovid_status_cnt.csv')

covid9<- co_studies %>% filter(overall_status == 'Recruiting'| overall_status == 'Completed'|overall_status == 'Enrolling by invitation'|overall_status == 'Active, not recruiting')
covid9<-covid9%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

covid9 %>% write_csv('finaloutput/regCovid_all_studies-a.csv')





#------Chapter 1: Covid studies by type
covid9_int<-covid9%>%filter(study_type == 'Interventional')
covid9_int %>%write_csv('finaloutput/regCovid_int_a.csv')

covid9_obs<-covid9%>%filter(study_type == 'Observational')
covid9_obs %>%write_csv('finaloutput/regCovid_obs_a.csv')

covid9_reg<-covid9%>%filter(study_type == 'Observational [Patient Registry]')
covid9_reg %>%write_csv('finaloutput/regCovid_registry_a.csv')

Study_counts<-data.frame(Type= c("Total_Studies","interventional", "Observational", "Registry"), 
                         Study_count= c(nrow(covid9),  nrow(covid9_int), nrow(covid9_obs), nrow(covid9_reg)))
Study_counts%>% write_csv("finaloutput/regCovid_study_cnts.csv")




#------Chapter 2: studies over time

#-interventional
studies_dates_int2<- table(covid9_int$study_first_submitted_date)
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
studies_dates_obs2<- table(covid9_obs$study_first_submitted_date)
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
studies_dates_reg2<- table(covid9_reg$study_first_submitted_date)
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


library(ggplot2)

png("finaloutput/regCovid_total_studies_time-a.png")

ggplot() + 
  geom_path(data = study_date2_int, aes(x = Date, y = study_count_int, group =1, color = 'Interventional')) +
  geom_path(data = study_date2_obs, aes(x = Date, y = study_count_obs, group =1, color= 'Observational')) +
  geom_path(data = study_date2_reg, aes(x = Date, y = study_count_reg, group =1,color = 'Registry')) +
  xlab('Date') + labs(color ='Study Type')+
  ylab('Study Count')+
  scale_x_discrete(breaks = study_date2_int$Date[seq(1, length(study_date2_int$Date), by = 14)])

dev.off() 





#------Chapter 3 Phase
phase<-covid9_int %>% group_by(phase) %>% count()
colnames(phase)<-c("Phase", "Study_Count")
phase <- phase[with(phase, order(-Study_Count)), ]
phase %>% write_csv('finaloutput/regCovid_phase.csv')





#------Chapter 4 Arms
covid_arms_cnt<-covid9_int %>% group_by(number_of_arms) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('finaloutput/regCovid_arm_cnts_int.csv')





#------Chapter 5 Enrollment
#Interventional
Phases<-sort(unique(covid9_int$phase))

enrollment_phase <- function(Phases) {
  covid9_phase<- covid9_int %>% filter(phase == Phases & enrollment != 'NA')

  enroll_sum<-summary(covid9_phase$enrollment)
  enroll_sum<- data.frame(unclass(enroll_sum), check.names = TRUE)
  colnames(enroll_sum)<-c(Phases)
  return(enroll_sum)}
regCovid_enroll<-data.frame(lapply(Phases,FUN =enrollment_phase))
regCovid_enroll2<- data.frame( row.names(regCovid_enroll), regCovid_enroll)
colnames(regCovid_enroll2)[1]<-"Measure"

regCovid_enroll2 %>% write_csv('finaloutput/regCovid_enroll_int.csv')


#Observational
Measures<-data.frame(Measures= c('Min','1st Qu','Median', 'Mean', '3rd Qu','Max'))
covid9_obs_en<- covid9_obs %>% filter(enrollment != 'NA')
enroll_sum<-tibble(summary(covid9_obs_en$enrollment))
enroll_sum<-cbind(Measures, enroll_sum)
colnames(enroll_sum)<-c("Measure", "Observational")
enroll_sum %>% write_csv('finaloutput/regCovid_enroll_obs.csv')


#Registries

covid9_reg_en<- covid9_reg %>% filter(enrollment != 'NA')
enroll_sum<-tibble(summary(covid9_reg_en$enrollment))
enroll_sum<-cbind(Measures, enroll_sum)
colnames(enroll_sum)<-c("Measure", "Registries")
enroll_sum %>% write_csv('finaloutput/regCovid_enroll_reg.csv')




#-----CHapter 6 Study Design
#Design Query
designs_b<-"select * FROM designs" 
designs<-dbGetQuery(con, designs_b)
design_group_b<-"select * FROM design_groups" 
design_groups<-dbGetQuery(con, design_group_b)
covid9_design<-left_join(covid9,designs, by = 'nct_id' )
covid9_design<-left_join(covid9_design,design_groups, by = 'nct_id' )

#Interventional Design
#Groups
covid9_design_int<-covid9_design %>%filter(study_type =='Interventional')
group_int<-covid9_design_int %>% group_by(group_type) %>% count()
group_int <- group_int[with(group_int, order(-n)), ]
colnames(group_int)[2]<-'Group_Count'
group_int %>% write_csv('finaloutput/regCovid_intervention_group_types.csv')

#Model
covid9_design_int2 <- covid9_design_int [!duplicated(covid9_design_int[c(1)]),]

interventional_model<-covid9_design_int2 %>% group_by(intervention_model) %>%count()
interventional_model <- interventional_model[with(interventional_model, order(-n)), ]
colnames(interventional_model)[2]<-'Study_Count'

interventional_model %>% write_csv('finaloutput/regCovid_interventionl_model.csv')

#purpose

purpose<-covid9_design_int2 %>% group_by(primary_purpose) %>%count()
purpose <- purpose[with(purpose, order(-n)), ]

colnames(purpose)[2]<-'Study_Count'

purpose %>% write_csv('finaloutput/regCovid_primary_purpose.csv')




#Observational
#Model
covid9_design_obs<-covid9_design %>%filter(study_type =='Observational')
covid9_design_obs2 <- covid9_design_obs [!duplicated(covid9_design_obs[c(1)]),]
observation_model<-covid9_design_obs2 %>% group_by(observational_model) %>%count()
observation_model <- observation_model[with(observation_model, order(-n)), ]
colnames(observation_model)[2]<-'Study_Count'
observation_model %>% write_csv('finaloutput/regCovid_observational_model.csv')

#Perspective
time_perspective_obs<-covid9_design_obs2 %>% group_by(time_perspective) %>%count()
time_perspective_obs <- time_perspective_obs[with(time_perspective_obs, order(-n)), ]
colnames(time_perspective_obs)[2]<-'Study_Count'
time_perspective_obs %>% write_csv('finaloutput/regCovid_observational_time_perspective.csv')





#Rgistries
#Model
covid9_design_reg<-covid9_design %>%filter(study_type =='Observational [Patient Registry]')
covid9_design_reg2 <- covid9_design_reg [!duplicated(covid9_design_reg[c(1)]),]
registry_model<-covid9_design_reg2 %>% group_by(observational_model) %>%count()
registry_model <- registry_model[with(registry_model, order(-n)), ]
colnames(registry_model)[2]<-'Study_Count'
registry_model %>% write_csv('finaloutput/regCovid_registry_model.csv')

#Perspective
time_perspective_reg<-covid9_design_reg2 %>% group_by(time_perspective) %>%count()
time_perspective_reg <- time_perspective_reg[with(time_perspective_reg, order(-n)), ]
colnames(time_perspective_reg)[2]<-'Study_Count'
time_perspective_reg %>% write_csv('finaloutput/regCovid_registry_timeperspective.csv')

#Duration
duration<-covid9_reg%>%group_by(target_duration)%>%count()
duration<-duration%>% filter(target_duration != 'NA')
duration <- duration[with(duration, order(-n)), ]
colnames(duration)[2]<-'Study_Count'
duration %>% write_csv('finaloutput/regCovid_follow-up_reg.csv')

group_reg<-covid9_design_reg %>% group_by(group_type) %>% count()
group_int <- group_int[with(group_int, order(-n)), ]
colnames(group_int)[2]<-'Group_Count'






#----- CHapter 7 Number of Sites

facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)

#All
covid9_facil<-left_join(covid9, facil, by= 'nct_id')
covid9_sites<-covid9_facil%>%group_by(nct_id)%>% count()
site_counts<-  covid9_sites%>%group_by(n)%>% count()
colnames(site_counts)<-c("Site_count","Study_Count")
site_counts %>% write_csv("finaloutput/regCovid_site_cnts.csv")

#Interventional
covid9_facil<-left_join(covid9_int, facil, by= 'nct_id')
covid9_sites<-covid9_facil%>%group_by(nct_id)%>% count()
site_counts_int<-  covid9_sites%>%group_by(n)%>% count()
colnames(site_counts_int)<-c("Site_count","Study_Count")
site_counts_int %>% write_csv("finaloutput/regCovid_site_cnts_int.csv")

#Observational
covid9_facil<-left_join(covid9_obs, facil, by= 'nct_id')
covid9_sites<-covid9_facil%>%group_by(nct_id)%>% count()
site_counts_obs<-  covid9_sites%>%group_by(n)%>% count()
colnames(site_counts_obs)<-c("Site_count","Study_Count")
site_counts_obs %>% write_csv("finaloutput/regCovid_site_cnts_obs.csv")

#Registry
covid9_facil<-left_join(covid9_reg, facil, by= 'nct_id')
covid9_sites<-covid9_facil%>%group_by(nct_id)%>% count()
site_counts_reg<-  covid9_sites%>%group_by(n)%>% count()
colnames(site_counts_reg)<-c("Site_count","Study_Count")
site_counts_reg %>% write_csv("finaloutput/regCovid_site_cnts_reg.csv")



#------Chapter 8 COuntries
facil_uniq<- facil [!duplicated(facil[c(2,8)]),]

#All
covid9_facil_uni<-left_join(covid9, facil_uniq, by= 'nct_id')
facil_country<-aggregate(covid9_facil_uni, list(covid9_facil_uni$nct_id), paste, collapse="|")
country_cnts<-  facil_country%>%group_by(country)%>% count()
colnames(country_cnts)<-c("Country","Study_Count")
country_cnts <- country_cnts[with(country_cnts, order(-Study_Count)), ]
country_cnts %>% write_csv("finaloutput/regCovid_facil_country_cnts.csv")

#Interventional
covid9_facil_uni<-left_join(covid9_int, facil_uniq, by= 'nct_id')
facil_country<-aggregate(covid9_facil_uni, list(covid9_facil_uni$nct_id), paste, collapse="|")
country_cnts_int<-  facil_country%>%group_by(country)%>% count()
colnames(country_cnts_int)<-c("Country","Study_Count")
country_cnts_int <- country_cnts_int[with(country_cnts_int, order(-Study_Count)), ]
country_cnts_int %>% write_csv("finaloutput/regCovid_facil_country_cnts_int.csv")

#Observational
covid9_facil_uni<-left_join(covid9_obs, facil_uniq, by= 'nct_id')
facil_country<-aggregate(covid9_facil_uni, list(covid9_facil_uni$nct_id), paste, collapse="|")
country_cnts_obs<-  facil_country%>%group_by(country)%>% count()
colnames(country_cnts_obs)<-c("Country","Study_Count")
country_cnts_obs <- country_cnts_obs[with(country_cnts_obs, order(-Study_Count)), ]
country_cnts_obs %>% write_csv("finaloutput/regCovid_facil_country_cnts_obs.csv")

#Registry
covid9_facil_uni<-left_join(covid9_reg, facil_uniq, by= 'nct_id')
facil_country<-aggregate(covid9_facil_uni, list(covid9_facil_uni$nct_id), paste, collapse="|")
country_cnts_reg<-  facil_country%>%group_by(country)%>% count()
colnames(country_cnts_reg)<-c("Country","Study_Count")
country_cnts_reg <- country_cnts_reg[with(country_cnts_reg, order(-Study_Count)), ]
country_cnts_reg %>% write_csv("finaloutput/regCovid_facil_country_cnts_reg.csv")


#------Chapter 8 Interventions
intervent_q<-"select* from interventions"
intervent<-dbGetQuery(con, intervent_q)
colnames(intervent)[4] <- "intervention_name"
covid9_intervent<- left_join(covid9, intervent, by= 'nct_id')

#normalizing interventions
covid9_intervent$interventions_normalized <- tolower(covid9_intervent$intervention_name)
covid9_intervent$interventions_normalized<- gsub("\\+", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" or ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" and ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" combined ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" plus ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" & ", "/", covid9_intervent$interventions_normalized)

covid9_intervent2 <- covid9_intervent %>% separate_rows(interventions_normalized, sep ='/')
covid9_intervent2$interventions_normalized<- trimws(covid9_intervent2$interventions_normalized)
covid9_intervent3<- rbind(covid9_intervent,covid9_intervent2)
 
#Intervention Map
intervention_map2 <- read_excel("intervention_map2.xlsx")
covid_int_studies_normal<-left_join(covid9_intervent3, intervention_map2, by = c('interventions_normalized' ='intervention') )
covid_int_studies_normal$mapped_intervention <- ifelse(is.na(covid_int_studies_normal$mapped_intervention), covid_int_studies_normal$interventions_normalized, covid_int_studies_normal$mapped_intervention)
covid_int_studies_normal <- within(covid_int_studies_normal, intervention_type[mapped_intervention  %like% "placebo" &  !mapped_intervention  %like% "/"] <- 'placebo')


#All
# Mapped Intervention
covid_int_studies_normal2 <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts-a.csv')

#Raw Intervention
covid_int_studies_raw <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts-a.csv')

#Intervention Type
covid_int_type <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts.csv")

#Interventions Over Time
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

all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("finaloutput/regCovid_interventions_time-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()


intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("finaloutput/regCovid_new_interventions_time-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')
dev.off()


#Interventional

# Mapped Intervention
covid_int_studies_normal_int<-covid_int_studies_normal%>% filter(study_type == 'Interventional')
covid_int_studies_normal2 <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_int-a.csv')

#Raw Intervention
covid_int_studies_raw <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts_int-a.csv')

#Intervention Type
covid_int_type <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,67)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts_int.csv")

#Interventions Over Time
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

all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("finaloutput/regCovid_interventions_time_int-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()


intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("finaloutput/regCovid_new_interventions_time_int-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')+ labs(variable = 'Intervention')


dev.off()


#Observational
covid_int_studies_normal_obs<-covid_int_studies_normal%>% filter(study_type == 'Observational')

# Mapped Intervention
covid_int_studies_normal2 <- covid_int_studies_normal_obs [!duplicated(covid_int_studies_normal_obs[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_obs-a.csv')

#Raw Intervention
covid_int_studies_raw <- covid_int_studies_normal_obs [!duplicated(covid_int_studies_normal_obs[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts_obs-a.csv')

#Intervention Type
covid_int_type <- covid_int_studies_normal_obs [!duplicated(covid_int_studies_normal_obs[c(1,66)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts_obs.csv")

#Interventions Over Time
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

all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("finaloutput/regCovid_interventions_time_obs-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()


intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("finaloutput/regCovid_new_interventions_time_obs-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')
dev.off()


#Registry
# Mapped Intervention
covid_int_studies_normal_reg<-covid_int_studies_normal%>% filter(study_type == 'Observational [Patient Registry]')
covid_int_studies_normal2 <- covid_int_studies_normal_reg [!duplicated(covid_int_studies_normal_reg[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_reg-a.csv')

#Raw Intervention
covid_int_studies_raw <- covid_int_studies_normal_reg [!duplicated(covid_int_studies_normal_reg[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts_reg-a.csv')

#Intervention Type
covid_int_type <- covid_int_studies_normal_reg [!duplicated(covid_int_studies_normal_reg[c(1,66)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts_reg.csv")

#Interventions Over Time
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

all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("finaloutput/regCovid_interventions_time_reg-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()


intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("finaloutput/regCovid_new_interventions_time_reg-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')
dev.off()



#------Chapter 9 History
total_history<-data.frame()
covid92<-covid9%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )
for(i in seq_len(nrow(covid92))){
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92$nct_id[i])
  history3<- htmltab(url, which =1, rm_nodata_cols = F)
  history4<- data.frame(covid92$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

#Change Type
total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]


Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt.csv')
history_change_per_row %>%write_csv('finaloutput/regCovid_updates_per_row.csv')

#Number of Updates
update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts.csv')

# Days Since Update
covid9$days_since_update<-Sys.Date()-covid9$last_update_submitted_date
Days_since_update<-covid9 %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update.csv')

#Updates by country
country_update1<-left_join(total_history,facil_uniq, by = 'nct_id')
cahnges_by_country<-country_update1%>%group_by(country)%>% count()
cahnges_by_country2<-left_join(cahnges_by_country,country_cnts, by =c('country'= 'Country') )
cahnges_by_country2$n<-cahnges_by_country2$n-cahnges_by_country2$Study_Count
cahnges_by_country2$changes_per_study<-cahnges_by_country2$n/cahnges_by_country2$Study_Count
colnames(cahnges_by_country2)[2]<-'Total_changes'
cahnges_by_country2 <- cahnges_by_country2[with(cahnges_by_country2, order(-changes_per_study)), ]

cahnges_by_country2%>% write_csv('finaloutput/regCovid_country_updates.csv')



#Interventional
total_history<-data.frame()
covid92_int<-covid9_int%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

for(i in seq_len(nrow(covid92_int))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92_int$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid92_int$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('finaloutput/regCovid_updates_per_row_int.csv')
history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]

Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_int.csv')

update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_int.csv')

covid9_int$days_since_update<-Sys.Date()-covid9_int$last_update_submitted_date

Days_since_update<-covid9_int %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_int.csv')

#Country Updates
country_update1<-left_join(total_history, facil_country, by = c('nct_id'= 'Group.1'))
cahnges_by_country<-country_update1%>%group_by(country)%>% count()
cahnges_by_country2<-left_join(cahnges_by_country,country_cnts_int, by =c('country'= 'Country') )
cahnges_by_country2$n<-cahnges_by_country2$n-cahnges_by_country2$Study_Count
cahnges_by_country2$changes_per_study<-cahnges_by_country2$n/cahnges_by_country2$Study_Count
colnames(cahnges_by_country2)[2]<-'Total_changes'
cahnges_by_country2 <- cahnges_by_country2[with(cahnges_by_country2, order(-changes_per_study)), ]

cahnges_by_country2%>% write_csv('finaloutput/regCovid_country_updates_int.csv')

#observational
total_history<-data.frame()
covid92_obs<-covid9_obs%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

for(i in seq_len(nrow(covid92_obs))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92_obs$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid92_obs$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)
history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('finaloutput/regCovid_updates_per_row_obs.csv')

history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]


Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_obs.csv')


update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()

colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_obs.csv')


covid9_obs$days_since_update<-Sys.Date()-covid9_obs$last_update_submitted_date

Days_since_update<-covid9_obs %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_obs.csv')

#Country Updates
country_update1<-left_join(total_history, facil_uniq, by = 'nct_id')
cahnges_by_country<-country_update1%>%group_by(country)%>% count()
cahnges_by_country2<-left_join(cahnges_by_country,country_cnts_obs, by =c('country'= 'Country') )
cahnges_by_country2$n<-cahnges_by_country2$n-cahnges_by_country2$Study_Count
cahnges_by_country2$changes_per_study<-cahnges_by_country2$n/cahnges_by_country2$Study_Count
colnames(cahnges_by_country2)[2]<-'Total_changes'
cahnges_by_country2 <- cahnges_by_country2[with(cahnges_by_country2, order(-changes_per_study)), ]

cahnges_by_country2%>% write_csv('finaloutput/regCovid_country_updates_obs.csv')


#Registry
total_history<-data.frame()
covid92_reg<-covid9_reg%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

for(i in seq_len(nrow(covid92_reg))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92_reg$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid92_reg$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

total_history$Changes<- gsub(" and ", ",", total_history$Changes)

history_change_per_row <- total_history %>% separate_rows(Changes, sep =',')
history_change_per_row %>%write_csv('finaloutput/regCovid_updates_per_row_reg.csv')

history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]

Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_reg.csv')

update_counts<-total_history %>% group_by(nct_id)%>%count()
update_counts2<-update_counts %>% group_by(n)%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_reg.csv')

covid9_reg$days_since_update<-Sys.Date()-covid9_reg$last_update_submitted_date

Days_since_update<-covid9_reg %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_reg.csv')


#Country Updates
total_history_uni <- total_history [!duplicated(history_change_per_row[c(1,3)]),]

country_update1<-left_join(total_history_uni, facil_uniq, by = 'nct_id')
cahnges_by_country<-country_update1%>%group_by(country)%>% count()
cahnges_by_country2<-left_join(cahnges_by_country,country_cnts_reg, by =c('country'= 'Country') )
cahnges_by_country2$n<-cahnges_by_country2$n-cahnges_by_country2$Study_Count
cahnges_by_country2$changes_per_study<-cahnges_by_country2$n/cahnges_by_country2$Study_Count
cahnges_by_country2 <- cahnges_by_country2[with(cahnges_by_country2, order(-changes_per_study)), ]

colnames(cahnges_by_country2)[2]<-'Total_changes'
cahnges_by_country2%>% write_csv('finaloutput/regCovid_country_updates_reg.csv')



#Vaccines
covid9_vacc<- covid9_int %>% filter (tolower(official_title)  %like% "vaccine" )
vacc_phase<- covid9_vacc %>% group_by(phase) %>% count()
covid9_vacc %>% write_csv('finaloutput/regCovid_vaccines.csv')
vacc_phase %>% write_csv('finaloutput/regCovid_vaccines_phase.csv')



#Book B:Search Method B (free text condition)
co_studies_qb<-"select * FROM studies s join conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2019-12-27' and (downcase_name like '%covid%' 
or downcase_name like '%sars-cov%'or downcase_name like '%2019-ncov%'
or downcase_name like '%coronavirus%'or downcase_name like '%corona virus%' )" 

co_studies_b<-dbGetQuery(con, co_studies_qb)
co_studies_b$nct_id = NULL
co_studies_b2<-co_studies_b[!duplicated(co_studies_b$nct_id),]

covid9b<- co_studies_b2 %>%  filter(overall_status == 'Recruiting'| overall_status == 'Completed'|overall_status == 'Enrolling by invitation'|overall_status == 'Active, not recruiting')

covid9b %>% write_csv('finaloutput/regCovid_all_studies-b.csv')




#Book C: Search Method C (MeSH Term)
covid_qc<-"select * from studies s join browse_conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2019-12-27' and downcase_mesh_term = 'coronavirus infections'"
co_studies_c<-dbGetQuery(con, covid_qc)
co_studies_c$nct_id = NULL

covid9c<- co_studies_c %>% filter(overall_status == 'Recruiting'| overall_status == 'Completed'|overall_status == 'Enrolling by invitation'|overall_status == 'Active, not recruiting')
co_studies_c2<-co_studies_c[!duplicated(co_studies_c$nct_id),]


covid9c %>% write_csv('finaloutput/regCovid_all_studies-c.csv')



