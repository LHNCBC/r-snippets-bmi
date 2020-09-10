#regCOVID

#ClinicalTrials.gov Connection
# To run this script  first go to https://aact.ctti-clinicaltrials.org/connect and create a login
# Enter created username and password below 
user=username
psw=password

# Connects to AACt relational version of CTG database
# AACT is about 4 days delayed
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


#-Book A: Search Method A (Search for select keywords in the title of study)
# The most complete method in our analysis
# Limited to studies started after the first reporting of COVID-19
# Some studies started before this time were modified to include COVID-19 
# These studies were excluded for consistency and to avoid the inclusion of non COVID-19 studies

# Generates list of COVID-19 studies from search method A
co_studies_q<-"select * FROM studies
where study_first_submitted_date > '2019-12-27'  and (lower(official_title) like '%covid%' 
or lower(official_title) like '%sars-cov%'or lower(official_title) like '%2019-ncov%'
or lower(official_title) like '%cov2%'or lower(official_title) like '%cov-2%'
or lower(official_title) like '%coronavirus%'or lower(official_title) like '%corona virus%' )" 

co_studies<-dbGetQuery(con, co_studies_q)

# Finds the amount of COVID-19 studies with each study status
Counts_by_status<-co_studies %>% group_by(overall_status)%>% count()
colnames(Counts_by_status)[2]<-"Study_Count"
Counts_by_status %>% write_csv('finaloutput/regCovid_status_cnt.csv')

# We chose to only look at studies that have progressed naturally due to the activation of certain quality checks 
#This meant we included recruiting, active or completed
# Excluded not yet recruiting, terminated and withdrawn
covid9<- co_studies %>% filter(overall_status == 'Recruiting'| overall_status == 'Completed'|overall_status == 'Enrolling by invitation'|overall_status == 'Active, not recruiting')
covid9<-covid9%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

# Final list of analyzed COVID-19 studies
covid9 %>% write_csv('finaloutput/regCovid_all_studies-a.csv')



# Primary Analysis portion
# We analyzed each study type separately

#------Chapter 1: Covid studies by type
# int suffix refers to Interventional Trials
# obs suffix refers to Observational Studies
# reg suffix refers to Registry-based Studies

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
# This chapter finds the number of total studies over time for each type and plots it

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

# Generates a png of the plot of the total number of studies for each study type ove time

library(ggplot2)
png("finaloutput/regCovid_total_studies_time-a.png")

ggplot() + 
  geom_path(data = study_date2_int, aes(x = Date, y = study_count_int, group =1, color = 'Interventional')) +
  geom_path(data = study_date2_obs, aes(x = Date, y = study_count_obs, group =1, color= 'Observational')) +
  geom_path(data = study_date2_reg, aes(x = Date, y = study_count_reg, group =1,color = 'Registry')) +
  xlab('Date') + labs(color ='Study Type')+
  ylab('Study Count')+
 scale_x_discrete(breaks = study_date2_int$Date[seq(1, length(study_date2_int$Date), by = 30)])



dev.off() 





#------Chapter 3 Phase
# For Interventional trials we calculate the number of studies of each phase
# Only relevent for interventional trials

phase<-covid9_int %>% group_by(phase) %>% count()
colnames(phase)<-c("Phase", "Study_Count")
phase <- phase[with(phase, order(-Study_Count)), ]
phase %>% write_csv('finaloutput/regCovid_phase.csv')





#------Chapter 4 Arms
# For Interventional trial we calculate the number of sutides with a given number of arms
# Only relevent for interventional trials

covid_arms_cnt<-covid9_int %>% group_by(number_of_arms) %>% count()
colnames(covid_arms_cnt)<- c('Number_of_Arms', 'Count_of_Studies')
covid_arms_cnt %>% write_csv('finaloutput/regCovid_arm_cnts_int.csv')





#------Chapter 5 Enrollment
# For each study type we calcualted summary statistics for enrollment
# Enrollment can be actual or anticipated

#Interventional
# For interventional studies we calcualted enrollment separate for each phase

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
# Query the design and design_group tables
designs_b<-"select * FROM designs" 
designs<-dbGetQuery(con, designs_b)
design_group_b<-"select * FROM design_groups" 
design_groups<-dbGetQuery(con, design_group_b)

# Join tables to overall list of covid-19  studies
covid9_design<-left_join(covid9,designs, by = 'nct_id' )
covid9_design<-left_join(covid9_design,design_groups, by = 'nct_id' )

#Interventional Trial Design
#Group/ arm type for interventional trials
# Only relevent for interventional trials
covid9_design_int<-covid9_design %>%filter(study_type =='Interventional')
group_int<-covid9_design_int %>% group_by(group_type) %>% count()
colnames(group_int)[2]<-'Group_Count'
group_int <- group_int[with(group_int, order(-Group_Count)), ]

group_int %>% write_csv('finaloutput/regCovid_intervention_group_types.csv')


#Intervention Model
# Only relevent for interventional trials
covid9_design_int2 <- covid9_design_int [!duplicated(covid9_design_int[c(1)]),]

interventional_model<-covid9_design_int2 %>% group_by(intervention_model) %>%count()
colnames(interventional_model)[2]<-'Study_Count'
interventional_model <- interventional_model[with(interventional_model, order(-Study_Count)), ]
interventional_model %>% write_csv('finaloutput/regCovid_interventionl_model.csv')

# Primary purpose of the trial
# Only relevent for interventional trials
purpose<-covid9_design_int2 %>% group_by(primary_purpose) %>%count()
colnames(purpose)[2]<-'Study_Count'
purpose <- purpose[with(purpose, order(-Study_Count)), ]
purpose %>% write_csv('finaloutput/regCovid_primary_purpose.csv')




# Observational Study Design
# Observational Model 
# Only relevent for observational studies and registries
covid9_design_obs<-covid9_design %>%filter(study_type =='Observational')
covid9_design_obs2 <- covid9_design_obs [!duplicated(covid9_design_obs[c(1)]),]
observation_model<-covid9_design_obs2 %>% group_by(observational_model) %>%count()
colnames(observation_model)[2]<-'Study_Count'
observation_model <- observation_model[with(observation_model, order(-Study_Count)), ]
observation_model %>% write_csv('finaloutput/regCovid_observational_model.csv')

# Study time perspective (i.e. retrospective, prospective)
# Only relevent for observational studies and registries
time_perspective_obs<-covid9_design_obs2 %>% group_by(time_perspective) %>%count()
colnames(time_perspective_obs)[2]<-'Study_Count'
time_perspective_obs <- time_perspective_obs[with(time_perspective_obs, order(-Study_Count)), ]
time_perspective_obs %>% write_csv('finaloutput/regCovid_observational_time_perspective.csv')





# Rgistry Design
# Observational Model 
# Only relevent for observational studies and registries
covid9_design_reg<-covid9_design %>%filter(study_type =='Observational [Patient Registry]')
covid9_design_reg2 <- covid9_design_reg [!duplicated(covid9_design_reg[c(1)]),]
registry_model<-covid9_design_reg2 %>% group_by(observational_model) %>%count()
colnames(registry_model)[2]<-'Study_Count'
registry_model <- registry_model[with(registry_model, order(-Study_Count)), ]
registry_model %>% write_csv('finaloutput/regCovid_registry_model.csv')


# Time perspective 
# Only relevent for observational studies and registries
time_perspective_reg<-covid9_design_reg2 %>% group_by(time_perspective) %>%count()
colnames(time_perspective_reg)[2]<-'Study_Count'
time_perspective_reg <- time_perspective_reg[with(time_perspective_reg, order(-Study_Count)), ]
time_perspective_reg %>% write_csv('finaloutput/regCovid_registry_timeperspective.csv')

# Duration until follow-up
# Only relevent for registries
duration<-covid9_reg%>%group_by(target_duration)%>%count()
duration<-duration%>% filter(target_duration != 'NA')
colnames(duration)[2]<-'Study_Count'
duration <- duration[with(duration, order(-Study_Count)), ]
duration %>% write_csv('finaloutput/regCovid_follow-up_reg.csv')







#----- CHapter 7 Number of Sites
# Calculates the number of studies broken down by the number of facilities in the study
# query facilities table

facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)

# For all studies (includes all study types)
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
# Calculates the number of studies with at least one facility in a country
# counts are based on the combination of countries in a study
# Ex. if a study has 2 sites in the US and 1 in Canada, the study will be counted once for US|Canada

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






#------Chapter 9 Interventions
# All study types can list an intervention, not just interventional trials

#Query Intervention table

intervent_q<-"select* from interventions"
intervent<-dbGetQuery(con, intervent_q)
colnames(intervent)[4] <- "intervention_name"
covid9_intervent<- left_join(covid9, intervent, by= 'nct_id')

# Normalizing interventions
# COnverting to / to represent and
covid9_intervent$interventions_normalized <- tolower(covid9_intervent$intervention_name)
covid9_intervent$interventions_normalized<- gsub("\\+", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" or ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" and ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" combined ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" plus ", "/", covid9_intervent$interventions_normalized)
covid9_intervent$interventions_normalized<- gsub(" & ", "/", covid9_intervent$interventions_normalized)

# COunting interventions as the combinations and individuals

covid9_intervent2 <- covid9_intervent %>% separate_rows(interventions_normalized, sep ='/')
covid9_intervent2$interventions_normalized<- trimws(covid9_intervent2$interventions_normalized)
covid9_intervent3<- rbind(covid9_intervent,covid9_intervent2)
 
# Intervention Map
# Mapping free text fields to identical terms for semantically  similar values
intervention_map2 <- read_excel("intervention_map2.xlsx")
covid_int_studies_normal<-left_join(covid9_intervent3, intervention_map2, by = c('interventions_normalized' ='intervention') )
covid_int_studies_normal$mapped_intervention <- ifelse(is.na(covid_int_studies_normal$mapped_intervention), covid_int_studies_normal$interventions_normalized, covid_int_studies_normal$mapped_intervention)
covid_int_studies_normal <- within(covid_int_studies_normal, intervention_type[mapped_intervention  %like% "placebo" &  !mapped_intervention  %like% "/"] <- 'placebo')


#All Study Types
# Mapped Intervention
# Intervention counts after semantically mapping similar terms
covid_int_studies_normal2 <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped <- intervention_count_mapped %>% filter(intervention != 'NA')
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts-a.csv')

# Raw Intervention
# COunts with no semantic mapping
covid_int_studies_raw <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts-a.csv')

#Intervention Type
# COunts of studies with a given intervention type or combination of intervention types
covid_int_type <- covid_int_studies_normal [!duplicated(covid_int_studies_normal[c(1,66)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts.csv")

# Interventions Over Time
# Number of studies with a given intervention over time
# Taken in weekly incremnts 
Dates<-data.frame(seq(as.Date(min(covid_int_studies_normal2$study_first_submitted_date)), as.Date(max(covid_int_studies_normal2$study_first_submitted_date)), by="days"))
colnames(Dates)<-c("Date")

weekly_intervent2<-Dates
weekly_intervent2<- weekly_intervent2 %>%
  tq_transmute(select=Date,
               mutate_fun = apply.weekly, FUN =sum)
weekly_intervent2<-data.frame(weekly_intervent2[,1])

# Total of top 10 interventions at a given week
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
  
  # Total of new studies for top 10 interventions at a given week
  
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

# Plot of total studies for interventions at a given time
all_intervent_date2 <- melt(all_intervent_date, id.vars="Date")
all_intervent_date3<-all_intervent_date2%>% filter(value != 'NA')
png("finaloutput/regCovid_interventions_time-a.png")
ggplot()+ geom_path(data =all_intervent_date3, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('Total Study Count')
dev.off()


# Plot of new weekly studies for each intervention
intervent_date2 <- melt(weekly_intervent2, id.vars="Date")
png("finaloutput/regCovid_new_interventions_time-a.png")
ggplot()+ geom_path(data =intervent_date2, aes(x =Date, y=value, group = variable, color=variable))+ geom_line() +
  ylab('New Study Count')
dev.off()


#Interventional

# Mapped Intervention
# Counts of semantically mapped interventions
covid_int_studies_normal_int<-covid_int_studies_normal%>% filter(study_type == 'Interventional')
covid_int_studies_normal2 <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
inter_first_date<- covid_int_studies_normal2%>% group_by(mapped_intervention )%>% summarise(first(study_first_submitted_date))
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped<-intervention_count_mapped %>% group_by(intervention)%>% summarise(sum(Study_count), first(Intervention_type))
colnames(intervention_count_mapped)<-c('intervention', 'Study_count', 'Intervention_type')
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped<-intervention_count_mapped[,c(3,1,2)]
intervention_count_mapped9<-left_join(intervention_count_mapped,inter_first_date, by =c('intervention' ='mapped_intervention') )
colnames(intervention_count_mapped9)[4]<-'Date of First Study'
intervention_count_mapped9 %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_int-a.csv')

# Raw Intervention
# Counts of free text listed interventions
covid_int_studies_raw <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,66, 67)]),]
intervention_count_raw<-covid_int_studies_raw %>% group_by(intervention_type, intervention_name )%>%count()
colnames(intervention_count_raw)<- c("Intervention_type","intervention", "Study_count")
intervention_count_raw <- intervention_count_raw[with(intervention_count_raw, order(-Study_count)), ]
intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_raw_cnts_int-a.csv')

# Intervention Type
# Counts of studies for a given intervention type
covid_int_type <- covid_int_studies_normal_int [!duplicated(covid_int_studies_normal_int[c(1,66)]),]
covid_int_type<-covid_int_type[order(covid_int_type$intervention_type),]
studies_int_type2<-aggregate(covid_int_type, list(covid_int_type$nct_id), paste, collapse="|")
int_type_cnt<-  studies_int_type2%>% group_by(intervention_type)%>% count()
colnames(int_type_cnt)<-c("intervention_type","Study_Count")
int_type_cnt <- int_type_cnt[with(int_type_cnt, order(-Study_Count)), ]
int_type_cnt %>% write_csv("finaloutput/regCovid_intervention-type_cnts_int.csv")

#Interventions Over Time
# COunts of total and new studies for the 10 most prevelent interventions in weekly increments
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
intervent_date2$Date2<-as.Date(intervent_date2$Date)
intervent_date2 %>% write_csv('intervent_date.csv')
png("finaloutput/regCovid_new_interventions_time_int-a.png")
 ggplot()+geom_path(data =intervent_date2, aes(x =Date2, y=value, group = variable, color=variable)) +
  ylab('New Study Count')+ labs(variable = 'Intervention')


dev.off()


#Observational
covid_int_studies_normal_obs<-covid_int_studies_normal%>% filter(study_type == 'Observational')

# Mapped Intervention
# Counts of semantically mapped interventions
covid_int_studies_normal2 <- covid_int_studies_normal_obs [!duplicated(covid_int_studies_normal_obs[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped <- intervention_count_mapped %>% filter(intervention != 'NA')

intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_obs-a.csv')

#Raw Intervention
# Counts of ree text interventions
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
# COunts of total and new studies for the 10 most prevelent interventions in weekly increments
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
# Counts of semantically mapped interventions
covid_int_studies_normal_reg<-covid_int_studies_normal%>% filter(study_type == 'Observational [Patient Registry]')
covid_int_studies_normal2 <- covid_int_studies_normal_reg [!duplicated(covid_int_studies_normal_reg[c(1,66, 70)]),]
intervention_count_mapped<-covid_int_studies_normal2 %>% group_by(intervention_type, mapped_intervention )%>%count()
colnames(intervention_count_mapped)<- c("Intervention_type","intervention", "Study_count")
intervention_count_mapped <- intervention_count_mapped[with(intervention_count_mapped, order(-Study_count)), ]
intervention_count_mapped <- intervention_count_mapped %>% filter(intervention != 'NA')

intervention_count_mapped %>% write_csv('finaloutput/regCovid_interventions_mapped_cnts_reg-a.csv')

#Raw Intervention
# Counts of free text interventions
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
# COunts of total and new studies for the 10 most prevelent interventions in weekly increments
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



#------Chapter 10 History
# Update history is not available through AACT or CTG API
# Requires web-scraping of CTG study records

total_history<-data.frame()
# These two studies do not include an update history as their records were removed

covid92<-covid9%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )
# Go to each record and scrape update history
for(i in seq_len(nrow(covid92))){
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92$nct_id[i])
  history3<- htmltab(url, which =1, rm_nodata_cols = F)
  history4<- data.frame(covid92$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

# Change Type
# Number of changes done by the set of studies and number of studies doing a specific type of change
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

# Number of Updates
# Number of studies counted by the number of updates done by each study including initial registration
update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts.csv')

# Days Since Update
# Number of days between the last update for a study and today 
covid9$days_since_update<-Sys.Date()-covid9$last_update_submitted_date
Days_since_update<-covid9 %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update.csv')

# Updates by country
# The rate of how many updates all studies from a given country does
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

# NCTs with deleted records
covid92_int<-covid9_int%>% filter(nct_id != 'NCT04331860'& nct_id!= 'NCT04372069' )

for(i in seq_len(nrow(covid92_int))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid92_int$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid92_int$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
  total_history<-rbind(total_history, history4)
}
colnames(total_history)<-c("nct_id", "version","Date", "Changes")

#Change Type
# Number of changes done by the set of studies and number of studies doing a specific type of change

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

Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_int.csv')


# Number of Updates
# Number of studies counted by the number of updates done by each study including initial registration
update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_int.csv')


# Days Since Update
# Number of days between the last update for a study and today 
covid9_int$days_since_update<-Sys.Date()-covid9_int$last_update_submitted_date

Days_since_update<-covid9_int %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_int.csv')

#Country Updates
# The rate of how many updates all studies from a given country does
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


#Change Type
# Number of changes done by the set of studies and number of studies doing a specific type of change
history_change_per_row$Changes<- trimws(history_change_per_row$Changes)
CHange_type<-history_change_per_row %>% group_by(Changes)%>%count()
history_change <- history_change_per_row [!duplicated(history_change_per_row[c(1,4)]),]
CHange_type_study<-history_change %>% group_by(Changes)%>%count()
Change_type2<-left_join(CHange_type,CHange_type_study, by = 'Changes' )
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]


Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_obs.csv')

# Number of Updates
# Number of studies counted by the number of updates done by each study including initial registration
update_counts<-total_history %>% group_by(nct_id )%>%count()
update_counts2<-update_counts %>% group_by(n )%>%count()

colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_obs.csv')

# Days Since Update
# Number of days between the last update for a study and today 
covid9_obs$days_since_update<-Sys.Date()-covid9_obs$last_update_submitted_date

Days_since_update<-covid9_obs %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_obs.csv')

#Country Updates
# The rate of how many updates all studies from a given country does
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

#Change Type
# Number of changes done by the set of studies and number of studies doing a specific type of change
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

Change_type2 %>%write_csv('finaloutput/regCovid_change_type_cnt_reg.csv')

# Number of Updates
# Number of studies counted by the number of updates done by each study including initial registration
update_counts<-total_history %>% group_by(nct_id)%>%count()
update_counts2<-update_counts %>% group_by(n)%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_update_counts_reg.csv')


# Days Since Update
# Number of days between the last update for a study and today 
covid9_reg$days_since_update<-Sys.Date()-covid9_reg$last_update_submitted_date

Days_since_update<-covid9_reg %>% group_by(days_since_update)%>%count()
colnames(Days_since_update)<-c("Days since Last update", "COunt of Studies")
Days_since_update %>% write_csv('finaloutput/regCOvid_Days_since_update_reg.csv')


#Country Updates
# The rate of how many updates all studies from a given country does
total_history_uni <- total_history [!duplicated(history_change_per_row[c(1,3)]),]

country_update1<-left_join(total_history_uni, facil_uniq, by = 'nct_id')
cahnges_by_country<-country_update1%>%group_by(country)%>% count()
cahnges_by_country2<-left_join(cahnges_by_country,country_cnts_reg, by =c('country'= 'Country') )
cahnges_by_country2$n<-cahnges_by_country2$n-cahnges_by_country2$Study_Count
cahnges_by_country2$changes_per_study<-cahnges_by_country2$n/cahnges_by_country2$Study_Count
cahnges_by_country2 <- cahnges_by_country2[with(cahnges_by_country2, order(-changes_per_study)), ]

colnames(cahnges_by_country2)[2]<-'Total_changes'
cahnges_by_country2%>% write_csv('finaloutput/regCovid_country_updates_reg.csv')






#-----Chapter 11 Vaccines
# Monitoring of COVID-19 vaccine trials
# No vaccine study type (listed as interventional)
# used keyword search for vaccine in study title

covid9_vacc<- covid9_int %>% filter (tolower(official_title)  %like% "vaccine" )

# Study count by trial phase
vacc_phase<- covid9_vacc %>% group_by(phase) %>% count()
colnames(vacc_phase)[2]<- 'Study_Count'

covid9_vacc %>% write_csv('finaloutput/regCovid_vaccines.csv')
vacc_phase %>% write_csv('finaloutput/regCovid_vaccines_phase.csv')
vaccine_country<-left_join(covid9_vacc, facil_uniq, by= 'nct_id')
vaccine_country_cnt <-vaccine_country %>% group_by(country)%>% count()
colnames(vaccine_country_cnt)[2]<- 'Study_Count'
vaccine_country_cnt %>% write_csv('finaloutput/regCovid_vaccines_country.csv')

covid9_facil_vacc<-left_join(covid9_vacc, facil, by= 'nct_id')
covid9_sites_vacc<-covid9_facil_vacc%>%group_by(nct_id)%>% count()
site_counts_vacc<-  covid9_sites_vacc%>%group_by(n)%>% count()
colnames(site_counts_vacc)<-c("Site_count","Study_Count")
site_counts_vacc %>% write_csv("finaloutput/regCovid_vaccines_site_cnts.csv")



# Update History
total_history<-data.frame()

for(i in seq_len(nrow(covid9_vacc))){
  
  url <- paste0('https://clinicaltrials.gov/ct2/history/', covid9_vacc$nct_id[i])
  
  history3<- htmltab(url, which =1)
  history4<- data.frame(covid9_vacc$nct[i],history3$Version, history3$`Submitted Date`, history3$Changes)
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
colnames(Change_type2)<-c("Change Type", "Count_of_Changes", "Count_of_Studies")
Change_type2$ratio<-Change_type2$Count_of_Changes/Change_type2$Count_of_Studies
Change_type2 <- Change_type2[with(Change_type2, order(-Count_of_Changes)), ]

Change_type2 %>%write_csv('finaloutput/regCovid_vaccines_change_type.csv')

update_counts<-total_history %>% group_by(nct_id)%>%count()
update_counts2<-update_counts %>% group_by(n)%>%count()
colnames(update_counts2)<-c("Count of Versions", "Count of Studies")
update_counts2 %>% write_csv('finaloutput/regCOvid_vaccines_update.csv')



# Number of arms for each study
vacc_arm_cnts<-covid9_vacc %>% group_by(number_of_arms) %>% count()
colnames(vacc_arm_cnts)[2]<- 'Study_Count'
vacc_arm_cnts %>% write_csv('finaloutput/regCovid_vaccines_arms.csv')

# Interventiona Model
vacc_design<-left_join(covid9_vacc, designs, by='nct_id')
vacc_model<-vacc_design %>% group_by(intervention_model) %>%count()
colnames(vacc_model)[2]<- 'Study_Count'
vacc_model %>% write_csv('finaloutput/regCovid_vaccines_model.csv')

# Enrollment (Actual and Anticipated)
Measures<-data.frame(Measures= c('Min','1st Qu','Median', 'Mean', '3rd Qu','Max'))
covid9_vacc_en<- covid9_vacc %>% filter(enrollment != 'NA')
enroll_sum_vacc<-tibble(summary(covid9_vacc_en$enrollment))
enroll_sum_vacc<-cbind(Measures, enroll_sum_vacc)
colnames(enroll_sum_vacc)<-c("Measure", "Vaccine")
enroll_sum_vacc %>% write_csv('finaloutput/regCovid_vaccines_enroll.csv')



# Group/Arm Types
covid9_design_vacc<-left_join(covid9_vacc,design_groups, by = 'nct_id' )
group_int_vacc<-covid9_design_vacc %>% group_by(group_type) %>% count()
colnames(group_int_vacc)[2]<-'Group_Count'
group_int_vacc <- group_int_vacc[with(group_int_vacc, order(-Group_Count)), ]
group_int_vacc <- group_int_vacc[with(group_int_vacc, order(-Group_Count)), ]
group_int_vacc %>% write_csv('finaloutput/regCovid_vaccines_group_types.csv')



# Secondary Analysis 
# Chapter 12: Completed Interventional trials
# Used in supplementary analysis of linked pubmed articles
completed_int<-covid9_int %>% filter(overall_status == 'Completed')

completed_int %>% write_csv('finaloutput/regCovid_completed_int.csv')


#Chapter 13: Has Results on CTG
covid_int_results_int<-covid9_int %>% filter(!is.na(results_first_submitted_date))
covid_int_results_obs<-covid9_obs %>% filter(!is.na(results_first_submitted_date))
covid_int_results_reg<-covid9_reg %>% filter(!is.na(results_first_submitted_date))


covid_int_results_int %>% write_csv('finaloutput/regCovid_has_results_int.csv')
covid_int_results_obs %>% write_csv('finaloutput/regCovid_has_results_obs.csv')
covid_int_results_reg %>% write_csv('finaloutput/regCovid_has_results_reg.csv')





# Secondary Search Methods

#Book B:Search Method B (free text condition)
# Seconed search method searches for keywords in free text condition field
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
# Third search method: includes studies with MeSH term coronavirus infections

covid_qc<-"select * from studies s join browse_conditions c on s.nct_id = c.nct_id
where study_first_submitted_date > '2019-12-27' and downcase_mesh_term = 'coronavirus infections'"
co_studies_c<-dbGetQuery(con, covid_qc)
co_studies_c$nct_id = NULL

covid9c<- co_studies_c %>% filter(overall_status == 'Recruiting'| overall_status == 'Completed'|overall_status == 'Enrolling by invitation'|overall_status == 'Active, not recruiting')
co_studies_c2<-co_studies_c[!duplicated(co_studies_c$nct_id),]


covid9c %>% write_csv('finaloutput/regCovid_all_studies-c.csv')



