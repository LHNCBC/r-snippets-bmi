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


library(RCurl)
x <- getURL

# Read into R lists of studies from project repository

covid9<-read_csv(url('https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_all_studies-a.csv'))
covid9_int<-read_csv(url('https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_int_a.csv'))
covid9_obs<-read_csv(url('https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_obs_a.csv'))
covid9_reg<-read_csv(url('https://raw.githubusercontent.com/lhncbc/r-snippets-bmi/master/regCOVID/regCovid_registry_a.csv'))
