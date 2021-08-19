#regCOVIDpublications-Linked publications to COVID-19 Clinical studies Code

# Load required libraries
library(easyPubMed)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(stringr)
library(lubridate)
library(tibble)

#Read in of all COVID-19 Studies generated from regCOVID and all nessecary information from CTG
CTG9<-read_csv('regCovid_all_studies-a.csv')

library(RPostgreSQL)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)

# FInd study references
pub_q<-'select * from study_references'
pub_ctg<-dbGetQuery(con, pub_q)

#Adding Trial intervention
inter_q<-"select * from interventions"
inter<-dbGetQuery(con, inter_q)
inter2<-inter%>%select(,c(2,4))

intervention<-read_csv('regCovid_intervention_per_row.csv'))%>% select(,c(1,70))
intervention_group<-aggregate(inter2, list(inter2$nct_id), paste, collapse="|")

# COuntry of Sites and US site flag
facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)
facil_uniq<- facil [!duplicated(facil[c(2,8)]),]


#---- Chapter 1: Abstract-Linked search (PubMED)
# Searching PubMed for linked publications by NCT ID (CLinicalTrials.gov identifier) in secondary identifier
# Identify linked publications
CTG9_df<-data.frame(CTG9)
nct<-CTG9_df[,1]
pubmed<-function(nct){
  out <- tryCatch({
    my_id<-data.frame()
    
    #search criteria
    my_query <- paste0(nct,'[si]')
   
    #Search
    #will return PMIDs when search returns a hit or will escape with no hit 
     my_entrez_id <- get_pubmed_ids(my_query)
    my_id<-data.frame(my_entrez_id$IdList)
    my_id$nct_id<-nct
    return(my_id)
  }
  , error=function(e) {write(nct,file="pubmed_error.txt",append=TRUE)})}

ids<-lapply(nct, pubmed)    %>% bind_rows()

#To ensure better formatting and find all useful information run each Pubmed ID again
#Second search due to aggregating to one row per publication/NCT ID
#Runs search of previously found PMIDs
ids2<-melt( ids, id.vars='nct_id')
ids3<-ids2 %>%filter(value!='NA')
ids4<-ids3[,3]
my_id2<-data.frame()
pubmed2<-function(ids4){
  out <- tryCatch({
    #Setup
    my_id<-data.frame()
    my_substance2<-' '
    my_mesh2<-' '
    
    #Grabbing information from PubMED
    my_entrez_id <- get_pubmed_ids(ids4)
    my_id<-data.frame(my_entrez_id$IdList)
    my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id)
    
    #Finds article type, substance, mesh, title, abstract, journal and pub date
    my_type <- data.frame(custom_grep(my_abstracts_xml, "PublicationType", "char"))
    colnames(my_type)<-'ArticleType'
    my_type <- my_type%>% arrange(-ArticleType)
    my_substance<- data.frame(custom_grep(my_abstracts_xml, "NameOfSubstance", "char"))
    my_substance<-rbind(my_substance,my_substance2)
    my_mesh<- data.frame(custom_grep(my_abstracts_xml, "DescriptorName", "char"))
    my_mesh<-rbind(my_mesh,my_mesh2)
    my_abstracts_xml_list <- articles_to_list(my_abstracts_xml)
    article_df<-article_to_df(pubmedArticle = my_abstracts_xml_list[[1]], autofill = FALSE)
    date<-article_df[1,5:7]
    my_year<-paste0(date$year,'-',date$month, '-',date$day)
    my_journal <- article_df$journal[1]
    my_titles <- article_df$title[1]
    pubmed_pub_list<-  cbind(my_id,  my_type)
    pubmed_pub_list_1<-aggregate(pubmed_pub_list, list(pubmed_pub_list$Id), paste, collapse="|")%>%select(,c(1,3))
    pubmed_pub_list2<-cbind(my_id, my_substance)
    pubmed_pub_list2_1<-aggregate(pubmed_pub_list2, list(pubmed_pub_list2$Id), paste, collapse="|")%>%select(,c(1,3))
    pubmed_pub_list3<-cbind(my_id, my_mesh)
    pubmed_pub_list3_1<-aggregate(pubmed_pub_list3, list(pubmed_pub_list3$Id), paste, collapse="|")%>%select(,c(1,3))
    pubmed_pub_list_full<-  merge(x=pubmed_pub_list_1,  y=pubmed_pub_list2_1, by.x= 'Group.1', by.y= 'Group.1')
    pubmed_pub_list_full2<-  merge(x=pubmed_pub_list_full,y=pubmed_pub_list3_1, by.x='Group.1', by.y= 'Group.1') 
    pubmed_pub_list_full3<-cbind(pubmed_pub_list_full2,my_titles, my_year, my_journal)
    my_abstract <- data.frame(custom_grep(my_abstracts_xml, "Abstract", "char"))
    pubmed_pub_list_0<-  cbind(pubmed_pub_list_full3,  my_abstract)
    
    
    colnames(pubmed_pub_list_0)<-c('pmid', 'ArticleType', 'Substance', 'MeSH', 'Title', 'PubDate', 'Journal', 'Article_Abstract')
    return(pubmed_pub_list_0)
    
    
  }
  , error=function(e) {})}

result_pubmed<-lapply(ids4, pubmed2)    %>% bind_rows()


# Link NCT ID to PMID
pubmed_pubs_all<-merge(x=result_pubmed, y=ids3,  by.x='pmid',by.y='value')
result2_1 <- pubmed_pubs_all [!duplicated(pubmed_pubs_all[c(1,8)]),]


#Add source and include useful columns
result2_1$pub_source<-'Abstract'
result2_2<-result2_1%>%select(,c(1:9,11))




#------ Chapter 2: Registry-Linked Search
# Search on ClinicalTrials.gov (CTG) for results publications

# COnnect to AACT relational CTG version


# Filter by CTG studies and only results references
CTG_ctgpub<-left_join(CTG9, pub_ctg, by = 'nct_id')
CTG_ctgpub_res<-CTG_ctgpub %>% filter(reference_type =='results_reference')
CTG_ctgpub_res2<-CTG_ctgpub_res %>% select(,c(1,31,34,66))
names(CTG_ctgpub_res2)

# Only use PubMed indexed publications
ctg_pubs<-CTG_ctgpub_res2 %>% filter(pmid!='NA')
ctg_pubs2<-data.frame(ctg_pubs)
pmid<-ctg_pubs2[,4]
# Connect referenced publications to the PubMed record
#Find useul publication data
my_id2<-data.frame()
pubmed3<-function(pmid){
  out <- tryCatch({
    my_id<-data.frame()
    my_substance2<-' '
    my_mesh2<-' '
    my_entrez_id <- get_pubmed_ids(pmid)
    my_id<-data.frame(my_entrez_id$IdList)
    
    my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id)
    my_type <- data.frame(custom_grep(my_abstracts_xml, "PublicationType", "char"))
    colnames(my_type)<-'ArticleType'
    my_type <- my_type%>% arrange(-ArticleType)
    
    my_substance<- data.frame(custom_grep(my_abstracts_xml, "NameOfSubstance", "char"))
    my_substance<-rbind(my_substance,my_substance2)
    my_mesh<- data.frame(custom_grep(my_abstracts_xml, "DescriptorName", "char"))
    my_mesh<-rbind(my_mesh,my_mesh2)
    
    my_abstracts_xml_list <- articles_to_list(my_abstracts_xml)
    article_df<-article_to_df(pubmedArticle = my_abstracts_xml_list[[1]], autofill = FALSE)
    date<-article_df[1,5:7]
    my_year<-paste0(date$year,'-',date$month, '-',date$day)    
    my_journal <- article_df$journal[1]
    my_titles <- article_df$title[1]
    
    pubmed_pub_list<-  cbind(my_id,  my_type)
    pubmed_pub_list_1<-aggregate(pubmed_pub_list, list(pubmed_pub_list$Id), paste, collapse="|")%>%select(,c(1,3))
    
    pubmed_pub_list2<-cbind(my_id, my_substance)
    pubmed_pub_list2_1<-aggregate(pubmed_pub_list2, list(pubmed_pub_list2$Id), paste, collapse="|")%>%select(,c(1,3))
    pubmed_pub_list3<-cbind(my_id, my_mesh)
    pubmed_pub_list3_1<-aggregate(pubmed_pub_list3, list(pubmed_pub_list3$Id), paste, collapse="|")%>%select(,c(1,3))
    pubmed_pub_list_full<-  merge(x=pubmed_pub_list_1,  y=pubmed_pub_list2_1, by.x= 'Group.1', by.y= 'Group.1')
    pubmed_pub_list_full2<-  merge(x=pubmed_pub_list_full,y=pubmed_pub_list3_1, by.x='Group.1', by.y= 'Group.1') 
    pubmed_pub_list_full3<-cbind(pubmed_pub_list_full2,my_titles, my_year, my_journal)
    
    my_abstract <- data.frame(custom_grep(my_abstracts_xml, "Abstract", "char"))
    pubmed_pub_list_0<-  cbind(pubmed_pub_list_full3,  my_abstract)
    
    colnames(pubmed_pub_list_0)<-c('pmid', 'ArticleType', 'Substance', 'MeSH', 'Title', 'PubDate', 'Journal', 'Article_Abstract')
    return(pubmed_pub_list_0)
    
    
  }
  , error=function(e) {write(pmid,file="pubmed_error.txt",append=TRUE)})}

result_ctg<-lapply(pmid, pubmed3)    %>% bind_rows()

ctg_pubs_all<-merge(result_ctg, ctg_pubs,  by ='pmid')%>%select(,c(1:9))
ctg_pubs_all2_1 <- ctg_pubs_all [!duplicated(ctg_pubs_all[c(1,9)]),]

ctg_pubs_all2_1$pub_source<-'Registry'





#-----Chapter 3: Combine results from PubMed (ABstract) and CTG (Registry)
# Combine registry and abstract linked results

regCTGpubs2<-rbind(result2_2,ctg_pubs_all2_1 )
regCTG_pubs<-left_join(regCTGpubs2, CTG9, by ='nct_id')


# Add links to article and CTG page
regCTG_pubs$pubmed_links<-paste0('https://pubmed.ncbi.nlm.nih.gov/',regCTG_pubs$pmid)
regCTG_pubs$ctg_links<-paste0('https://clinicaltrials.gov/ct2/show/',regCTG_pubs$nct_id)


#--------Book 2: Analysis
#--------Chapter 4. COunts of publications for each study
#Counts of publications by source and in total
regCTG_pubs_cnts<-regCTG_pubs %>% group_by(nct_id,pub_source) %>% count()
regCTG_pubs_cnts2<-regCTG_pubs_cnts%>%dcast( nct_id ~ pub_source,sum)
regCTG_pubs2<-left_join(regCTG_pubs, regCTG_pubs_cnts2, by ='nct_id')
regCTG_pubs2$Total_pubs<-regCTG_pubs2$Abstract+regCTG_pubs2$Registry



#--------Chapter 5. Adding key information and flags for useful criteria
# Flags: 1=True, 0=False




#US site flag: does the study have at least one site in US
CTG9_facil_us<-facil_uniq %>% filter(country=='United States') 
regCTG_pubs2$US_site_flag<-0
trial_list3<- left_join(regCTG_pubs2,CTG9_facil_us, by='nct_id' )
trial_list3 <- within(trial_list3, US_site_flag[country != 'NA'] <- 1)
trial_list4<-trial_list3%>%select(,c(1:80))
facil_country<-aggregate(facil_uniq, list(facil_uniq$nct_id), paste, collapse="|")
trial_list5<-left_join(trial_list4, facil_country, by=c('nct_id'= 'Group.1'))%>%select(,c(1:80, 88))


# Add flag for CTG deposited results
CTG9_res<-CTG9 %>% filter(!is.na(results_first_posted_date))%>%select(,c(1,11))
trial_list6<-left_join(trial_list5,CTG9_res, by ='nct_id')
colnames(trial_list6)[82]<-'Has_Results'
trial_list6$Has_Results<- trial_list6$Has_Results %>%as.character()
trial_list6 <- within(trial_list6, Has_Results[Has_Results!='NA'] <- 1)
trial_list6 <- within(trial_list6, Has_Results[is.na(Has_Results)] <- 0)




#Update
#Number of Updates
update_counts_all<-read_csv('regCovid_updates_by_study.csv')
trial_list2_updtes<-left_join(trial_list6, update_counts_all, by ='nct_id')
trial_list2_updtes$n<-trial_list2_updtes$n-1
colnames(trial_list2_updtes)[83]<-"Number_of_updates"


# Add intervention
trial_list7_expanded<-left_join(trial_list2_updtes, intervention_group, by=c('nct_id'= 'Group.1'))

#-Final master file organization
trial_list8<-trial_list7_expanded%>%select(,c(1:10,30,38,40,43,45,47,50,54,75:83,85))

colnames(trial_list8)[28]<-'intervention'


# Remove publications from prior to start date (Misclassification)
trial_list9<-trial_list8


#Ranks for generating attention score for prioritization
##Phase
trial_list9$phase_rank<-0
trial_list9 <- within(trial_list9, phase_rank[phase=='Phase 2/Phase 3'|phase== 'Phase 3'|phase== 'Phase 4'] <- 1)
trial_list9 <- within(trial_list9, phase_rank[phase=='Phase 1/Phase 2'|phase== 'Phase 2'] <- 0.5)

##Pubdate
trial_list9$days_since_pub<- as.numeric( Sys.Date()- as.Date(trial_list9$PubDate))
trial_list9$date_rank<-1-(trial_list9$days_since_pub/max(trial_list9$days_since_pub))
trial_list9 <- within(trial_list9, date_rank[days_since_pub>1800] <- -0.25)

trial_list9 <- within(trial_list9, date_rank[days_since_pub>3600] <- -0.5)




#Trial Updates
trial_list9$update_Rank<-0
trial_list9 <- within(trial_list9, update_Rank[Number_of_updates>0] <- 0.5)
trial_list9 <- within(trial_list9, update_Rank[Number_of_updates>2] <- 1)    

##protocol
trial_list9$protocol<-0
trial_list9<- within(trial_list9, protocol[grepl("Protocol",ArticleType)] <- -1)





trial_list9$score<-trial_list9$US_site_flag+trial_list9$phase_rank + trial_list9$date_rank+trial_list9$update_Rank+ trial_list9$protocol
trial_list10<-trial_list9%>%select(,c(1:28,34))%>%arrange(-score)



#Adding Vaccines flag for  subset

covid9_vacc<-read_csv('regCovid_vaccines.csv')

trial_list_vscc<-left_join(trial_list10,covid9_vacc, by='nct_id') %>% select(,c(1:69))
colnames(trial_list_vscc)[30]<-'vaccine_trial_flag'
trial_list_vscc <- within(trial_list_vscc, vaccine_trial_flag[vaccine_trial_flag != 'NA'] <- 1)
trial_list_vscc <- within(trial_list_vscc, vaccine_trial_flag[is.na(vaccine_trial_flag)] <- 0)
trial_list_vscc0<-trial_list_vscc
trial_list_vscc0$PubDate<-as.Date(trial_list_vscc0$PubDate)
trial_list_vscc01<-trial_list_vscc0%>%filter(PubDate>'2019-12-25')
trial_list_vscc2<-trial_list_vscc01%>%select(,c(1:30))
trial_list_vscc2_0<-trial_list_vscc01%>%select(,c(1:28,30))


#Write master file

trial_list_vscc2%>%write_csv('regCovidpublications_Master.csv')







# ---------Chapter 6. Publications from all trials


#------Section 6.1: All Trial Publications List
trial_list9$PubDate<-as.Date(trial_list9$PubDate)
trial_list9<-trial_list9%>%filter(PubDate>'2019-12-25')
trial_list9_rep<-trial_list9%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
trial_list9%>%write_csv('regCovidpublications_publication_list_all.csv')

#Article_type
type <- trial_list9 [!duplicated(trial_list9[c(1)]),]
type1<-type%>%filter(pmid!='NA')
type2<-type1%>% group_by(ArticleType)%>% count()
type2<-type2%>%arrange(-n)
colnames(type2)[2]<-'Article_Count'
type2%>%write_csv('regCovidpublications_articletype_cnt_all.csv')

#------Section 6.2: interventional Trials Publications
pub_int<-trial_list9%>%filter(study_type=='Interventional')
pub_int_rep<-pub_int%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_int%>%write_csv('regCovidpublications_publication_list_int.csv')


#interventional Article type
type_int <- pub_int [!duplicated(pub_int[c(1)]),]
type1_int<-type_int%>%filter(pmid!='NA')
type2_int<-type1_int%>% group_by(ArticleType)%>% count()
type2_int<-type2_int%>%arrange(-n)
colnames(type2_int)[2]<-'Article_Count'
type2_int%>%write_csv('regCovidpublications_articletype_cnt_int.csv')

#------Section 6.3: observational Publications
pub_obs<-trial_list9%>%filter(study_type=='Observational')
pub_obs_rep<-pub_obs%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_obs%>%write_csv('regCovidpublications_publication_list_obs.csv')


#observational Article type
type_obs <- pub_obs [!duplicated(pub_obs[c(1)]),]
type1_obs<-type_obs%>%filter(pmid!='NA')
type2_obs<-type1_obs%>% group_by(ArticleType)%>% count()
type2_obs<-type2_obs%>%arrange(-n)
colnames(type2_obs)[2]<-'Article_Count'
type2_obs%>%write_csv('regCovidpublications_articletype_cnt_obs.csv')

#------Section 6.4: Registry Trials Publications
pub_reg<-trial_list9%>%filter(study_type=='Observational [Patient Registry]')
pub_reg_rep<-pub_reg%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_reg%>%write_csv('regCovidpublications_publication_list_reg.csv')


#Registry Article type
type_reg <- pub_reg [!duplicated(pub_reg[c(1)]),]
type1_reg<-type_reg%>%filter(pmid!='NA')
type2_reg<-type1_reg%>% group_by(ArticleType)%>% count()
type2_reg<-type2_reg%>%arrange(-n)
colnames(type2_reg)[2]<-'Article_Count'
type2_reg%>%write_csv('regCovidpublications_articletype_cnt_reg.csv')



#-----Chapter 7: intervention overview

trial_list11<-trial_list10
trial_list11$PubDate<-as.Date(trial_list11$PubDate)
trial_list12<-trial_list11%>%filter(PubDate>'2019-12-25')


int_per_row<-read_csv('regCOVID_intervention_per_row.csv')
trial_int<-left_join(trial_list12,int_per_row, by ='nct_id' )
trial_int2<-trial_int%>%group_by(pmid, mapped_intervention)%>%count()
trial_int3<-trial_int2%>%group_by( mapped_intervention)%>%count()
colnames(trial_int3)[2]<-'NUmber_of_Publications'
int_overview<-read_csv('regCovid_intervention-phase_cnts_int.csv')
trial_int4<-left_join(int_overview, trial_int3, by =c( 'intervention'='mapped_intervention'))

trial_int4%>%write_csv('regCovid_intervention-phase_cnts_int2.csv')



#-------Chapter 8: Trial list

#All trials


#US site flag: does the study have at least one site in US
all_list<-CTG9
all_list$US_site_flag<-0
all_list3<- left_join(all_list,CTG9_facil_us, by='nct_id' )
all_list3 <- within(all_list3, US_site_flag[country != 'NA'] <- 1)
all_list4<-all_list3%>%select(,c(1:65))

all_list5<-left_join(all_list4, facil_country, by=c('nct_id'= 'Group.1'))%>%select(,c(1:65, 73))


# Add flag for CTG deposited results
all_list6<-left_join(all_list5,CTG9_res, by ='nct_id')
colnames(all_list6)[67]<-'Has_Results'
all_list6$Has_Results<- all_list6$Has_Results %>%as.character()
all_list6 <- within(all_list6, Has_Results[Has_Results!='NA'] <- 1)
all_list6 <- within(all_list6, Has_Results[is.na(Has_Results)] <- 0)




#Updte
#Number of Updates
all_list2_updtes<-left_join(all_list6, update_counts_all, by ='nct_id')
all_list2_updtes$n<-all_list2_updtes$n-1
colnames(all_list2_updtes)[68]<-"Number_of_updates"



all_list7_expanded<-left_join(all_list2_updtes, intervention_group, by=c('nct_id'= 'Group.1'))
colnames(all_list7_expanded)[70]<-'intervention'
all_list_vscc<-left_join(all_list7_expanded,covid9_vacc, by='nct_id') %>% select(,c(1:71))
colnames(all_list_vscc)[71]<-'vaccine_trial_flag'
all_list_vscc <- within(all_list_vscc, vaccine_trial_flag[vaccine_trial_flag != 'NA'] <- 1)
all_list_vscc <- within(all_list_vscc, vaccine_trial_flag[is.na(vaccine_trial_flag)] <- 0)

Full_trials<-bind_rows(trial_list_vscc2, all_list_vscc)%>%select(,c(9,11:23,25:28, 30))
Full_trials2 <- Full_trials [!duplicated(Full_trials[c(1)]),]
Full_trials2%>%write_csv('regCOVIDpublications_trials_all.csv')

full_int<-Full_trials2%>%filter(study_type.x=='Interventional')
full_int%>%write_csv('regCOVIDpublications_trials_int.csv')

full_obs<-Full_trials2%>%filter(study_type.x=='Observational')
full_obs%>%write_csv('regCOVIDpublications_trials_obs.csv')

full_reg<-Full_trials2%>%filter(study_type.x=='Observational [Patient Registry]')
full_reg%>%write_csv('regCOVIDpublications_trials_reg.csv')
