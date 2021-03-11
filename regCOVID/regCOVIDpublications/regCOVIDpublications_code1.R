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

#Read in of all COVID-19 Studies generated from regCOVID
covid9<-read_csv('regCovid_all_studies-a.csv')

#---- Chapter 1: Abstract-Linked search (PubMED)
# Searching PubMed for linked publications by NCT ID (CLinicalTrials.gov identifier) in secondary identifier
# Identify linked publications
covid9_df<-data.frame(covid9)
nct<-covid9_df[,1]
pubmed<-function(nct){
  out <- tryCatch({
    my_id<-data.frame()
    my_query <- paste0(nct,'[si]')
    my_entrez_id <- get_pubmed_ids(my_query)
    my_id<-data.frame(my_entrez_id$IdList)
    my_id$nct_id<-nct
    return(my_id)
  }
  , error=function(e) {write(nct,file="pubmed_error.txt",append=TRUE)})}

ids<-lapply(nct, pubmed)    %>% bind_rows()

#To ensure better formatting and find all useful information run each Pubmed ID again
#Second search due to aggregating to one row per publication/NCT ID

ids2<-melt( ids, id.vars='nct_id')
ids3<-ids2 %>%filter(value!='NA')
ids4<-ids3[,3]
my_id2<-data.frame()
pubmed2<-function(ids4){
  out <- tryCatch({
    my_id<-data.frame()
    my_substance2<-' '
    my_mesh2<-' '
    my_entrez_id <- get_pubmed_ids(ids4)
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
    colnames(pubmed_pub_list_full3)<-c('pmid', 'ArticleType', 'Substance', 'MeSH', 'Title', 'PubDate', 'Journal')
    return(pubmed_pub_list_full3)
    
    
  }
  , error=function(e) {})}

result_pubmed<-lapply(ids4, pubmed2)    %>% bind_rows()


# Link NCT ID to PMID
pubmed_pubs_all<-merge(x=result_pubmed, y=ids3,  by.x='pmid',by.y='value')
result2_1 <- pubmed_pubs_all [!duplicated(pubmed_pubs_all[c(1,8)]),]


#Add source and include useful columns
result2_1$pub_source<-'Abstract'
result2_2<-result2_1%>%select(,c(1:8,10))




#------ Chapter 2: Registry-Linked Search
# Search on ClinicalTrials.gov (CTG) for results publications

# COnnect to AACT relational CTG version

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)

# FInd study references
pub_q<-'select * from study_references'
pub_ctg<-dbGetQuery(con, pub_q)

# Filter by COVID studies and only results references
covid_ctgpub<-left_join(covid9, pub_ctg, by = 'nct_id')
covid_ctgpub_res<-covid_ctgpub %>% filter(reference_type =='results_reference')
covid_ctgpub_res2<-covid_ctgpub_res %>% select(,c(1,31,36,66))
names(covid_ctgpub_res2)

# Only use PubMed indexed publications
ctg_pubs<-covid_ctgpub_res2 %>% filter(pmid!='NA')
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
    colnames(pubmed_pub_list_full3)<-c('pmid', 'ArticleType', 'Substance', 'MeSH', 'Title', 'PubDate', 'Journal')
    return(pubmed_pub_list_full3)
    
    
  }
  , error=function(e) {write(pmid,file="pubmed_error.txt",append=TRUE)})}

result_ctg<-lapply(pmid, pubmed3)    %>% bind_rows()

ctg_pubs_all<-merge(result_ctg, ctg_pubs,  by ='pmid')%>%select(,c(1:8))
ctg_pubs_all2_1 <- ctg_pubs_all [!duplicated(ctg_pubs_all[c(1,8)]),]

ctg_pubs_all2_1$pub_source<-'Registry'



#-----CHapter 3: Combine results from PubMed and CTG
# Combine registry and abstract linked results

regCOVIDpubs2<-rbind(result2_2,ctg_pubs_all2_1 )
regCOVID_pubs<-left_join(regCOVIDpubs2, covid9, by ='nct_id')


# Add links to article and CTG page
regCOVID_pubs$pubmed_links<-paste0('https://pubmed.ncbi.nlm.nih.gov/',regCOVID_pubs$pmid)
regCOVID_pubs$ctg_links<-paste0('https://clinicaltrials.gov/ct2/show/',regCOVID_pubs$nct_id)


#--------Book 2: Analysis
#--------Chapter 4. COunts of publications for each study
#Counts of publications by source and in total
regCOVID_pubs_cnts<-regCOVID_pubs %>% group_by(nct_id,pub_source) %>% count()
regCOVID_pubs_cnts2<-regCOVID_pubs_cnts%>%dcast( nct_id ~ pub_source,sum)
regCOVID_pubs2<-left_join(regCOVID_pubs, regCOVID_pubs_cnts2, by ='nct_id')
regCOVID_pubs2$Total_pubs<-regCOVID_pubs2$Abstract+regCOVID_pubs2$Registry



#--------Chapter 5. Adding key information and flags for useful criteria
# Flags: 1=True, 0=False

#Adding Vaccines flag for subset
covid9_vacc<-read_csv('regCovid_vaccines.csv')
trial_list2<-left_join(regCOVID_pubs2,covid9_vacc, by='nct_id') %>% select(,c(1:78))
colnames(trial_list2)[78]<-'vaccine_trial_flag'
trial_list2 <- within(trial_list2, vaccine_trial_flag[vaccine_trial_flag != 'NA'] <- 1)
trial_list2 <- within(trial_list2, vaccine_trial_flag[is.na(vaccine_trial_flag)] <- 0)



#Number of Updates
update_counts_all<-read_csv('regCovid_updates_by_study.csv')
trial_list2_updtes<-left_join(trial_list2, update_counts_all, by ='nct_id')
trial_list2_updtes$n<-trial_list2_updtes$n-1
colnames(trial_list2_updtes)[79]<-"Number_of_updates"


# COuntry of Sites and US site flag
facil_q<-"select * from facilities"
facil<-dbGetQuery(con, facil_q)
facil_uniq<- facil [!duplicated(facil[c(2,8)]),]

#US site flag: does the study have at least one site in US
covid9_facil_us<-facil_uniq %>% filter(country=='United States') 
trial_list2_updtes$US_site_flag<-0
trial_list3<- left_join(trial_list2_updtes,covid9_facil_us, by='nct_id' )
trial_list3 <- within(trial_list3, US_site_flag[country != 'NA'] <- 1)
trial_list4<-trial_list3%>%select(,c(1:80))
facil_country<-aggregate(facil_uniq, list(facil_uniq$nct_id), paste, collapse="|")
trial_list5<-left_join(trial_list4, facil_country, by=c('nct_id'= 'Group.1'))%>%select(,c(1:80, 88))


# Add flag for CTG deposited results
covid9_res<-covid9 %>% filter(!is.na(results_first_posted_date))%>%select(,c(1,11))
trial_list6<-left_join(trial_list5,covid9_res, by ='nct_id')
colnames(trial_list6)[82]<-'Has_Results'
trial_list6$Has_Results<- trial_list6$Has_Results %>%as.character()
trial_list6 <- within(trial_list6, Has_Results[Has_Results!='NA'] <- 1)
trial_list6 <- within(trial_list6, Has_Results[is.na(Has_Results)] <- 0)

#Adding Trial intervention
intervention<-read_csv('regCOVID_intervention_per_row.csv')%>% select(,c(1,70))
intervention_group<-aggregate(intervention, list(intervention$nct_id), paste, collapse="|")
trial_list7_expanded<-left_join(trial_list6, intervention_group, by=c('nct_id'= 'Group.1'))

#-Final master file organization
trial_list8<-trial_list7_expanded%>%select(,c(1:9,29,37,39,42,44,46,49,53,73:82,84))
colnames(trial_list8)[10]<-'start_date'
colnames(trial_list8)[11]<-'primary_completion__date'
colnames(trial_list8)[12]<-'study_type'
colnames(trial_list8)[13]<-'brief_title'
colnames(trial_list8)[14]<-'overall_status'
colnames(trial_list8)[15]<-'phase'
colnames(trial_list8)[16]<-'sponsor'
colnames(trial_list8)[17]<-'why_stopped'
trial_list8$PubDate<-as.Date(trial_list8$PubDate)
trial_list8$start_date<-as.Date(trial_list8$start_date, '%m/%d/%Y')

# Remove publications from prior to start date (Misclassification)
trial_list9<-trial_list8%>%filter(PubDate>=start_date)
trial_list9%>%write_csv('regCOVIDpubs_Master_G.csv')




#------Chapter 5. Trial list one line by trial
#All
all_trials<-aggregate(trial_list9, list(trial_list9$nct_id), paste, collapse="/")%>%select(,c(1,3))
all_trials2<- trial_list9 [!duplicated(trial_list9[c(8)]),]
all_trials3<-left_join(all_trials2,all_trials, by =c('nct_id'='Group.1'))%>%select(,c(8,10:29))
colnames(all_trials3)[21]<-'ArticleType'
all_trials4<-all_trials3%>%select(,c(1:9, 11:21))
#all_trials4%>%write_csv('regCOVIDpublications_trial_list.csv')


# No publications
covid9_0pubs<-anti_join(covid9,all_trials4, by ='nct_id')

# Add links to article and CTG page
covid9_0pubs$ctg_links<-paste0('https://clinicalzeros.gov/ct2/show/',covid9_0pubs$nct_id)

# Zero Pubs
covid9_0pubs$Abstract<-0
covid9_0pubs$Registry<-0
covid9_0pubs$Total_pubs<-0


#Adding Vaccines flag for future subset
zero_list2<-left_join(covid9_0pubs,covid9_vacc, by='nct_id') %>% select(,c(1:69))
colnames(zero_list2)[69]<-'vaccine_trial_flag'
zero_list2 <- within(zero_list2, vaccine_trial_flag[vaccine_trial_flag != 'NA'] <- 1)
zero_list2 <- within(zero_list2, vaccine_trial_flag[is.na(vaccine_trial_flag)] <- 0)

#Number of Updates
zero_list2_updtes<-left_join(zero_list2, update_counts_all, by ='nct_id')
zero_list2_updtes$n<-zero_list2_updtes$n-1
colnames(zero_list2_updtes)[70]<-"Number_of_updates"

# COuntry of Sites
zero_list2_updtes$US_site_flag<-0
zero_list3<- left_join(zero_list2_updtes,covid9_facil_us, by='nct_id' )
zero_list3 <- within(zero_list3, US_site_flag[country != 'NA'] <- 1)
zero_list4<-zero_list3%>%select(,c(1:71))
zero_list5<-left_join(zero_list4, facil_country, by=c('nct_id'= 'Group.1'))%>%select(,c(1:71, 79))


# Add flag for CTG deposited results
zero_list6<-left_join(zero_list5,covid9_res, by ='nct_id')
colnames(zero_list6)[73]<-'Has_Results'
zero_list6$Has_Results<- zero_list6$Has_Results %>%as.character()
zero_list6 <- within(zero_list6, Has_Results[Has_Results!='NA'] <- 1)
zero_list6 <- within(zero_list6, Has_Results[is.na(Has_Results)] <- 0)

#trial intervention
zero_list7_expanded<-left_join(zero_list6, intervention_group, by=c('nct_id'= 'Group.1'))

zero_list8<-zero_list7_expanded%>%select(,c(1,21,29,31,34, 36,38,41,45, 65:73,75))
colnames(zero_list8)[2]<-'start_date'
colnames(zero_list8)[3]<-'primary_completion__date'
colnames(zero_list8)[4]<-'study_type'
colnames(zero_list8)[5]<-'brief_title'
colnames(zero_list8)[6]<-'overall_status'
colnames(zero_list8)[7]<-'phase'
colnames(zero_list8)[8]<-'sponsor'
colnames(zero_list8)[9]<-'why_stopped'
zero_list8$start_date<-as.Date(zero_list8$start_date, '%m/%d/%Y')
zero_list8$ArticleType<-NA

#Final list: Combine trials with pubs and without for complete list
Full_trials<-rbind(all_trials4, zero_list8)
Full_trials%>% write_csv('regCOVIDpublications_trials_all.csv')


#Trials by type-All triials
int_trials3<-Full_trials%>%filter(study_type=='Interventional')
int_trials3%>%write_csv('regCOVIDpublications_trials_int.csv')
obs_trials3<-Full_trials%>%filter(study_type=='Observational')
obs_trials3%>%write_csv('regCOVIDpublications_trials_obs.csv')
reg_trials3<-Full_trials%>%filter(study_type=='Observational [Patient Registry]')
reg_trials3%>%write_csv('regCOVIDpublications_trials_reg.csv')




# ---------Chapter 6. Publications from all trials


#------Section 6.1: All Trial Publications
trial_list9_rep<-trial_list9%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
trial_list9%>%write_csv('regCOVIDpublications_publication_list_all.csv')

#Article_type
type <- trial_list9 [!duplicated(trial_list9[c(1)]),]
type1<-type%>%filter(pmid!='NA')
type2<-type1%>% group_by(ArticleType)%>% count()
type2<-type2%>%arrange(-n)
colnames(type2)[2]<-'Article_Count'
type2%>%write_csv('regCOVIDpublications_articletype_cnt_all.csv')

#------Section 6.2: interventional Trials Publications
pub_int<-trial_list9%>%filter(study_type=='Interventional')
pub_int_rep<-pub_int%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_int%>%write_csv('regCOVIDpublications_publication_list_int.csv')


#interventional Article type
type_int <- pub_int [!duplicated(pub_int[c(1)]),]
type1_int<-type_int%>%filter(pmid!='NA')
type2_int<-type1_int%>% group_by(ArticleType)%>% count()
type2_int<-type2_int%>%arrange(-n)
colnames(type2_int)[2]<-'Article_Count'
type2_int%>%write_csv('regCOVIDpublications_articletype_cnt_int.csv')

#------Section 6.3: observational Publications
pub_obs<-trial_list9%>%filter(study_type=='Observational')
pub_obs_rep<-pub_obs%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_obs%>%write_csv('regCOVIDpublications_publication_list_obs.csv')


#observational Article type
type_obs <- pub_obs [!duplicated(pub_obs[c(1)]),]
type1_obs<-type_obs%>%filter(pmid!='NA')
type2_obs<-type1_obs%>% group_by(ArticleType)%>% count()
type2_obs<-type2_obs%>%arrange(-n)
colnames(type2_obs)[2]<-'Article_Count'
type2_obs%>%write_csv('regCOVIDpublications_articletype_cnt_obs.csv')

#------Section 6.4: Registry Trials Publications
pub_reg<-trial_list9%>%filter(study_type=='Observational [Patient Registry]')
pub_reg_rep<-pub_reg%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_reg%>%write_csv('regCOVIDpublications_publication_list_reg.csv')


#Registry Article type
type_reg <- pub_reg [!duplicated(pub_reg[c(1)]),]
type1_reg<-type_reg%>%filter(pmid!='NA')
type2_reg<-type1_reg%>% group_by(ArticleType)%>% count()
type2_reg<-type2_reg%>%arrange(-n)
colnames(type2_reg)[2]<-'Article_Count'
type2_reg%>%write_csv('regCOVIDpublications_articletype_cnt_reg.csv')




#------Chapter 7: Effectively Completed Studies (studies past their primary completion date)
Full_trials$primary_completion__date<-as.Date(Full_trials0$primary_completion__date, '%m/%d/%Y')
Full_trials_ec<-Full_trials%>%filter(Sys.Date()>primary_completion__date)

Full_trials_ec%>% write_csv('regCOVIDpublications_trials_ec_all.csv')

#Trials by type-EC triials
int_trials3_ec<-Full_trials_ec%>%filter(study_type=='Interventional')
int_trials3_ec%>%write_csv('regCOVIDpublications_trials_ec_int.csv')
obs_trials3_ec<-Full_trials_ec%>%filter(study_type=='Observational')
obs_trials3_ec%>%write_csv('regCOVIDpublications_trials_ec_obs.csv')
reg_trials3_ec<-Full_trials_ec%>%filter(study_type=='Observational [Patient Registry]')
reg_trials3_ec%>%write_csv('regCOVIDpublications_trials__ec_reg.csv')

# ---------Section 7.1: EC Trials: Publications
#Overall publications list for report
trial_list9$primary_completion__date<-as.Date(trial_list9$primary_completion__date, '%m/%d/%Y')
trial_list9_ec<-trial_list9%>%filter(Sys.Date()>primary_completion__date)

trial_list9_rep_ec<-trial_list9_ec%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
trial_list9_ec%>%write_csv('regCOVIDpublications_publication_list_ec_all.csv')

#Article_type
type_ec <- trial_list9_ec [!duplicated(trial_list9[c(1)]),]
type1_ec<-type_ec%>%filter(pmid!='NA')
type2_ec<-type1_ec%>% group_by(ArticleType)%>% count()
type2_ec<-type2_ec%>%arrange(-n)
colnames(type2_ec)[2]<-'Article_Count'
type2_ec%>%write_csv('regCOVIDpublications_articletype_cnt_ec_all.csv')

#------Section 7.2: interventional Trials Publications
pub_int_ec<-trial_list9_ec%>%filter(study_type=='Interventional')
pub_int_rep_ec<-pub_int_ec%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_int_ec%>%write_csv('regCOVIDpublications_publication_list_ec_int.csv')


#interventional Article type
type_int_ec <- pub_int_ec [!duplicated(pub_int_ec[c(1)]),]

type1_int_ec<-type_int_ec%>%filter(pmid!='NA')
type2_int_ec<-type1_int_ec%>% group_by(ArticleType)%>% count()
type2_int_ec<-type2_int_ec%>%arrange(-n)
colnames(type2_int_ec)[2]<-'Article_Count'
type2_int_ec%>%write_csv('regCOVIDpublications_articletype_cnt_ec_int.csv')

#------Section 7.3: Observational Publications
pub_obs_ec<-trial_list9_ec%>%filter(study_type=='Observational')
pub_obs_rep_ec<-pub_obs_ec%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_obs_ec%>%write_csv('regCOVIDpublications_publication_list_ec_obs.csv')


#observational Article type
type_obs_ec <- pub_obs_ec [!duplicated(pub_obs_ec[c(1)]),]
type1_obs_ec<-type_obs_ec%>%filter(pmid!='NA')
type2_obs_ec<-type1_obs_ec%>% group_by(ArticleType)%>% count()
type2_obs_ec<-type2_obs_ec%>%arrange(-n)
colnames(type2_obs_ec)[2]<-'Article_Count'
type2_obs_ec%>%write_csv('regCOVIDpublications_articletype_cnt_ec_obs.csv')

#------Section 7.4: Registry Trials Publications
pub_reg_ec<-trial_list9_ec%>%filter(study_type=='Observational [Patient Registry]')
pub_reg_rep_ec<-pub_reg_ec%>%select(,c(1,8,5, 2:4,6:7,9,18:19))
pub_reg_ec%>%write_csv('regCOVIDpublications_publication_list_ec_reg.csv')


#Registry Article type
type_reg_ec <- pub_reg_ec [!duplicated(pub_reg_ec[c(1)]),]
type1_reg_ec<-type_reg_ec%>%filter(pmid!='NA')
type2_reg_ec<-type1_reg_ec%>% group_by(ArticleType)%>% count()
type2_reg_ec<-type2_reg_ec%>%arrange(-n)
colnames(type2_reg_ec)[2]<-'Article_Count'
type2_reg_ec%>%write_csv('regCOVIDpublications_articletype_cnt_ec_reg.csv')

#--------Chapter 8 Studies in Writing Phase
#Studies in writing phase
#interventional Trials
# Days past COmpletion
#int_trials3_ec$days_past_completion<-Sys.Date()- int_trials3_ec$primary_completion__date
int_trials3_ec$days_past_completion<-Sys.Date()- int_trials3_ec$primary_completion__date

int_trials3_ec$Total_pubs2<-int_trials3_ec$Total_pubs
int_trials3_ec<- within(int_trials3_ec, Total_pubs2[grepl("Protocol",ArticleType)& Total_pubs==1] <- 0)
int_trials3_ec<- within(int_trials3_ec, Total_pubs2[Total_pubs2>0] <- 1)
within_30_int<-int_trials3_ec%>%filter(days_past_completion<31)
within_60_int<-int_trials3_ec%>%filter(days_past_completion<61)
within_90_int<-int_trials3_ec%>%filter(days_past_completion<91)
within_120_int<-int_trials3_ec%>%filter(days_past_completion<121) 
within_180_int<-int_trials3_ec%>%filter(days_past_completion<181) 
within_365_int<-int_trials3_ec%>%filter(days_past_completion<366) 

more_than_365_int<-int_trials3_ec%>%filter(days_past_completion>365) 
within_30cnt_int<-within_30_int%>%group_by(Total_pubs2)%>%count()
within_60cnt_int<-within_60_int%>%group_by(Total_pubs2)%>%count()
within_90cnt_int<-within_90_int%>%group_by(Total_pubs2)%>%count()
within_1200cnt_int<-within_120_int%>%group_by(Total_pubs2)%>%count()
within_180cnt_int<-within_180_int%>%group_by(Total_pubs2)%>%count()
within_365cnt_int<-within_365_int%>%group_by(Total_pubs2)%>%count()
within_morecnt_int<-more_than_365_int%>%group_by(Total_pubs2)%>%count()
Days_past_int<-merge(within_30cnt_int,within_60cnt_int, by ='Total_pubs2' )%>%merge(within_90cnt_int)
Days_past2_int<-merge(Days_past_int,within_1200cnt_int, by ='Total_pubs2')
Days_past2_1_int<-merge(Days_past2_int,within_180cnt_int, by ='Total_pubs2')
Days_past2_2_int<-merge(Days_past2_1_int,within_365cnt_int, by ='Total_pubs2')#%>%merge(within_morecnt_int)


colnames(Days_past2_2_int)<-c('Days_since_completion', 'under_30', 'under_60', 'under_90', 'under_120',  'under_180' , 'under_365')
Days_past3_int<-t(Days_past2_2_int)
colnames(Days_past3_int)<-c( 'Studies_with_no_pubs', 'Studies_with_pubs')
Days_past4_int<-data.frame(Days_past3_int[2:7,])
Days_past4_int$Total_studies<-Days_past4_int$Studies_with_no_pubs+Days_past4_int$Studies_with_pubs
Days_past5_int<- Days_past4_int %>% rownames_to_column("Days_since_completion")
Days_past5_int%>%write_csv('regCOVIDpublications_results_writing_phase_int.csv')



#Observational Trials
# Days past COmpletion
obs_trials3_ec$days_past_completion<-Sys.Date()- obs_trials3_ec$primary_completion__date

obs_trials3_ec$Total_pubs2<-obs_trials3_ec$Total_pubs
obs_trials3_ec<- within(obs_trials3_ec, Total_pubs2[grepl("Protocol",ArticleType)& Total_pubs==1] <- 0)
obs_trials3_ec<- within(obs_trials3_ec, Total_pubs2[Total_pubs2>0] <- 1)
within_30_obs<-obs_trials3_ec%>%filter(days_past_completion<31)
within_60_obs<-obs_trials3_ec%>%filter(days_past_completion<61)
within_90_obs<-obs_trials3_ec%>%filter(days_past_completion<91)
within_120_obs<-obs_trials3_ec%>%filter(days_past_completion<121) 
within_180_obs<-obs_trials3_ec%>%filter(days_past_completion<181) 
within_365_obs<-obs_trials3_ec%>%filter(days_past_completion<366) 

more_than_365_obs<-obs_trials3_ec%>%filter(days_past_completion>365) 
within_30cnt_obs<-within_30_obs%>%group_by(Total_pubs2)%>%count()
within_60cnt_obs<-within_60_obs%>%group_by(Total_pubs2)%>%count()
within_90cnt_obs<-within_90_obs%>%group_by(Total_pubs2)%>%count()
within_1200cnt_obs<-within_120_obs%>%group_by(Total_pubs2)%>%count()
within_180cnt_obs<-within_180_obs%>%group_by(Total_pubs2)%>%count()
within_365cnt_obs<-within_365_obs%>%group_by(Total_pubs2)%>%count()
within_morecnt_obs<-more_than_365_obs%>%group_by(Total_pubs2)%>%count()
Days_past_obs<-merge(within_30cnt_obs,within_60cnt_obs, by ='Total_pubs2' )%>%merge(within_90cnt_obs)
Days_past2_obs<-merge(Days_past_obs,within_1200cnt_obs, by ='Total_pubs2')
Days_past2_1_obs<-merge(Days_past2_obs,within_180cnt_obs, by ='Total_pubs2')
Days_past2_2_obs<-merge(Days_past2_1_obs,within_365cnt_obs, by ='Total_pubs2')#%>%merge(within_morecnt_obs)


colnames(Days_past2_2_obs)<-c('Days_since_completion', 'under_30', 'under_60', 'under_90', 'under_120',  'under_180' , 'under_365')
Days_past3_obs<-t(Days_past2_2_obs)
colnames(Days_past3_obs)<-c( 'Studies_with_no_pubs', 'Studies_with_pubs')
Days_past4_obs<-data.frame(Days_past3_obs[2:7,])
Days_past4_obs$Total_studies<-Days_past4_obs$Studies_with_no_pubs+Days_past4_obs$Studies_with_pubs
Days_past5_obs<- Days_past4_obs %>% rownames_to_column("Days_since_completion")
Days_past5_obs%>%write_csv('regCOVIDpublications_results_writing_phase_obs.csv')

#Registry Trials
# Days past COmpletion
reg_trials3_ec$days_past_completion<-Sys.Date()- reg_trials3_ec$primary_completion__date
reg_trials3_ec$Total_pubs2<-reg_trials3_ec$Total_pubs
reg_trials3_ec<- within(reg_trials3_ec, Total_pubs2[grepl("Protocol",ArticleType)& Total_pubs==1] <- 0)
reg_trials3_ec<- within(reg_trials3_ec, Total_pubs2[Total_pubs2>0] <- 1)
within_30_reg<-reg_trials3_ec%>%filter(days_past_completion<31)
within_60_reg<-reg_trials3_ec%>%filter(days_past_completion<61)
within_90_reg<-reg_trials3_ec%>%filter(days_past_completion<91)
within_120_reg<-reg_trials3_ec%>%filter(days_past_completion<121) 
within_180_reg<-reg_trials3_ec%>%filter(days_past_completion<181) 
within_365_reg<-reg_trials3_ec%>%filter(days_past_completion<366) 

more_than_365_reg<-reg_trials3_ec%>%filter(days_past_completion>365) 
within_30cnt_reg<-within_30_reg%>%group_by(Total_pubs2)%>%count()
within_60cnt_reg<-within_60_reg%>%group_by(Total_pubs2)%>%count()
within_90cnt_reg<-within_90_reg%>%group_by(Total_pubs2)%>%count()
within_1200cnt_reg<-within_120_reg%>%group_by(Total_pubs2)%>%count()
within_180cnt_reg<-within_180_reg%>%group_by(Total_pubs2)%>%count()
within_365cnt_reg<-within_365_reg%>%group_by(Total_pubs2)%>%count()
within_morecnt_reg<-more_than_365_reg%>%group_by(Total_pubs2)%>%count()
Days_past_reg<-merge(within_30cnt_reg,within_60cnt_reg, by ='Total_pubs2' )%>%merge(within_90cnt_reg)
Days_past2_reg<-merge(Days_past_reg,within_1200cnt_reg, by ='Total_pubs2')
Days_past2_1_reg<-merge(Days_past2_reg,within_180cnt_reg, by ='Total_pubs2')
Days_past2_2_reg<-merge(Days_past2_1_reg,within_365cnt_reg, by ='Total_pubs2')#%>%merge(within_morecnt_reg)


colnames(Days_past2_2_reg)<-c('Days_since_completion', 'under_30', 'under_60', 'under_90', 'under_120',  'under_180' , 'under_365')
Days_past3_reg<-t(Days_past2_2_reg)
colnames(Days_past3_reg)<-c( 'Studies_with_no_pubs', 'Studies_with_pubs')
Days_past4_reg<-data.frame(Days_past3_reg[2:7,])
Days_past4_reg$Total_studies<-Days_past4_reg$Studies_with_no_pubs+Days_past4_reg$Studies_with_pubs
Days_past5_reg<- Days_past4_reg %>% rownames_to_column("Days_since_completion")
Days_past5_reg%>%write_csv('regCOVIDpublications_results_writing_phase_reg.csv')



#All Trials
# Days past COmpletion
Full_trials_ec$days_past_completion<-Sys.Date()- Full_trials_ec$primary_completion__date
Full_trials_ec$Total_pubs2<-Full_trials_ec$Total_pubs
Full_trials_ec<- within(Full_trials_ec, Total_pubs2[grepl("Protocol",ArticleType)& Total_pubs==1] <- 0)
Full_trials_ec<- within(Full_trials_ec, Total_pubs2[Total_pubs2>0] <- 1)
within_30_all<-Full_trials_ec%>%filter(days_past_completion<31)
within_60_all<-Full_trials_ec%>%filter(days_past_completion<61)
within_90_all<-Full_trials_ec%>%filter(days_past_completion<91)
within_120_all<-Full_trials_ec%>%filter(days_past_completion<121) 
within_180_all<-Full_trials_ec%>%filter(days_past_completion<181) 
within_365_all<-Full_trials_ec%>%filter(days_past_completion<366) 

more_than_365_all<-Full_trials_ec%>%filter(days_past_completion>365) 
within_30cnt_all<-within_30_all%>%group_by(Total_pubs2)%>%count()
within_60cnt_all<-within_60_all%>%group_by(Total_pubs2)%>%count()
within_90cnt_all<-within_90_all%>%group_by(Total_pubs2)%>%count()
within_1200cnt_all<-within_120_all%>%group_by(Total_pubs2)%>%count()
within_180cnt_all<-within_180_all%>%group_by(Total_pubs2)%>%count()
within_365cnt_all<-within_365_all%>%group_by(Total_pubs2)%>%count()
within_morecnt_all<-more_than_365_all%>%group_by(Total_pubs2)%>%count()
Days_past_all<-merge(within_30cnt_all,within_60cnt_all, by ='Total_pubs2' )%>%merge(within_90cnt_all)
Days_past2_all<-merge(Days_past_all,within_1200cnt_all, by ='Total_pubs2')
Days_past2_1_all<-merge(Days_past2_all,within_180cnt_all, by ='Total_pubs2')
Days_past2_2_all<-merge(Days_past2_1_all,within_365cnt_all, by ='Total_pubs2')#%>%merge(within_morecnt_all)


colnames(Days_past2_2_all)<-c('Days_since_completion', 'under_30', 'under_60', 'under_90', 'under_120',  'under_180' , 'under_365')
Days_past3_all<-t(Days_past2_2_all)
colnames(Days_past3_all)<-c( 'Studies_with_no_pubs', 'Studies_with_pubs')
Days_past4_all<-data.frame(Days_past3_all[2:7,])
Days_past4_all$Total_studies<-Days_past4_all$Studies_with_no_pubs+Days_past4_all$Studies_with_pubs
Days_past5_all<- Days_past4_all %>% rownames_to_column("Days_since_completion")
Days_past5_all%>%write_csv('regCOVIDpublications_results_writing_phase_all.csv')



#------Chapter 9:Status overview, Subsets, and scenarios

#-----All
#All Interventional trials by status and scenario
int_status_pubs<-int_trials3%>%group_by(overall_status, Total_pubs)%>%count()
int_status_pubs<- within(int_status_pubs, Total_pubs[Total_pubs>3] <- 3)
int_status_pubs_cast<-int_status_pubs%>%dcast( overall_status ~ Total_pubs,sum)
int_trials3_pubs<-int_trials3%>%filter(Total_pubs!=0)
int_status<-int_trials3_pubs%>%group_by(overall_status)%>%count()
one_protocol_int<-int_trials3%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
int_prot1<-one_protocol_int%>%group_by(overall_status)%>%count()
any_protocol_int<-int_trials3%>%filter(grepl("Protocol",ArticleType))
int_prot<-any_protocol_int%>%group_by(overall_status)%>%count()
status_cnt_int<-int_trials3%>%group_by(overall_status)%>%count()
zombie_trials_int<-int_trials3%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_int<-zombie_trials_int%>%group_by(overall_status)%>%count()
status_comb_int<-merge(status_cnt_int, int_status, by ='overall_status')
status_comb2_int<-merge(status_comb_int, int_status_pubs_cast, by ='overall_status')
status_comb3_int<-left_join(status_comb2_int, int_prot, by ='overall_status')
status_comb4_int<-left_join(status_comb3_int, int_prot1, by ='overall_status')
status_comb5_int<-left_join(status_comb4_int, zombie_trials_cnt_int, by ='overall_status')
colnames(status_comb5_int)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_int[is.na(status_comb5_int)] = 0
Totals_int<-colSums(status_comb5_int[,-1])
Totals2_int<- data.frame(Totals_int)
Totals_t_int<-t(Totals2_int)
Totals_t_int$overall_status<-'Totals'
Totals_t_int<-data.frame(Totals_t_int)
Totals_t_int<-Totals_t_int%>%select(,c(10,1:9))
colnames(Totals_t_int)<-names(status_comb5_int)
status_comb6_int<-rbind(status_comb5_int,Totals_t_int)
status_comb6_int%>%write_csv('regCOVIDpublications_status_overview_int.csv')



#All Observational trials by status and scenario
obs_status_pubs<-obs_trials3%>%group_by(overall_status, Total_pubs)%>%count()
obs_status_pubs<- within(obs_status_pubs, Total_pubs[Total_pubs>3] <- 3)
obs_status_pubs_cast<-obs_status_pubs%>%dcast( overall_status ~ Total_pubs,sum)
obs_trials3_pubs<-obs_trials3%>%filter(Total_pubs!=0)
obs_status<-obs_trials3_pubs%>%group_by(overall_status)%>%count()
one_protocol_obs<-obs_trials3%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
obs_prot1<-one_protocol_obs%>%group_by(overall_status)%>%count()
any_protocol_obs<-obs_trials3%>%filter(grepl("Protocol",ArticleType))
obs_prot<-any_protocol_obs%>%group_by(overall_status)%>%count()
status_cnt_obs<-obs_trials3%>%group_by(overall_status)%>%count()
zombie_trials_obs<-obs_trials3%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_obs<-zombie_trials_obs%>%group_by(overall_status)%>%count()
status_comb_obs<-merge(status_cnt_obs, obs_status, by ='overall_status')
status_comb2_obs<-merge(status_comb_obs, obs_status_pubs_cast, by ='overall_status')
status_comb3_obs<-left_join(status_comb2_obs, obs_prot, by ='overall_status')
status_comb4_obs<-left_join(status_comb3_obs, obs_prot1, by ='overall_status')
status_comb5_obs<-left_join(status_comb4_obs, zombie_trials_cnt_obs, by ='overall_status')
colnames(status_comb5_obs)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_obs[is.na(status_comb5_obs)] = 0
Totals_obs<-colSums(status_comb5_obs[,-1])
Totals2_obs<- data.frame(Totals_obs)
Totals_t_obs<-t(Totals2_obs)
Totals_t_obs$overall_status<-'Totals'
Totals_t_obs<-data.frame(Totals_t_obs)
Totals_t_obs<-Totals_t_obs%>%select(,c(10,1:9))
colnames(Totals_t_obs)<-names(status_comb5_obs)
status_comb6_obs<-rbind(status_comb5_obs,Totals_t_obs)
status_comb6_obs%>%write_csv('regCOVIDpublications_status_overview_obs.csv')




#EAll Registry trials by status and scenario
reg_status_pubs<-reg_trials3%>%group_by(overall_status, Total_pubs)%>%count()
reg_status_pubs<- within(reg_status_pubs, Total_pubs[Total_pubs>3] <- 3)
reg_status_pubs_cast<-reg_status_pubs%>%dcast( overall_status ~ Total_pubs,sum)
reg_trials3_pubs<-reg_trials3%>%filter(Total_pubs!=0)
reg_status<-reg_trials3_pubs%>%group_by(overall_status)%>%count()
one_protocol_reg<-reg_trials3%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
reg_prot1<-one_protocol_reg%>%group_by(overall_status)%>%count()
any_protocol_reg<-reg_trials3%>%filter(grepl("Protocol",ArticleType))
reg_prot<-any_protocol_reg%>%group_by(overall_status)%>%count()
status_cnt_reg<-reg_trials3%>%group_by(overall_status)%>%count()
zombie_trials_reg<-reg_trials3%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_reg<-zombie_trials_reg%>%group_by(overall_status)%>%count()
status_comb_reg<-merge(status_cnt_reg, reg_status, by ='overall_status')
status_comb2_reg<-merge(status_comb_reg, reg_status_pubs_cast, by ='overall_status')
status_comb3_reg<-left_join(status_comb2_reg, reg_prot, by ='overall_status')
status_comb4_reg<-left_join(status_comb3_reg, reg_prot1, by ='overall_status')
status_comb5_reg<-left_join(status_comb4_reg, zombie_trials_cnt_reg, by ='overall_status')
colnames(status_comb5_reg)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_reg[is.na(status_comb5_reg)] = 0
Totals_reg<-colSums(status_comb5_reg[,-1])
Totals2_reg<- data.frame(Totals_reg)
Totals_t_reg<-t(Totals2_reg)
Totals_t_reg$overall_status<-'Totals'
Totals_t_reg<-data.frame(Totals_t_reg)
Totals_t_reg<-Totals_t_reg%>%select(,c(10,1:9))
colnames(Totals_t_reg)<-names(status_comb5_reg)
status_comb6_reg<-rbind(status_comb5_reg,Totals_t_reg)
status_comb6_reg%>%write_csv('regCOVIDpublications_status_overview_reg.csv')



# All trials by status and scenario
all_status_pubs<-Full_trials%>%group_by(overall_status, Total_pubs)%>%count()
all_status_pubs<- within(all_status_pubs, Total_pubs[Total_pubs>3] <- 3)
all_status_pubs_cast<-all_status_pubs%>%dcast( overall_status ~ Total_pubs,sum)
all_trials3_pubs<-Full_trials%>%filter(Total_pubs!=0)
all_status<-all_trials3_pubs%>%group_by(overall_status)%>%count()
one_protocol_all<-Full_trials%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
all_prot1<-one_protocol_all%>%group_by(overall_status)%>%count()
any_protocol_all<-Full_trials%>%filter(grepl("Protocol",ArticleType))
all_prot<-any_protocol_all%>%group_by(overall_status)%>%count()
status_cnt_all<-Full_trials%>%group_by(overall_status)%>%count()
zombie_trials_all<-Full_trials%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_all<-zombie_trials_all%>%group_by(overall_status)%>%count()
status_comb_all<-merge(status_cnt_all, all_status, by ='overall_status')
status_comb2_all<-merge(status_comb_all, all_status_pubs_cast, by ='overall_status')
status_comb3_all<-left_join(status_comb2_all, all_prot, by ='overall_status')
status_comb4_all<-left_join(status_comb3_all, all_prot1, by ='overall_status')
status_comb5_all<-left_join(status_comb4_all, zombie_trials_cnt_all, by ='overall_status')
colnames(status_comb5_all)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_all[is.na(status_comb5_all)] = 0
Totals_all<-colSums(status_comb5_all[,-1])
Totals2_all<- data.frame(Totals_all)
Totals_t_all<-t(Totals2_all)
Totals_t_all$overall_status<-'Totals'
Totals_t_all<-data.frame(Totals_t_all)
Totals_t_all<-Totals_t_all%>%select(,c(10,1:9))
colnames(Totals_t_all)<-names(status_comb5_all)
status_comb6_all<-rbind(status_comb5_all,Totals_t_all)
status_comb6_all%>%write_csv('regCOVIDpublications_status_overview_all.csv')




#Effectively Completed
#Effectively Completed Interventional trials by status and scenario
int_status_pubs_ec<-int_trials3_ec%>%group_by(overall_status, Total_pubs)%>%count()
int_status_pubs_ec<- within(int_status_pubs_ec, Total_pubs[Total_pubs>3] <- 3)
int_status_pubs_cast_ec<-int_status_pubs_ec%>%dcast( overall_status ~ Total_pubs,sum)
int_trials3_pubs_ec<-int_trials3_ec%>%filter(Total_pubs!=0)
int_status_ec<-int_trials3_pubs_ec%>%group_by(overall_status)%>%count()
one_protocol_int_ec<-int_trials3_ec%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
int_prot1_ec<-one_protocol_int_ec%>%group_by(overall_status)%>%count()
any_protocol_int_ec<-int_trials3_ec%>%filter(grepl("Protocol",ArticleType))
int_prot_ec<-any_protocol_int_ec%>%group_by(overall_status)%>%count()
status_cnt_int_ec<-int_trials3_ec%>%group_by(overall_status)%>%count()
zombie_trials_int_ec<-int_trials3_ec%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_int_ec<-zombie_trials_int_ec%>%group_by(overall_status)%>%count()
status_comb_int_ec<-merge(status_cnt_int_ec, int_status_ec, by ='overall_status')
status_comb2_int_ec<-merge(status_comb_int_ec, int_status_pubs_cast_ec, by ='overall_status')
status_comb3_int_ec<-left_join(status_comb2_int_ec, int_prot_ec, by ='overall_status')
status_comb4_int_ec<-left_join(status_comb3_int_ec, int_prot1_ec, by ='overall_status')
status_comb5_int_ec<-left_join(status_comb4_int_ec, zombie_trials_cnt_int_ec, by ='overall_status')
colnames(status_comb5_int_ec)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_int_ec[is.na(status_comb5_int_ec)] = 0
Totals_int_ec<-colSums(status_comb5_int_ec[,-1])
Totals2_int_ec<- data.frame(Totals_int_ec)
Totals_t_int_ec<-t(Totals2_int_ec)
Totals_t_int_ec$overall_status<-'Totals'
Totals_t_int_ec<-data.frame(Totals_t_int_ec)
Totals_t_int_ec<-Totals_t_int_ec%>%select(,c(10,1:9))
colnames(Totals_t_int_ec)<-names(status_comb5_int_ec)
status_comb6_int_ec<-rbind(status_comb5_int_ec,Totals_t_int_ec)
status_comb6_int_ec%>%write_csv('regCOVIDpublications_status_overview_ec_int.csv')



#Effectively Completed Observational trials by status and scenario
obs_status_pubs_ec<-obs_trials3_ec%>%group_by(overall_status, Total_pubs)%>%count()
obs_status_pubs_ec<- within(obs_status_pubs_ec, Total_pubs[Total_pubs>3] <- 3)
obs_status_pubs_cast_ec<-obs_status_pubs_ec%>%dcast( overall_status ~ Total_pubs,sum)
obs_trials3_pubs_ec<-obs_trials3_ec%>%filter(Total_pubs!=0)
obs_status_ec<-obs_trials3_pubs_ec%>%group_by(overall_status)%>%count()
one_protocol_obs_ec<-obs_trials3_ec%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
obs_prot1_ec<-one_protocol_obs_ec%>%group_by(overall_status)%>%count()
any_protocol_obs_ec<-obs_trials3_ec%>%filter(grepl("Protocol",ArticleType))
obs_prot_ec<-any_protocol_obs_ec%>%group_by(overall_status)%>%count()
status_cnt_obs_ec<-obs_trials3_ec%>%group_by(overall_status)%>%count()
zombie_trials_obs_ec<-obs_trials3_ec%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_obs_ec<-zombie_trials_obs_ec%>%group_by(overall_status)%>%count()
status_comb_obs_ec<-merge(status_cnt_obs_ec, obs_status_ec, by ='overall_status')
status_comb2_obs_ec<-merge(status_comb_obs_ec, obs_status_pubs_cast_ec, by ='overall_status')
status_comb3_obs_ec<-left_join(status_comb2_obs_ec, obs_prot_ec, by ='overall_status')
status_comb4_obs_ec<-left_join(status_comb3_obs_ec, obs_prot1_ec, by ='overall_status')
status_comb5_obs_ec<-left_join(status_comb4_obs_ec, zombie_trials_cnt_obs_ec, by ='overall_status')
colnames(status_comb5_obs_ec)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_obs_ec[is.na(status_comb5_obs_ec)] = 0
Totals_obs_ec<-colSums(status_comb5_obs_ec[,-1])
Totals2_obs_ec<- data.frame(Totals_obs_ec)
Totals_t_obs_ec<-t(Totals2_obs_ec)
Totals_t_obs_ec$overall_status<-'Totals'
Totals_t_obs_ec<-data.frame(Totals_t_obs_ec)
Totals_t_obs_ec<-Totals_t_obs_ec%>%select(,c(10,1:9))
colnames(Totals_t_obs_ec)<-names(status_comb5_obs_ec)
status_comb6_obs_ec<-rbind(status_comb5_obs_ec,Totals_t_obs_ec)
status_comb6_obs_ec%>%write_csv('regCOVIDpublications_status_overview_ec_obs.csv')




#Effectively Completed Registry trials by status and scenario
reg_status_pubs_ec<-reg_trials3_ec%>%group_by(overall_status, Total_pubs)%>%count()
reg_status_pubs_ec<- within(reg_status_pubs_ec, Total_pubs[Total_pubs>3] <- 3)
reg_status_pubs_cast_ec<-reg_status_pubs_ec%>%dcast( overall_status ~ Total_pubs,sum)
reg_trials3_pubs_ec<-reg_trials3_ec%>%filter(Total_pubs!=0)
reg_status_ec<-reg_trials3_pubs_ec%>%group_by(overall_status)%>%count()
one_protocol_reg_ec<-reg_trials3_ec%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
reg_prot1_ec<-one_protocol_reg_ec%>%group_by(overall_status)%>%count()
any_protocol_reg_ec<-reg_trials3_ec%>%filter(grepl("Protocol",ArticleType))
reg_prot_ec<-any_protocol_reg_ec%>%group_by(overall_status)%>%count()
status_cnt_reg_ec<-reg_trials3_ec%>%group_by(overall_status)%>%count()
zombie_trials_reg_ec<-reg_trials3_ec%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_reg_ec<-zombie_trials_reg_ec%>%group_by(overall_status)%>%count()
status_comb_reg_ec<-merge(status_cnt_reg_ec, reg_status_ec, by ='overall_status')
status_comb2_reg_ec<-merge(status_comb_reg_ec, reg_status_pubs_cast_ec, by ='overall_status')
status_comb3_reg_ec<-left_join(status_comb2_reg_ec, reg_prot_ec, by ='overall_status')
status_comb4_reg_ec<-left_join(status_comb3_reg_ec, reg_prot1_ec, by ='overall_status')
status_comb5_reg_ec<-left_join(status_comb4_reg_ec, zombie_trials_cnt_reg_ec, by ='overall_status')
colnames(status_comb5_reg_ec)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_reg_ec[is.na(status_comb5_reg_ec)] = 0
Totals_reg_ec<-colSums(status_comb5_reg_ec[,-1])
Totals2_reg_ec<- data.frame(Totals_reg_ec)
Totals_t_reg_ec<-t(Totals2_reg_ec)
Totals_t_reg_ec$overall_status<-'Totals'
Totals_t_reg_ec<-data.frame(Totals_t_reg_ec)
Totals_t_reg_ec<-Totals_t_reg_ec%>%select(,c(10,1:9))
colnames(Totals_t_reg_ec)<-names(status_comb5_reg_ec)
status_comb6_reg_ec<-rbind(status_comb5_reg_ec,Totals_t_reg_ec)
status_comb6_reg_ec%>%write_csv('regCOVIDpublications_status_overview_ec_reg.csv')



#Effectively Completed All trials by status and scenario
all_status_pubs_ec<-Full_trials_ec%>%group_by(overall_status, Total_pubs)%>%count()
all_status_pubs_ec<- within(all_status_pubs_ec, Total_pubs[Total_pubs>3] <- 3)
all_status_pubs_cast_ec<-all_status_pubs_ec%>%dcast( overall_status ~ Total_pubs,sum)
all_trials3_pubs_ec<-Full_trials_ec%>%filter(Total_pubs!=0)
all_status_ec<-all_trials3_pubs_ec%>%group_by(overall_status)%>%count()
one_protocol_all_ec<-Full_trials_ec%>%filter(grepl("Protocol",ArticleType))%>%filter(Total_pubs==1)
all_prot1_ec<-one_protocol_all_ec%>%group_by(overall_status)%>%count()
any_protocol_all_ec<-Full_trials_ec%>%filter(grepl("Protocol",ArticleType))
all_prot_ec<-any_protocol_all_ec%>%group_by(overall_status)%>%count()
status_cnt_all_ec<-Full_trials_ec%>%group_by(overall_status)%>%count()
zombie_trials_all_ec<-Full_trials_ec%>%filter(Number_of_updates==0&Total_pubs==0)
zombie_trials_cnt_all_ec<-zombie_trials_all_ec%>%group_by(overall_status)%>%count()
status_comb_all_ec<-merge(status_cnt_all_ec, all_status_ec, by ='overall_status')
status_comb2_all_ec<-merge(status_comb_all_ec, all_status_pubs_cast_ec, by ='overall_status')
status_comb3_all_ec<-left_join(status_comb2_all_ec, all_prot_ec, by ='overall_status')
status_comb4_all_ec<-left_join(status_comb3_all_ec, all_prot1_ec, by ='overall_status')
status_comb5_all_ec<-left_join(status_comb4_all_ec, zombie_trials_cnt_all_ec, by ='overall_status')
colnames(status_comb5_all_ec)<-c('overall_status', 'Total_Studies', 'Studies_with_Pubs', 'Zero-Pub_studies','One-Pub_Studies', 'two-Pub_Studies', 'Three-Plus-Pub_Studies','Studies_with_protocol', 'Studies_with_only_protocol','Zero_Updates_Zero_pubs')
status_comb5_all_ec[is.na(status_comb5_all_ec)] = 0
Totals_all_ec<-colSums(status_comb5_all_ec[,-1])
Totals2_all_ec<- data.frame(Totals_all_ec)
Totals_t_all_ec<-t(Totals2_all_ec)
Totals_t_all_ec$overall_status<-'Totals'
Totals_t_all_ec<-data.frame(Totals_t_all_ec)
Totals_t_all_ec<-Totals_t_all_ec%>%select(,c(10,1:9))
colnames(Totals_t_all_ec)<-names(status_comb5_all_ec)
status_comb6_all_ec<-rbind(status_comb5_all_ec,Totals_t_all_ec)
status_comb6_all_ec%>%write_csv('regCOVIDpublications_status_overview_ec_all.csv')

#-------Chaoter 9. Miscellaneous analysis
#Pub source overlap
overlap_int<-pub_obs%>%group_by(pmid, nct_id)%>%count()
overlap2_int<-overlap_int%>%filter(n!=1)


overlap_obs<-pub_obs%>%group_by(pmid, nct_id)%>%count()
overlap2_obs<-overlap_obs%>%filter(n!=1)

overlap_reg<-pub_reg%>%group_by(pmid, nct_id)%>%count()
overlap2_reg<-overlap_reg%>%filter(n!=1)


#non protocol articles
pubs_non_prot_int<-pub_int%>%filter(!grepl("Protocol",ArticleType))
pubs_non_prot_obs<-pub_obs%>%filter(!grepl("Protocol",ArticleType))
pubs_non_prot_reg<-pub_reg%>%filter(!grepl("Protocol",ArticleType))

#pubs by status
completed_pubs_int<-pub_int%>%filter(overall_status=='Completed')
completed_pubs_obs<-pub_obs%>%filter(overall_status=='Completed')
completed_pubs_reg<-pub_reg%>%filter(overall_status=='Completed')

Terminated_pubs_int<-pub_int%>%filter(overall_status=='Terminated')
Terminated_pubs_obs<-pub_obs%>%filter(overall_status=='Terminated')
Terminated_pubs_reg<-pub_reg%>%filter(overall_status=='Terminated')



# Vaccine trials
trials_pubs_vacc<-Full_trials%>%filter(vaccine_trial_flag==1& Total_pubs!=0)
pub_vacc<-pub_int%>%filter(vaccine_trial_flag==1)