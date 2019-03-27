#AACT Access

user='REDACTED'
psw='REDACTED'

library(RPostgreSQL)
library(tidyverse)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user= user, password= psw)
tbls<-RPostgreSQL::dbListTables(con)
tbls



# Total number of studies by mesh term with answer to sharing

total_mesh_query<-"SELECT downcase_mesh_term, count(downcase_mesh_term) 
FROM studies s join browse_conditions c on s.nct_id=c.nct_id
where  (plan_to_share_ipd='Yes'or plan_to_share_ipd='No' or plan_to_share_ipd='Undecided') 
and enrollment > '0' and study_first_submitted_date >'2015-11-30
GROUP by downcase_mesh_term"

total_mesh<-dbGetQuery(con, total_mesh_query)

# Total number by mesh term of yes to sharing

#mesh counts for sharing = Yes

share_query<-"SELECT downcase_mesh_term, count(downcase_mesh_term) 
FROM studies s join browse_conditions c on s.nct_id=c.nct_id
where  plan_to_share_ipd='Yes'and enrollment > '0' and study_first_submitted_date >'2015-11-30
GROUP by downcase_mesh_term"

share_mesh<-dbGetQuery(con, share_query)


#Ratio

#rate of sharing by mesh
total_share_ratio<-merge(share_mesh, total_mesh ,by="downcase_mesh_term")

total_share_ratio[, "percentage_share"] <- total_share_ratio[, "count.x"] / total_share_ratio[, "count.y"]  
total_share_ratio %>% write_csv('2015mesh_share_rate.csv')

#total predictor

pred <- "SELECT *
FROM studies s join browse_conditions c on s.nct_id=c.nct_id and join sponsors p on s.nct_id=c.nct_id
where  (plan_to_share_ipd='Yes'or plan_to_share_ipd='No' or plan_to_share_ipd='Undecided') and enrollment > '0'"


pred_full<-glm(plan_to_share_ipd~results_first_submitted_date + start_date + study_type + phase + enrollment + agency_class,family=binomial, data = pred)
coe_full<-coef(pred_full)
coeff_full <-exp(coe_full)


write.csv(coelist, file = "total_weights.csv")


#predictor for each mesh

meshlist = NULL
coelist=NULL

for (row in nrow(total_mesh)){
  meshlist = NULL
  mesh <- total_mesh[row,1]
  for (rowb in 1:nrow(pred)){ 
    if (pred[rowb,68] == mesh){
      meshlist <- rbind(meshlist , data.frame(pred[rowb,]))
    }}
  pred_glm<-glm(plan_to_share_ipd~results_first_submitted_date + start_date + study_type + phase + enrollment + agency_class,family=binomial, data = meshlist)
  coe<-coef(pred_glm)
  coeff <-exp(coe)
  coelist <- rbind(coelist , coeff)
}


write.csv(coelist, file = "coeff_mesh.csv")





#HIV seperator


sql<-"select *  from studies s join browse_conditions c on s.nct_id=c.nct_id"

ab <- dbGetQuery(con, sql)
ab %>% write_csv('allstudies.csv')
names(ab)


hset<-c('hiv infections'
        ,'hiv seropositivity'
        ,'aids-related complex'
        ,'aids dementia complex'
        ,'hiv-associated lipodystrophy syndrome'
        ,'lymphoma, aids-related'
        ,'aids-related opportunistic infections'
        ,'hiv wasting syndrome'
        ,'aids-associated nephropathy'
        ,'hiv enteropathy')



#column 66 has same name as one other column, .....damn....
#we will remove it from the data.frame ()
ab$nct_id=NULL
names(ab)

hnct<-ab %>% filter(downcase_mesh_term %in% hset) %>% select(nct_id) %>% distinct()

hnct%>% write_csv('hivstudies.csv')




# HIV studies saying yes to sharing
sql_yes<-"select *  from studies s join browse_conditions c on s.nct_id=c.nct_id
where plan_to_share_ipd = 'Yes'"

share <- dbGetQuery(con, sql_yes)
share %>% write_csv('allstudies_share.csv')
names(share)



#column 66 has same name as one other column, .....damn....
#we will remove it from the data.frame ()
ab$nct_id=NULL
names(share)

hnct_yes<-hiv_share %>% filter(downcase_mesh_term %in% hset) %>% select(nct_id) %>% distinct()
hnct_yes %>% write_csv('hivsharing.csv')



#retroactive addition of plan_to share


retro_query<-"SELECT * 
FROM studies 
where  (plan_to_share_ipd='Yes'or plan_to_share_ipd='No' or plan_to_share_ipd='Undecided') 
and study_first_submitted_date < '2015-12-01
GROUP by downcase_mesh_term"

retro_plan<-dbGetQuery(con, retro_query)





