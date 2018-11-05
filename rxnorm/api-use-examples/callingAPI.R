#this file provides same demo as the R Markdown file
#used in the workshop

#crtl L cleaers consolre, after that ctrl 1 focus on source

#next line would load rxnorm concepts
#source('loadConcepts.R')
#there are many JSON packages in R, we will pick one of them

library(jsonlite)
library(tidyverse)
library(magrittr)

#some examples

example='atenolol'
example='nexium'
example='Esomeprazole'


example='bicycle'
input=example


# url %>% URLencode() %>% fromJSON()

#wrapping a call to API into R function
findRxCuibyString<-function(input) {
  url<-sprintf('https://rxnav.nlm.nih.gov/REST/rxcui.json?search=2&allsrc=0&name=%s',input)
  url2<-URLencode(url)
  j<-jsonlite::fromJSON(url2)
  #result in in variable j, we can traverse JSON as traversing a list in R as in the line below
  out=data.frame(rxnormId=as.integer(j$idGroup$rxnormId))
  #output is just a list of strings, we will extend it with more info
  if (nrow(out)>0) {out$input=input;out$match=1:nrow(out)}   else out=data.frame(rxnormId=NA,input=input,match=NA)
  out
}


example='atenolol'
example='nexium'
example='Esomeprazole'
example='esomeprazole'
example='esomeprazolE'

example='pregabalin'
example='lyrica'
findRxCuibyString(example)




#JSON parsing

input='283742' #esomeprasole
input='187832' #pregabalin

whatFunction='allrelated'
url<-sprintf('https://rxnav.nlm.nih.gov/REST/rxcui/%s/%s.json',input,whatFunction)

url
cat(url,file='clipboard')
j2<-jsonlite::fromJSON(URLencode(url))
print(j2)
#display highest level
str(j2,max.level = 1)

#traverse the tree
str(j2$allRelatedGroup)
str(j2$allRelatedGroup,max.level = 1)


#using JSON parsing shortcut

j3<-jsonlite::fromJSON(URLencode(url),flatten = T)
str(j3$allRelatedGroup)

str(j3$allRelatedGroup$conceptGroup,max.level =1)

str(j3$allRelatedGroup$conceptGroup$conceptProperties,max.level =1)

#making it one large table

oneBigTable<-plyr::rbind.fill(j3$allRelatedGroup$conceptGroup$conceptProperties) 
oneBigTable %<>% as.tibble() 
oneBigTable
pander::pander(oneBigTable)
View(oneBigTable)
