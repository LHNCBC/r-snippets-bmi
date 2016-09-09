source('loadConcepts.R')

#there are many JSON packages in R, we will pick one of them
library(jsonlite)



#some examples
example='atenolol'
example='bicycle'



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


findRxCuibyString(example)
