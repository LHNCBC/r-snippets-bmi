Now that you know what you are looking for you might try to make a cohort. Our team generated several cohorts over our time working with VRDC. A cohort is a select list of patients who have conditions of interest, and all care they have experienced over an observation window. Cohorts may be outcome based (mortality in the follow-up period) or simply observational. 

This program makes the files you need to define your cohort. We work in two steps to support additional research tasks. The first step is the ‘finder’ and the second step is the ‘harvester’. 

The scripts look very similar but in reality, they are very different. The finder finds all cases that match a case definition. 

A case definition is either a short list of codes (often with matching code type if working with ICD9 years) or a list file that contains cohort qualifying codes.  

#The Finder 

The finder program consumes the qualifying codes and returns qualifying events. This is not a distinct list of beneficiaries, but one can be made from this list. Further pruning, such as index event prior ranges or post ranges, reoccurrence of the index condition or other demographic features can be sourced at this step from the Finder output by joining a list of distinct bene_ids to the MBSF file or NDI files. Further throwing out cases who are too old, young, urban, rural and the like can be done using the MBSF file. The finder program does not support this step but it is a fairly easy task, as Finder turns the RIF file into one subset file containing qualified clinical events.  

Finder is one directional, and is designed to make one subset. Complex case definitions are not supported directly but can be produced using ‘intersect or inner join’ of several sets of bene_ids that finder produces. For example if you want all patients with hip fracture and opioid use you would find the patients with hip fracture, and the patients with opioid use (learned from a list of NDC); split the file by RX and PX type code and see who has both using an inner join.  VRDC case definitions are complex as simple clinical questions often have to be encoded into ICD, CPT and HCPCS as well as NDC to be expressed to VRDC. Finder returns all to codes you ask it to find, its up to you to build the study population from the Finder output. 

#The Collector 

The Collector will pull all care these patients received from the warehouse. This resulting file will support time to event, segmentation and causal analysis from multiple sources (including medications). Harvester is not necessary, and it takes a while to run. You can control how much of the warehouse you want to harvest from but the best part of having a lot of data is allowing yourself to be surprised. Harvester does not support SAS STAT operations, and you will have to write and run that yourself. However, your model should benefit from producing a data set with all attributable care to your cohort. You should prune this file. Think about the non-relevant codes and the codes you actually want in your analysis. Return only those codes of relevant events to your SAS STAT operation. You could, and it is interesting to use every code in logistic regression or PCP. These whole view models are difficult to explain to non-researchers but can be the basis of new understandings in medicine ( if you can describe why you get the result you did). 

#Essential declarations: 

1. Host Library: Declare your user library. Do not run this in the work directory.  

2.Start Year-First year to collect counts from. 

3.End Year- Las year to collect counts from; This can equal first year if you only want one year. 

4.First State-First state on the list to collect counts from. (MAX option only) 

5.Last State-Last state on the list to collect counts from; this can equal the first state if you only want one state. (MAX option only) 

6.First Month-First month on the list to collect counts from. (RIF option only) 

7.Last Month-Last month on the list to collect counts from. (RIF option only) 

#‘The List File’ for fancy configurations 

The program loops through item positions in this list file, not actual file names or state names. You must load this file into your host library. The program learns the year, month, state and file names from this file. The year1 or state1 in this list corresponds to the first item in the corresponding column.  You can manually change the order of the items in the file to control the loop. Double clicking on the SAS file while in view mode should enable edit privileges. For example if you want to run a loop for year 2000 and 2016 only you could run the program twice or change the order of the year column in the list to first:2000 and second:2016 instead of the native first:1999 and second:2001; then run from &year1 to &year2. Remember SAS has a hard time looping over strings, but can manage a list like this.  

 

#Welcome to VRDC and good luck. 
