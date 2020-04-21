Warehouse Statistics 

The following program can be configured to produce ‘warehouse statistics’ for files within VRDC that describe patient care. There are additional features of patient care this script does not facilitate harvesting such as the location of care, the demography of patients, mortality, and costs of care. On some level these are secondary analysis use cases that would only be interesting if known about a predefined cohort.  Ideally users would use this output to establish their research question. Seeing how many kinds of relative care events there were should inform cohort design. This output should support cohort design efforts. 

We consider care in three wide categories:  

Diagnosis: ICD9-CM, ICD10-CM DX Codes 

Procedures: ICD9-CM-Procedure, ICD10-CM-Procedure, CPT and HCPCS 

Medications: National Drug Codes or RX_NORM Clinical Drugs 

We do not disambiguate inpatient, outpatient, long term or elective care from one another. However users could use the native file names and the codes themselves to decide on the above classifications.  

Configuration: This script uses several configuration options including source files, months of interest, years of interest as well as input and output files.  

Essential declarations: 

1. Host Library: Declare your user library. Do not run this in the work directory.  

2. ndc_map_b: Right now the RX program is configured to work with our MMbox clinical drug translation file. Open the RX scripts and comment out between the ‘cut here’ comments to disable this. Disabling this will return NDCs instead of RX norm Clinical Drugs. 

3.Year- The current version is for one year at a time. This should help balance memory issues. This is a large result that may excede your VRDC allocation. 

4.First State-First state on the list to collect counts from. (MAX option only) 

5.Last State-Last state on the list to collect counts from; this can equal the first state if you only want one state. (MAX option only) 

6.First Month-First month on the list to collect counts from. (RIF option only) 

7.Last Month-Last month on the list to collect counts from. (RIF option only) 

‘The List File’ for fancy configurations 

The program loops through item positions in this list file, not actual file names or state names. You must load this file into your host library. The program learns the year, month, state and file names from this file. The year1 or state1 in this list corresponds to the first item in the corresponding column.  You can manually change the order of the items in the file to control the loop. Double clicking on the SAS file while in view mode should enable edit privileges. For example if you want to run a loop for year 2000 and 2016 only you could run the program twice or change the order of the year column in the list to first:2000 and second:2016 instead of the native first:1999 and second:2001; then run from &year1 to &year2. Remember SAS has a hard time looping over strings, but can manage a list like this.  

Medicaid vs Medicare 

This script uses a 100% sample of VRDC RIF and MAX Files. Your VRDC configuration may differ, and in turn results may vary. You could modify the script to reflect your file name structure. This is tricky and takes a little hit and miss. Most VRDC file name structures are dynamic. For example: 

&filelib&year..&filename.%sysfunc(putn(&month,z2.)); 

Resolves to 

Rif2016.bcarrier_claims_01 

This is the file for b carrier claims records from the month of January in the year 2016. 

Medicaid is more complicated, as the MAX file names are not functional (left to right). 

Max&year..&file.&&STATE&a..&subtype&year 

Resolves to 

Max2014.Maxdata_CA_RX_2014 

This file contains prescription records for Medicare patients in the state of California for the year 2014.  

You may need to edit these complex macro strings to match your VRDC configuration. 

Good luck and welcome to VRDC. 
