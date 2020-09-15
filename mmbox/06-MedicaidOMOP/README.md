# Medicaid to OMOP ETL code

Source data: CMS VRDC Medicaid data. For both current (TAF) format and legacy (MAX) format.

See folder TAF or folder MAX for files organized by OMOP table name.

Additional Medicaid releveant link: https://github.com/lhncbc/CRI/tree/master/VRDC/project/Medicaid

# Comments on individual tables

## TAF
JUS_TAF_CASES
JUS_TAF_LKUP

### person
Max_and_Taf_Age_Extraction

### observation_period

MAX_Enrollment_Table_Maker
	This Max File will turn several years of MAX Enrollment records into
	an observaiton_period table.
SQL extract for assessing age from TAF Demography file.

### observation_period
TAF contains an observation period table natively; this file assesses that table for longest and first enrollment episode.

TAF_Enrollment_Assessment
	TAF contains an observation period table for enrollment periods natively;
	This file will assess enrollment in SAS for longest and first enrollment 
	period. 

## MAX
JUS_MAX_CASES
JUS_MAX_LKIP
### person
SQL extract for assessing age from MAX PS File.

### observation_period
This file creates an observation table out of MAX monthly enrollment status assessments. To assess longest or first enrollment
episode, modify TAF observation period file to use the final file from this script. 
