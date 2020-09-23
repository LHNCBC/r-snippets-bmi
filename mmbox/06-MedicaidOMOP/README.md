# Medicaid to OMOP ETL code

Source data: CMS VRDC Medicaid data. For both current (TAF) format and legacy (MAX) format.

See folder TAF or folder MAX for files organized by OMOP table name.

Additional Medicaid releveant link: https://github.com/lhncbc/CRI/tree/master/VRDC/project/Medicaid

# Comments on individual tables

## TAF
### person
Person table OMOP ETL to generate OMOP Person Table.

### observation_period
Observation period ETL to generate OMOP Observation Period Table.
TAF contains an observation period 'like'table natively unlike MAX.

## MAX
### person
Person table OMOP ETL to generate OMOP Person Table.

### observation_period
This file creates an OMOP observation table out of MAX monthly enrollment status assessments. 


