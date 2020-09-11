# Medicaid to OMOP ETL code

Source data: CMS VRDC Medicaid data. For both current (TAF) format and legacy (MAX) format.

See folder TAF or folder MAX for files organized by OMOP table name.

Additional Medicaid releveant link: https://github.com/lhncbc/CRI/tree/master/VRDC/project/Medicaid

# Comments on individual tables

## TAF (new format, 2016 - present)

### person
SQL extract for assessing age from TAF x file.

### observation_period
TAF contains an observation period table natively; this file assesses that table for longest and first enrollment episode.

## MAX (legacy format, 1999-2016)

### person
SQL extract for assessing age from MAX PS File.

### observation_period
This file creates an observation table out of MAX monthly enrollment status assessments. To assess longest or first enrollment
episode, modify TAF observation period file to use the final file from this script. 
