
# COVID19 Interventional Trials, Observational Studies and Registries

Welcome to the page that collects COVID19 studies registered at ClinicalTrials.gov. This page is updated weekly (each Monday).

- [CLICK HERE FOR REPORT](https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCovid_notebook2.html). This is the most important link on this page.
- To see the R code for the analysis and all input and result files, follow this link: [https://github.com/lhncbc/r-snippets-bmi/tree/master/regCOVID](https://github.com/lhncbc/r-snippets-bmi/tree/master/regCOVID)  
- [DESCRIPTION OF EACH DATA FILE](https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCOVID_data_file_descript.html). This is provided if you plan to use our data or see the results in table format. (instead of a report above)
- Old releases (data from previous weeks) are available via GitHub repo release mechanism here: https://github.com/lhncbc/r-snippets-bmi/releases

Additional important links are

- [A user friendly landing page link is here](https://lhncbc.github.io/r-snippets-bmi/regCOVID). This links does not display a long list of data files in this repository and is easier to consume for non-experts.


# regCOVID
regCOVID stands for REGistered COVID-19 Interventional trials and observational Studies. We chose this acronym/name so that people can refer to the script/project/outputs uniquely.


# Report
The report is the most important output. It is linked at the top of this documentation. The link to the report is [https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCovid_notebook2.html](https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCovid_notebook2.html)


# Database form of ClinicalTrials.gov data - AACT database
This project relies heavily on prior project by Duke University. It is the AACT database. It stands for Aggregate Analysis of ClinicalTrials.gov. The database offers most recent CTG (=ClinicalTrials.gov) data in database form.

- For researchers interested in using teh AACT database, we recommend reviewing the Entity-Relationship diagram here: https://aact.ctti-clinicaltrials.org/schema  
- Detailed documentation of the database columns is at https://aact.ctti-clinicaltrials.org/data_dictionary 
- Excel version https://aact.ctti-clinicaltrials.org/static/documentation/aact_data_definitions.xlsx

## Data lag
AACT database is 48 hours (or so) behind. To eliminate the lag and get more recent data (compared to AACT), you can also use CTG API call like this one: https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=csv

## ClinicalTrials.gov documentation

In addition, data dictionary from CTG is available at https://prsinfo.clinicaltrials.gov/definitions.html and very usefull to researchers extenind this analysis or conducting other CTG based analyses.  


# Analysis comments

## Search strategies
We use letters to denote search strategies. Strategy A uses title of trial. Strategy B uses free-text condition. Strategy C uses MeSH keywords assigned by CTG (their algorhitm; not publicaly posted).

# How to obtain a copy of the database we created

See [DESCRIPTION OF EACH DATA FILE](https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCOVID_data_file_descript.html). For example, files regCovid_int-a.csv, regCovid_obs-a.csv, and regCovid_reg-a.csv are the lists of interventional trials, observational studies, and registries generated from search method A. Replace letter at the end with b and c to obtain the set of trials using the other search strategy. Note that these files will only include the STUDIES table metadata (from AACT). Inspect the other CSV files that support all the web report analyses to get additional data (e.g., interventions, study updates, etc.)


# Code Files
See files with extension .R (regCovid_code_for_analysis.R)   
To run the script, you must provide your details for db connection (you must provide your own user name). See their site for instructions https://aact.ctti-clinicaltrials.org/connect.

Code Sections:
- Chapter 1: Studies by type
- Chapter 2: Studies over time
- Chapter 3: Phase
- Chapter 4: Arms
- Chapter 5: Enrollment
- Chapter 6: Study Design
- Chapter 7: Number of sites
- Chapter 8: Country
- Chapter 9: Intervention
- Chapter 10: Update history
- Chapter 11: Vaccines
- Chapter 12: Completed interventional trials
- Chapter 13: Has results


# Data Files
(will be updated frequently)

Suffix of file name captures subseting. INT means Interventional trials only. OBS means observational studies only. REGISTRY means CTG's registry category.

Another suffix indicates search strategy: A, B, or C

A list of the data files included and descriptions of what the files are, can be found at this link:  https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCOVID_data_file_descript.html


# View
We plan to have multiple views: Condensed will be a single file. Custom will be tweak based on purpose. Relational (full or partial) will list study data by domain (will mirror the AACT schema).

# Feedback
We don't claim our script is the best approach and are open to suggestions for improvement

# Future additions
- for each NCT - webscrape (or via XML) obtain evidence of updating a trial. An updated trial is viewed as more creditable.
- other registries (besides CTG)
- view of all intervations that allows overview of drugs used in trials (over time)
- relational view 

# References
- 
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=plain
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=tsv
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=csv
- https://www.who.int/ictrp/en/
- similar efforts
  - http://www.cebm.net/covid-19/registered-trials-and-analysis/
  - http://www.cebm.net/covid-19/
  - https://www.covid-trials.org/
- AACT relational database version of ClinicalTrials.gov data https://aact.ctti-clinicaltrials.org/schema
- https://www.ncbi.nlm.nih.gov/pubmed/23874614
- https://www.ncbi.nlm.nih.gov/pubmed/23304310
- LitCovid (set created by NLM) https://www.ncbi.nlm.nih.gov/research/coronavirus  (we hope to link trials to publications in PubMed (considering LitCovid subset first)
- OHDSI COVID-19 efforts https://www.ohdsi.org/covid-19-updates/ (this project is part of that effort and also self standing)

