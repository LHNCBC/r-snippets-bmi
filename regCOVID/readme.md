
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
Some of our outputs are available in a generated report at this link: https://lhncbc.github.io/r-snippets-bmi/regCOVID/regCovid_notebook2.html


# Introduction
We use AACT database from Duke U to obtain most recent CTG (=ClinicalTrials.gov) data.
The script can be further improved.
AACT database is 48 hours (or so) behind. To get the latest trials, you can also use CTG API call like this one: https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=csv

## Search strategies
We use letters to denote search strategies. Strategy A uses title of trial. Strategy B uses free-text condition. Strategy C uses MeSH keywords assigned by CTG (their algorhitm; not publicaly posted).

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



<br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>

# Copy of presentation slides used at a webinar

<br><br><br><br><br><br><br><br>


# Presentation slides

<br><br><br>

## What is regCOVID? (slide 1)
- analysis of COVID-19 studies registered in ClinicalTrials.gov using informatics methods
- 'reg' stands for registered clinical studies
- studies: 1.interventional clinical trials, 2. observational studies, 3. registries
- informatics methods: no or minimal human curation of the data
- open-source code (R language); demonstration of data science
<br><br>
- authors: Vojtech Huser, Craig Mayer 
  
___
<br><br><br><br><br><br><br><br>


## Motivation
- We build on previous work that 
  - linked Clinicaltrials.gov records to PubMed articles ([PMC3706420](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3706420),[PMC3540528](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3540528/))
  - analysis of ClinicalTrials.gov data for study outcomes ([PMC6371255](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6371255/))
  - analysis of sharing Individual Participant Data on ClinicalTrials.gov ([PMC7153161](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7153161/))
- ClinicalTrials.gov function: inform the public (COVID-19;perfect use case)
- Single disease snapshot analysis of registered studies 
  - prior work focusing on HIV
  - `D-SHOT` Knowledge Base (disease-intervention)
___
<br><br><br><br><br><br><br><br><br><br><br><br>


## Context
- few similar efforts (see references in this readme file)
  - main difference: fully open source, small size of our team, single registry)
- Inclusion criteria
  - 1: search criterion: three compared search methods (title,keyword, MeSH keyword)      
  - 2: metadata criteria: study status (`Active, not recruiting`, `Completed`) (excluded 'Not yet recruiting')
  - Typical flow is `Not yet recruiting` --> `active` --> `completed`)

___
<br><br><br><br><br><br><br><br><br><br><br><br>

## Report Demo 
- live demo of report now (COVID-19 version; v1)
<br><br>
- other analyses
  - custom analyses for journal article draft
  - general analysis (for any disease; v2)
  - intervention-only focused analyses (`d-shot`; v3)
  



___
<br><br><br><br><br><br><br><br>

## Lessons learned
- Study record updates
- Dates logic (recency, comparison to "today's date")
- Interventions
  - Placebo (sham)
  - presence of interventions in observational studies and registries
- Limitations of computerized analysis, limit the burden of ClinicalTrials.gov record creation and maintenance
- Study design metadata
- Special analysis context for Registries and Observational Studies (FDA's framework for RWE/RWD)
   

___
<br><br><br><br><br><br><br><br><br><br><br><br>

## Additional COVID-19 work
- Participation on Observational Health Data Sciences and Informatics (OHDSI) consortium COVID-19 efforts
- Characterization of COVID-19 cases (co-author, [MedrXiv preprint](https://www.medrxiv.org/content/10.1101/2020.04.22.20074336v1))

<br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br>
