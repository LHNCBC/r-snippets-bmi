# COVID19 Interventional Trials, Observational Studies and Registries

Welcome to the page that collects COVID19 studies registered at various registries (and focusing mainly on ClinicalTrials.gov)

To see the files, follow this link: [https://github.com/lhncbc/r-snippets-bmi/tree/master/regCOVID](https://github.com/lhncbc/r-snippets-bmi/tree/master/regCOVID)  
A landing page link is: [https://lhncbc.github.io/r-snippets-bmi/regCOVID](https://lhncbc.github.io/r-snippets-bmi/regCOVID)

# regCOVID
regCOVID stands for REGistered COVID-19 Interventional trials and observational Studies. We chose this acronym/name so that people can refer to the script/project/outputs uniquely.


# Report
Some of our outputs are available in a generated report at this link: https://lhncbc.github.io/r-snippets-bmi/regCOVID/r-notebook-regCovid.html  
Some direct links within this report are:  
- Graph of studies over time: https://lhncbc.github.io/r-snippets-bmi/regCOVID/r-notebook-regCovid.html#studies_over_time  
- Studied interventions: https://lhncbc.github.io/r-snippets-bmi/regCOVID/r-notebook-regCovid.html#count_of_studies_by_intervention_(normalized_and_mapped)  

# Introduction
We use AACT database from Duke U to obtain most recent CTG (=ClinicalTrials.gov) data.
The script can be further improved.
AACT database is 48 hours (or so) behind. To get the latest trials, you can also use CTG API call like this one: https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=csv

## Search strategies
We use letters to denote search strategies. Strategy A uses title of trial. Strategy B uses MeSH keywords assigned by CTG (their algorhitm; not publicaly posted).

# Code Files
See files with extension .R   
To run the script, you must provide your details for db connection; you must provide your own user name). See their site for instructions.

# Data Files
(will be updated frequently)

Suffix of file name captures subseting. INT means Interventional trials only. OBS means observational studies only. REGISTRY means CTG's registry category.

Another suffix indicates search strategy

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
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=plain
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=tsv
- https://clinicaltrials.gov/ct2/results/download_fields?cond=covid-19&down_count=10000&down_flds=all&down_fmt=csv
- https://www.who.int/ictrp/en/
- http://www.cebm.net/covid-19/registered-trials-and-analysis/
- http://www.cebm.net/covid-19/
- https://aact.ctti-clinicaltrials.org/schema
- https://www.ncbi.nlm.nih.gov/pubmed/23874614
- https://www.ncbi.nlm.nih.gov/pubmed/23304310
- LitCovid (set created by NLM) https://www.ncbi.nlm.nih.gov/research/coronavirus  (we hope to link trials to publications in PubMed (considering LitCovid subset first)
- OHDSI COVID-19 efforts https://www.ohdsi.org/covid-19-updates/ (this project is part of that effort and also self standing)
