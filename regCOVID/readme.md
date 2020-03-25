# COVID19 Interventional Trials and Observational Studies


# regCOVID
regCOVID stands for REGistered COVID-19 Interventional trials and observational Studies. We chose this acronym/name so that people can refer to the script/project/outputs uniquely.


# Introduction
We use AACT (db connection via registered user; you must provide your own user name) to obtain most recent CTG (=ClinicalTrials.gov) data.
The script can be further improved.

## Search strategies
We use letters to denote search strategies. Strategy A uses title of trial. Strategy B uses MeSH keywords assigned by CTG (their algorhitm; not publicaly posted).

# Files
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
- https://aact.ctti-clinicaltrials.org/schema
- https://www.ncbi.nlm.nih.gov/pubmed/23874614
- https://www.ncbi.nlm.nih.gov/pubmed/23304310
- LitCovid (set created by NLM) https://www.ncbi.nlm.nih.gov/research/coronavirus  (we hope to link trials to publications in PubMed (considering LitCovid subset first)
- OHDSI COVID-19 efforts https://www.ohdsi.org/covid-19-updates/ (this project is part of that effort and also self standing)
