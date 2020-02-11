# NDC transfrmed into RxNorm Clinical Drug (using OMOP vocabulary)

# Problem
NDCs do not represent optimal level of granularity for clinical analysis.

# Files

## tranlate_ndc_b-all.csv

Knowledge base. This file is derived from OMOP Vocabulary (concept_relationship table). It maps each NDC code first into `Branded Drug` (if applicable) and then to `Clinical Drug`.
