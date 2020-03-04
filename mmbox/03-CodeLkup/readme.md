# Code Look Up Resources

Set of CSV files derived from OMOP Vocabulary optimized for use in VRDC.
For example, dot in ICD codes is removed.

# Files

## lkupALL.csv
Combined file with several terminologies included. Unfortunatelly, the HCPCS level 1 codes (CPT) are excluded for complex reasons.


# Note of .csv.zip files 
Puting files >100MB on Github is restricted. For this reason, large full files are zipped. In order to see a preview of the file, view the file with suffix `-preview' to see a preview of the full file.

# Staying up to date
The code depends on latest version of terminologies. The files will age and become less current. However lkup file generated in, say, 2019 may still be usefull if all your data is anyway prior the file generation date (2019).

## Create refreshed tables
If refresh is needed, notes here provide links to related code.
Related R code is at https://github.com/vojtechhuser/Athenian
and in file athena.R
