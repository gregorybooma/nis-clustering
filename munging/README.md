### Directory for data munging

This directory contains scripts that load, clean, and validate data; and perform some feature engineering.
* **munging-master.R** is the master script. It contains code controlling the environment and flow of processing, and creation of data subsets. The scripts it calls are as follows:
    + **parseStataNis.R**: parses the HCUP-provided Stata load script to generate metadata used for loading the NIS into R
    + **\*Check.R**: scripts generate summary counts of variable values that must be checked against NIS documentation to validate the data load
    + **set\*NA.R**: scripts recode NIS missing and invalid values to R-compatible "NA"
    + **othervars.R**: generates new features for use in data validation and analysis
    + **makeFactors.R**: converts categorical variables in numeric and character modes to R factors
    + **spatial.R**: extracts hospital geographies from ancillary data sources, for use in linking and clustering

N.B.: All project scripts are commented with technical detail where it was deemed that clarification would be helpful. Reading scripts top to bottom is recommended, as comments are typically only provided for the first use of a method, argument, etc.
