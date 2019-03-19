# This is the master script that handles the data munging for this project. The inline comments
# should explain what is going on at each step.  Other scripts sourced from this one should
# also be reasonably-well commented.

# RStudio resets the wd to the top level project dir (as opposed to working dir) after the
# project has been closed, but work should be done in "workspace" to avoid writing to Github
# repository (workspace is in .gitignore).
if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}

# Per the developers, plyr has to be loaded before dplyr to avoid problems
# character.only = T ensures names are passed as text.
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr", "magrittr"), require, quietly = T, character.only = T)

# Look at the warnings as packages load.
Sys.sleep(5)

# Avoid rounding in printing to stdout, conversion of character vars to factor, and scientific notation.
options(digits = 15, stringsAsFactors = F, scipen = 999)

# Get metadata needed to load the NIS files.
source("../munging/parse_stata_nis.R")
core_meta <- parse_stata_nis("../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Core.Do")
hosp_meta <- parse_stata_nis("../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Hospital.Do")
dxpr_meta <- parse_stata_nis("../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_DX_PR_GRPS.Do")
# leaving out severity file for now, as DRG-based, and trying CCS dx, pr, ec aggregations first
# sevr_meta <- parse_stata_nis("../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Severity.Do")

# Make sure nothing looks funny in the metadata.
str(core_meta[1:4])
# pause to take a look
Sys.sleep(10)
# The NA values metadata is long, so just look at the first 10. digits.d ensures no rounding in display of data.
lapply(core_meta$na_vals, function(x) { str(x[1:10], digits.d = 15)} )

# Load the core data file. iotools input.file(...) considerably faster than other tools for fwf.
core_data <- input.file("../../../consulting/mgh_acad/hcup/NIS_2009_Core.ASC",dstrfw, col_types = core_meta$var_types[[1]], widths = core_meta$var_widths) %>% set_colnames(core_meta$var_names) %>% arrange(KEY)

# Make sure the df has the right dimensions, valid, and missing (check against NIS documentation)
# for some reason message() strips whitespace, so using paste()
message(paste(dim(core_data)[1],dim(core_data)[2]))
# pause to look at the dims
Sys.sleep(3)
# iotools loads fixed-width as-is (i.e. no trimming, etc.), so need to run through all vars of
# each dataset as we load them, to compare against NIS documentation. Results are output
# to markdown files for integration into subsequent reports.
source("../munging/core_check.R")
core_check(core_data)
# checking codes for diagnoses, procedures, and external causes separately, to make sure
# no anomalies (prior years' CCS coding is rumored to have added problematic whitespace)
source("../munging/dxecpr_check.R")
dxecpr_check(core_data)

# Add labels for vars (Hmisc required). Need as.character() to strip name attribute before assigning.
label(core_data) <- lapply(names(core_meta$var_labs), function(x) { label(core_data[,x]) <- as.character(core_meta$var_labs[x]) })

# Subset the df, getting vars of interest, and taking only data for Northeast states. Can trim
# white space now.
coreset <- core_data %>% filter(HOSPST %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT")) %>% select(setdiff(names(core_data), c(c("AGEDAY","ASOURCE","ASOURCEUB92", "ASOURCE_X","AWEEKEND","DQTR_X","DRG","DRG24","DRGVER","DRG_NoPOA", "H_CONTRL", "HCUP_ED","HOSPBRTH","HOSPST","LOS_X","MDC","MDC24","MDC_NoPOA","MDNUM1_R","MDNUM2_R","NECODE","NEOMAT","NIS_STRATUM","NPR","PointOfOriginUB04","PAY1_X","PAY2_X","PointOfOrigin_X","TOTCHG_X"),grep("PRD.+",names(core_data),value = T)))) %>% mutate_if(is.character,str_trim)
message("Created subset of core data.")
message(nrow(coreset))
# free-up some memory
rm(core_data)
gc(verbose = F)

# Recode core data missing and blank as NA.
source("../munging/set_core_na.R")

# Load and check the diagnosis and procedure group data.
dxpr_data <- input.file("../../../consulting/mgh_acad/hcup/NIS_2009_DX_PR_GRPS.ASC", dstrfw, col_types = dxpr_meta$var_types[[1]], widths = dxpr_meta$var_widths) %>% set_colnames(dxpr_meta$var_names) %>% arrange(KEY)

source("dxpr_check.R")
dxpr_check(dxpr_data)

# Add labels for vars (Hmisc required). need as.character() to strip name attribute before assigning.
label(dxpr_data) <- lapply(names(dxpr_meta$var_labs), function(x) { label(dxpr_data[,x]) <- as.character(dxpr_meta$var_labs[x]) })

# Subset diagnosis and procedure data, keeping only KEY, procedure, and chronic vars -- and clean-up.
dxpr_set <- dxpr_data %>% select(grep("^(CHR|PCL|KEY)", names(dxpr_data), value = T)) %>% mutate_if(is.character,str_trim)
rm(dxpr_data)

# Recode diagnosis and procedure data missing and blank as NA.
source("set_dxpr_na.R")

# Join diagnosis and procedure subset to core subset, matching and sorting on KEY.
merged_set <- merge(core_set,dxpr_set, by = "KEY", all.x = T, all.y = F, sort = T)
rm(core_set, dxpr_set)
message("Merged the core and diagnosis/procedure subsets.")
gc(verbose = F)

# Load and check the hospital data.
hosp_data <- input.file("../../../../consulting/mgh_acad/hcup/NIS_2009_Hospital.ASC", dstrfw, col_types = hosp_meta$var_types[[1]], widths = hosp_meta$var_widths) %>% set_colnames(hosp_meta$var_names) %>% arrange(HOSPID)

source("hosp_check.R")
hosp_check(hosp_data)

# Add labels for vars (Hmisc required). need as.character() to strip name attribute before assigning.
label(hosp_data) <- lapply(names(hosp_meta$var_labs), function(x) { label(hosp_data[,x]) <- as.character(hosp_meta$var_labs[x]) })

# Subset hospital data, keeping key vars and vars of interest.
hosp_set <- hosp_data %>% select(grep("^(HOSPID|AHAID|HOSPNAME|HOSPCITY|HOSPST|HOSPSTCO|HOSPZIP|HOSP_BEDSIZE|H_CONTRL|HOSP_LOCTEACH|HOSP_RNPCT|HOSP_RNFTEAPD|HOSP_LPNFTEAPD)", names(hosp_data), value = T)) %>% mutate_if(is.character,str_trim)
rm(hosp_data)

# Recode hospital data missing and blank as NA.
source("set_hosp_na.R")

# Join hospital subset to the merged subset, matching on HOSPID -- leave sorted on KEY.
nis_set <- merge(merged_set, hosp_set, by = "HOSPID", all.x = T, sort = F)
rm(merged_set, hosp_set)
message("Merged the hospital subset to the core/diagnosis and procedure subset.")
gc(verbose = F)

# Maine bins ages, so create a var for Maine age from AGE; Also create additional
# features for chronic conditions and hospital rural and teaching status
source("other_vars.R")

# Make sure the final set has all rows (check against message for coreset, above).
message(nrow(nis_set))

# Write the full subset to a file. NOTE: For better performance, using iotools raw (unquoted) output,
# so using pipe as delimiter to avoid errors when loading back into R (risk different row lengths if commas used).
# A bug in iotools 0-1.12 requires writing headers to file and then appending. Resolved in subsequent versions.
cat(noquote(paste0(paste0(names(nis_set),collapse = "|"),"\n")),file = "nis-subset-large.csv")
write.csv.raw(nis_set, "nis-subset-large.csv", sep = "|", append=TRUE)
message("Wrote larger subset to nis-subset-large.csv")

# Drop vars that are not currently needed.
dropvars <- c(paste(grep("DXCCS[0-9]+",names(nis_set),value = T), sep = ","),paste(grep("DX[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("E_CCS[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("ECODE[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("PR.+",names(nis_set),value = T), sep = ","), paste(grep("CHRON[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("CHRONB[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("PCLASS[0-9]+",names(nis_set),value = T), sep = ","), paste(grep("PR[0-9]+",names(nis_set),value = T), sep = ","), "PAY1_X", "PAY2", "AHAID", "HFIPSSTCO", "HOSPCITY","HOSPSTCO","HOSP_RNPCT", "HOSP_RNFTEAPD","HOSP_LPNFTEAPD")
nis_set <- nis_set[setdiff(names(nis_set),dropvars)]

# keeping dxccs1-3 for potential later use
#nis_set <- nis_set[-grep("DXCCS([4-9]|1[0-9]|2[0-5])",names(nis_set))]

# convert categorical vars to factors from numeric and character
source("makeFactors.R")
glimpse(nis_set)
str(nis_set)

# In general, patients stay close to home for emergencies, and relatively close to home for elective
# and follow-up hospitalizations. Since patients will not always use the same hospital for follow-up care
# (when greater expertise is needed, e.g.), I need to create hospital area variables and assign them to
# hospitals based upon hospital zipcode.  This is particularly needed in New England, upstate New York,
# and New Jersey where large proportions of hospital areas cross state boundaries.  For hospital areas, I
# use zipcode to hospital zone crosswalk tables developed as part of the Dartmouth Atlas of Health Care project.
source("spatial.R")

# Write the smaller subset of data to a file.
names(nis_set) <- str_to_upper(names(nis_set))
# A bug in iotools 0-1.12 requires writing headers to file and then appending. Resolved in subsequent versions.
cat(noquote(paste0(paste0(names(nis_set),collapse = "|"),"\n")),file = "nis-subset-small.csv")
write.csv.raw(nis_set, "nis-subset-small.csv", sep = "|", append=TRUE)
message("Wrote smaller subset to nis-subset-small.csv")

rm(dropvars)
message("Done.")