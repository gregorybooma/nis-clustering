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
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)

# Look at the warnings as packages load.
Sys.sleep(5)

# Avoid rounding in printing to stdout, conversion of character vars to factor, and scientific notation.
options(digits = 15, stringsAsFactors = F, scipen = 999999999)

# Get metadata needed to load the NIS files.
source("parseStataNis.R")
coreMeta <- parseStataNis("../../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Core.Do")
hospMeta <- parseStataNis("../../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Hospital.Do")
dxprMeta <- parseStataNis("../../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_DX_PR_GRPS.Do")
# leaving out severity file for now, as DRG-based, and trying CCS dx, pr, ec aggregations first
# sevrMeta <- parseStataNis("../../../../consulting/mgh_acad/hcup/StataLoad_NIS_2009_Severity.Do")

# Make sure nothing looks funny in the metadata.
str(coreMeta[1:4])
# pause to take a look
Sys.sleep(10)
# The NA values metadata is long, so just look at the first 10. digits.d ensures no rounding in display of data.
lapply(coreMeta$naVals, function(x) { str(x[1:10], digits.d = 15)} )

# Load the core data file.
coredataraw <- input.file("../../../../consulting/mgh_acad/hcup/NIS_2009_Core.ASC",dstrfw, col_types = coreMeta$varTypes[[1]], widths = coreMeta$varWidths)
names(coredataraw) <- coreMeta$varNames

# making sure data are ordered by observation id
coredata <- coredataraw[order(coredataraw$KEY),]
rm(coredataraw)

# Make sure the df has the right dimensions, valid, and missing (check against NIS documentation)
# for some reason message() strips whitespace, so using paste()
message(paste(dim(coredata)[1],dim(coredata)[2]))
# pause to look at the dims
Sys.sleep(3)
# iotools loads fixed-width as-is (i.e. no trimming, etc.), so need to run through all vars of
# each dataset as we load them, to compare against NIS documentation. Results are output
# to markdown files for integration into subsequent reports.
source("coreCheck.R")
coreCheck(coredata)
# checking codes for diagnoses, procedures, and external causes separately, to make sure
# no anomalies (prior years' CCS coding is rumored to have added problematic whitespace)
source("dxEcPrCheck.R")
dxEcPrCheck(coredata)

# Add labels for vars (Hmisc required). Need as.character() to strip name attribute before assigning.
label(coredata) <- lapply(names(coreMeta$varLabs), function(x) { label(coredata[,x]) <- as.character(coreMeta$varLabs[x]) })

# Subset the df, getting vars of interest, and taking only data for Northeast states
# (since clinical practice can be substantially different between regions).
coreset <- coredata[coredata$HOSPST %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT"),]
coreset <- coreset[setdiff(names(coreset), c(c("AGEDAY","ASOURCE","ASOURCEUB92", "ASOURCE_X","AWEEKEND","DQTR_X","DRG","DRG24","DRGVER","DRG_NoPOA", "H_CONTRL", "HCUP_ED","HOSPBRTH","HOSPST","LOS_X","MDC","MDC24","MDC_NoPOA","MDNUM1_R","MDNUM2_R","NECODE","NEOMAT","NIS_STRATUM","NPR","PointOfOriginUB04","PAY1_X","PAY2_X","PointOfOrigin_X","TOTCHG_X"),grep("PRD.+",names(coreset),value = T)))]
message("Created subset of core data.")
message(nrow(coreset))
# free-up some memory
rm(coredata)

# Recode core data missing and blank as NA, and trim leading and trailing whitespace on the vars.
coreset[,unlist(lapply(coreset,function(x){typeof(x)=="character"}),use.names = F)] <- lapply(coreset[,unlist(lapply(coreset,function(x){typeof(x)=="character"}),use.names = F)], function(x) { str_trim(x, "both") })
source("setCoreNA.R")

# Load and check the diagnosis and procedure data.
dxprdataraw <- input.file("../../../../consulting/mgh_acad/hcup/NIS_2009_DX_PR_GRPS.ASC", dstrfw, col_types = dxprMeta$varTypes[[1]], widths = dxprMeta$varWidths)
names(dxprdataraw) <- dxprMeta$varNames
dxprdata <- dxprdataraw[order(dxprdataraw$KEY),]
rm(dxprdataraw)

source("dxPrCheck.R")
dxPrCheck(dxprdata)

# Add labels for vars (Hmisc required). need as.character() to strip name attribute before assigning.
label(dxprdata) <- lapply(names(dxprMeta$varLabs), function(x) { label(dxprdata[,x]) <- as.character(dxprMeta$varLabs[x]) })

# Subset diagnosis and procedure data, keeping only KEY, procedure, and chronic vars -- and clean-up.
dxprset <- dxprdata[,grep("^(CHR|PCL|KEY)", names(dxprdata))]
rm(dxprdata)

# Recode diagnosis and procedure data missing and blank as NA, and trim whitespace.
dxprset[,unlist(lapply(dxprset,function(x){typeof(x)=="character"}),use.names = F)] <- lapply(dxprset[,unlist(lapply(dxprset,function(x){typeof(x)=="character"}),use.names = F)], function(x) { str_trim(x, "both") })
source("setDxPrNA.R")

# Join diagnosis and procedure subset to core subset, matching and sorting on KEY.
mergedset <- merge(coreset,dxprset, by = "KEY", all.x = T, all.y = F, sort = T)
rm(coreset, dxprset)
message("Merged the core and diagnosis/procedure subsets.")

# Load and check the hospital data.
hospdataraw <- input.file("../../../../consulting/mgh_acad/hcup/NIS_2009_Hospital.ASC", dstrfw, col_types = hospMeta$varTypes[[1]], widths = hospMeta$varWidths)
names(hospdataraw) <- hospMeta$varNames
hospdata <- hospdataraw[order(hospdataraw$HOSPID),]
rm(hospdataraw)

source("hospCheck.R")
hospCheck(hospdata)

# Add labels for vars (Hmisc required). need as.character() to strip name attribute before assigning.
label(hospdata) <- lapply(names(hospMeta$varLabs), function(x) { label(hospdata[,x]) <- as.character(hospMeta$varLabs[x]) })

# Subset hospital data, keeping key vars and vars of interest.
hospset <- hospdata[,grep("^(HOSPID|AHAID|HOSPNAME|HOSPCITY|HOSPST|HOSPSTCO|HOSPZIP|HOSP_BEDSIZE|H_CONTRL|HOSP_LOCTEACH|HOSP_RNPCT|HOSP_RNFTEAPD|HOSP_LPNFTEAPD)", names(hospdata))]
rm(hospdata)

# Recode hospital data missing and blank as NA, and trim whitespace.
hospset[,unlist(lapply(hospset,function(x){typeof(x)=="character"}),use.names = F)] <- lapply(hospset[,unlist(lapply(hospset,function(x){typeof(x)=="character"}),use.names = F)], function(x) { str_trim(x, "both") })
source("setHospNA.R")

# Join hospital subset to the merged subset, matching on HOSPID -- leave sorted on KEY.
nisSet <- merge(mergedset, hospset, by = "HOSPID", all.x = T, sort = F)
rm(mergedset, hospset)
message("Merged the hospital subset to the core/diagnosis and procedure subset.")

# Maine bins ages, so create a var for Maine age from AGE; Also create additional
# features for chronic conditions and hospital rural and teaching status
source("othervars.R")

# Make sure the final set has all rows (check against message for coreset, above).
message(nrow(nisSet))

# Write the full subset to a file. NOTE: For better performance, using iotools raw (unquoted) output,
# so using pipe as delimiter to avoid errors when loading back into R (risk different row lengths if commas used).
# A bug in iotools 0-1.12 requires writing headers to file and then appending. Resolved in subsequent versions.
cat(noquote(paste0(paste0(names(nisSet),collapse = "|"),"\n")),file = "nis-subset-large.csv")
write.csv.raw(nisSet, "nis-subset-large.csv", sep = "|", append=TRUE)
message("Wrote larger subset to nis-subset-large.csv")

# Drop vars that are not currently needed.
dropvars <- c(paste(grep("DXCCS[0-9]+",names(nisSet),value = T), sep = ","),paste(grep("DX[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("E_CCS[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("ECODE[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("PR.+",names(nisSet),value = T), sep = ","), paste(grep("CHRON[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("CHRONB[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("PCLASS[0-9]+",names(nisSet),value = T), sep = ","), paste(grep("PR[0-9]+",names(nisSet),value = T), sep = ","), "PAY1_X", "PAY2", "AHAID", "HFIPSSTCO", "HOSPCITY","HOSPSTCO","HOSP_RNPCT", "HOSP_RNFTEAPD","HOSP_LPNFTEAPD")
nisSet <- nisSet[setdiff(names(nisSet),dropvars)]

# keeping dxccs1-3 for potential later use
#nisSet <- nisSet[-grep("DXCCS([4-9]|1[0-9]|2[0-5])",names(nisSet))]

# convert categorical vars to factors from numeric and character
source("makeFactors.R")
glimpse(nisSet)
str(nisSet)

# In general, patients stay close to home for emergencies, and relatively close to home for elective
# and follow-up hospitalizations. Since patients will not always use the same hospital for follow-up care
# (when greater expertise is needed, e.g.), I need to create hospital area variables and assign them to
# hospitals based upon hospital zipcode.  This is particularly needed in New England, upstate New York,
# and New Jersey where large proportions of hospital areas cross state boundaries.  For hospital areas, I
# use zipcode to hospital zone crosswalk tables developed as part of the Dartmouth Atlas of Health Care project.
source("spatial.R")

# Write the smaller subset of data to a file.
names(nisSet) <- str_to_upper(names(nisSet))
# A bug in iotools 0-1.12 requires writing headers to file and then appending. Resolved in subsequent versions.
cat(noquote(paste0(paste0(names(nisSet),collapse = "|"),"\n")),file = "nis-subset-small.csv")
write.csv.raw(nisSet, "nis-subset-small.csv", sep = "|", append=TRUE)
message("Wrote smaller subset to nis-subset-small.csv")

rm(dropvars)
message("Done.")