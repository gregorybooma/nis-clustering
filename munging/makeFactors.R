# create factors for categorical variables, dropping (most) originals
if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999999999)

# HOSPITAL LINKAGE VARS -- NOT CONVERTING, IN THE INTEREST OF PROCESSING OVERHEAD
# hospital id
# varlabel <- as.character(coreMeta$varLabs["HOSPID"])
# nisSet$HOSPID <- as.factor(nisSet$HOSPID)
# label(nisSet$HOSPID) <- varlabel
#
# # hospital zip code
# varlabel <- as.character(hospMeta$varLabs["HOSPZIP"])
# nisSet$HOSPZIP <- as.factor(nisSet$HOSPZIP)
# label(nisSet$HOSPZIP) <- varlabel
#
# # hospital state
# varlabel <- as.character(coreMeta$varLabs["HOSPST"])
# nisSet$HOSPST <- as.factor(nisSet$HOSPST)
# label(nisSet$HOSPST) <- varlabel

# died in hospital indicator
varlabel <- as.character(coreMeta$varLabs["DIED"])
nisSet$DIED <- as.factor(nisSet$DIED)
label(nisSet$DIED) <- varlabel

# patient dispostion
varlabel <- as.character(coreMeta$varLabs["DISPUB04"])
nisSet$DISPUB04 <- as.factor(nisSet$DISPUB04)
label(nisSet$DISPUB04) <- varlabel

varlabel <- as.character(coreMeta$varLabs["DISPUNIFORM"])
nisSet$DISPUNIFORM <- as.factor(nisSet$DISPUNIFORM)
label(nisSet$DISPUNIFORM) <- varlabel

# elective indicator
varlabel <- as.character(coreMeta$varLabs["ELECTIVE"])
nisSet$ELECTIVE <- as.factor(nisSet$ELECTIVE)
label(nisSet$ELECTIVE) <- varlabel

# female indicator
varlabel <- as.character(coreMeta$varLabs["FEMALE"])
nisSet$FEMALE <- as.factor(nisSet$FEMALE)
label(nisSet$FEMALE) <- varlabel

# payer information
varlabel <- as.character(coreMeta$varLabs["PAY1"])
nisSet$PAY1 <- as.factor(nisSet$PAY1)
label(nisSet$PAY1) <- varlabel

# patient location
varlabel <- as.character(coreMeta$varLabs["PL_NCHS2006"])
nisSet$PL_NCHS2006 <- as.factor(nisSet$PL_NCHS2006)
label(nisSet$PL_NCHS2006) <- varlabel

# patient race
varlabel <- as.character(coreMeta$varLabs["RACE"])
nisSet$RACE <- as.factor(nisSet$RACE)
label(nisSet$RACE) <- varlabel

# transfer indicator
varlabel <- as.character(coreMeta$varLabs["TRAN_IN"])
nisSet$TRAN_IN <- as.factor(nisSet$TRAN_IN)
label(nisSet$TRAN_IN) <- varlabel

# income in patient zip code
varlabel <- as.character(coreMeta$varLabs["ZIPINC_QRTL"])
nisSet$ZIPINC_QRTL <- factor(nisSet$ZIPINC_QRTL, labels = sort(unique(nisSet$ZIPINC_QRTL)), ordered = T)
label(nisSet$ZIPINC_QRTL) <- varlabel

# hospital bed size
varlabel <- as.character(hospMeta$varLabs["HOSP_BEDSIZE"])
nisSet$HOSP_BEDSIZE <- factor(nisSet$HOSP_BEDSIZE, labels = sort(unique(nisSet$HOSP_BEDSIZE)), ordered = T)
label(nisSet$HOSP_BEDSIZE) <- varlabel

# hospital location/teaching status
varlabel <- as.character(hospMeta$varLabs["HOSP_LOCTEACH"])
nisSet$HOSP_LOCTEACH <- as.factor(nisSet$HOSP_LOCTEACH)
label(nisSet$HOSP_LOCTEACH) <- varlabel

# hospital rural location indicator
varlabel <- "Hospital rural location indicator"
nisSet$HOSP_RURAL <- as.factor(nisSet$HOSP_RURAL)
label(nisSet$HOSP_RURAL) <- varlabel

# hospital teaching status indicator
varlabel <- "Hospital teaching status indicator"
nisSet$HOSP_TEACH <- as.factor(nisSet$HOSP_TEACH)
label(nisSet$HOSP_TEACH) <- varlabel

# grouped diagnoses
dxlabs <- c("Unique grouped diagnosis, order of diagnosis","Unique grouped diagnosis, sorted")
# have to sort bc grep gives matches in order of match, and glabs is in order of varname alphabetically sorted
dxvars <- sort(grep("DX_", names(nisSet), value = T))
for (i in seq_along(dxvars)) {
    varname <- dxvars[i]
    nisSet[,varname] <- as.factor(nisSet[,varname])
    label(nisSet[,varname]) <- dxlabs[i]
}

rm(dxlabs,dxvars,varname,i,varlabel)
message("Factor variables were written.")