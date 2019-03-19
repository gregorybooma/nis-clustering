if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999)

# need to identify record-specific groups of diagnoses, so start with a
# df of diagnoses
dx_df_in <- select(nis_set,matches("^DX[0-5]{1}$"))

# want to truncate to 3-digit dx codes to avoid variations in physician diagnosis, and
# reduce the number of levels
dx_trunc <- dx_df_in %>% mutate_all(~str_sub(.,1,3))

# now form the groups and write vars to the merged set...
# N.B.: this does not work with row-wise apply(), so using do.call()
dx_raw <- do.call(paste,dx_trunc)
message("created raw vector of concatenated dx vals")

dx_clean <- gsub("NA\\.?|\\.NA","",gsub("\\s+",".",dx_raw))
message("cleaned the vector")

dx_ord <- unlist(lapply(dx_clean,function(x) {
    paste(unique(unlist(strsplit(x,"\\."))),collapse = ".")
})) %>% na_if("")
message("created dx order vector")

dx_srt <- unlist(lapply(dx_clean,function(x) {
    # have to use as.numeric() to get a numeric sequence instead of character (e.g. 888 before 9) from sort
    paste(sort(unique(as.numeric(unlist(strsplit(x,"\\."))))),collapse = ".")
})) %>% na_if("")
message("created sorted dx vector")

dx_df <- data.frame(cbind(dx_ord,dx_srt)) %>% set_colnames(c("DX_ORD","DX_SRT"))

nis_set <- cbind(nis_set,dx_df)
label(nis_set$DX_ORD) <- "Unique grouped diagnosis, in order of diagnosis"
label(nis_set$DX_SRT) <- "Unique grouped diagnosis, sorted"

rm(dx_df_in, dx_trunc, dx_raw, dx_clean, dx_ord, dx_srt, dx_df)
message("Finished dx vars")

# alternate ages, based-upon Maine's assignment of age (midpoint of 5-year bins
# after age 4; 0 is 0, and 1-4 is 2; max age is 110)
age_vals <- c(0:110)
maine_age_vals <- c(0, rep.int(2,4), rep.int(7,5), rep.int(12,5), rep.int(17,5), rep.int(22,5), rep.int(27,5), rep.int(32,5), rep.int(37,5), rep.int(42,5), rep.int(47,5), rep.int(52,5), rep.int(57,5), rep.int(62,5), rep.int(67,5), rep.int(72,5), rep.int(77,5), rep.int(82,5), rep.int(87,26))
nis_set$MAINEAGE <- as.integer(mapvalues(nis_set$AGE, age_vals, maine_age_vals))
label(nis_set$MAINEAGE) <- "Age integer value using Maine age assignments"

# admission year -- YEAR corresponds to discharge year
nis_set$AYEAR <- 2009
nis_set$AYEAR[nis_set$AMONTH == 12 & nis_set$LOS > 31] <- 2008
nis_set$AYEAR[nis_set$AMONTH == 12 & nis_set$DQTR !=4] <- 2008
label(nis_set$AYEAR) <- "Admission year"

# code of admission year and month
# make sure no NAs in AMONTH
nis_set$AMONTH[is.na(nis_set$AMONTH)] <- 0
nis_set$AYRMO <- (nis_set$AYEAR * 100) + nis_set$AMONTH
label(nis_set$AYRMO) <- "Code for admission year and month"
nis_set$AYRMO[nis_set$AMONTH == 0] <- NA

# first month of quarter in which patient died
# can't initialize with NA, so using 0 at first
nis_set$DIEDQTRSTARTMO <- 0
nis_set$DIEDQTRSTARTMO[nis_set$DIED[!is.na(nis_set$DIED)] == 1] <- nis_set$DQTR[nis_set$DIED[!is.na(nis_set$DIED)] == 1]
nis_set$DIEDQTRSTARTMO[nis_set$DISPUB04[!is.na(nis_set$DISPUB04)] %in% c(20,40,41,42)] <- nis_set$DQTR[nis_set$DISPUB04[!is.na(nis_set$DISPUB04)] %in% c(20,40,41,42)]
nis_set$DIEDQTRSTARTMO <- mapvalues(nis_set$DIEDQTRSTARTMO,c(2,3,4),c(4,7,10))
label(nis_set$DIEDQTRSTARTMO) <- "First month of quarter in which patient died"
# need 0 value for code of year and month of death, so holding-off on setting NAs

# code of died year and month
nis_set$DIEDYRMO <- 0
nis_set$DIEDYRMO <- (nis_set$YEAR * 100) + nis_set$DIEDQTRSTARTMO
nis_set$DIEDYRMO[nis_set$DIEDYRMO == 200900] <- NA
label(nis_set$DIEDYRMO) <- "Code for year and month of death (month = first month of quarter)"

# write NAs for AMONTH and DIEDQTRSTARTMO
nis_set$AMONTH[nis_set$AMONTH == 0] <- NA
nis_set$DIEDQTRSTARTMO[nis_set$DIEDQTRSTARTMO == 0] <- NA

# var indicating rural (or not) location of hospital
nis_set$HOSP_RURAL <- as.integer(rep.int(0,nrow(nis_set)))
nis_set$HOSP_RURAL[nis_set$HOSP_LOCTEACH == 1] <- 1
label(nis_set$HOSP_RURAL) <- "Hospital rural location indicator"

# var indicating teaching status of hospital
nis_set$HOSP_TEACH <- as.integer(rep.int(0,nrow(nis_set)))
nis_set$HOSP_TEACH[nis_set$HOSP_LOCTEACH == 3] <- 1
label(nis_set$HOSP_TEACH) <- "Hospital teaching status indicator"

rm(age_vals,maine_age_vals)
