if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999)

# need to identify record-specific groups of diagnoses, so start with a
# df of diagnoses
dx_df_in <- nis_set %>% select(matches("^DX[0-5]{1}$"))

# want to truncate to 3-digit dx codes to avoid variations in physician diagnosis, and
# reduce the number of levels
dx_trunc <- dx_df_in %>% mutate_all(~str_sub(.,1,3))

# now form the groups and write vars to the merged set...
# Update: found that this can be done with apply() by adding "collapse = ..."
# e.g. dx_raw <- apply(dx_trunc,1,paste,collapse = " "), and tidyr unite() also
# works. Regardless, do.call() with paste() is faster.
dx_raw <- do.call(paste,dx_trunc)
message("created raw vector of concatenated dx vals")

dx_clean <- dx_raw %>% str_replace_all("\\s+",".") %>% str_replace_all("NA\\.?|\\.NA","")
message("cleaned the vector")

# unique() drops duplicate dx codes
# NB: using str_split(), str_c(), etc. considerably slower than base R for this
dx_ord <- unlist(lapply(dx_clean,function(x) {
    paste(unique(unlist(strsplit(x,"\\."))),collapse = ".")
})) %>% na_if("")
message("created dx vector")

dx_srt <- unlist(lapply(dx_clean,function(x) {
    # have to use as.numeric() to get a numeric sequence instead of character (e.g. 888 before 9) from sort
    paste(sort(unique(as.numeric(unlist(strsplit(x,"\\."))))),collapse = ".")
})) %>% na_if("")
message("created sorted dx vector")

dx_df <- bind_cols(data.frame(dx_ord),data.frame(dx_srt)) %>% set_colnames(c("DX_ORD","DX_SRT"))

nis_set <- bind_cols(nis_set,dx_df)
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
nis_set <- nis_set %>% mutate(AYEAR = case_when(AMONTH == 12 & LOS > 31 ~ 2008, AMONTH == 12 & DQTR != 4 ~ 2008, TRUE ~ 2009))
label(nis_set$AYEAR) <- "Admission year"

# code of admission year and month
# make sure no NAs in AMONTH
# have to force integer type bc interpreting as double
nis_set <- nis_set %>% mutate_at("AMONTH", ~coalesce(.,as.integer(0)))
nis_set <- nis_set %>% mutate(AYRMO = if_else(AMONTH == 0, NA_real_, as.numeric((AYEAR * 100) + AMONTH)))
label(nis_set$AYRMO) <- "Code for admission year and month"

# first month of quarter in which patient died
nis_set <- nis_set %>% mutate(DIEDQTRSTARTMO = case_when(DIED == 1 & !is.na(DIED) ~ DQTR, DISPUB04 %in% c(20,40,41,42) & !is.na(DISPUB04) ~ DQTR))
nis_set$DIEDQTRSTARTMO <- mapvalues(nis_set$DIEDQTRSTARTMO,c(2,3,4),c(4,7,10))
# need 0 value for code of year and month of death
nis_set <- nis_set %>% mutate_at("DIEDQTRSTARTMO", ~coalesce(.,0))
label(nis_set$DIEDQTRSTARTMO) <- "First month of quarter in which patient died"

# code of died year and month
nis_set <- nis_set %>% mutate(DIEDYRMO = if_else(DIEDQTRSTARTMO == 0, NA_real_, as.numeric((YEAR * 100) + DIEDQTRSTARTMO)))
label(nis_set$DIEDYRMO) <- "Code for year and month of death (month = first month of quarter)"

# write NAs for AMONTH and DIEDQTRSTARTMO
nis_set <- nis_set %>% mutate_at("AMONTH",~na_if(.,0))
nis_set <- nis_set %>% mutate_at("DIEDQTRSTARTMO",~na_if(.,0))

# var indicating rural (or not) location of hospital
nis_set <- nis_set %>% mutate(HOSP_RURAL = if_else(HOSP_LOCTEACH == 1,1,0))
label(nis_set$HOSP_RURAL) <- "Hospital rural location indicator"

# var indicating teaching status of hospital
nis_set <- nis_set %>% mutate(HOSP_TEACH = if_else(HOSP_LOCTEACH == 3,1,0))
label(nis_set$HOSP_TEACH) <- "Hospital teaching status indicator"

rm(age_vals,maine_age_vals)
