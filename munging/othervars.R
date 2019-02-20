if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999999999)

# need to identify record-specific groups of diagnoses, so start with a
# df of diagnoses
dxdfin <- select(nisSet,matches("^DX[0-5]{1}$"))

# want to truncate to 3-digit dx codes to avoid variations in physician diagnosis, and
# reduce the number of levels
for(i in seq_along(dxdfin)) {
    indcol <- names(dxdfin)[i]
    #message(indcol)
    dxdfin[,indcol] <- str_sub(dxdfin[,indcol],1,3)
    rm(indcol)
}

# now form the groups and write vars to the merged set...
# N.B.: this does not work with row-wise apply(), so using do.call()
dxraw <- do.call(paste,dxdfin)
message("created raw vector of concatenated dx vals")

dxclean <- gsub("NA\\.?|\\.NA","",gsub("\\s+",".",dxraw))
message("cleaned the vector")

dxord <- unlist(lapply(dxclean,function(x) {
    paste(unique(unlist(strsplit(x,"\\."))),collapse = ".")
}))
dxord[which(dxord == "")] <- NA
message("created dx order vector")

dxsrt <- unlist(lapply(dxclean,function(x) {
    # have to use as.numeric() to get a numeric sequence instead of character (e.g. 888 before 9) from sort
    paste(sort(unique(as.numeric(unlist(strsplit(x,"\\."))))),collapse = ".")
}))
dxsrt[which(dxsrt == "")] <- NA
message("created sorted dx vector")

dxdf <- data.frame(cbind(dxord,dxsrt))
names(dxdf) <- c("DX_ORD","DX_SRT")

nisSet <- cbind(nisSet,dxdf)
label(nisSet$DX_ORD) <- "Unique grouped diagnosis, in order of diagnosis"
label(nisSet$DX_SRT) <- "Unique grouped diagnosis, sorted"

rm(dxdfin,dxraw,dxclean,dxord,dxsrt,dxdf)
message("Finished dx vars")

# alternate ages, based-upon Maine's assignment of age (midpoint of 5-year bins
# after age 4; 0 is 0, and 1-4 is 2; max age is 110)
agevals <- c(0:110)
maineagevals <- c(0, rep.int(2,4), rep.int(7,5), rep.int(12,5), rep.int(17,5), rep.int(22,5), rep.int(27,5), rep.int(32,5), rep.int(37,5), rep.int(42,5), rep.int(47,5), rep.int(52,5), rep.int(57,5), rep.int(62,5), rep.int(67,5), rep.int(72,5), rep.int(77,5), rep.int(82,5), rep.int(87,26))
nisSet$MAINEAGE <- as.integer(mapvalues(nisSet$AGE, agevals, maineagevals))
label(nisSet$MAINEAGE) <- "Age integer value using Maine age assignments"

# admission year -- YEAR corresponds to discharge year
nisSet$AYEAR <- 2009
nisSet$AYEAR[nisSet$AMONTH == 12 & nisSet$LOS > 31] <- 2008
nisSet$AYEAR[nisSet$AMONTH == 12 & nisSet$DQTR !=4] <- 2008
label(nisSet$AYEAR) <- "Admission year"

# code of admission year and month
# make sure no NAs in AMONTH
nisSet$AMONTH[is.na(nisSet$AMONTH)] <- 0
nisSet$AYRMO <- (nisSet$AYEAR * 100) + nisSet$AMONTH
label(nisSet$AYRMO) <- "Code for admission year and month"
nisSet$AYRMO[nisSet$AMONTH == 0] <- NA

# first month of quarter in which patient died
# can't initialize with NA, so using 0 at first
nisSet$DIEDQTRSTARTMO <- 0
nisSet$DIEDQTRSTARTMO[nisSet$DIED[!is.na(nisSet$DIED)] == 1] <- nisSet$DQTR[nisSet$DIED[!is.na(nisSet$DIED)] == 1]
nisSet$DIEDQTRSTARTMO[nisSet$DISPUB04[!is.na(nisSet$DISPUB04)] %in% c(20,40,41,42)] <- nisSet$DQTR[nisSet$DISPUB04[!is.na(nisSet$DISPUB04)] %in% c(20,40,41,42)]
nisSet$DIEDQTRSTARTMO <- mapvalues(nisSet$DIEDQTRSTARTMO,c(2,3,4),c(4,7,10))
label(nisSet$DIEDQTRSTARTMO) <- "First month of quarter in which patient died"
# need 0 value for code of year and month of death, so holding-off on setting NAs

# code of died year and month
nisSet$DIEDYRMO <- 0
nisSet$DIEDYRMO <- (nisSet$YEAR * 100) + nisSet$DIEDQTRSTARTMO
nisSet$DIEDYRMO[nisSet$DIEDYRMO == 200900] <- NA
label(nisSet$DIEDYRMO) <- "Code for year and month of death (month = first month of quarter)"

# write NAs for AMONTH and DIEDQTRSTARTMO
nisSet$AMONTH[nisSet$AMONTH == 0] <- NA
nisSet$DIEDQTRSTARTMO[nisSet$DIEDQTRSTARTMO == 0] <- NA

# var indicating rural (or not) location of hospital
nisSet$HOSP_RURAL <- as.integer(rep.int(0,nrow(nisSet)))
nisSet$HOSP_RURAL[nisSet$HOSP_LOCTEACH == 1] <- 1
label(nisSet$HOSP_RURAL) <- "Hospital rural location indicator"

# var indicating teaching status of hospital
nisSet$HOSP_TEACH <- as.integer(rep.int(0,nrow(nisSet)))
nisSet$HOSP_TEACH[nisSet$HOSP_LOCTEACH == 3] <- 1
label(nisSet$HOSP_TEACH) <- "Hospital teaching status indicator"

rm(agevals,maineagevals)
