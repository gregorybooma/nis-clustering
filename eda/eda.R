if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr","ggplot2","RColorBrewer","vcd","vcdExtra","VIM"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999999999)

#########################################################################################
# WARNING: Code below relies on objects created in the munging step.  If starting fresh,#
# source run_processing.R first.                                                        #
#########################################################################################

# Generate a codebook with descriptive statistics and data definitions for the dataset and write
# to a file (outputs in *.tex format) -- requires Hmisc, LaTeX with setspace and relsize styles
# installed (on a Mac, MacTeX 2015 includes). N.B.: to save as pdf, either use file -> 'save as'
# in the X-window launched from the latex command, or edit the output *.tex file to include
# proper document start/end markup before sending to knitr. For some reason the engine called
# from the latex command does not do so.
names(nisSet) <- str_to_upper(names(nisSet))
cb <- describe(nisSet, descript = "Codebook: Identifying NIS Readmissions")
latex(cb, file = "codebook.tex")
rm(cb)

# avoid the caps lock and shift keys
names(nisSet) <- str_to_lower(names(nisSet))

# I want to look at the distributions of AGE and MAINEAGE, becuase different states handle coding
# of age differently for children, and special cases (i.e. rare diagnoses); and Maine bins ages
# for privacy protection.

# note the max age and count for binning
sort(unique(nisSet$age))

ggplot(nisSet[!is.na(nisSet$age),],aes(x=age)) + geom_histogram(binwidth = 0.5, bins = 222) +
    scale_x_continuous(breaks = seq(0,110,5))

# very high number of zeros. maybe newborn deliveries, which are not needed for this project. I
# think the newborn ATYPE (4) is for routine deliveries, which the documentation seems
# to indicate, but want to confirm with data
aa <- select(nisSet[!is.na(nisSet$age) | !is.na(nisSet$atype),], age = age, atype = atype)
unique(aa$atype)
# still some NAs, so drop them...
aa <- aa[!is.na(aa$atype) & !is.na(aa$age),]
# and some visualization...
aa$atypef <- cut(aa$atype,breaks = 5)
ggatype <- ggplot(aa,aes(x=atypef))
ggatype + geom_jitter(aes(y=age),width = 0.2, alpha = 0.5) +
    geom_boxplot(aes(y=age),width = 0.4, color = "blue", fill = NA, outlier.colour = "red") +
    scale_x_discrete(labels=c("Emerg.","Urg.","Elect.","Newbn.","Traum.")) + coord_flip()
# almost all zeros in ATYPE for newborn, with any over zero marked as outliers. one outlier
# is very near zero, and all others over age 15. assuming the one near zero is a complicated
# delivery, and all others are coding errors. so I think it's safe to drop ATYPE 4, and take
# a look at distribution of ages again...
rm(aa, ggatype)
ggplot(select(nisSet[!is.na(nisSet$age) & !is.na(nisSet$atype) & nisSet$atype != 4,],age,atype),aes(x=age)) +
    geom_histogram(binwidth = 0.5, bins = 222) +
    scale_x_continuous(breaks = seq(0,110,5))
# better, but still more zeros than I expected. the documentation for ATYPE indicates that
# states in subset may or may not report newborns in ATYPE, so I can't necessarily trust that
# all have been removed. the printed documentation that came with the dataset specifically
# describes the binning of ages in Maine, making no mention of other subset states; but the
# state notes in the online documentation for AGE do indicate special situations (unusual and/or
# controversial diagnoses) where age is re-coded or set to missing -- but not for newborn deliveries.
# So, I need to remove rows with a diagnosis code of newborn delivery, and gynecological diagnoses
# that are random and not so uncommon (e.g. unusual position of fetus, etc.). To be conservative,
# I will only drop rows with principal diagnosis of these conditions.  Since I am not using
# disaggregated diagnoses, I have to temporarily add the original data back to the dataset.
setdfraw <- read.csv.raw("nis-subset-large.csv", sep = "|",nrowsClasses = 10000)
setdf <- setdfraw[order(setdfraw$KEY),]
glimpse(setdf)
names(setdf) <- str_to_lower(names(setdf))
rm(setdfraw)

# also take ccs1 to more easily subset delivery-related
dxdf <- select(setdf,key,age,dx1temp = dx1,dxccs1)

# newborn deliveries
v3x <- dxdf[grep("V3[0-9]+",dxdf$dx1temp),]
# first make sure all age 0
unique(v3x$age) # 0 and NA
nrow(v3x) # 121005
rm(v3x)
dxdf <- select(dxdf,-age)

nrow(dxdf)
nrow(nisSet)
newdf <- merge(nisSet, dxdf, by = "key", all.x = T, sort = T)
glimpse(newdf)
rm(setdf)
rm(dxdf)
# N.B.: I previously was using row indexing to subset instead of filter() for the next step, but
# the df resulting from the second iteration of dropping rows actually added rows of NA values
# back to the df. I had never encountered this before, but it seemed like R was using cached row
# indices. I caught the problem because summary statistics in the codebook seemed inconsistent
# with what I knew about the data. Using filter() resolved the issue. Lesson learned: be very
# careful if over-writing objects with the same name.
newdf1 <- filter(newdf, !grepl("V3[0-9]+",dx1temp))
# also need to drop rows where dx is delivery-related
newdf2 <- filter(newdf1, !(dxccs1 %in% c(178,180,181,182,184,185,187,188,189,190,191,192,193,194,195,196)))

# make sure all ok, and remove old nisSet
glimpse(newdf2)
head(newdf2)
tail(newdf2)
str(newdf2)
rm(nisSet)

# ...and atype 4, in case any rows were missed -- creating new nisSet
nisSet <- filter(newdf2, atype != 4)
rm(newdf,newdf1,newdf2)
nisSet$dx1temp <- NULL
nisSet$dxccs1 <- NULL
nisSet$atype <- NULL

# and look at age again
ggplot(select(nisSet[!is.na(nisSet$age),],age),aes(x=age)) +
    geom_histogram(binwidth = 0.5, bins = 222) +
    scale_x_continuous(breaks = seq(0,110,5))
# well, on the bright side, I won't have to do != newborn from here on...

# wondering about age distribution without Maine...
ggplot(select(nisSet[!is.na(nisSet$age) & !is.na(nisSet$hospst) & nisSet$hospst != "ME",],age,hospst),aes(x=age)) +
    geom_histogram(binwidth = 0.5, bins = 222) +
    scale_x_continuous(breaks = seq(0,110,5))
# not much different

# ...and using Maine's age bins
ggplot(select(nisSet[!is.na(nisSet$maineage),],maineage), aes(x=maineage)) +
    geom_histogram(binwidth = 0.5, bins = 222) +
    scale_x_continuous(breaks = seq(0,110,5))
# there really doesn't seem to be any case to be made for not matching Maine on its own.
nisSet$maineage <- NULL

# documentation seems to indicate that discwt is hospital-specific -- if so, it can be dropped
# because hospitalid is a sufficient (and more intuitive) categorical var to represent hospital.
# invisible() supresses echo of the object returned -- it's redundant since I am printing
invisible(lapply(unique(nisSet$hospid), function(x) {
    print(paste(x,":",length(unique(nisSet$discwt[nisSet$hospid == x]))))
}))
# only one weight per hospital
nisSet$discwt <- NULL

# write the reduced dataset to a file
# a bug in iotools 0-1.12 requires writing headers to file and then appending. resolved in subsequent versions.
cat(noquote(paste0(paste0(names(nisSet),collapse = "|"),"\n")),file = "nis-eda.csv")
write.csv.raw(nisSet, "nis-eda.csv", sep = "|", append=TRUE)

# re-create the codebook
names(nisSet) <- str_to_upper(names(nisSet))
cb <- describe(nisSet, descript = "Codebook: Identifying NIS Readmissions")
latex(cb, file = "codebook.tex")
names(nisSet) <- str_to_lower(names(nisSet))
rm(cb)

# Now some more in-depth EDA of the variables. Only need vars that will be used in clustering,
# and subsequent validation.
eda <- select(nisSet,key,age,female,nchronic,pay1,pl_nchs2006,race,zipinc_qrtl,dx_srt,hrrnum,hrrstate)
# decided to convert age to factor, given that states bin, and reassign ages under special
# circumstances.
eda$age <- factor(eda$age, labels = sort(unique(eda$age)), ordered = T)
label(eda$age) <- "Age in years at admission"

# subset data by clustering region and age
# cls is only interested in potential pedi population
#eda <- filter(eda, age < 22) #eda[eda$age < 22,]

# drop unused levels in age
edame <- eda %>% filter(hrrstate=="ME" & age < 22) %>% droplevels(except = c(1,3,4,5,6,7,8,9,10,11))

# need to get environment for assigning objects inside of loops
thisenvir <- environment()

invisible(lapply(unique(eda$hrrnum[eda$hrrstate!="ME"]), function(x) {
    # hrrnum is all na for maine so need to avoid
    state <- str_to_lower(unique(eda$hrrstate[!is.na(eda$hrrnum) & eda$hrrnum==x]))
    hrrnum <- str_to_lower(unique(eda$hrrnum[!is.na(eda$hrrnum) & eda$hrrnum==x]))
    fname <- paste0("eda",state,hrrnum)
    print(fname)
    assign(fname,filter(eda,hrrnum==x),envir = thisenvir)
}))

# looking at pairs of vars -- maine is the most constrained in terms of available data, binning,
# etc., so should show patterns in data in a more pronounced way.
combos <- combn(names(edame[-c(1,length(edame),length(edame)-1)]),2)
apply(combos,2,function(i){
    print(i[1])
    if (str_sub(i[1],1,2) != "dx" & str_sub(i[2],1,2) != "dx") {
        tname <- paste0("tedame",i[1],i[2])
        print(tname)
        invisible(assign(tname,table(edame[,i[1]],edame[,i[2]],useNA = "no",dnn = c(i[1],i[2])), envir = thisenvir))
        mosaic(get(tname), gp = shading_Friendly, main = tname)
    }
})

# non-maine: subset by hrr too time-consuming, so running over all non-maine
# drop unused levels in age
edanome <- eda %>% filter(hrrstate!="ME" & age < 22) %>% droplevels(except = c(1,3,4,5,6,7,8,9,10,11))
combos <- combn(names(edanome[-c(1,length(edanome),length(edanome)-1)]),2)
apply(combos,2,function(i){
    print(i[1])
    if (str_sub(i[1],1,2) != "dx" & str_sub(i[2],1,2) != "dx") {
        tname <- paste0("tedanome",i[1],i[2])
        print(tname)
        invisible(assign(tname,table(edanome[,i[1]],edanome[,i[2]],useNA = "no",dnn = c(i[1],i[2])), envir = thisenvir))
        mosaic(get(tname), gp = shading_Friendly, main = tname)
        # bc NIS is stratified, not surprising to see correlations, but most are intuitive (e.g. chronic
        # disease increases with age, etc.)
    }
})

# backup data to files
# a bug in iotools 0-1.12 requires writing headers to file and then appending. resolved in subsequent versions.
cat(noquote(paste0(paste0(names(edame),collapse = "|"),"\n")),file = "nis-eda-maine.csv")
write.csv.raw(edame, "nis-eda-maine.csv", sep = "|", append=TRUE)
cat(noquote(paste0(paste0(names(edanome),collapse = "|"),"\n")),file = "nis-eda-no-maine.csv")
write.csv.raw(edanome, "nis-eda-no-maine.csv", sep = "|", append=TRUE)

