if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}

lapply(c("iotools", "Hmisc", "ggplot2","RColorBrewer","stringr","cba"), require, quietly = T, character.only = T) #,"vcd","vcdExtra","VIM"

# want to avoid dplyr masks of set operations and summarize
# if ("dplyr" %in% (.packages())) {
#     detach("package:dplyr", unload = T)
# }

options(digits = 15, stringsAsFactors = F, scipen = 999)

#########################################################################################
# WARNING: Code below relies on objects created in the EDA step.  If starting fresh,    #
# source run_eda.R first.                                                                   #
#########################################################################################

# Maine first
# drop unneeded vars
edame <- edame %>% select(-c(hrrnum,hrrstate))

# create binary vars for each dx.
# NA indicates no chronic disease (as opposed to missing value), so recoding to be
# more clear -- using 0 as level, since unused as a dx code.
# Base R indexing is much faster for this.
levels(edame$dx_srt) <- c(levels(edame$dx_srt),"0")
edame$dx_srt[is.na(edame$dx_srt)] <- "0"
# split each dx group to get all unique diagnosis codes
dxvect <- paste(sort(unique(as.numeric(unlist(str_split(edame$dx_srt,"\\."))))))
message(length(dxvect))

# Set binary vars for each dx
# This version is much faster (seconds v. hour-plus)!
for (i in seq_along(dxvect)) {
  dxnum <- dxvect[i]
  varname <- paste0("dx",dxnum)
  edame <- edame %>% mutate(!!varname := factor(rep.int("0",nrow(edame)),levels = c("0","1")))
  #edame <- edame %>% mutate(!!varname := case_when(str_detect(dx_srt,paste0("(^|\\.)",dxnum,"{1}(\\.|$)")) ~ "1", TRUE ~ "0"))

  # This is a bit faster and more readable. Leaving the above mutate() with case_when() as example
  # of recoding with multiple values.
  idx_rows <- str_which(edame$dx_srt,paste0("(^|\\.)",dxnum,"{1}(\\.|$)"))
  edame[,varname][idx_rows] <- "1"
}

#edame$dx_srt <- NULL
#edame$nchronic <- NULL

# cluster maine female and male separately
edame_fem <- edame %>% filter(female == "1")

# first step: cluster by dx only, and add dx cluster labels as feature to female df
edame_fem_dxdf <- edame_fem %>% select(key,matches("^dx[0-9]+"))
# clean up female df
edame_fem <- edame_fem %>% select(-matches("^dx[0-9]+"))
row.names(edame_fem_dxdf) <- edame_fem_dxdf$key
edame_fem_dxdf <- edame_fem_dxdf %>% select(-key)

edame_fem_dxmat <- as.dummy(edame_fem_dxdf)

# theta of 0.99392 gets best goodness numbers
dx_clust <- rockCluster(edame_fem_dxmat,n=nrow(edame_fem_dxdf)*0.5,theta = 0.99392,debug = TRUE)
dx_clust_fit <- fitted(dx_clust)

edame_fem$cl <- dx_clust_fit$cl
edame_fem$dxclust <- paste0("dxf",edame_fem$cl)

edame_fem <- edame_fem %>% mutate_at("dxclust",~na_if(.,"dxfNA")) %>% select(-cl)
edame_fem <- edame_fem %>% mutate_at("dxclust",~coalesce(dxclust,as.character(key)))

# next step: cluster maine female df using all features
# note that diagnosis cluster included
me_fem_to_clust <- edame_fem %>% select(key,age,nchronic,pay1,pl_nchs2006,race,zipinc_qrtl,dxclust)
row.names(me_fem_to_clust) <- me_fem_to_clust$key
me_fem_to_clust <- me_fem_to_clust %>% select(-key) %>% mutate(dxclust = as.factor(dxclust))

me_fem_to_clust_mat <- as.dummy(me_fem_to_clust)

me_fem_clust <- rockCluster(me_fem_to_clust_mat,n=nrow(me_fem_to_clust)*0.5,theta = 0.77,debug = T)
me_fem_clust_fit <- fitted(me_fem_clust)

edame_fem$cl <- me_fem_clust_fit$cl
edame_fem$clustid <- paste0("MEF",edame_fem$cl)

edame_fem <- edame_fem %>% mutate_at("clustid",~na_if(.,"MEFNA")) %>% select(-cl)
edame_fem <- edame_fem %>% mutate_at("clustid",~coalesce(clustid,as.character(key)))

# maine male dx only
edame_male <- edame %>% filter(female=="0")
edame_male_dxdf <- edame_male %>% select(key,matches("^dx[0-9]+"))
edame_male <- edame_male %>% select(-matches("^dx[0-9]+"))
row.names(edame_male_dxdf) <- edame_male_dxdf$key
edame_male_dxdf <- edame_male_dxdf %>% select(-key)

edame_male_dxmat <- as.dummy(edame_male_dxdf)

# theta of 0.99395 gets best goodness numbers
dxclust <- rockCluster(edame_male_dxmat,n=nrow(edame_male_dxdf)*0.5,theta = 0.99395,debug = TRUE)
dx_clust_fit <- fitted(dxclust)

edame_male$cl <- dx_clust_fit$cl
edame_male$dxclust <- paste0("dxm",edame_male$cl)

edame_male <- edame_male %>% mutate_at("dxclust",~na_if(.,"dxmNA")) %>% select(-cl)
edame_male <- edame_male %>% mutate_at("dxclust",~coalesce(dxclust,as.character(key)))

# maine male all features
# note that diagnosis cluster included
me_male_to_clust <- select(edame_male,key,age,nchronic,pay1,pl_nchs2006,race,zipinc_qrtl,dxclust)
row.names(me_male_to_clust) <- me_male_to_clust$key
me_male_to_clust <- me_male_to_clust %>% select(-key) %>% mutate(dxclust = as.factor(dxclust))

me_male_to_clust_mat <- as.dummy(me_male_to_clust)

me_male_clust <- rockCluster(me_male_to_clust_mat,n=nrow(me_male_to_clust)*0.5,theta = 0.76,debug = T)
me_male_clust_fit <- fitted(me_male_clust)

edame_male$cl <- me_male_clust_fit$cl
edame_male$clustid <- paste0("MEM",edame_male$cl)

edame_male <- edame_male %>% mutate_at("clustid",~na_if(.,"MEMNA")) %>% select(-cl)
edame_male <- edame_male %>% mutate_at("clustid",~coalesce(clustid,as.character(key)))

# and look at proximus clustering
par(mfrow=c(1,2), pty="s")
lmplot(me_fem_to_clust_mat, main = "Data")
box()
# results same 0.1 to 0.01 radius so stopped
pr <- proximus(me_fem_to_clust_mat,max.radius = 0.01,debug = T)
lmplot(fitted(pr)$x, main = "Proxy")
box()
edame_fem$prox <- fitted(pr)$pl
edame_fem$proxclustid <- paste0("PRMEF",edame_fem$prox)

edame_fem <- edame_fem %>% mutate_at("proxclustid",~na_if(.,"PRMEFNA")) %>% select(-prox)
edame_fem <- edame_fem %>% mutate_at("proxclustid",~coalesce(proxclustid,as.character(key)))

par(mfrow=c(1,2), pty="s")
lmplot(me_male_to_clust_mat, main = "Data")
box()
# results same 0.1 to 0.01 radius so stopped
pr <- proximus(me_male_to_clust_mat,max.radius = 0.01,debug = T)
lmplot(fitted(pr)$x, main = "Proxy")
box()
edame_male$prox <- fitted(pr)$pl
edame_male$proxclustid <- paste0("PRMEM",edame_male$prox)

edame_male <- edame_male %>% mutate_at("proxclustid",~na_if(.,"PRMEFNA")) %>% select(-prox)
edame_male <- edame_male %>% mutate_at("proxclustid",~coalesce(proxclustid,as.character(key)))

# combine maine male and female into one df
me_clusters <- rbind(edame_fem,edame_male)

# now rock clustering all other geographies
# WARNING: very time consuming (days)
# Future: more power and parallel processing
hrrdfs <- grep("^eda[a-z]{2}\\d{3}", ls(), value = T)
for (a in seq_along(hrrdfs)) {
    # input df name
    dfname <- hrrdfs[a]
    message(dfname)

    # have to use get() to get data, as dfname is just character
    dfraw <- get(dfname)
    # don't need hosp geog
    dfraw <- dfraw %>% select(-c(hrrnum,hrrstate))

    # make dx group of NA a level so observation will be retained
    levels(dfraw$dx_srt) <- c(levels(dfraw$dx_srt),"0")
    dfraw$dx_srt[is.na(dfraw$dx_srt)] <- "0"

    # create dummy cols for unique dx codes
    dxvect <- paste(sort(unique(as.numeric(unlist(str_split(dfraw$dx_srt,"\\."))))))
    message(length(dxvect))
    # this should be much faster!
    for (i in seq_along(dxvect)) {
      dxnum <- dxvect[i]
      varname <- paste0("dx",dxnum)
      dfraw <- dfraw %>% mutate(!!varname := factor(rep.int("0",nrow(dfraw)),levels = c("0","1")))
      #dfraw <- dfraw %>% mutate(!!varname := case_when(str_detect(dx_srt,paste0("(^|\\.)",dxnum,"{1}(\\.|$)")) ~ "1", TRUE ~ "0"))
      
      # This is a bit faster and more readable. Leaving the above mutate() with case_when() as example
      # of recoding with multiple values.
      idx_rows <- str_which(dfraw$dx_srt,paste0("(^|\\.)",dxnum,"{1}(\\.|$)"))
      dfraw[,varname][idx_rows] <- "1"
    }

    # need environment for assigning objects with assign()
    thisenvir <- environment()
    # split by gender to make dimensionality more manageable
    lapply(c("female","male"), function(x) {
        if (x == "female") {
            assign(x, filter(dfraw[c(1:length(dfraw))], female == 1))
        } else {
            assign(x, filter(dfraw[c(1:length(dfraw))], female == 0))
        }
        df <- get(x)
        row.names(df) <- df$key
        df <- df %>% select(-key)
        # have to make factors for as.dummy()
        dxnames <- grep("^dx[0-9]+",names(df),value = T)
        for (i in seq_along(dxnames)) {
            varname <- dxnames[i]
            #df[,varname] <- as.factor(df[,varname])
            df <- df %>% mutate(varname = as.factor(varname))
        }
        inmat <- as.dummy(df)
        # predicting using a 50% sample
        # WARNING: This can take hours for a large number of observations...
        #rclust <- rockCluster(inmat[sample(dim(inmat)[1],ceiling(dim(inmat)[1] * 0.5)),], n = 10, theta = 0.995, debug = T)
        #rclustpred <- predict(rclust, inmat)
        # don't need to sample, but can still take hours...
        rclust <- rockCluster(inmat, n = 10, theta = 0.993, debug = T)
        rclustfit <- fitted(rclust)
        # rowid to use as a check that cluster vals are linked in order
        df$rowid <- paste0(x,row.names(df))
        #df$clustid <- rclustpred$cl
        df$clustid <- rclustfit$cl
        assign(paste0(x,"clust"),df)
    })

    # put the clustered df back together
    assign(paste0(dfname,"clusters"), rbind(femaleclust,maleclust))

}
