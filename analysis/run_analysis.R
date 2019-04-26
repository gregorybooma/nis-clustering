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
# source eda.R first.                                                                   #
#########################################################################################

# Maine first
# unneeded vars get in the way
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
  # for recoding with multiple values.
  idx_rows <- str_which(edame$dx_srt,paste0("(^|\\.)",dxnum,"{1}(\\.|$)"))
  edame[,varname][idx_rows] <- "1"
}

#edame$dx_srt <- NULL
#edame$nchronic <- NULL

# cluster female and male separately
edame_fem <- edame %>% filter(female == "1")

# first step: cluster by dx only, and add dx cluster labels as feature to female df
edame_fem_dxdf <- edame_fem %>% select(key,matches("^dx[0-9]+"))
# clean up female df
edame_fem <- edame_fem %>% select(-matches("^dx[0-9]+"))
row.names(edame_fem_dxdf) <- edame_fem_dxdf$key
edame_fem_dxdf <- edame_fem_dxdf %>% select(-key)

edame_fem_dxmat <- as.dummy(edame_fem_dxdf)

# theta of 0.99392 gets best goodness numbers (best fit)
dx_clust <- rockCluster(edame_fem_dxmat,n=nrow(edame_fem_dxdf)*0.5,theta = 0.99392,debug = TRUE)
dx_clust_fit <- fitted(dx_clust)

edame_fem$cl <- dx_clust_fit$cl
edame_fem$dxclust <- paste0("dxf",edame_fem$cl)

edame_fem <- edame_fem %>% mutate_at("dxclust",~na_if(.,"dxfNA")) %>% select(-cl)
edame_fem <- edame_fem %>% mutate_at("dxclust",~coalesce(dxclust,as.character(key)))

# next step: cluster female df using all features
me_fem_to_clust <- edame_fem %>% select(key,age,nchronic,pay1,pl_nchs2006,race,zipinc_qrtl,dxclust)
row.names(me_fem_to_clust) <- me_fem_to_clust$key
me_fem_to_clust <- me_fem_to_clust %>% select(-key) %>% mutate(dxclust = as.factor(dxclust))

me_fem_to_clust_mat <- as.dummy(me_fem_to_clust)

me_fem_clust <- rockCluster(me_fem_to_clust_mat,n=nrow(me_fem_to_clust)*0.5,theta = 0.77,debug = T)
me_fem_clust_fit <- fitted(me_fem_clust)

edame_fem$cl <- me_fem_clust_fit$cl
edame_fem$clustid <- paste0("MEF",edame_fem$cl)

edame_fem$clustid[edame_fem$clustid=="MEFNA"] <- NA
edame_fem$cl <- NULL
edame_fem$clustid[is.na(edame_fem$clustid)] <- edame_fem$key[is.na(edame_fem$clustid)]

# male dx only
edamemale <- filter(edame,female=="0")
edamemaledxdf <- select(edamemale,key,matches("^dx[0-9]+"))
edamemale <- edamemale[,1:9]
row.names(edamemaledxdf) <- edamemaledxdf$key
edamemaledxdf$key <- NULL
edamemaledxmat <- as.dummy(edamemaledxdf)
# theta of 0.99395 gets best goodness numbers (best fit)
dxclust <- rockCluster(edamemaledxmat,n=nrow(edamemaledxdf)*0.5,theta = 0.99395,debug = TRUE)
dx_clust_fit <- fitted(dxclust)
edamemale$cl <- dx_clust_fit$cl
edamemale$dxclust <- paste0("dxm",edamemale$cl)
edamemale$dxclust[edamemale$dxclust=="dxmNA"] <- NA
edamemale$cl <- NULL
edamemale$dxclust[is.na(edamemale$dxclust)] <- edamemale$key[is.na(edamemale$dxclust)]

# male all features
memaletoclustdf <- select(edamemale,key,age,nchronic,pay1,pl_nchs2006,race,zipinc_qrtl,dxclust)
row.names(memaletoclustdf) <- memaletoclustdf$key
memaletoclustdf$key <- NULL
memaletoclustdf$dxclust <- as.factor(memaletoclustdf$dxclust)
memaletoclustmat <- as.dummy(memaletoclustdf)
memaleclust <- rockCluster(memaletoclustmat,n=nrow(memaletoclustdf)*0.5,theta = 0.76,debug = T)
memaleclustfit <- fitted(memaleclust)
edamemale$cl <- memaleclustfit$cl
edamemale$clustid <- paste0("MEM",edamemale$cl)
edamemale$clustid[edamemale$clustid=="MEMNA"] <- NA
edamemale$cl <- NULL
edamemale$clustid[is.na(edamemale$clustid)] <- edamemale$key[is.na(edamemale$clustid)]

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
edame_fem$proxclustid[edame_fem$proxclustid=="PRMEFNA"] <- NA
edame_fem$prox <- NULL
edame_fem$proxclustid[is.na(edame_fem$proxclustid)] <- edame_fem$key[is.na(edame_fem$proxclustid)]

par(mfrow=c(1,2), pty="s")
lmplot(memaletoclustmat, main = "Data")
box()
# results same 0.1 to 0.01 radius so stopped
pr <- proximus(memaletoclustmat,max.radius = 0.01,debug = T)
lmplot(fitted(pr)$x, main = "Proxy")
box()
edamemale$prox <- fitted(pr)$pl
edamemale$proxclustid <- paste0("PRMEM",edamemale$prox)
edamemale$proxclustid[edamemale$proxclustid=="PRMEMNA"] <- NA
edamemale$prox <- NULL
edamemale$proxclustid[is.na(edamemale$proxclustid)] <- edamemale$key[is.na(edamemale$proxclustid)]

# combine male and female into one df
meclusters <- rbind(edame_fem,edamemale)

# now rock clustering all other geographies
# WARNING: very time consuming (days)
hrrdfs <- grep("^eda[a-z]{2}\\d{3}", ls(), value = T)
for (a in seq_along(hrrdfs)) {
    # input df name
    dfname <- hrrdfs[a]
    message(dfname)

    # have to use get() to get data, as dfname is just character
    dfraw <- get(dfname)
    # don't need hosp geog
    dfraw$hrrnum <- NULL
    dfraw$hrrstate <- NULL

    # make dx group of NA a level so observation will be retained
    levels(dfraw$dx_srt) <- c(levels(dfraw$dx_srt),"0")
    dfraw$dx_srt[is.na(dfraw$dx_srt)] <- "0"

    # create dummy cols for unique dx codes
    # WARNING: the for loop takes 24+ hours...
    dxvect <- paste(sort(unique(as.numeric(unlist(str_split(dfraw$dx_srt,"\\."))))))
    message(length(dxvect))

    # doing binary variable creation in a function makes computing time linear, as opposed
    # to nested for loops, which are exponential -- still a f*^&ing long time...
    makedxbin <- function(dxnum,invect) {
        outvect <- sapply(invect,function(x) {
            if (dxnum %in% unlist(str_split(x,"\\."))) {
                binval <- "1"
            } else {
                binval <- "0"
            }
            return(binval)
        })
        return(outvect)
    }
    for (i in seq_along(dxvect)) {
        dxnum <- dxvect[i]
        varname <- paste0("dx",dxnum)
        message(varname)
        dfraw[,varname] <- makedxbin(dxnum,dfraw$dx_srt)
    }
    dfraw$dx_srt <- NULL

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
        df$key <- NULL
        # have to make factors for as.dummy()
        dxnames <- grep("^dx[0-9]+",names(df),value = T)
        for (i in seq_along(dxnames)) {
            varname <- dxnames[i]
            df[,varname] <- as.factor(df[,varname])
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
