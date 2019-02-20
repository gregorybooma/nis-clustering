# Set coded missing, blank, and incn/invl to NA. PCRE is allegedly faster than default.
for (x in names(coreset)) {
    coreset[,x][grep("^$", coreset[,x], perl = T)] <- NA
    coreset[,x][grep("(incn|invl)", coreset[,x], perl = T)] <- NA
    missVals <- unlist(coreMeta$naVals[[1]][x])
    if (!is.null(missVals)) { 
        coreset[,x][coreset[,x] %in% missVals] <- NA 
    }
    rm(missVals, x)
}
