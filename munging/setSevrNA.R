# Set coded missing, blank, and incn/invl to NA. PCRE is allegedly faster than default.
for (x in names(sevrset)) {
    sevrset[,x][grep("^$", sevrset[,x], perl = T)] <- NA
    missVals <- unlist(sevrMeta$naVals[[1]][x])
    if (!is.null(missVals)) { 
        sevrset[,x][sevrset[,x] %in% missVals] <- NA
    }
    rm(missVals, x)
}
