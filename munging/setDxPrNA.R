# Set coded missing, blank, and incn/invl to NA. PCRE is allegedly faster than default.
for (x in names(dxprset)) {
    dxprset[,x][grep("^$", dxprset[,x], perl = T)] <- NA
    missVals <- unlist(dxprMeta$naVals[[1]][x])
    if (!is.null(missVals)) { 
        dxprset[,x][dxprset[,x] %in% missVals] <- NA
    }
    rm(missVals, x)
}
