# Set coded missing, blank, and incn/invl to NA. PCRE is allegedly faster than default.
for (x in names(hospset)) {
    hospset[,x][grep("^$", hospset[,x], perl = T)] <- NA
    missVals <- unlist(hospMeta$naVals[[1]][x])
    if (!is.null(missVals)) { 
        hospset[,x][hospset[,x] %in% missVals] <- NA
    }
    rm(missVals, x)
}
