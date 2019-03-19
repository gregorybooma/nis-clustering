# Set coded missing, blank, and incn/invl to NA. PCRE is allegedly faster than default.
# Could do this using multiple calls to na_if in dplyr, but this is more efficient: As far
# as I can tell, na_if doesn't support multiple values or expressions.
for (x in names(hosp_set)) {
    hosp_set[,x][grep("^$", hosp_set[,x], perl = T)] <- NA
    miss_vals <- unlist(hosp_meta$na_vals[[1]][x])
    if (!is.null(miss_vals)) { 
        hosp_set[,x][hosp_set[,x] %in% miss_vals] <- NA
    }
    rm(miss_vals, x)
}
