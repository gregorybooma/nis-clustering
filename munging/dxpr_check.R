dxpr_check <- function (df = NULL) {
    # HCUP removes the decimals from codes, so need to make sure no leading
    # whitespace that might indicate errors. Also checking how many valid, invalid,
    # and blank values there are for comparison to documentation. 

    require(stringr, quietly = T)
    
    # open a file connection to write a markdown table containing function output.
    dxpr_con <- file("dxpr_check.md", open = "w")
    
    # write out a header row to the markdown file.
    header <- paste("Variable", "Description", "Type", "Valid", "Prop. Valid", "Coded Missing/Inconsistent/Invalid", "Invalid Whitespace", "Blank", sep = " | ")
    writeLines(header, con = dxpr_con)
    
    # write out a row to the markdown file inidicating table column alignment.
    writeLines(":--------|:-----------|:----|:-----|:-----------|:-------------|:-----|:--------------", con = dxpr_con)

    # need value = T to pass variable name along.
    lapply(names(df), function(x) {
        message(x)
        N <- nrow(df)
        var_vect <- df[,x]
        description <- dxpr_meta$var_labs[x]
        var_type <- dxpr_meta$var_types[["words"]][x]
        valid <- length(grep("^[A-Z]?\\d+\\s*", var_vect))
        prp_valid <- round(valid / N, 4)
        # need perl = T for the negative lookahead; and CCS software writes "incn" and "invl" 
        # in addition to negative-filled values, so need to check
#        incInvl <- length(grep("^(?!\\s+)([a-z]+|^-\\d)", varVect, perl = T))
        incInvl <- length(grep("^(?!\\s+)(\\s*)([a-z]+|-\\d+)(\\s*)$", var_vect, perl = T))
        # irregular whitespace has been reported to be found in these variables, so checking...
        invlSp <- length(grep("^\\s*\\w+?\\s+\\w+$", var_vect))
        blank <- length(grep("^\\s+$",var_vect))
        
        var_info <- paste(x, description, var_type, valid, prp_valid, incInvl, invlSp, blank, sep = " | ")
        
        # print variable info. to the console for quick review.
        print(var_info)
        
        # write variable info. to the markdown file.
        writeLines(var_info, dxpr_con)
        
        # this speeds things up a bit.
        rm(list = c("var_vect", "valid", "incInvl", "invlSp", "blank", "var_info", "prp_valid", "N", "description", "var_type"))
        gc(verbose = F)
        
        })
    
    # close the markdown file connection.
    close(dxpr_con)
    message("Diagnostic and procedure group codes were checked.")
    }
