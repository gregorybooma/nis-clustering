dxEcPrCheck <- function (df = NULL) {
    # HCUP removes the decimals from codes, so need to make sure no leading
    # whitespace that might indicate errors. Also checking how many valid, invalid,
    # and blank values there are for comparison to documentation. 

    require(stringr, quietly = T)
    
    # open a file connection to write a markdown table containing function output.
    dxEcPrCon <- file("dxEcPrCheck.md", open = "w")
    
    # write out a header row to the markdown file.
    header <- paste("Variable", "Description", "Type", "Valid", "Prop. Valid", "Coded Missing/Inconsistent/Invalid", "Invalid Whitespace", "Blank", sep = " | ")
    writeLines(header, con = dxEcPrCon)
    
    # write out a row to the markdown file inidicating table column alignment.
    writeLines(":--------|:-----------|:----|:-----|:-----------|:-------------|:-----|:--------------", con = dxEcPrCon)
    
    # need value = T to pass variable name along.
    sapply(grep("^DX.+|^(PR[0-9]+|PRC.+)|^E(C|_)\\w+", names(df), value = T), function(x) {
        message(x)
        N <- nrow(df)
        varVect <- df[,x]
        description <- coreMeta$varLabs[x]
        varType <- coreMeta$varTypes[["words"]][x]
        valid <- length(grep("^[A-Z]?\\d+\\s*", varVect))
        prpValid <- round(valid / N, 4)
        # need perl = T for the negative lookahead; and CCS software writes "incn" and "invl" 
        # in addition to negative-filled values, so need to check
#        incInvl <- length(grep("^(?!\\s+)([a-z]+|^-\\d)", varVect, perl = T))
        incInvl <- length(grep("^(?!\\s+)(\\s*)([a-z]+|-\\d+)(\\s*)$", varVect, perl = T))
        # irregular whitespace has been reported to be found in these variables, so checking...
        invlSp <- length(grep("^\\s*\\w+?\\s+\\w+$", varVect))
        blank <- length(grep("^\\s+$",varVect))
        
        varInfo <- paste(x, description, varType, valid, prpValid, incInvl, invlSp, blank, sep = " | ")
        
        # print variable info. to the console for quick review.
        print(varInfo)
        
        # write variable info. to the markdown file.
        writeLines(varInfo, dxEcPrCon)
        
        # this speeds things up a bit.
#        rm(list = c("varVect", "valid", "incInv", "leadWhite", "blank"))
        rm(list = c("varVect", "valid", "incInvl", "invlSp", "blank", "varInfo", "prpValid", "N", "description", "varType"))
        gc(verbose = F)
        
        }, USE.NAMES = F)
    
    # close the markdown file connection.
    close(dxEcPrCon)
    message("Diagnostic and procedure codes were checked.")
    }
