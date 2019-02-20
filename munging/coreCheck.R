coreCheck <- function (df = NULL) {

    require(stringr, quietly = T)

    # open a file connection to write a markdown table containing function output.
    coreCon <- file("coreCheck.md", open = "w")

    # write out a header row to the markdown file.
    header <- paste("Variable", "Description", "Type", "Valid", "Prop. Valid", "Coded Missing/Inconsistent/Invalid", "Blank", sep = " | ")
    writeLines(header, con = coreCon)

    # write out a row to the markdown file inidicating table column alignment.
    writeLines(":--------|:-----------|:----|:-----|:-------------|:--------------------------------|:-----", con = coreCon)

    # need perl = T for the negative lookahead, and value = T to pass variable name along.
    sapply(grep("^(?!(DX.+|(PR[0-9]+|PRC.+)|(EC.+|E_C.+)))", names(df), perl = T, value = T), function(x) {
        N <- nrow(df)
        varVect <- df[,x]
        description <- coreMeta$varLabs[x]
        varType <- coreMeta$varTypes[["words"]][x]
        missVals <- paste(unlist(coreMeta$naVals[[2]][x]), collapse = "|")
        grepBeg <- c("^(?!\\s+)(")
        grepEnd <- c(")")
        grepPat <- paste0(grepBeg, missVals, grepEnd)
        # need perl = T for the negative lookahead.
        incInvl <- ifelse(missVals == "", 0, length(grep(grepPat, varVect, perl = T)))
        blank <- length(grep("^\\s+$",varVect))
        valid <- N - incInvl - blank
        prpValid <- round(valid / N, 4)
        
        #        varInfo <- paste(x, valid, leadWhite, blank, incInv, sep = " | ")
        varInfo <- paste(x, description, varType, valid, prpValid, incInvl, blank, sep = " | ")
        
        # print variable info. to the console for quick review.
        print(varInfo)
        
        # write variable info. to the markdown file.
        writeLines(varInfo, coreCon)
        
        # this speeds things up a bit.
        rm(list = c("varVect", "description", "varType", "valid", "prpValid", "incInvl", "blank", "varInfo", "missVals", "grepBeg", "grepEnd", "grepPat"))
        gc(verbose = F)

        }, USE.NAMES = F)

    # close the markdown file connection.
    close(coreCon)
    message("Core variables other than diagnosis, procedure, and external cause codes have been checked.")
    }
