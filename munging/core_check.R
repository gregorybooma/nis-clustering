core_check <- function (df = NULL) {

    require(stringr, quietly = T)

    # open a file connection to write a markdown table containing function output.
    core_con <- file("core_check.md", open = "w")

    # write out a header row to the markdown file.
    header <- paste("Variable", "Description", "Type", "Valid", "Prop. Valid", "Coded Missing/Inconsistent/Invalid", "Blank", sep = " | ")
    writeLines(header, con = core_con)

    # write out a row to the markdown file inidicating table column alignment.
    writeLines(":--------|:-----------|:----|:-----|:-------------|:--------------------------------|:-----", con = core_con)

    # need perl = T for the negative lookahead, and value = T to pass variable name along.
    sapply(grep("^(?!(DX.+|(PR[0-9]+|PRC.+)|(EC.+|E_C.+)))", names(df), perl = T, value = T), function(x) {
        N <- nrow(df)
        var_vect <- df[,x]
        description <- core_meta$var_labs[x]
        var_type <- core_meta$var_types[["words"]][x]
        miss_vals <- paste(unlist(core_meta$na_vals[[2]][x]), collapse = "|")
        grep_beg <- c("^(?!\\s+)(")
        grep_end <- c(")")
        grep_pat <- paste0(grep_beg, miss_vals, grep_end)
        # need perl = T for the negative lookahead.
        incInvl <- ifelse(miss_vals == "", 0, length(grep(grep_pat, var_vect, perl = T)))
        blank <- length(grep("^\\s+$",var_vect))
        valid <- N - incInvl - blank
        prp_valid <- round(valid / N, 4)
        
        #        var_info <- paste(x, valid, leadWhite, blank, incInv, sep = " | ")
        var_info <- paste(x, description, var_type, valid, prp_valid, incInvl, blank, sep = " | ")
        
        # print variable info. to the console for quick review.
        print(var_info)
        
        # write variable info. to the markdown file.
        writeLines(var_info, core_con)
        
        # this speeds things up a bit.
        rm(list = c("var_vect", "description", "var_type", "valid", "prp_valid", "incInvl", "blank", "var_info", "miss_vals", "grep_beg", "grep_end", "grep_pat"))
        gc(verbose = F)

        }, USE.NAMES = F)

    # close the markdown file connection.
    close(core_con)
    message("Core variables other than diagnosis, procedure, and external cause codes have been checked.")
    }
