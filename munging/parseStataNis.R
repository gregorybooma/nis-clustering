parseStataNis <- function(stataFile, charNas = TRUE) {
    #   Parses the Stata load scripts for the HCUP NIS database, returning metadata
    #   to be used with methods for reading fixed-width text data files (see iotools, 
    #   readr, and LaF packages).
    #
    #   Arguments:
    #      stataFile: the name of the input Stata *.Do script.
    #      charNas: whether or not to return NA values as a character vector
    #                   in addition to integer and numeric.
    #
    #   Returns:
    #      A list object containing NIS variable names, labels, types, lengths, and NA values.

    require(stringr)
    # plyr needed for revalue, as dplyr has yet to add a revalue method
    require(plyr)

    # Avoid factors and scientific notation.
    options(stringsAsFactors = FALSE)
    options(scipen = 9999999)

    # Get the current environment name for storing objects in subfunctions.
    theEnvir <- environment()

    # Read in the code. Skipping blank lines so text delimiters get cleaned properly.
    rawCode <- scan(stataFile, what = "", sep = "\n", blank.lines.skip = T)

    # Get and clean code blocks for variables and na values.
    # N.B.: the space in "label " is necessary to avoid capturing the string in the dofile comments.
    bounds <- list(c("varHead" = "infix"), c("varTail" = "using"), c("labHead" = "label "), c("labTail" = "Convert"), c("naHead" = "recode"), c("naTail" = "save"))
    invisible(lapply(bounds, function(x) {
        elementName <- names(c(x))
        pfx <- str_sub(elementName, 1, nchar(elementName) - 4)
        assign(elementName, min(grep(c(x), rawCode)), envir = theEnvir)

        if ( str_sub(elementName, -4, -1) == "Tail" ) {
            blockName <- paste(pfx, "Block", sep = "")
            theBlock <- rawCode[get(paste(pfx, "Head", sep = "")):get(elementName)]
            theBlock <- str_trim(gsub("infix", "", theBlock[1:(length(theBlock) - 1)]))
            assign(blockName, theBlock, envir = theEnvir)
        }
    }))

    # Get var names and types.
    varNames <- str_extract(varBlock, "[A-Z]\\w+")

    typeWordMap <- c("int" = "integer", "byte" = "integer", "double" = "double", "long" = "double", "float" = "double", "str" = "character")
    typeAbbrevMap <- c("int" = "i", "byte" = "i", "double" = "d", "long" = "d", "float" = "d", "str" = "c")
    rawTypes <- str_extract(varBlock, "^\\w+")
    typeWords <- revalue(rawTypes, typeWordMap)
    names(typeWords) <- varNames
    typeAbbrevs <- revalue(rawTypes, typeAbbrevMap)
    names(typeAbbrevs) <- varNames
    varTypes <- list(words = typeWords, abbrevs = typeAbbrevs)

    # Get var labels.
    varLabs <- gsub("\\\"","",str_extract(labBlock, "\\\".+\\\"$"))
    names(varLabs) <- varNames

    # Get var widths.
    rawWidths <- str_extract_all(varBlock, "(\\s\\d+)|(-\\s?\\d+)")
    varBounds <- gsub("(\\s+)|(-)", "", do.call(rbind,rawWidths))
    varStarts <- as.integer(varBounds[,1])
    varStops <- as.integer(varBounds[,2])
    varWidths <- varStops - varStarts + 1
    names(varWidths) <- varNames
    
    # Get NA values.
    naNames <- str_extract(naBlock, "[A-Z]\\w+")
    rawNas <- str_extract(naBlock, "-.*[0-9]+")
    numNas <- lapply(rawNas, function(x){x <- as.numeric(unlist(str_split(x, " ")))})
    names(numNas) <- naNames
    if ( charNas == TRUE ) {
        charNas <- lapply(numNas, function(x) { as.character(x) })
        naVals <- list(num = numNas, char = charNas)
    } else {
        naVals <- numNas
    }
    
    return(list(varNames = varNames, varLabs = varLabs, varTypes = varTypes, varWidths = varWidths, naVals = naVals))

}