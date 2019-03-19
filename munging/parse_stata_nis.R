#   parse_stata_nis.R
#
#   Parses the Stata load scripts for the HCUP NIS database, returning metadata
#   to be used with methods for reading fixed-width text data files (see iotools,
#   readr, and LaF packages).
#
#   Arguments:
#      stata_file: the name of the input Stata *.Do script.
#      char_na: whether or not to return NA values as a character vector
#                   in addition to integer and numeric.
#
#   Returns:
#      A list object containing NIS variable names, labels, types, lengths, and NA values.

parse_stata_nis <- function(stata_file, char_na = TRUE) {

    require(stringr)
    # plyr needed for revalue, as dplyr has yet to add a revalue method
    require(plyr)

    # Avoid factors and scientific notation.
    options(stringsAsFactors = FALSE)
    options(scipen = 999)

    # Get the current environment name for storing objects in subfunctions.
    envmt <- environment()

    # Read in the code. Skipping blank lines so text delimiters get cleaned properly.
    raw_code <- scan(stata_file, what = "", sep = "\n", blank.lines.skip = T)

    # Get and clean code blocks for variables and na values.
    # N.B.: the space in "label " is necessary to avoid capturing the string in the dofile comments.
    bounds <- list(c("varHead" = "infix"), c("varTail" = "using"), c("labHead" = "label "), c("labTail" = "Convert"), c("naHead" = "recode"), c("naTail" = "save"))
    invisible(lapply(bounds, function(x) {
        element_name <- names(c(x))
        pfx <- str_sub(element_name, 1, nchar(element_name) - 4)
        assign(element_name, min(grep(c(x), raw_code)), envir = envmt)

        if ( str_sub(element_name, -4, -1) == "Tail" ) {
            block_name <- paste(pfx, "Block", sep = "")
            the_block <- raw_code[get(paste(pfx, "Head", sep = "")):get(element_name)]
            the_block <- str_trim(gsub("infix", "", the_block[1:(length(the_block) - 1)]))
            assign(block_name, the_block, envir = envmt)
        }
    }))

    # Get var names and types.
    var_names <- str_extract(varBlock, "[A-Z]\\w+")

    type_wordmap <- c("int" = "integer", "byte" = "integer", "double" = "double", "long" = "double", "float" = "double", "str" = "character")
    type_abbrevmap <- c("int" = "i", "byte" = "i", "double" = "d", "long" = "d", "float" = "d", "str" = "c")
    raw_types <- str_extract(varBlock, "^\\w+")
    # dplyr recode with !!! seems to work -- figure out if plyr no longer needed!
    # type_words <- revalue(raw_types, type_wordmap)
    type_words <- recode(raw_types, !!!type_wordmap)
    names(type_words) <- var_names
    #type_abbrevs <- revalue(raw_types, type_abbrevmap)
    type_abbrevs <- recode(raw_types, !!!type_abbrevmap)
    names(type_abbrevs) <- var_names
    var_types <- list(words = type_words, abbrevs = type_abbrevs)

    # Get var labels.
    var_labs <- gsub("\\\"","",str_extract(labBlock, "\\\".+\\\"$"))
    names(var_labs) <- var_names

    # Get var widths.
    raw_widths <- str_extract_all(varBlock, "(\\s\\d+)|(-\\s?\\d+)")
    var_bounds <- gsub("(\\s+)|(-)", "", do.call(rbind,raw_widths))
    var_starts <- as.integer(var_bounds[,1])
    var_stops <- as.integer(var_bounds[,2])
    var_widths <- var_stops - var_starts + 1
    names(var_widths) <- var_names

    # Get NA values.
    na_names <- str_extract(naBlock, "[A-Z]\\w+")
    raw_nas <- str_extract(naBlock, "-.*[0-9]+")
    num_nas <- lapply(raw_nas, function(x){x <- as.numeric(unlist(str_split(x, " ")))})
    names(num_nas) <- na_names
    if ( char_na == TRUE ) {
        char_nas <- lapply(num_nas, function(x) { as.character(x) })
        na_vals <- list(num = num_nas, char = char_nas)
    } else {
        na_vals <- num_nas
    }

    return(list(var_names = var_names, var_labs = var_labs, var_types = var_types, var_widths = var_widths, na_vals = na_vals))

}