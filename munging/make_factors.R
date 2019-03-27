# create factors for categorical variables, dropping (most) originals
# much more efficient and readable than old version!
if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr"), require, quietly = T, character.only = T)
options(digits = 15, stringsAsFactors = F, scipen = 999)

unordered_vars <- c("DIED","DISPUB04","DISPUNIFORM","ELECTIVE","FEMALE","PAY1","PL_NCHS2006","RACE","TRAN_IN","HOSPID","HOSPZIP","HOSPST","HOSP_LOCTEACH","HOSP_RURAL","HOSP_TEACH","DX_ORD","DX_SRT")

ordered_vars <- c("HOSP_BEDSIZE","ZIPINC_QRTL")

make_unordered_fac <- function(x) {
  var_label <- attr(x,"label")
  x <- as.factor(x)
  if ( !is.null(var_label) ) { label(x) <- var_label }
  x
}

make_ordered_fac <- function(x) {
  var_label <- attr(x,"label")
  x <- factor(x, labels = sort(unique(x)), ordered = T)
  if ( !is.null(var_label) ) { label(x) <- var_label }
  x
}

nis_set <- nis_set %>% mutate_at(unordered_vars, make_unordered_fac)
nis_set <- nis_set %>% mutate_at(ordered_vars, make_ordered_fac)

rm(ordered_vars,unordered_vars,make_ordered_fac,make_unordered_fac)
message("Factor variables were written.")