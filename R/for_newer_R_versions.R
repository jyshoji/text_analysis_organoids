### This R script file includes modified functions to be used on R version 4.3.0. and later.
###
### The read_bibliography function of the revtools package gives an error on R version 4.3.0 and later.
### This seems to be because of the following change introduced in R 4.3.0.: 
### "Regular expression functions now check more thoroughly whether their inputs are valid strings 
### (in their encoding, e.g. in UTF-8)."
### (https://cran.r-project.org/doc/manuals/r-release.save/NEWS.pdf)
###
### As a workaround, the read_bibliography function (along with an internal function) of revtools can be modified 
### to avoid this error.
###
### In the following code, modified functions are made and named as read_bibliography_internal2 and read_bibliography2.
### The modification is essentially to disable the code line; 
### Encoding(z) <- "latin1"
### in the read_bibliography_internal function, which read_bibliography function uses.
###
### Overall, running the following code and replacing all occurrences of read_bibliography with read_bibliography2 
### in the ./R/formatting.R should allow reproduction of the analysis.
### However, this workaround will still give somewhat different results.
### The major cause of the difference is that random seeds do not give the identical results, for example when choosing 
### a copy of the same document when deduplicating the corpus.

###
### Making modified read_bibliography function.
###

library(revtools)

read_bibliography_internal2 <- function (filename, return_df = TRUE) 
{
  if (grepl(".csv$", filename)) {
    result <- revtools_csv(filename)
    if (!return_df) {
      result <- as.bibliography(result)
    }
  }
  else {
    z <- tryCatch({
      scan(filename, sep = "\t", what = "character", quote = "", 
           quiet = TRUE, blank.lines.skip = FALSE)
    }, warning = function(w) {
      stop("file import failed: data type not recognized by read_bibliography", 
           call. = FALSE)
    }, error = function(e) {
      stop("file import failed: data type not recognized by read_bibliography", 
           call. = FALSE)
    })
    #Encoding(z) <- "latin1"
    z <- gsub("<[[:alnum:]]{2}>", "", z, useBytes = TRUE)
    nrows <- min(c(200, length(z)))
    zsub <- z[seq_len(nrows)]
    n_brackets <- length(grep("\\{", zsub))
    n_dashes <- length(grep(" - ", zsub))
    if (n_brackets > n_dashes) {
      result <- revtools:::read_bib(z)
    }
    else {
      if (grepl(".ciw$", filename)) {
        tag_type <- "wos"
      }
      else {
        tag_type <- "ris"
      }
      z_dframe <- revtools:::prep_ris(z, revtools:::detect_delimiter(zsub))
      if (any(z_dframe$ris == "PMID")) {
        result <- revtools:::read_medline(z_dframe)
      }
      else {
        result <- revtools:::read_ris(z_dframe, tag_type)
      }
    }
    if (return_df) {
      result <- as.data.frame(result)
    }
  }
  return(result)
}


read_bibliography2 <- function (filename, return_df = TRUE) 
{
  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))
  if (missing(filename)) {
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if (any(!file_check)) {
    stop("file not found")
  }
  if (length(filename) > 1) {
    result_list <- lapply(filename, function(a, df) {
      read_bibliography_internal2(a, df)
    }, df = return_df)
    names(result_list) <- filename
    if (return_df) {
      result <- merge_columns(result_list)
      result$filename <- unlist(lapply(seq_len(length(result_list)), 
                                       function(a, data) {
                                         rep(names(data)[a], nrow(data[[a]]))
                                       }, data = result_list))
      if (any(colnames(result) == "label")) {
        result$label <- make.unique(result$label)
      }
      return(result)
    }
    else {
      result <- do.call(c, result_list)
      return(result)
    }
  }
  else {
    return(read_bibliography_internal2(filename, return_df))
  }
}