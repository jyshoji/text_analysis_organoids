### This R script file is for importing academic publications on: 
### 1. microphysiological systems, 
### 2. organ-on-a-chip, 
### 3. and organoids.
###
### Overall, each of the three corpora is cleaned up, deduplicated, and reformatted separately, 
### and is combined at a later step.
###
### The file includes following steps: 
### 1. Importing literature metadata of microphysiological systems.
### 2. Deduplicating and reformatting metadata on microphysiological systems.
### 3. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
### 4. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
### 5. Combining the organ-on-a-chip and microphysiological system corpora.
### 6. Deduplicating the combined organ-on-a-chip corpus
### 7. Reformatting the organ-on-a-chip corpus
### 8. Importing, reformatting, and deduplicating literature metadata on organoids.
### 9. Combining orgnaoid and organ-on-a-chip corpora
###
### The outcome of the code is saved as:
### ./R_results/all_corpus
### which is used for subsequent analysis.


### Loading packages.
library(tidyverse)
library(revtools)


### Setting the file path.
### In my case, I stored all the data and results in (subfolders in) ~/Research_data/Hybrida/final_analysis/, 
### so this is my root folder for the analysis.
###
### Setting the path of the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"

### If you are using a Mac, and want to store the data and results in, for example, "organoid_meta_analysis" folder 
### in your home directory (i.e., /Users/<username>/organoid_meta_analysis/), then set the file path as: 
### root_path <- "~/organoid_meta_analysis/"
### See "./README_reproduction.md" for more details.
###
### If you are using Windows, then set the file path as (assuming the data is stored in "organoid_meta_analysis" folder on the C drive): 
### root_path <- "C:\\organoid_meta_analysis\\"
### You will also need to replace "/" with "\\" in the file path throughout the script files, whenever a file is loaded/saved.



##########
###
### 1. Importing literature metadata of microphysiological systems.
###
##########

### Setting the folder path where the literature data of microphysiological systems is stored.
path_ms <- paste0(root_path, "raw_data2/ms/")

### Setting paths of files to import.
file_names_ms <- paste0(path_ms, list.files(path = path_ms))

### Importing the literature files.
data_all_ms <- read_bibliography(file_names_ms)

### The above data object may be saved.
### save(data_all_ms, file = paste0(root_path, "R_temps/data_all_ms"))


### Making a custom function for cleaning up the metadata corpus. 
fn_cleanup_metadata <- function(x) {
  x %>% 
    ### adding a "database" column" showing the database origin of documents.
    ### Note that the files are expected to have names following certain rules.
    mutate(database = 
             ifelse(grepl("pubmed", filename), "PubMed", 
                    ifelse(grepl("savedrecs", filename), "Web of Science", 
                           ifelse(grepl("scopus_", filename), "Scopus", 
                                  ifelse(grepl("em_", filename), "EMBASE", 
                                         ifelse(grepl("BR_", filename), "bioRxiv", NA)))))) %>% 
    ### Assigning unique identification numbers (ID) to each document so that a document can be easily retrieved later by specifying them.
    mutate(ID = c(1:nrow(.))) %>% 
    ### Homogenizing the writing style of the DOI field, by making it lower case and removing URL portion. 
    mutate(doi = tolower(doi)) %>% 
    mutate(doi = gsub("http://dx.doi.org/", "", doi, fixed = TRUE)) %>% 
    mutate(doi = gsub("https://dx.doi.org/", "", doi, fixed = TRUE)) %>%  
    mutate(doi = gsub("https://doi.org/", "", doi, fixed = TRUE)) %>% 
    ### For papers lacking the publication year (all from Web of Science), "year" is taken from the "earlyaccessdate" column.
    mutate(year = ifelse(!is.na(year), year, str_extract(earlyaccessdate, "[0-9]{4}$"))) %>% 
    ### adding a "type" column showing the article type of documents.
    mutate(type = 
             ifelse(grepl("_RV", filename), "Review", 
                    ifelse(grepl("_RA", filename), "Research article", 
                           ifelse(grepl("BR_", filename), "Preprint", NA))))
}

### Using the above custom function on data_all_ms data frame.
cleanedup_ms <- fn_cleanup_metadata(data_all_ms) 





##########
####
#### 2. Deduplicating and reformatting metadata on microphysiological systems.
####
####
##########

### Below, extra copies of the same metadata documents will be removed (i.e., deduplication).
###
### Web of Science metadata have a higher occurrences of mis-spelling in the text fields.
### Because of this, it is desirable that metadata from Web of Science are preferentially removed by deduplication.
### 
### For this purpose, two-step deduplication is performed.
### The first step deduplicates documents from  Web of Science, 
### The second step deduplicates documents in the entire corpus by preferentially removing extra copies in Web of Science.
### The reason for including the first step is so as not to remove all copies of the same documents when all copies are from Web of Science.
###
### Making a custom function for the first step of deduplicating Web of Science documents.
fn_wos_deduplication <- function(x) {
  if(!"Web of Science" %in% x$database) {
    wos_dedup <- x
  }
  else {
    ### Selecting documents from Web of Science
    wos <- x %>% 
      filter(database == "Web of Science")
    ### Finding duplicate documents from the selected Web of Science documents.
    wos_matches <- find_duplicates(wos)
    ### Setting a random seed for reproducibility.
    set.seed(1)
    ### Removing extra copies of the same documents from the Web of Science document.
    wos_unique <- extract_unique_references(wos, wos_matches) 
    ### Web of Science documents in the original corpus are replaced with deduplcicated Web of Science documents.
    wos_dedup <- x %>% 
      filter(!database == "Web of Science") %>% 
      rbind(., wos_unique[, -ncol(wos_unique)])
  }
  return(wos_dedup)
}

### Using the custom function on the metadata on microphysiological systems.
wos_dedup_ms <- fn_wos_deduplication(cleanedup_ms)

### Making a custom function for deduplicating the entire corpus by preferentially removing extra copies in Web of Science.
fn_all_deduplication <- function(x) {
  ### Finding duplicate documents from the entire corpus, based on DOIs.
  matches_doi <- find_duplicates(x)
  ### Selecting duplicate documents.
  duplicates_no <- data.frame(table(matches_doi)) %>% 
    filter(Freq > 1)
  ### Removing Web of Science documents that have extra copies.
  ### These documents should have extra copies in other databases because of the step taken above.
  wos_dedup_2 <- x %>% 
    mutate(match = matches_doi) %>% 
    filter(!(match %in% duplicates_no$matches_doi & database == "Web of Science"))
  ### Finding duplicate documents from the entire corpus again, based on DOIs.
  matches_doi_wos_dedup <- find_duplicates(wos_dedup_2)
  ### Setting a random seed.
  set.seed(1)  
  ### Removing duplicate documents.
  unique_ducuments <- extract_unique_references(wos_dedup_2, matches_doi_wos_dedup)
}

### Applying the above custom function to the metadata corpus.
unique_ms <- fn_all_deduplication(wos_dedup_ms)

### Checking the number of documents from each database
unique_ms %>% count(database)
##         database   n
## 1        bioRxiv  27
## 2         EMBASE  90
## 3         PubMed 422
## 4         Scopus 153
## 5 Web of Science  88

### Making a custom function for modifying the keyword and abstract fields.
fn_keywords_abstract_adjustment <- function(x) {
  ### In some papers, the keyword field seemed to be (computer-)generated at publishers, and included many 
  ### unrelated/only loosely related keywords, and therefore it was not very informative when compared to author keywords.
  ### To remove such less useful keywords, 
  ### 1) author keywords are chosen for the keyword field when present, 
  ### 2) long keywords are removed.    
  if("author_keywords" %in% colnames(x)) {
    x <- x %>% 
      mutate(keywords = ifelse(!is.na(author_keywords), author_keywords, keywords))
  }
  cleaned_corpus <- x %>% 
    mutate(keywords = ifelse(is.na(keywords), NA, 
                             ifelse(nchar(keywords) > 250, NA, keywords))) %>% 
    ### Also, author declaration was removed.
    mutate(abstract = gsub("copyright.*$|competing interest statement.*$|declaration.*$", "", abstract, ignore.case = TRUE))
}

### Applying the custom function to the corpus.
unique_ms_F <- fn_keywords_abstract_adjustment(unique_ms)

### The above data object may be saved.
save(unique_ms_F, file = paste0(root_path, "R_temps/unique_ms_F"))





##########
###
### 3. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
###
##########

### Setting paths of files to import.
path_oc <- paste0(root_path, "raw_data2/onchip/")

file_names_oc <- paste0(path_oc, list.files(path = path_oc))

### Importing the literature metadata files.
data_all_oc <- read_bibliography(file_names_oc)

### The above data object may be saved.
save(data_all_oc, file = paste0(root_path, "R_temps/data_all_oc"))

### cleaning up the corpus.
cleanedup_oc <- fn_cleanup_metadata(data_all_oc) 

### removing extra copies of documents in the Web of Science corpus. 
wos_dedup_oc <- fn_wos_deduplication(cleanedup_oc)

### removing extra copies of document from the entire corpus.
### Note that this will take ~ 30 minutes.
unique_oc <- fn_all_deduplication(wos_dedup_oc)

### Checking the number of papers from each database
unique_oc %>% count(database)
##         database     n
## 1        bioRxiv   248
## 2         EMBASE  1245
## 3         PubMed  7410
## 4         Scopus 30717
## 5 Web of Science  4168

### Adjusting the keyword and abstract fields.
unique_oc_F <- fn_keywords_abstract_adjustment(unique_oc) %>% 
  ### Also making a new column to store combined titles, keywords, and abstracts in lower cases, with a homogenized writing style for 
  ### "on-chip", "on-a-chip", "on chip", "on a chip".
  mutate(text_all_mod_lower = tolower(paste(title, abstract, keywords, sep = "; "))) %>% 
  mutate(text_all_mod_lower = gsub("[- ]?\\bon[- ]chips?\\b|[- ]?\\bon[- ]a[- ]chip\\b", " onchip", text_all_mod_lower))

### The above data object may be saved.
save(unique_oc_F, file = paste0(root_path, "R_temps/unique_oc_F"))






##########
###
### 4. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
###
### This step select organ-on-a-chip documents from the on-chip corpus, by choosing documents where 
### organ names or equivalents occur right before "onchip".
### For this purpose, all words that occur before "onchip" are extracted, manually inspected, and 
### organ names and equivalents are selected for consideration.
###
##########

### !!!Note!!!
### The following step is for manually selecting terms to consider as organ names or equivalents. 
### If you want to skip the manual selection, ignore the following lines and resume at "4.1. Selecting organ-on-a-chip publications."


### Extracting all occurrences of the term "onchip", along with a word before it.
pre_onchip_all <- unlist(str_extract_all(unique_oc_F$text_all_mod_lower, "[a-z0-9]+\\)? onchip\\b"))

### Removing duplicates from the above, also removing " onchip"
pre_onchip_unique <- unique(gsub("\\)? onchip", "", pre_onchip_all))

### Making a data frame of the extracted words.
### A column "include" is made.
pre_onchip_all_df <- data.frame(pre_word = pre_onchip_unique, include = "") 

### And saving it as a .csv file
write.csv(pre_onchip_all_df, file = paste0(root_path, "csv/temps/pre_onchip_all_df.csv"), row.names = FALSE)

### This csv file was opened on Excel and manually checked, and the "include" column was filled with either 
### "c", "n", "y", or "o" meaning "consider", "not to include as an organ name", "include as an organ name", or "organism".
### "consider" here means that a word further before was also checked and considered so that, for example, 
### "rubber tube-on-chip" may not be included but "fallopian tube-on-chip" is included.
### We did not include terms classified as "organism" in the end.
### The checked file was saved as "./csv/pre_onchip_all_F.csv"


###
### 4.1. Selecting organ-on-a-chip publications.
###

### Loading the csv file of terms after manual screening.
pre_onchip_all_F <- read.csv(paste0(root_path, "csv/pre_onchip_all_F.csv"))

### Selecting words to include.
pre_onchip_inc <- pre_onchip_all_F %>% 
  filter(include == "y")

### After looking into words classified as "consider", following word combinations were included.
### Here, for example, pre_node_w <- c("lymph") means that node-on-chip, when following "lymph",  will be included as an organ-on-a-chip document.
pre_network_w <- c("vascular", "corticostriatal", "neural", "neuronal", 
                   "microvascular", "hippocampal", "hd")
pre_duct_w <- c("bile", "collecting") 
pre_barrier_w <- c("corneal", "multicorneal", "epithelium", "brain", 
                   "placental?", "tissue", "intestinal")
pre_circuitry_w <- c("epithelial")
pre_vessel_w <- c("blood", "lymph", "lymphatic", "tumou?r")
pre_circuit_w <- c("neuronal", "neural")
pre_junction_w <- c("neuromuscular")
pre_node_w <- c("lymph")
pre_axis_w <- c("intestine", "kidney", "liver", "brain")
pre_membrane_w <- c("fetal")
pre_cortex_w <- c("cerebral")
pre_cell_w <- c("epithelial", "oral", "stem", "liver", "cancer", "endothelial", "immune")
pre_unit_w <- c("neurovascular", "osteochondral")
pre_monolayer_w <- c("epithelial", "gut")

### Combining the above vectors into a single vector.
pre_2words_w <- c(paste0(pre_network_w, "[- ]networks?"), paste0(pre_duct_w, "[- ]ducts?"), 
                  paste0(pre_barrier_w, "[- ]barriers?"), paste0(pre_circuitry_w, "[- ]circuitry"), 
                  paste0(pre_vessel_w, "[- ]vessels?"), paste0(pre_circuit_w, "[- ]circuits?"), 
                  paste0(pre_junction_w, "[- ]junctions?"), paste0(pre_node_w, "[- ]nodes?"), 
                  paste0(pre_axis_w, "[- ]ax[ei]s"), paste0(pre_membrane_w, "[- ]membranes?"), 
                  paste0(pre_cortex_w, "[- ]cortex"), paste0(pre_cell_w, "[- ]cells?"), 
                  paste0(pre_unit_w, "[- ]units?"), paste0(pre_monolayer_w, "[- ]monolayers?"))

### Combining the lists of words and two-word combinations.
pre_onchip_all_w <- c(pre_2words_w, pre_onchip_inc$pre_word)

### From the deduplicated metadata corpus of on-chip technology, only selecting papers where a word (or two-word combination) 
### in pre_onchip_all_w occurs right before "onchip".
data_ooc <- unique_oc_F %>% 
  filter(grepl(paste0("\\b", pre_onchip_all_w, "\\)? onchip", collapse = "|"), text_all_mod_lower))
### This is considered as an organ-on-a-chip corpus.

### The above data object may be saved.
save(data_ooc, file = paste0(root_path, "R_temps/data_ooc"))






##########
###
### 5. Combining the organ-on-a-chip and microphysiological system corpora.
###
##########

### Comparing the dimensions of the organ-on-a-chip and microphysiological system corpora.
dim(data_ooc)
## [1] 3357  122

dim(unique_ms_F)
## [1] 780 109

### These two corpora need to have identical columns in order for them to be combined.
###
### Selecting columns of the organ-on-a-chip corpus that also exist in the microphysiological system corpus.
data_ooc2 <- data_ooc %>% 
  select(any_of(colnames(unique_ms_F)))

### Selecting columns of the microphysiological system corpus that also exist in the organ-on-a-chip corpus.
unique_ms_F2 <- unique_ms_F %>% 
  select(any_of(colnames(data_ooc2)))

### Combining the two corpora, and assigning fresh ID numbers to each document.
combined_ooc <- rbind(data_ooc2, unique_ms_F2) %>% 
  mutate(ID = c(1:nrow(.))) %>% 
  ### Discarding the columns used for deduplication, as they would conflict with subsequent deduplication.
  select(!c("match", "n_duplicates"))

dim(combined_ooc)
## [1] 4137  105







##########
###
### 6. Deduplicating the combined organ-on-a-chip corpus
###
##########

### Removing extra copies of documents from Web of Science from the combined corpus.
wos_dedup_ooc <- fn_wos_deduplication(combined_ooc)

### removing extra copies of documents from the corpus (not just from Web of Science.)
unique_ooc <- fn_all_deduplication(wos_dedup_ooc)

unique_ooc %>% count(database)
#         database    n
# 1        bioRxiv  139
# 2         EMBASE  346
# 3         PubMed 2107
# 4         Scopus 1076
# 5 Web of Science   90

### !!!Note!!!
### The following step is for manually selecting duplicate publications by manual inspection, and can be time-consuming.
### You may skip this step without a significant loss of data quality.
### If you want to skip this step, then run the below line: 
### unique_ooc_F <- unique_ooc
### And then resume from the 
### "7. Reformatting the organ-on-a-chip corpus"
###
### Next, title-based deduplication is performed on papers NOT from bioRxiv.
### Documents that are flagged as potential duplicates will be manually checked.
### For this purpose, a custom function is made for detecting potential duplicates based on titles.
fn_find_duplicates_title <- function(x) {
  ### Excluding bioRxiv papers.
  unique_rest <- x %>% 
    filter(!database == "bioRxiv")
  ### Finding potential duplicates based on title.
  matches_rest <- find_duplicates(unique_rest, match_variable = "title", to_lower = TRUE)
  ### selecting the potential duplicates, and selecting required columns for manual checking.
  duplicate_no_title <- data.frame(table(matches_rest)) %>% 
    filter(Freq > 1)
  unique_rest %>% 
    mutate(match = matches_rest) %>% 
    filter(match %in% duplicate_no_title$matches_rest) %>% 
    select(author, title, journal, abstract, doi, ID, database) %>% 
    arrange(.$title) 
}

### Applying the custom function to the combined organ-on-a-chip corpus.
duplicates_ooc <- fn_find_duplicates_title(unique_ooc)

### Saving the data object as a csv file.
duplicates_ooc %>% mutate(status = "") %>% 
  write.csv(., file = paste0(root_path, "csv/temps/duplicates_ooc.csv"), row.names = FALSE)

### The above csv file was manually checked, and the "status" column was filled with either: 
### "k" when the document has extra copies but is chosen to be the copy to be kept, 
### "r" when the document has extra copies and is to be removed,
### "u" when the document is unique and has to be kept.
### The manually checked file was saved as ./csv/duplicates_ooc_checked.csv and loaded below.
duplicates_ooc_checked <- read.csv(paste0(root_path, "csv/duplicates_ooc_checked.csv"))

duplicates_ooc_checked %>% count(status)
##   status  n
## 1      k 53
## 2      r 57
## 3      u  8
## This means that;
## 1) 8 / 2 = 4 unique papers could have been removed by the title-based deduplication. 
## 2) 110 duplicate papers were not deduplicated by DOI-matching either because; 
## a) the paper was recently published and lacked DOI, b) misspelling in the doi field and/or 
### b2) differences in DOIs between electronic and printed publications.
## 
## Overall, title-based deduplication improved the dataset quality by 1.6% by removing 57 duplicates 
## from 3619 papers, but would have reduced the dataset quality by 0.1% by erroneously removing 
## 4 unique papers without manual inspection.

### Selecting papers to remove
duplicates_ooc_to_remove <- duplicates_ooc_checked %>% 
  filter(status == "r")

### Removing duplicate papers to remove.
unique_ooc_F <- unique_ooc %>% 
  filter(!ID %in% duplicates_ooc_to_remove$ID) 






##########
###
### 7. Reformatting the organ-on-a-chip corpus
###
##########

### Making a custom function for reformatting the corpus.
fn_reformatting <- function(x) {
  if(!"month" %in% colnames(x)) {
    x <- x %>% 
      mutate(month = "")
  }
  reformatted_corpus <- x %>% 
    ### Removing documents lacking author or abstracts.
    drop_na(author) %>% 
    drop_na(abstract) %>% 
    ### Removing documents with the author being anonymous.
    filter(!author == "Anonymous.") %>% 
    ### The address field, except in EMBASE documents, contains addresses of publishers.
    ### Addresses of research institutes are instead included in "affiliation".
    ### Below, the address field is changed so that it contains addresses of research institutes.
    mutate(address = ifelse(database == "EMBASE", address, affiliation)) %>% 
    ### A column for combined text fields is also made.
    mutate(text_all = paste(title, keywords, abstract, sep = "; ")) %>% 
    ### Selecting important columns.
    select(ID, author, title, type, year, doi, database, keywords, abstract, text_all, 
           address, journal, volume, pages, month) %>% 
    ### Below, "modified" title, keywords, and abstract columns are made, where all occurrences of "on-chip", "on-a-chip", etc. are converted to 
    ### "onchip" to homogenize the writing style.
    mutate(title_mod = gsub("[- ]?\\bon[- ]chips?\\b|[- ]?\\bon[- ]a[- ]chip\\b", " onchip", title, ignore.case = TRUE)) %>% 
    mutate(keywords_mod = gsub("[- ]?\\bon[- ]chips?\\b|[- ]?\\bon[- ]a[- ]chip\\b", " onchip", keywords, ignore.case = TRUE)) %>%   
    mutate(abstract_mod = gsub("[- ]?\\bon[- ]chips?\\b|[- ]?\\bon[- ]a[- ]chip\\b", " onchip", abstract, ignore.case = TRUE)) %>%  
    ### In the above "modified" text fields, all occurrences of "non-" or "non " are converted to "non", so that, for example, "non-human" is 
    ### changed to "nonhuman" and will not be matched by "human".
    mutate(across(c(16:18), ~ gsub("\\bnon[- ]", "non", ., ignore.case = TRUE))) %>% 
    ### "." followed by a number is changed to "," to distinguish between full stops and decimal points.
    mutate(abstract_mod = gsub("\\.(?=[0-9])", ",", abstract_mod, perl = TRUE)) %>% 
    ### Further new columns are made for: 1) combined keywords and abstracts, and 2) combined titles, keywords, and abstracts.
    mutate(keywords_abstract_mod = paste(keywords_mod, abstract_mod, sep = "; ")) %>% 
    mutate(text_all_mod = paste(title_mod, keywords_mod, abstract_mod, sep = "; "))
}

### Applying the above custom function to the deduplicated organ-on-a-chip corpus.
ooc_reformatted <- fn_reformatting(unique_ooc_F)

### Making a character vector of words and phrases that we consider equivalent to "organ-on-a-chip".
onchip_w <- c("onchip", "[Mm]icro[- ]?[Pp]hysiological [Ss]ystems?", "[Mm]icro[- ]?[Pp]hysiology [Ss]ystems?", 
              "[Mm]icro[- ]?[Pp]hysiologic [Ss]ystems?", "O[Oo]Cs?")


ooc_corpus <- ooc_reformatted %>% 
  ### Making "key" columns, in which sentences are stored that contain the above words/phrases equivalent to "organ-on-a-chip".
  mutate(title_key = 
           ifelse(is.na(title_mod), NA, 
                  ifelse(grepl(pattern = paste0("\\b", onchip_w, "\\b", collapse = "|"), title_mod) == TRUE, title_mod, NA))) %>% 
  mutate(keywords_key = 
           ifelse(is.na(keywords_mod), NA, 
                  ifelse(grepl(pattern = paste0("\\b", onchip_w, "\\b", collapse = "|"), keywords_mod) == TRUE, keywords_mod, NA))) %>% 
  mutate(abstract_key = 
           apply(
             str_extract_all(abstract_mod, 
                             pattern = paste0("[^.]*\\b", onchip_w, "\\b[^.]*\\.", collapse = "|"), simplify = TRUE), 
             1, paste, collapse = "; ")) %>% 
  ### Making new columns for combined "key" sentences for: 1) keywords and abstracts, and 2) title, keywords, and abstracts.
  mutate(keywords_abstract_key = paste(keywords_key, abstract_key, sep = "; ")) %>% 
  mutate(text_all_key = paste(title_key, keywords_key, abstract_key, sep = "; "))  


colnames(ooc_corpus)

### The above data object may be saved.
save(ooc_corpus, file = paste0(root_path, "R_temps/ooc_corpus"))





##########
###
### 8. Importing, reformatting, and deduplicating literature metadata on organoids.
###
### Note that initially some of the files could not be imported into R by "read_bibliography()" for an unknown reason.
### To overcome this issue, publications from PubMed were imported as 1) reviews only, 2) reviews + research articles.
### Therefore, reviews from PubMed are duplicated, and had to be deduplicated at a later point.
### Same goes for blastoid metadata from EMBASE.
### 
##########

### Setting paths of files to import.
path_or <- paste0(root_path, "raw_data2/organoid/")

file_names_or <- paste0(path_or, list.files(path = path_or))

### Importing the literature files.
data_all_or <- read_bibliography(file_names_or)

### The above data object may be saved.
save(data_all_or, file = paste0(root_path, "R_temps/data_all_or"))

### Cleaning up the metadata corpus.
cleanedup_or <- fn_cleanup_metadata(data_all_or) %>% 
  ### As mentioned above, some of the files of review papers from PubMed and EMBASE couldn't be imported.
  ### Because of this, they were stored in files ending with "All", and now have "NA" in the "type" column.
  ### The NA in this column is changed to "Research article".
  mutate(type = ifelse(is.na(type), "Research article", type))

### Making a custom function for removing duplicated reviews from PubMed and EMBASE publications.
fn_cleanup_or <- function(x) {
  ### Removing documents from the pubmed_organoid_All.nbib file that have "Review" in the publication_type.
  cleanedup_or2 <- x %>% 
    filter(!(grepl("pubmed.+All", filename) & grepl("Review", publication_type)))
  if(!"EMBASE" %in% cleanedup_or2$database) {
    cleanedup_or_F <- cleanedup_or2
  }
  else {
    ### Choosing blastoid review publications from EMBASE
    em_blastoid <- cleanedup_or2 %>% 
      filter(grepl("em_blastoid_RV", filename)) %>% 
      filter(!is.na(doi))
    ### Removing publications from the em_blastoid_All.ris.txt file that have matching doi in the above blastoid reviews.
    cleanedup_or_F <- cleanedup_or2 %>% 
      filter(!(grepl("em_blastoid_All", filename) & doi %in% em_blastoid$doi)) 
  }
  return(cleanedup_or_F)
}

### Applying the custom function.
cleanedup_or_F <- fn_cleanup_or(cleanedup_or)

### removing extra copies of document from the Web of Science corpus. 
wos_dedup_or <- fn_wos_deduplication(cleanedup_or_F)

### removing extra copies of document from the  corpus (not just from Web of Science.)
unique_or <- fn_all_deduplication(wos_dedup_or)

### Checking the number of papers from each database
unique_or %>% count(database)
#         database    n
# 1        bioRxiv 1202
# 2         EMBASE 1477
# 3         PubMed 7781
# 4         Scopus 1985
# 5 Web of Science  739

### The above data object may be saved.
save(unique_or, file = paste0(root_path, "R_temps/unique_or"))

### !!!Note!!!
### The following step is for manually selecting duplicate publications by manual inspection, and can be time-consuming.
### You may skip this step without a significant loss of data quality.
### If you want to skip this step, then run the below line: 
### unique_or_F <- unique_or
### And then resume from the "8.1 Cleaning up the organoid corpus."

### Title-based detection of potential duplicates of the organoid corpus.
### The following computation could take one hour or longer.
duplicates_or <- fn_find_duplicates_title(unique_or)

### Saving the detected potential duplicates in a csv file for manual inspection.
duplicates_or %>% mutate(status = "") %>% 
  write.csv(., file = paste0(root_path, "csv/temps/duplicates_or.csv"), row.names = FALSE)

### The above csv file was manually checked, and the "status column was filled with either "k", "r", or "u", 
### And save as "./csv/duplicates_or_checked.csv"
### Reading the csv file.
duplicates_or_checked <- read.csv(paste0(root_path, "csv/duplicates_or_checked.csv"))

duplicates_or_checked %>% count(status)
##   status   n
## 1      k 177
## 2      r 235
## 3      u  18
## This means that;
## 1) 18 / 2 = 9 unique papers could have been removed by the title-based deduplication. 
## 2) 235 papers were not removed at the doi-based deduplication step .
## 
## Overall, title-based deduplication improved the dataset quality by 2% by removing 235 duplicates 
## from 11982 papers, but would have reduced the dataset quality by 0.07% by erroneously removing 
## 9 unique papers without manual inspection.

### Selecting papers to remove from the above csv file.
duplicates_or_to_remove <- duplicates_or_checked %>% 
  filter(status == "r")

### Removing the duplicate papers to remove.
unique_or_F <- unique_or %>% 
  filter(!ID %in% duplicates_or_to_remove$ID) 
### This is the final deduplicated corpus of organoids.

dim(unique_or_F)
## [1] 12949   120



###
### 8.1 Cleaning up the organoid corpus.
###

### Applying custom functions for keywords/abstract adjustment and reformatting.
pre_organoid_corpus <- fn_keywords_abstract_adjustment(unique_or_F) %>% 
  fn_reformatting(.)

### Papers on "nevus sebaceus" (aka "organoid nevus") are removed.
### These papers are retrieved primarily from PubMed even if they don't contain the term "organoid", because of how MeSH works.
### First "nevus sebaceus" and its synonyms are stored in a vector.
nevus_sebaceus_w <- c("na?evus sebaceus", "sebaceus na?evus", "sebaceus na?evi", 
                      "organoid na?evus", "organoid na?evi", "angora hair na?evus", 
                      "angora hair na?evi", "organoid epidermal na?evus", 
                      "organoid epidermal na?evi")

### Removing papers containing any of the above terms in the vector.
pre_organoid_corpus2 <- pre_organoid_corpus %>% 
  filter(!grepl(paste(nevus_sebaceus_w, collapse = "|"), text_all, ignore.case = TRUE))

dim(pre_organoid_corpus2)
### [1] 12603   20

colnames(pre_organoid_corpus2)

### Making a vector of the term "organoid" and its synonyms.
organoid_w <- c("[Oo]rganoids?", "[Ee]nteroids?", "[Gg]astruloids?", 
                "[Cc]olonoids?", "i?[Bb]lastoids?", "[Tt]umou?roids?", 
                "[Aa]ssembloids?", "[Ee]mbryoids?", "[Cc]erebroids?", 
                "[Cc]ardioids?")
### Note that spheroids are not included here,

### Modifying text fields.
### Adding "_key" columns containing key sentences (i.e., sentences that contain one of above terms).
###
### The following computation may take 30 minutes or so.
organoid_corpus <- pre_organoid_corpus2 %>% 
  mutate(title_key = 
           ifelse(is.na(title_mod), NA, 
                  ifelse(grepl(pattern = paste(organoid_w, collapse = "|"), title_mod) == TRUE, title_mod, NA))) %>% 
  mutate(keywords_key = 
           ifelse(is.na(keywords_mod), NA, 
                  ifelse(grepl(pattern = paste(organoid_w, collapse = "|"), keywords_mod) == TRUE, keywords_mod, NA))) %>% 
  mutate(abstract_key = 
           apply(
             str_extract_all(abstract_mod, 
                             pattern = paste0("[^.]*", organoid_w, "[^.]*\\.", collapse = "|"), 
                             simplify = TRUE), 
             1, paste, collapse = "; ")) %>% 
  mutate(keywords_abstract_key = paste(keywords_key, abstract_key, sep = "; ")) %>% 
  mutate(text_all_key = paste(title_key, keywords_key, abstract_key, sep = "; "))  

colnames(organoid_corpus)

### The above data object may be saved.
save(organoid_corpus, file = paste0(root_path, "R_temps/organoid_corpus"))




##########
###
### 9. Combining orgnaoid and organ-on-a-chip corpora
###
##########


### Combining organoid and organ-on-a-chip corpora, while making a new column "corpus" which is either "organoid" or "OoC", 
### depending on whether the document is from the organoid or organ-on-a-chip corpora.
all_corpus <- rbind((organoid_corpus %>% mutate(corpus = "organoid")), 
                         (ooc_corpus %>%  mutate(corpus = "OoC"))) %>% 
  ### Reassigning IDs.
  mutate(ID = c(1:nrow(.))) %>% 
  ### Making a new column "phase", which shows if the document is from early or later phases of the research development.
  mutate(phase = ifelse(year %in% c(2020:2023), "later", "early")) 


save(all_corpus, file = paste0(root_path, "R_results/all_corpus"))