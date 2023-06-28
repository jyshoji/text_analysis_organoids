### This R script file is for updating the metadata corpus of academic publications on: 
### 1. microphysiological systems, 
### 2. organ-on-a-chip, 
### 3. and organoids.
###
### The code here is mostly based on ./R/formatting.R, with a few modifications.
### The modifications include; 
### 1. file paths for importing literature metadata.
### 2. If a document has a copy in both existing and newly added corpus, the "year" was taken from the latter.
### 3. Names of R objects are mostly the same, but they were saved under different names as R object files. 
###
### Overall, metadata of new publications is cleaned up, added to the existing corpus (./R_results/all_corpus), and deduplicated.
###
### The code includes following steps.
### 1. Saving the existing corpus as a back-up.
### 2. Importing literature metadata of microphysiological systems.
### 3. Deduplicating and reformatting metadata on microphysiological systems.
### 4. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
### 5. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
### 6. Combining the organ-on-a-chip and microphysiological system corpora.
### 7. Deduplicating the combined organ-on-a-chip corpus. 
### 8. Reformatting the organ-on-a-chip corpus and adding it to the existing corpus
### 9. Importing, reformatting, and deduplicating literature metadata on organoids, and adding it to the existing corpus
### 10. Combining orgnaoid and organ-on-a-chip corpora
### 
### The outcome of the code is saved as:
### ./R_results/all_corpus
### by overwriting the existing file, and is used for subsequent analysis.


### Loading packages.
library(tidyverse)
library(revtools)


### Setting the file path.
### In my case, I stored all the data and results in (subfolders in) ~/Research_data/Hybrida/final_analysis/, 
### so this is my root folder for the analysis.
###
### Setting the path of the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"



##########
###
### 1. Saving the existing corpus as a back-up.
###
##########

### Loading the existing corpus.
### load(paste0(root_path, "R_results/all_corpus"))

### Renaming the corpus.
### all_corpus_old <- all_corpus

### Saving as a back-up.
### save(all_corpus_old, file = paste0(root_path, "R_results/all_corpus_old"))

### Loading the existing list of terms that appear before "onchip".
### pre_onchip_all_F <- read.csv(paste0(root_path, "csv/pre_onchip_all_F.csv"))

### Taking a back-up of the existing pre_onchip_all_F.csv.
### write.csv(pre_onchip_all_F, file = paste0(root_path, "csv/pre_onchip_all_F_old.csv"), row.names = FALSE)


### The lines below are to rerun the entire code for quality checking using the old corpus .
### load(file = paste0(root_path, "R_results/all_corpus_old"))
### all_corpus <- all_corpus_old
### rm(all_corpus_old)
### pre_onchip_all_F <- read.csv(file = paste0(root_path, "csv/pre_onchip_all_F_old.csv"))


##########
###
### 2. Importing literature metadata of microphysiological systems.
###
##########

### Setting the folder path where the literature data of microphysiological systems is stored.
path_ms <- paste0(root_path, "raw_data_update/ms/")

### Setting paths of files to import.
file_names_ms <- paste0(path_ms, list.files(path = path_ms))

### Importing the literature files.
data_all_ms <- read_bibliography(file_names_ms)

### Checking if the papers are from 2022 onwards.
data_all_ms %>% count(year)


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
#### 3. Deduplicating and reformatting metadata on microphysiological systems.
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
### save(unique_ms_F, file = paste0(root_path, "R_temps/unique_ms_update"))





##########
###
### 4. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
###
##########

### Setting paths of files to import.
path_oc <- paste0(root_path, "raw_data_update/onchip/")

file_names_oc <- paste0(path_oc, list.files(path = path_oc))

### Importing the literature metadata files.
data_all_oc <- read_bibliography(file_names_oc)

### Checking if the papers are from 2022 onwards.
data_all_oc %>% count(year)

### Cleaning up and deduplicating the corpus.
unique_oc_F <- fn_cleanup_metadata(data_all_oc) %>% 
  ### removing extra copies of documents in the Web of Science corpus. 
  fn_wos_deduplication(.) %>% 
  ### removing extra copies of document from the entire corpus.
  fn_all_deduplication(.) %>% 
  ### Adjusting the keyword and abstract fields.
  fn_keywords_abstract_adjustment(.) %>% 
  ### Also making a new column to store combined titles, keywords, and abstracts in lower cases, with a homogenized writing style for 
  ### "on-chip", "on-a-chip", "on chip", "on a chip".
  mutate(text_all_mod_lower = tolower(paste(title, abstract, keywords, sep = "; "))) %>% 
  mutate(text_all_mod_lower = gsub("[- ]?\\bon[- ]chips?\\b|[- ]?\\bon[- ]a[- ]chip\\b", " onchip", text_all_mod_lower))

### The above data object may be saved.
### save(unique_oc_F, file = paste0(root_path, "R_temps/unique_oc_update"))






##########
###
### 5. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
###
### This step select organ-on-a-chip documents from the on-chip corpus, by choosing documents where 
### organ names or equivalents occur right before "onchip".
### For this purpose, all words that occur before "onchip" are extracted, manually inspected, and 
### organ names and equivalents are selected for consideration.
###
##########

### Extracting all occurrences of the term "onchip", along with a word before it.
pre_onchip_all <- unlist(str_extract_all(unique_oc_F$text_all_mod_lower, "[a-z0-9]+\\)? onchip\\b"))

### Removing duplicates from the above, also removing " onchip"
pre_onchip_unique <- unique(gsub("\\)? onchip", "", pre_onchip_all))

### Making a data frame of the extracted words.
### A column "include" is made.
pre_onchip_all_df <- data.frame(pre_word = pre_onchip_unique, include = "") 

pre_onchip_all_update <- rbind(pre_onchip_all_F, pre_onchip_all_df %>% filter(!pre_word %in% pre_onchip_all_F$pre_word))

### And saving it as a .csv file
write.csv(pre_onchip_all_update, file = paste0(root_path, "csv/temps/pre_onchip_all_update.csv"), row.names = FALSE)
### This csv file was opened on Excel and manually checked, and the "include" column was filled with either 
### "c", "n", "y", or "o" meaning "consider", "not to include as an organ name", "include as an organ name", or "organism".
### "consider" here means that a word further before was also checked and considered so that, for example, 
### "rubber tube-on-chip" may not be included but "fallopian tube-on-chip" is included.
### We did not include terms classified as "organism" in the end.
### The checked file was saved as "./csv/pre_onchip_all_F.csv"


###
### 5.1. Selecting organ-on-a-chip publications.
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







##########
###
### 6. Combining the organ-on-a-chip and microphysiological system corpora.
###
##########

### Comparing the dimensions of the organ-on-a-chip and microphysiological system corpora.
dim(data_ooc)
## [1] 1200  109

dim(unique_ms_F)
## [1] 322 103

### These two corpora need to have identical columns in order for them to be combined.
###
### Selecting columns of the organ-on-a-chip corpus that also exist in the microphysiological system corpus.
combined_ooc <- rbind(data_ooc %>% 
                        select(any_of(colnames(unique_ms_F))), 
                      unique_ms_F %>% 
                        select(any_of(colnames(data_ooc)))) %>% 
  ### Giving fresh ID numbers to each document, starting from 30000 so that the numbers do not match with IDs in the existing corpus.
  mutate(ID = c(1:nrow(.))) %>% 
  mutate(ID = ID + 30000) %>% 
  ### Discarding the columns used for deduplication, as they would conflict with subsequent deduplication.
  select(!c("match", "n_duplicates"))

dim(combined_ooc)
## [1] 1522  101

### save(combined_ooc, file = paste0(root_path, "R_temps/combined_ooc"))





##########
###
### 7. Deduplicating the combined organ-on-a-chip corpus
###
##########

### Removing extra copies of documents from Web of Science from the combined corpus.
unique_ooc <- fn_wos_deduplication(combined_ooc) %>% 
  fn_all_deduplication(.)

### save(unique_ooc, file = paste0(root_path, "R_temps/unique_ooc"))

### The metadata of the new publications are combined with the old corpus, and then duplicate entries are removed.
### There are two types of duplicate entries to be considered.
### 1. The same publication with the same DOI.
### 2. The same publication, but with different DOI (e.g., DOI being changed upon publication of a printed version)
###
### For 1, the original publication metadata in the existing corpus will be kept.
### For 2, the new publication metadata will be kept, with "ID" column changed to corresponding ID number.

colnames(all_corpus)

### Selecting OoC publications from the existing corpus, and then selecting required columns.
ooc_selected_columns <- all_corpus %>% 
  filter(corpus == "OoC") %>% 
  select(c(1:9, 11:15)) 
  
### Combining new publication metadata and the existing corpus.
ooc_doi_unique <- ooc_selected_columns %>% 
  rbind(., unique_ooc %>% 
          select(any_of(colnames(ooc_selected_columns)))) %>% 
  ### Identifying duplicate publications based on DOI.
  mutate(duplicate = duplicated(.["doi"])) %>% 
  ### Removing duplicate publications.
  filter(!(!is.na(doi) & duplicate)) %>% 
  select(!duplicate)

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
duplicates_ooc <- fn_find_duplicates_title(ooc_doi_unique)

### Saving the data object as a csv file.
duplicates_ooc %>% mutate(status = "") %>% 
  write.csv(., file = paste0(root_path, "csv/temps/duplicates_ooc_update.csv"), row.names = FALSE)

### The above csv file was manually checked, and the "status" column was filled with either: 
### "k" when the document has extra copies but is chosen to be the copy to be kept, 
### "r" when the document has extra copies and is to be removed,
### "u" when the document is unique and has to be kept.
### The manually checked file was saved as ./csv/duplicates_ooc_checked.csv and loaded below.
duplicates_ooc_checked <- read.csv(paste0(root_path, "csv/duplicates_ooc_update_checked.csv"))

### Selecting papers to remove
duplicates_ooc_to_remove <- duplicates_ooc_checked %>% 
  filter(status == "r")

### Removing duplicate papers to remove.
unique_ooc_F <- unique_ooc %>% 
  ### Only keeping publications that remained after DOI-based deduplication.
  filter(ID %in% ooc_doi_unique$ID) %>% 
  ### Removing publications that were identified as duplicates by title-based deduplication.
  filter(!ID %in% duplicates_ooc_to_remove$ID) 

dim(unique_ooc_F)
## [1] 683 103
save(unique_ooc_F, file = paste0(root_path, "R_temps/unique_ooc_update"))





##########
###
### 8. Reformatting the organ-on-a-chip corpus and adding it to the existing corpus
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


ooc_corpus_update <- ooc_reformatted %>% 
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
  mutate(text_all_key = paste(title_key, keywords_key, abstract_key, sep = "; ")) %>% 
  mutate(corpus = "OoC", 
         phase = "later")


colnames(all_corpus)
colnames(ooc_corpus_update)

### Adding the new reformatted OoC corpus to the existing OoC corpus.
###
### Selecting OoC corpus from the existing corpus.
ooc_corpus <- all_corpus %>% 
  filter(corpus == "OoC") %>% 
  ### Replacing the "year" column of the existing corpus with the column from the new corpus.
  left_join(., unique_ooc %>% 
              rename(year_new = year) %>% 
              filter(!is.na(doi)) %>% 
              select(doi, year_new), 
            by = "doi") %>% 
  mutate(year = ifelse(!is.na(year_new), year_new, year)) %>% 
  select(!year_new) %>% 
  ### Adding publications that do not exist in the existing corpus.
  rbind(., ooc_corpus_update)

dim(ooc_corpus)
## [1] 4303   27

### The above data object may be saved.
### save(ooc_corpus, file = paste0(root_path, "R_temps/ooc_corpus_updated"))




##########
###
### 9. Importing, reformatting, and deduplicating literature metadata on organoids, and adding it to the existing corpus
###
### Note that initially some of the files could not be imported into R by "read_bibliography()" for an unknown reason.
### To overcome this issue, publications from PubMed were imported as 1) reviews only, 2) reviews + research articles.
### Therefore, reviews from PubMed are duplicated, and had to be deduplicated at a later point.
### Same goes for blastoid metadata from EMBASE.
### 
##########

### Setting paths of files to import.
path_or <- paste0(root_path, "raw_data_update/organoid/")

file_names_or <- paste0(path_or, list.files(path = path_or))

### Importing the literature files.
data_all_or <- read_bibliography(file_names_or)

### Cleaning and deduplicating the corpus.
unique_or <- fn_cleanup_metadata(data_all_or) %>% 
  mutate(ID = ID + 40000) %>% 
  fn_wos_deduplication(.) %>% 
  fn_all_deduplication(.)

### The above data object may be saved.
### save(unique_or, file = paste0(root_path, "R_temps/unique_or_update"))

colnames(all_corpus)

### From the existing corpus, selecting the organoid publications and selecting required columns.
or_selected_columns <- all_corpus %>% 
  filter(corpus == "organoid") %>% 
  select(c(1:9, 11:15)) 

### Combining the new publications and the existing publications.
or_doi_unique <- or_selected_columns %>% 
  rbind(., unique_or %>% 
          select(any_of(colnames(or_selected_columns)))) %>% 
  mutate(duplicate = duplicated(.["doi"])) %>% 
  filter(!(!is.na(doi) & duplicate)) %>% 
  select(!duplicate)

### Finding duplicate publications based on the titles.
### This process may take an hour or so.
duplicates_or <- fn_find_duplicates_title(or_doi_unique)

### Saving the data object as a csv file.
duplicates_or %>% mutate(status = "") %>% 
  write.csv(., file = paste0(root_path, "csv/temps/duplicates_or_update.csv"), row.names = FALSE)

### The above csv file was manually checked, and the "status" column was filled with either: 
### "k" when the document has extra copies but is chosen to be the copy to be kept, 
### "r" when the document has extra copies and is to be removed,
### "u" when the document is unique and has to be kept.
### The manually checked file was saved as ./csv/duplicates_ooc_checked.csv and loaded below.
duplicates_or_checked <- read.csv(paste0(root_path, "csv/duplicates_or_update_checked.csv"))

### Selecting papers to remove
duplicates_or_to_remove <- duplicates_or_checked %>% 
  filter(status == "r")

### Only keeping unique publications.
unique_or_F <- unique_or %>% 
  ### Only keeping publications that remained after DOI-based deduplication.
  filter(ID %in% or_doi_unique$ID) %>% 
  ### Removing publications detected by title-based deduplication
  filter(!ID %in% duplicates_or_to_remove$ID) 

dim(unique_or_F)
## [1] 2963  113



###
### 9.1 Cleaning up the organoid corpus.
###

### Applying custom functions for keywords/abstract adjustment and reformatting.
pre_organoid_corpus <- fn_keywords_abstract_adjustment(unique_or_F) %>% 
  fn_reformatting(.)

### Papers on "nevus sebaceus" (aka "organoid nevus") are removed.
### These papers are retrieved primarily from PubMed even if they don't contain the term "organoid", because of how MeSH works.
### First "nevus sebaceus" and its synonyms are stored in a vector.
nevus_sebaceus_w <- c("na?evus sebaceo?us", "sebaceo?us na?evus", "sebaceo?us na?evi", 
                      "organoid na?evus", "organoid na?evi", "angora hair na?evus", 
                      "angora hair na?evi", "organoid epidermal na?evus", 
                      "organoid epidermal na?evi")

### Removing papers containing any of the above terms in the vector.
pre_organoid_corpus2 <- pre_organoid_corpus %>% 
  filter(!grepl(paste(nevus_sebaceus_w, collapse = "|"), text_all, ignore.case = TRUE))

dim(pre_organoid_corpus2)
### [1] 2841   20

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
organoid_corpus_update <- pre_organoid_corpus2 %>% 
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
  mutate(text_all_key = paste(title_key, keywords_key, abstract_key, sep = "; ")) %>% 
  mutate(corpus = "organoid", 
         phase = "later")

organoid_corpus <- all_corpus %>% 
  ### From the existing corpus, selecting organoid publications.
  filter(corpus == "organoid") %>% 
  ### If a document has a copy in both existing and newly added corpus, the "year" was taken from the latter.
  left_join(., unique_or %>% 
              rename(year_new = year) %>% 
              filter(!is.na(doi)) %>% 
              select(doi, year_new), 
            by = "doi") %>% 
  mutate(year = ifelse(!is.na(year_new), year_new, year)) %>% 
  select(!year_new) %>% 
  ### There is one publication whose DOI changed from an E-print DOI to a print-version DOI.
  ### For this publication, the newly obtained record is used.
  ###
  ### Deleting the existing record of this publication.
  filter(!ID == 1334) %>% 
  ### Adding newly obtained publications.
  rbind(., organoid_corpus_update) %>% 
  ### Changing the ID of the new record of the publication.
  mutate(ID = ifelse(ID == 45554, 1334, ID)) %>% 
  arrange(ID)

colnames(organoid_corpus)

### The above data object may be saved.
### save(organoid_corpus, file = paste0(root_path, "R_temps/organoid_corpus_updated"))



##########
###
### 10. Combining orgnaoid and organ-on-a-chip corpora
###
##########


### Combining organoid and organ-on-a-chip corpora.
all_corpus_new <- rbind(organoid_corpus, ooc_corpus) %>% 
  arrange(ID) %>% 
  ### Reassigning IDs.
  mutate(ID = c(1:nrow(.)))

### To be only saved after checking that the above data frame looks good.
### all_corpus <- all_corpus_new
### save(all_corpus, file = paste0(root_path, "R_results/all_corpus"))
