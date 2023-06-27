### This R script file is for identifying cell sources used to generate organoid/organ-on-a-chip models.
###
### The file uses "./R_results/all_corpus" as an input.
###
### The file includes following steps: 
### 1. Identifying terms to capture.
### 2. Listing terms to capture.
### 3. Capturing the terms in titles.
### 4. Capturing the terms in keywords_abstract 
### 5. Assigning the cell source classification.
### 
### The outcome of the codes are saved as:
### ./R_results/cell_types_F

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/all_corpus"))






##########
###
### 1. Identifying terms to capture.
###
### Specifically, words before "derived stem cell", "derived organoid", and "stem cell" will be looked at.
###
##########

### !!! Note !!! 
### This step is for manually selecting terms to consider as cell sources.
### If you want to skip this step, ignore following lines and resume 
### at: 2. Listing terms to capture.


### Loading the tidytext package.
library(tidytext)

### To facilitate the process, "cells" and "organoids" are converted to their respective singular forms.
### Parentheses are deleted.
###
### Sentences are split into chunks at either ".", ",", ";", or ":".
phrases <- all_corpus %>% 
  mutate(text_all2 = gsub("\\bcells\\b", "cell", text_all_mod, ignore.case = TRUE)) %>% 
  mutate(text_all2 = gsub("\\borganoids\\b", "organoid", text_all2, ignore.case = TRUE)) %>%  
  mutate(text_all2 = gsub("\\(|\\)", "", text_all2)) %>% 
  select(ID, text_all2) %>% 
  unnest_tokens(phrase, text_all2, token = stringr::str_split, pattern = "\\.|,|;|:") 

### Tetragrams (four words occurring together) ending with "derived stem cell" are selected.
derived_stem_cell <- phrases %>% 
  unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
  filter(word4 == "cell") %>% 
  filter(word3 == "stem") %>% 
  filter(word2 == "derived") %>% 
  filter(!word1 %in% stop_words$word)

### Showing the words that occurred before "derived stem cell".
unique(derived_stem_cell$word1)

### From the above words, following words were selected.
### If these words appear before "derived stem cell", then the cell source is considered to be "adult stem cell".
pre_derived_stem_w <- c("patient", "adipose", "gingiva", "tissue", "marrow", "urine", "gland", "adult", "crypt", "muscle", 
                        "adipocyte", "skin")



### Trigrams ending with "derived organoid" are selected.
derived_organoid <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(word3 == "organoid") %>% 
  filter(word2 == "derived") %>% 
  filter(!word1 %in% stop_words$word)

### Making a data frame of words that occurred before "derived organoid".
pre_derived_organoid <- data.frame(pre_words = unique(derived_organoid$word1), include = "")

### Saving as a csv file.
write.csv(pre_derived_organoid, file = paste0(root_path, "csv/temps/pre_derived_organoid.csv"), row.names = FALSE)
### This file was manually checked, and the "include" column was filled with either "y", "n", or "t", 
### which respectively mean "include as tissue", "not to include", or "include as tumor" as a cell source.
### The file was then saved as "./csv/pre_derived_organoid_F.csv".

### Trigrams ending with "stem cell"
stem_cell <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(word3 == "cell") %>% 
  filter(word2 == "stem") %>% 
  filter(!word1 %in% stop_words$word)


### Making a data frame from the above vector
pre_stem <- data.frame(pre_words = unique(stem_cell$word1), include = "")

### Saving as a csv file.
write.csv(pre_stem, file = paste0(root_path, "csv/temps/pre_stem.csv"), row.names = FALSE)
### This file was manually checked, and the "include" column was filled with either "y", "n", or "t", 
### which respectively mean "include as adult stem cell", "not to include", or "include as tumor" as a cell source.
### The file was then saved as "./csv/pre_stem_F.csv".









##########
###
### 2. Listing terms to capture.
###
##########

### For words before "derived stem"
pre_derived_stem_w <- c("patient", "adipose", "gingiva", "tissue", "marrow", "urine", "gland", "adult", "crypt", "muscle", 
                        "adipocyte")

### loading a previous csv file of words before "derived organoid"
pre_derived_organoid_F <- read.csv(paste0(root_path, "csv/pre_derived_organoid_F.csv"))

### Selecting words with include == "y", to be considered as "tissue" when followed by "derived organoid"
pre_derived_organoid_inc <- pre_derived_organoid_F %>% 
  filter(include == "y")

### Selecting words with include == "t", to be added to oncology terms below.
pre_derived_organoid_tum <- pre_derived_organoid_F %>% 
  filter(include == "t")

### loading a previous csv file of words before "stem cell"
pre_stem_F <- read.csv(paste0(root_path, "csv/pre_stem_F.csv"))

### Selecting words with include == "y", to be considered as "adult stem cell" when followed by "stem cell"
pre_stem_inc <- pre_stem_F %>% 
  filter(include == "y")

### Selecting words with include == "t", to be added to oncology terms below.
pre_stem_tum <- pre_stem_F %>% 
  filter(include == "t")

### Oncology words. They are considered to be "tumor" as a cell source when followed by either 
### "cell", "stem cell", "organoid", or "derived organoid".
tumor_w <- unique(c("cancers?", "tumou?rs?", "[a-z]*omas?", "neoplasms?", "neoplastic", 
                    "metastas[ei]s", "metastatic", pre_stem_tum$pre_words, pre_derived_organoid_tum$pre_words))

### Making a nested list containing words for cell sources to be captured.
cell_source_words <- list(
  ### The following four groups of terms are for capturing stem cell terms when they appear in combinations.
  iPS_ESC_w = c("induced pluripotent,? and embryonic stem cell", 
                "embryonic,? and induced pluripotent stem cell"), 
  iPS_ASC_w = c("induced pluripotent,? and adult stem cell", 
                         "adult,? and induced pluripotent stem cell"), 
  ESC_ASC_w = c("embyonic,? and adult stem cell", 
                "adult,? and embyonic stem cell"), 
  iPS_ESC_ASC_w = c("induced pluripotent, embryonic,? and adult stem cell", 
                    "induced pluripotent, adult,? and embryonic stem cell", 
                    "embryonic, induced pluripotent,? and adult stem cell", 
                    "embryonic, adult,? and induced pluripotent stem cell", 
                    "adult, induced pluripotent,? and embryonic stem cell", 
                    "adult, embryonic,? and induced pluripotent stem cell"), 
  iPSC_w = c("induced[- ]pluripotent[- ]stem", "\\b[hm]?ipscs?\\b", "\\b[hm]?ips\\)? cell"), 
  ESC_w = c("embryonic[- ]stem", "\\b[hm]?escs?\\b", "\\b[hm]?es\\)? cell"), 
  ### Note that, below, "ASCs" is not considered as adult stem cells, as it's often used as an abbreviations for different terms.
  ASC_w = c("adult[- ][a-z]* ?stem", 
            paste0("\\b", pre_derived_stem_w, "[- ]derived stem cell"), 
            paste0("\\b", pre_stem_inc$pre_words, "[- ]stem cell")), 
  PSC_w = c("pluripotent stem", "\\b[hm]?pscs?\\b", "\\b[hm]?ps\\)? cell"), 
  PC_w = c("progenitor cells?"), 
  TC_w = c(paste0("\\b", tumor_w, "[- ]cell"), 
           paste0("\\b", tumor_w, "[- ]stem cell"), 
           paste0("\\b", tumor_w, "[- ]organoid"), 
           paste0("\\b", tumor_w, "[- ]derived organoid")), 
  Tissue_w = paste0("\\b", pre_derived_organoid_inc$pre_words, "[- ]derived organoid"), 
  SC_w = c("(?<!mesenchymal )stem cells?")
  ) %>% 
  lapply(., paste, collapse = "|")

### Checking the above list.
print(cell_source_words)








##########
###
### 3. Capturing the terms in titles.
###
##########

### Capturing the terms for cell sources in the "title_key" column that contains titles that include the term organoid/onchip.
TF_cell_title <- as.data.frame(sapply(cell_source_words, function(x) grepl(x, all_corpus$title_key, ignore.case = TRUE, perl = TRUE)))

colnames(TF_cell_title)

### Adjusting the above logical data frame.
### For example, iPSC_w == TRUE if the paper is TRUE for iPS_ESC_w.
### PSC_w == TRUE if the paper is TRUE for either iPSC or ESC.
cell_title_modified <- TF_cell_title %>% 
  mutate(iPSC_w = ifelse(iPS_ESC_w == TRUE | iPS_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, iPSC_w), 
         ESC_w = ifelse(iPS_ESC_w == TRUE | ESC_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, ESC_w), 
         ASC_w = ifelse(iPS_ASC_w == TRUE | ESC_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, ASC_w), 
         PSC_w = ifelse(rowSums(.[, 5:6]) > 0, TRUE, PSC_w))

### Renaming the columns
cell_title_modified2 <- cell_title_modified %>% 
  rename_with(~ gsub("_w", "_title", .))




##########
###
### 4. Capturing the terms in keywords_abstract 
###
##########

### Doing the same for keywords-abstracts.
###
### Capturing the terms for cell sources in the "keywords_abstract_key" column that contains keywords and abstract 
### that include the term organoid/onchip.
TF_cell_KA <- as.data.frame(sapply(cell_source_words, function(x) grepl(x, all_corpus$keywords_abstract_key, ignore.case = TRUE, perl = TRUE)))

colnames(TF_cell_KA)

### Adjusting the logical data frame.
cell_KA_modified <- TF_cell_KA %>% 
  mutate(iPSC_w = ifelse(iPS_ESC_w == TRUE | iPS_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, iPSC_w), 
         ESC_w = ifelse(iPS_ESC_w == TRUE | ESC_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, ESC_w), 
         ASC_w = ifelse(iPS_ASC_w == TRUE | ESC_ASC_w == TRUE | iPS_ESC_ASC_w == TRUE, TRUE, ASC_w), 
         PSC_w = ifelse(rowSums(.[, 5:6]) > 0, TRUE, PSC_w))

### Renaming the columns
cell_KA_modified2 <- cell_KA_modified %>% 
  rename_with(~ gsub("_w", "_KA", .))



##########
###
### 5. Assigning the cell source classification.
###
##########

### Adding the above logical data frames to the corpus data frame, excluding columns for stem cell combinations.
TF_cell_combined <- all_corpus %>% 
  cbind(., cell_title_modified2[, 5:12], cell_KA_modified2[, 5:12])

colnames(TF_cell_combined)

### Combining the results of term occurrence in titles and keywords_abstracts.

pre_cell_types <- TF_cell_combined %>% 
  ### The logical value for the term occurrence in keywords_abstract is changed to FALSE 
  ### if any of the cell source terms (except just "stem cells" which is not specific) occur in titles.
  mutate(across(c(36:42), ~ ifelse(rowSums(TF_cell_combined[, 28:34]) > 0, FALSE, .))) %>% 
  ### Making new columns of cell source categories. 
  ### Essentially, the column is TRUE if the corresponding cell source term appears in either title or keywords_abstracts,
  mutate(TF_induced_pluripotent_stem_cell = as.logical(rowSums(across(starts_with("iPSC_"))))) %>% 
  mutate(TF_embryonic_stem_cell = as.logical(rowSums(across(starts_with("ESC_"))))) %>% 
  mutate(TF_adult_stem_cell = as.logical(rowSums(across(starts_with("ASC_"))))) %>% 
  mutate(TF_progenitor_cell = as.logical(rowSums(across(starts_with("PC_"))))) %>% 
  mutate(TF_tissue = as.logical(rowSums(across(starts_with("Tissue_"))))) %>% 
  ### "pluripotent stem cell" is TRUE if a document is TRUE for both or either of induced pluripotent stem cell and/or embryonic stem cell.
  mutate(TF_pluripotent_stem_cell = ifelse(rowSums(.[, 44:45]) > 0, TRUE, 
                                           as.logical(rowSums(across(starts_with("PSC_")))))) %>% 
  ### A new category "adult cell" is added as a column, which is TRUE if the paper is TRUE for any of "adult stem cell", 
  ### "progenitor cell", or "tissue".
  mutate(TF_adult_cell = as.logical(rowSums(.[, 46:48]))) %>% 
  mutate(TF_tumor_cell = as.logical(rowSums(across(starts_with("TC_"))))) %>% 
  ### "Stem cell is TRUE if the document is TRUE for any of stem cell types.
  mutate(TF_stem_cell = ifelse(rowSums(.[44:50]) > 0 | SC_title == TRUE, TRUE, SC_KA))

colnames(pre_cell_types)

### Assigning summary classifications.
### Essentially, the classification is "mixed" when more than one type of stem cells are used.
### Category "unspecified stem cell" is used only when none of other categories are applied.
### cell_type_simplified column is a simplified version where "adult stem cell", "progenitor cell", and "tissue" are 
### combined together as "adult cell".
cell_types_F <- pre_cell_types %>% 
  mutate(cell_type = 
           ifelse(
             rowSums(.[, c(46:49, 51)]) > 1, "mixed", 
             ifelse(rowSums(.[, c(46:49, 51)]) == 1, colnames(.[, c(46:49, 51)])[max.col(.[, c(46:49, 51)])], 
                    ifelse(TF_stem_cell == TRUE, "unspecified stem cell", NA))
           )) %>% 
  mutate(cell_type = 
           ifelse(cell_type == "TF_pluripotent_stem_cell" & rowSums(.[, 44:45]) == 1, colnames(.[, 44:45])[max.col(.[, 44:45])], 
                  cell_type)) %>% 
  mutate(cell_type_simplified = 
           ifelse(
             rowSums(.[, 49:51]) > 1, "mixed", 
             ifelse(rowSums(.[, 49:51]) == 1, colnames(.[, 49:51])[max.col(.[, 49:51])], 
                    ifelse(TF_stem_cell == TRUE, "unspecified stem cell", NA))
           )) %>% 
  mutate(cell_type_simplified = 
           ifelse(cell_type_simplified == "TF_pluripotent_stem_cell" & rowSums(.[, 44:45]) == 1, colnames(.[, 44:45])[max.col(.[, 44:45])], 
                  cell_type_simplified)) %>% 
  mutate(across(c(53:54), ~ gsub("TF_", "", .))) %>% 
  mutate(across(c(53:54), ~ gsub("_", " ", .)))


### Saving the result.
save(cell_types_F, file = paste0(root_path, "R_results/cell_types_F"))

### Loading an R data file that shows which publications are on organoids/OoCs, rather than on tumor organoids/ToCs.
load(paste0(root_path, "R_results/organ_types_P"))

### Removing text fields.
### Also removing tumor organoids and ToC corpora.
cell_types_P <- cell_types_F %>% 
  filter(ID %in% organ_types_P$ID) %>% 
  select(!c(7:25))

save(cell_types_P, file = paste0(root_path, "R_results/cell_types_P"))
