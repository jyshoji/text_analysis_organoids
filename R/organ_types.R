### This R script file is for identifying organ types modelled as organoid or 
### organ-on-chip in academic publications
###
### In short, we first identified organ names and their equivalents that needed capturing.
### Here, "organ name equivalents" mean terms that are indicative of organ names, such as "glioblastoma (brain)", "hepatocytes (liver)". 
### Subsequently, the organ names and their equivalents were extracted separately from:
### a) A few words before organoid/onchip in titles
### b) A few words before organoid/onchip in keywords_abstracts
### c) Whole sentence containing organoid/onchip of titles
### d) Whole sentences containing organoid/onchip in keywords_abstracts
### and organ types are determined based on the occurring organ names, with higher priority given to the earlier step.
###
### The file uses "./R_results/all_corpus" as an input.
###
### The file includes following steps: 
### 1. Identifying organ names that appear immediately before "organoid" or "onchip"
### 2. Reshaping the "all_corpus" data frame to facilitate later extraction of organ names.
### 3. Extracting a few words before "organod", "assembloid", or "onchip"
### 4. Listing words to capture
### 5. Capturing full terms of organ names in the combined text field.
### 6. Capturing all terms (i.e., full terms and abbreviations) of organ names in pw (a few words before organoid/onchip) in titles
### 7. Capturing all terms in pw in keywords_abstracts
### 8. Capturing all terms in key sentences in titles (i.e., titles that includes the term "organoid" or "onchip").
### 9. Capturing all terms in key sentences of keywords_abstracts
### 10. Assigning the final classification
###
### The outcome of the codes are saved as:
### "./R_results/organ_types"
### which were used for many of following analysis.



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
### 1. Identifying organ names that appear immediately before "organoid" or "onchip"
###
##########

### !!! Note !!!
### This step is to manually select terms to consider as organ names or their equivalents.
### If you want to skip the manual selection, ignore the following lines and resume 
### at "2. Reshaping the "all_corpus" data frame to facilitate later extraction of organ names."

colnames(all_corpus)

### Only selecting organoid publications
organoid_corpus <- all_corpus %>% 
  filter(corpus == "organoid")

### Only selecting organ-on-a-chip publications
onchip_corpus <- all_corpus %>% 
  filter(corpus == "OoC")

### Extracting "any word + organoid" from the key sentence column of the organoid corpus, 
### which contains all sentences that contain the term "organoid".
pre_words_organoid <- unlist(str_extract_all(tolower(organoid_corpus$text_all_key), pattern = "[a-z0-9]+\\)?[- ]organoid"))

### Removing the word "organoid"
pre_words_organoid2 <- gsub("\\)?[- ]organoid", "", pre_words_organoid)

### Extracting "any word + onchip" from the key sentence column of the onchip corpus, 
### which contains all sentences that contain the term "onchip".
pre_words_onchip <- unlist(str_extract_all(tolower(onchip_corpus$text_all_key), pattern = "[a-z0-9]+\\)? onchip"))

### Removing the word "onchip"
pre_words_onchip2 <- gsub("\\)? onchip", "", pre_words_onchip)

### Combining the above two word lists, and discarding duplicate entries.
pre_words_all <- unique(c(pre_words_organoid2, pre_words_onchip2))

### Making a data frame of the combined word list, with a new column "include" which is later used to mark whether or not 
### the corresponding word should be considered as an organ name or equivalent.
pre_words_list <- data.frame(ID = c(1:length(pre_words_all)), pre_word = pre_words_all) %>% 
  mutate(include = "") 

### The data frame of the word list is saved as a csv file.
write.csv(pre_words_list, file = paste0(root_path, "csv/temps/pre_words_list.csv"), row.names = FALSE)
### This file is manually checked, and the "include" column is filled with either "y", "n", or "c", which respectively mean 
### "include as an organ name", "not to include as an organ name", and "consideration needed".
### Words with include == "c" are possible part of two-words organ names, such as "network", "gland".
###
### The result is saved as:
### "./csv/pre_words_F.csv"

### The code below is to update the existing word list.
### pre_words_F <- read.csv(paste0(root_path, "csv/pre_words_F.csv"))
### pre_words_list[, 1:2] %>% 
###   left_join(., pre_words_F[, 2:3], by = "pre_word") %>% 
###   arrange(include) %>% 
###   write.csv(., file = paste0(root_path, "csv/temps/pre_words_list.csv"), row.names = FALSE)




##########
###
### 2. Reshaping the "all_corpus" data frame to facilitate later extraction of organ names.
### 
##########
colnames(all_corpus)

### Making new columns where texts are in lower cases
all_corpus_lower <- all_corpus %>% 
  mutate(title_lower = tolower(title_mod)) %>% 
  mutate(KA_lower = tolower(keywords_abstract_mod)) %>% 
  mutate(text_all_lower = tolower(text_all_mod)) %>% 
  mutate(title_key_lower = tolower(title_key)) %>% 
  mutate(KA_key_lower = tolower(keywords_abstract_key))

colnames(all_corpus_lower)

### Two-word organ names are force-united to be one words in the lower-case columns, in order to facilitate following steps.
### These two-word organ names were identified by taking a closer look at "./csv/pre_words_F.csv", 
### where "include" == "c".
### Organoids for specific organs (e.g., enteroids) were changed to "organ name + organoid" (e.g., smallintestine organoid)
modified_corpus <- all_corpus_lower %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "\\)?[- ]organoid", " organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "pancreatic[- ]ductal|pancreatic[- ]ducts?", "pancreaticduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "liver[- ]ductal|liver[- ]ducts?|bile[- ]ductal|bile[- ]ductular|bile[- ]ducts?|hepatobiliary[- ]ductal|hepatobiliary[- ]ducts?", "bileduct"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "collecting[- ]ductal|collecting[- ]ducts?", "collectingduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "breast[- ]ductal|breast[- ]ducts?", "breastduct"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "biliary[- ]tracts?", "biliarytract"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "reproductive[- ]tracts?", "reproductivetract"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "fallopian[- ]tubes?|uterine[- ]tubes?", "oviduct"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neural[- ]tubes?", "neuraltube"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "renal[- ]tubules?|renal[- ]tubular|kidney[- ]tubules?|kidney[- ]tubular", "renaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "proximal[- ]tubules?", "proximaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "distal[- ]tubules?", "distaltubule"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "salivary[- ]glands?|salivary[- ]glandular", "salivarygland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "parotid[- ]glands?|parotid[- ]glandular", "parotidgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "submandibular[- ]glands?|submandibular[- ]glandular", "submandibulargland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lacrimal[- ]glands?|lacrimal[- ]glandular", "lacrimalgland"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "venom[- ]glands?|venom[- ]glandular", "venomgland"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "endometrial[- ]glands?|endometrial[- ]glandular", "endometrialgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "gastric[- ]glands?|gastric[- ]glandular", "gastricgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "mammary[- ]glands?|mammary[- ]glandular", "mammarygland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "sebaceous[- ]glands?|sebaceous[- ]glandular", "sebaceousgland"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "sweat[- ]glands?|sweat[- ]glandular|eccrine[- ]glands?|eccrine[- ]glandular", "sweatgland"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "thyroid[- ]glands?|thyroid[- ]glandular", "thyroidgland"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "small[- ]intestinal|small[- ]intestines?", "smallintestine"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "large[- ]intestinal|large[- ]intestines?", "largeintestine"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "vascular[- ]networks?", "vascularnetwork"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "corticostriatal[- ]networks?", "corticostriatalnetwork"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "corneal[- ]barriers?", "cornealbarrier"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]brain[- ]barriers?", "bloodbrainbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]retinal?[- ]barriers?", "bloodretinalbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]air[- ]barriers?|air[- ]blood[- ]barriers?|alveolar[- ]capillary barriers?", "bloodairbarrier"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "placental[- ]barriers?", "placentalbarrier"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "basal ganglion|basal ganglia", "basalganglion"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "dorsal root ganglion|dorsal root ganglia|sensory ganglion|sensory ganglia", "dorsalrootganglion"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "blood[- ]vessels?", "bloodvessel"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lymph[- ]vessels?|lymphatic[- ]vessels?|lymphatic[- ]vasculatures?", "lymphaticvessel"))) %>%     
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "tumou?r[- ]vessels?", "tumorvessel"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "perivascular[- ]niches?", "perivascularniche"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "metastatic[- ]niches?", "metastaticniche"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "taste[- ]buds?", "tastebud"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "circumvallate[- ]papillae?", "circumvallatepapilla"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "spinal[- ]cords?", "spinalcord"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "optic[- ]cups?|retinal?[- ]cup", "opticcup"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "bone[- ]marrows?", "bonemarrow"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "synovial[- ]joints?", "synovialjoint"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "gall[- ]bladder", "gallbladder"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "choroid[- ]plexus", "choroidplexus"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "dental pulps?", "dentalpulp"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neuromuscular[- ]junctions?", "neuromuscularjunction"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "neurovascular[- ]units?", "neurovascularunit"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "lymph[- ]nodes?|lymph[- ]glands?|lymph[- ]glandular", "lymphnode"))) %>%    
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "fetal[- ]membranes?", "fetalmembrane"))) %>%  
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "optic[- ]vesicles?", "opticvesicle"))) %>%   
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "enteroid", "smallintestine organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "colonoid", "colon organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "embryoid", "embryonic organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "cerebroid", "cerebral organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "cardioid", "heart organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "tumou?roid", "tumor organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "organoids\\b", "organoid"))) %>% 
  mutate(across(ends_with("_lower"), 
                ~ str_replace_all(., "assembloids\\b", "assembloid")))


### The resulting data frame may be saved.
save(modified_corpus, file = paste0(root_path, "R_temps/modified_corpus"))

### Making a vector containing the force-united organ names.
pre_2words_w <- c("pancreaticduct", "bileduct", "collectingduct", "breastduct", "biliarytract", "reproductivetract", 
                 "oviduct", "neuraltube", "renaltubule", "proximaltubule", "distaltubule", "salivarygland", "parotidgland", 
                 "submandibulargland", "lacrimalgland" , "venomgland" , "endometrialgland", "gastricgland", "mammarygland", 
                 "sebaceousgland", "sweatgland", "thyroidgland", "smallintestine", "largeintestine", "vascularnetwork", 
                 "corticostriatalnetwork", "cornealbarrier", "bloodbrainbarrier", "placentalbarrier", "dorsalrootganglion", 
                 "bloodvessel", "lymphaticvessel", "tumorvessel", "perivascularniche", "metastaticniche", "tastebud", 
                 "circumvallatepapilla", "spinalcord", "opticcup", "bonemarrow", "synovialjoint", "choroidplexus", 
                 "dentalpulp", "neuromuscularjunction", "neurovascularunit", "lymphnode", "fetalmembrane", "opticvesicle", 
                 "bloodairbarrier")

### Loading the list of words before organoid/onchip, generated in the above section 1.
pre_words_F <- read.csv(paste0(root_path, "csv/pre_words_F.csv"))

### Selecting words to be considered as organ names.
pre_inc_pre <- pre_words_F %>% 
  filter(include %in% c("y", "c"))

### Combining the above list of words with the list of force-united organ names.
pre_inc2_w <- c(pre_inc_pre$pre_word, pre_2words_w)
### This lists all the words that will be considered as organ names.






##########
###
### 3. Extracting a few words before "organod", "assembloid", or "onchip"
###
### In later steps, organ names will be captured using either strict or generous approaches.
### The strict approach will consider organ names only when they appear immediately (within three words) before "organoid" or "onchip" 
### (e.g., brain tumor organoid), whereas the generous approach will consider organ names if they appear 
### anywhere in the same sentence as organoid/onchip (e.g., organoid with a brain tumor origin).
### The current step aims to extract a few words before organoid/onchip for the strict approach.
###
### Note that now this process uses the tidytext package, meaning that all hyphens are removed from resulting columns.
###
##########

library(tidytext)


### Extracting a few words before "organoid" or "assembloid" from each document of the organoid corpus, 
### and a few words before "onchip" from each document of the onchip corpus.

### First, selecting ID, corpus, and title columns, and then each title is split into chunks at either ., ;, or :.
title_phrases <- modified_corpus %>% 
  select(ID, corpus, title_key_lower) %>% 
  filter(!is.na(title_key_lower)) %>% 
  unnest_tokens(phrase, title_key_lower, token = stringr::str_split, pattern = "\\.|;|:")

### Similarly, each keywords_abstracts is split into chunks at either ., ;, or :.
KA_phrases <- modified_corpus %>% 
  select(ID, corpus, KA_key_lower) %>% 
  filter(!is.na(KA_key_lower)) %>% 
  unnest_tokens(phrase, KA_key_lower, token = stringr::str_split, pattern = "\\.|;|:")


### Making a custom function for extracting a few words before "organoid" or "onchip".
fn_extract_pw_phrase <- function(x) {
  ### tetragrams (four words occurring together) are extracted from the above chunks.
  ### Then, tetragrams that meet the following criteria are extracted.
  ### "any one word", followed by either "and" or "or", followed by a word in pre_inc2_w, 
  ### followed by "organoid/assembloid" (for organoid corpus) or "onchip" (for onchip corpus).
  tetragrams <- x %>% 
    ### extracting tetragrams.
    unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
    ### placing each word of tetragrams in separate columns
    separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
    ### selecting tetragrams ending with "organoid"/"assembloid" or "onchip"
    filter((corpus == "organoid" & word4 %in% c("organoid", "assembloid")) | 
             corpus == "OoC" & word4 == "onchip") %>% 
    ### selecting tetragrams that have a pre_inc2_w word before organoid/assembloid/onchip
    filter(word3 %in% pre_inc2_w) %>% 
    ### selecting tetragrams that have either and/or before the pre_inc2_w word
    filter(word2 %in% c("and", "or")) %>% 
    # removing duplicate rows.
    distinct()
  
  ### Similarly, trigrams meeting the following criteria are extracted.
  ### "any one word", followed by a pre_inc2_w word, followed by organoid/assembloid/onchip.
  trigrams <- x %>% 
    unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
    separate(trigram, c("word2", "word3", "word4"), sep = " ") %>% 
    filter((corpus == "organoid" & word4 %in% c("organoid", "assembloid")) | 
             corpus == "OoC" & word4 == "onchip") %>% 
    filter(word3 %in% pre_inc2_w) %>% 
    distinct()
  
  ### Similarly, bigrams are extracted.
  bigrams <- x %>% 
    unnest_tokens(bigram, phrase, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word3", "word4"), sep = " ") %>% 
    filter((corpus == "organoid" & word4 %in% c("organoid", "assembloid")) | 
             corpus == "OoC" & word4 == "onchip") %>% 
    filter(word3 %in% pre_inc2_w) %>% 
    distinct()
  
  ### The extracted bigrams and trigrams are combined.
  ### As extracted bigrams also contain bigram-portions of the extracted trigrams, 
  ### bigrams that are part of extracted trigrams are removed.
  bi_trigrams <- bind_rows(trigrams, bigrams) %>% 
    arrange(ID, word2) %>% 
    filter(!(is.na(word2) & duplicated(.[, c(1, 2, 4, 5)]))) 
  
  ### Similarly, the extracted tetragrams are combined with the combined bigrams-trigrams.
  bi_tri_tetragrams <- bind_rows(tetragrams, bi_trigrams) %>% 
    arrange(ID, word1) %>% 
    filter(!(is.na(word1) & duplicated(.[, c(1, 2, 4:6)]))) %>% 
    distinct() %>% 
    ### Replacing "NA" with "".
    mutate(across(starts_with("word"), ~ replace_na(., ""))) %>% 
    ### Uniting the words consisting bi-, tri-, and pentagrams into a single column.
    unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
    group_by(ID) %>% 
    ### Placing all extracted pentagrams from the same publication in a single column.
    mutate(pw_sentence = paste(tetragram, collapse = "; ")) %>% 
    ungroup() %>% 
    select(!tetragram) %>% 
    distinct()
}

### Applying the custom function to titles, thus extracting all occurrences of 
### "organoid/assembloid/onchip" along with a few words before them from titles.
pw_title_all <- fn_extract_pw_phrase(title_phrases) %>% 
  rename(pw_title_key_all = pw_sentence)

### Applying the custom function to keywords_abstracts
pw_KA_all <- fn_extract_pw_phrase(KA_phrases) %>% 
  rename(pw_KA_key_all = pw_sentence)

### Adding the pw_ columns to the "modified_corpus" generated in the section 2.
pre_word_corpus <- modified_corpus %>% 
  left_join(., pw_title_all[, c(1, 3)], by = "ID") %>% 
  left_join(., pw_KA_all[, c(1, 3)], by = "ID")

save(pre_word_corpus, 
     file = paste0(root_path, "R_temps/pre_word_corpus"))






##########
###
### 4. Listing words to capture.
###
### This section makes lists of all words to capture.
### Later, for example, "liver", "hepatic", "hepatocyte" will be all captured and categorized as "liver"
### The list was largely made by adding synonyms and related words to pre_inc2_w.
###
### Overall, three types of word lists were made.
### 1) word vectors starting with "p_", which contains words that need look-around options with perl = TRUE.
### 2) A word list of organ names and their equivalents which are indicative of organs (e.g., glioblastoma), including abbreviations.
### 3) A word list of organ names and their equivalents that in the above 2 included abbreviations. In this list, abbreviations are excluded 
### so that these organ names and key terms only house full terms.
### 
### 2) and 3) were made so that abbreviations are only captured when the corresponding full names appears somewhere in the text 
### (i.e., titles-keywords-abstracts, referred below to "combined text field").
### Note that some words are captured but not being used for analysis at the moment.
###
##########

###
### Character vectors that need perl = TRUE for capturing.
### (?!) means negative look ahead when perl = TRUE, avoiding capturing the word when it's followed by the word inside the parentheses.
###
p_ganglion_w <- c("ganglion", "ganglia", "ganglionic(?! eminence)")
p_epidermis_w <- c("epidermi[cs]", "epidermal(?! growth)", "epidermi[ds]es", "keratinocytes?")
p_hair_w <- c("hairs?(?! cell)")
p_mesonephros_w <- c("(?<!aorta[- ]gonad[- ])mesonephros")
p_embryonic_w <- c("embryonic(?! stem)", "embryos?")


###
### Making a nested list of all terms (both full terms and abbreviations) of organ names and their equivalents.
### Essentially, all terms in the second-level lists will be captured, and summarized according to the first-level lists.
### Adding word boundaries at the end.
###
all_terms <- list(
  ### brain
  brain_w = c("brains?", "cyborg", "ovb"), 
  # abbreviation
  forebrain_w = c("forebrains?"), 
  cerebrum_w = c("cerebrums?", "cerebral?", "telencephalons?", "telencephalic", "endbrains?"), 
  cortex_w = c("cortex", "cortices", "cortical", "neocortex", "neocortices", "neocortical"), 
  # cortex should be considered only when "brain/cerebrum" appear somewhere in texts
  corticostriatal_network_w = c("corticostriatalnetwork", "cortico[- ]striatal"),   
  hippocampus_w = c("hippocampus", "hippocampi", "hippocampal"), 
  basal_ganglion_w = c("basalganglion", "bg"), 
  # abbreviation
  striatum_w = c("striatum", "striatal?"),   
  diencephalon_w = c("diencephalons?", "diencephalic"), 
  thalamus_w = c("thalamus", "thalamuses", "thalamic?"), 
  hypothalamus_w = c("hypothalamus", "hypothalamic?", "hp"), 
  # abbreviation
  pituitary_w = c("pituitary", "pituitaries", "hypophys[ei]s", "hypophyseal"), 
  brainstem_w = c("brain ?stems?"), 
  midbrain_w = c("midbrains?"), 
  ventral_midbrain_w = c("ventral midbrains?", "vm"), 
  hindbrain_w = c("hindbrains?"), 
  cerebellum_w = c("cerebellums?", "cerebellar?"), 
  
  ### barriers, meninges, and  neuroimmune
  bbb_w = c("bloodbrainbarrier", "h?bbbs?"), 
  choroid_plexus_w = c("choroidplexus", "choroidplexuses", "chps?", "cerebrospinal fluids?", "csfs?"),
  # Capturing only when either "choroidplexus" or "cerebrospinal fluid" appears.
  glymphatic_w = c("glymphatics?"), 
  gliovascular_unit_w = c("gliovascular units?", "gvus?"), 
  meninges_w = c("meninges", "meninx", "meninge?al"), 
  neuroimmune_w = c("neuroimmune"), 
  
  ### nerve
  nerve_w = c("nerves?", "nervous", "neuro", "neuroepithelial", "neurons?", "neuronal", 
              "axons?", "synapses?", "synaptic"), 
  ganglion_w = c("ganglion", "ganglia"), 
  # Another term to capture with perl = TRUE
  autonomic_ganglion_w = c("autonomic ganglion", "autonomic ganglia", "i?ags?"), 
  dorsal_root_ganglion_w = c("drgs?", "dorsalrootganglion", "i?sgs?"), 
  # abbreviation
  neurovascular_w = c("neurovascular", "neurovasculatures?"), 
  neurovascular_unit_w = c("neurovascularunit", "nvus?"), 
  # abbreviation
  neuromuscular_w = c("neuromuscular"), 
  neuromuscular_junction_w = c("neuromuscularjunction"), 

  
  ### Spine
  spine_w = c("spines?", "spinal", "intervertebral dis[ck]s?"), 
  spinal_cord_w = c("spinalcord"), 
  
  ### other neural
  enteric_nervous_system_w = c("enteric nervous systems?", "enteric neurons?", "ens"), 
  
  ### ear
  cochlea_w = c("cochlea[rs]?", "hair[- ]cells?"), 
  umbo_w = c("umbos?", "umbones"), 
  
  ### ocular 
  optic_cup_w = c("opticcup"),   
  retina_w = c("retina[els]?"), 
  blood_retinal_barrier_w = c("bloodretinalbarrier", "o?brb"), 
  lacrimal_gland_w = c("lacrimalgland", "lacrimal", "lg"), 
  # abbreviation
  meibomian_gland_w = c("meibomian glands?", "meibocytes?", "mgs?"), 
  # abbreviation
  cornea_w = c("cornea[ls]?", "minicornea[ls]?"), 
  corneal_limbus_w = c("limbal", "limbus", "limbi", "limbuses"), 
  corneal_barrier_w = c("cornealbarrier", "microcornealbarrier"), 
  choroid_w = c("choroids?", "choroidal"), 
  # Make it "FALSE" when choroid plexus is TRUE.

  
  ### mouth
  tooth_w = c("tooth", "teeth", "dental"), 
  dental_follicle_w = c("dental follicles?", "dfc"), 
  # abbreviation
  dental_papilla_w = c("dental papillae?", "apical papillae?", "dpmc"), 
  # abbreviation, "dpmc" being dental papillae mesenchymal cell
  enamel_organ_w = c("enamel"), 
  periodontium_w = c("periodontium", "periodontia", "periodontal"),   
  dental_pulp_w = c("dentalpulps?", "dpsc"), 
  # abbreviation
  tongue_w = c("tongues?", "lingual"), 
  taste_bud_w = c("tastebud"), 
  circumvallate_papilla_w = c("circumvallatepapilla"), 
  salivary_gland_w = c("salivarygland", "sgs?"), 
  # abbreviation
  parotid_gland_w = c("parotidgland"), 
  submandibular_gland_w = c("submandibulargland"), 
  gingival_crevice_w = c("gingival crevices?", "gingival sulcus"), 
  
  ### lymphatic
  lymphoid_w = c("lymphoids?"), 
  bone_marrow_w = c("h?bm", "bonemarrow"), 
  # abbreviation
  thymus_w = c("thymus", "thymuses", "thymic?"), 
  spleen_w = c("spleens?"), 
  germinal_center_w = c("germinal centers?", "gc"), 
  # abbreviation
  tonsil_w = c("tonsils?", "tonsillar"), 
  lymph_node_w = c("lymphnode", "ln"), 
  # abbreviation
  sinus_w = c("sinus", "sinuses", "sinusoid"), 
  # Only capture when it occurs with lymphnode.
  lymphatic_vessel_w = c("lymphaticvessel", "lv"), 
  # abbreviation
  
  ### endocrine
  adrenal_w = c("adrenals?"), 
  neuroendocrine_w = c("neuroendocrine"), 
  thyroid_w = c("thyroids?", "tfc"), 
  # abbreviation, tfc being thyroid follicular cell
  parathyroid_w = c("parathyroids?"), 
  thyroid_gland_w = c("thyroidgland"), 
  
  ### breast
  mammary_gland_w = c("mammarygland"), 
  breast_duct_w = c("breastduct"), 
  
  ### cardiovascular
  heart_w = c("hearts?", "cardiac"), 
  myocardium_w = c("myocardium", "myocardial?", "cardiomyocytes?"), 
  valve_w = c("valves?", "valvular"), 
  vascular_w = c("vascular", "vascularnetwork", "vasculature"), 
  blood_w = c("bloods?", "ha?ematopoietic"), 
  blood_vessel_w = c("bloodvessel"), 
  artery_w = c("artery", "arteries", "arterial"), 
  aorta_w = c("aorta[ls]?"), 
  # make it FALSE if TRUE for "aorta-gonad-mesonephros"
  microvascular_w = c("microvascularnetwork", "microvascular", "microvasculatures?", "microvessels?", "angiogenes[ei]s", "capillary", "capillaries"), 
  perivascular_niche_w = c("perivascularniche"), 
  
  ### respiratory
  mucociliary_w = c("mucociliary"), 
  airway_w = c("airways?"), 
  pharynx_w = c("pharynx", "pharynxes", "pharynges", "pharynge?al"), 
  nasopharynx_w = c("nasopharynx", "nasopharynxes", "nasopharynges", "nasopharynge?al"), 
  oropharynx_w = c("oropharynx", "oropharynxes", "oropharynges", "oropharynge?al"), 
  hypopharynx_w = c("hypopharynx", "hypopharynxes", "hypopharynges", "hypopharynge?al"), 
  larynx_w = c("larynx", "larynxes", "larynges", "larynge?al"), 
  trachea_w = c("trachea[els]?", "windpipes?"), 
  tracheosphere_w = c("tracheospheres?"), 
  lung_w = c("lungs?", "pulmonary", "bronchioalveolar"), 
  alveolus_w = c("alveolus", "alveoli", "alveolar", "alveolospheres?", "pneumocytes?"), 
  bronchus_w = c("bronchus", "bronchi", "bronchial"), 
  bronchiole_w = c("bronchioles?", "bronchiolar"), 
  blood_air_barrier = c("bloodairbarrier", "abb"), 
  pleura_w = c("pleura[el]?"), 
  
  ### gastrointestinal
  esophagus_w = c("o?esophagus", "o?esophagi", "o?esophage?al", "o?esophagical"), 
  gastroesophageal_junction_w = c("gastroesophage?al junctions?", "gej"), 
  stomach_w = c("stomachs?", "gastric", "gins"), 
  gastric_antrum_w = c("antrum", "antral?"), 
  gastric_fundus_w = c("fundus", "fundic?"), 
  gastric_corpus_w = c("corpus", "corporal?"), 
  # The above three are only considered when gastric/stomach appears somewhere in the texts.
  gastric_gland_w = c("gastricgland"), 
  abomasum_w = c("abomasums?", "abomasal?"), 
  intestine_w = c("intestines?", "intestinal", "bowels?", "guts?", "enteral", "enteric", "enterocytes?"), 
  small_intestine_w = c("smallintestines?", "h?si"), 
  # abbreviation
  duodenum_w = c("duodenum", "duodenal?"), 
  ileum_w = c("ileum", "ileal?"), 
  jejunum_w = c("jejunum", "jejunal?"), 
  large_intestine_w = c("largeintestines?", "colorectal?", "colorectum"), 
  caecum_w = c("ca?ecums?", "ca?ecal?"), 
  colon_w = c("colons?", "colonic"), 
  appendix_w = c("appendix", "appendixes", "appendices", "appendiceal"), 
  rectum_w = c("rectums?", "rectal?"), 
  
  ### biliary
  pancreas_w = c("pancreas", "pancreases", "pancreatic"), 
  pancreatic_duct_w = c("pancreaticduct"), 
  islet_w = c("islets?"), 
  liver_w = c("livers?", "hepatic", "lsc", "hepatocytes?", "h?ihep", "hepatostellate", "hepatic progenitor cells?", "hpcs?"), 
  # abbreviation
  lobule_w = c("lobules?", "lobular", "vlsll", "very large-scale liver-lobules?"), 
  # Only capture when liver or hepatic appears.
  sinusoid_w = c("sinusoids?", "sinusoidal"), 
  # Only capture when liver or hepatic appears.
  biliary_w = c("biliary", "hepatobiliary", "biliarytract"), 
  bile_duct_w = c("bileduct", "cholangiocytes?"), 
  ihbd_w = c("intrahepatic bileduct", "ihbd", "intrahepatic ducts?", "ihd"), 
  # abbreviation
  ehbd_w = c("extrahepatic bileduct", "ehbd", "extrahepatic ducts?", "ehd"), 
  # abbreviation
  gallbladder_w = c("gallbladders?", "gb"), 
  # abbreviation
  
  ### reproductive
  female_reproductive_w = c("reproductivetract"), 
  ovary_w = c("ovary", "ovaries", "ovarian"), 
  oviduct_w = c("oviducts?", "oviductal", "salpinx", "salpinges", "salpinxes", "ftec"), 
  uterus_w = c("uterus", "uteri", "uteruses", "uterine", "wombs?"), 
  endometrium_w = c("endometrium", "endometrial?", "adenomyosis"), 
  endometrial_gland_w = c("endometrialgland"), 
  decidua_w = c("decidua[el]?"), 
  cervix_w = c("cervix", "cervices", "cervical", "ectocervix", "ectocervices", "ectocervical", "endocervix", "endocervices", "endocervical"), 
  # ectocervix and endocervix are not distinguished at the moment.
  vagina_w = c("vagina[ls]?"), 
  testes_w = c("test[ei]s", "testicles?", "testicular"), 
  epididymis_w = c("epididymis", "epididymides", "epididymal"), 
  prostate_w = c("prostates?", "prostatic"), 
  
  ### urinary system
  kidney_w = c("kidneys?", "renal"), 
  nephron_w = c("nephrons?", "nephronal"), 
  renal_tubule_w = c("renaltubule"), 
  collecting_duct_w = c("collectingduct", "cd"), 
  # abbreviation
  proximal_tubule_w = c("proximaltubule"), 
  distal_tubule_w = c("distaltubule"), 
  glomerulus_w = c("glomerulus", "glomerular", "glomeruli"), 
  # capture when either kidney, renal, or nephron occurs.
  bladder_w = c("bladders?", "vesical"), 
  urethra_w = c("urethra[els]?"), 
  
  ### dermal
  hair_follicle_w = c("hair follicles?", "hfscs?", "hf"), 
  # abbreviation  
  dermal_papilla_w = c("dermal papillae?", "dp"), 
  # abbreviation
  sebaceous_gland_w = c("sebaceousgland", "sg", "sebaceous"), 
  # abbreviation
  sweat_gland_w = c("sweatgland", "swg"), 
  # abbreviation
  epidermis_w = c("epidermi[cs]", "epidermi[ds]es", "keratinocytes?"), 
  # another term to capture with perl
  
  ### musculoskeletal
  osteochondral_w = c("osteochondral"), 
  bone_w = c("bones?", "osteogenic", "osteo", "osteogenes[ei]s", "osteoclastogenes[ei]s"), 
  cartilage_w = c("cartilages?", "cartilaginous", "chondrogenic", "chondral", "chondrocytes?"), 
  joint_w = c("joints?"), 
  synovial_joint_w = c("synovialjoint", "synovial"), 
  synovium_w = c("synovium", "synovial membranes?", "synovial stratum", "synovial strata", "stratum synoviales?"), 
  anterior_cruciate_ligament_w = c("anterior cruciate ligaments?", "acls?"), 
  artificial_joint_w = c("periprosthetic joints?", "artificial joints?", "prosthes[ei]s", "pji"), 
  muscle_w = c("muscles?", "myotubes?", "muscular", "smcs?"), 
  tendon_w = c("tendons?", "tendinous"), 
  anulus_fibrosus_w = c("ann?ulus fibrosus", "ann?ulus"), 
  
  ### adipose
  white_adipose_tissue_w = c("wats?", "white adipose tissues?"), 
  # abbreviation
  omentum_w = c("omentums?", "omental?"), 
  
  ### Major organs
  neural_w = c("central nervous systems?", "cnss?", "peripheral nervous systems?", "pnss?", "neural"), 
  otic_w = c("ears?", "otic"), 
  ocular_w = c("eyes?", "ocular", "multiocular"), 
  nasal_w = c("noses?", "olfactory", "nasal"), 
  oral_w = c("mouths?", "oral"), 
  lymphatic_w = c("lymphatic", "lymphs?", "lympho"),
  endocrine_w = c("endocrine"), 
  mammary_w = c("breasts?", "mammary", "mammaries", "masc"), 
  # abbreviation
  cardiovascular_w = c("cardiovascular"), 
  respiratory_w = c("respiratory"), 
  gastrointestinal_w = c("gastrointestinal", "gastrointestines?", "git?"), 
  # abbreviation
  hpb_w = c("hepatic, pancreatic,? and biliary", "hpb"), 
  # abbreviation
  reproductive_w = c("reproductive", "gonads?", "gonadal"), 
  # make it FALSE if TRUE for "aorta-gonad-mesonephros"
  urinary_w = c("urinary", "urines?", "usc", "urothelial?", "urothelium"), 
  # abbreviation
  dermal_w = c("skins?", "dermal", "dermis", "cutaneous"), 
  musculoskeletal_w = c("musculoskeletal"), 
  adipose_w = c("adiposes?", "adipocytes?"), 
  venom_gland_w = c("venomgland"), 
  
  ### organoid variants
  assembloid_w = c("assembloids?"), 
  gastruloid_w = c("gastruloids?"), 
  blastoid_w = c("blastoids?", "iblastoids?"), 
  
  ### embryonic
  blastocyst_w = c("blastocysts?"), 
  inner_cell_mass_w = c("inner cell mass", "inner cell masses", "icm"), 
  # abbreviation
  epiblast_w = c("epiblasts?"), 
  hypoblast_w = c("hypoblasts?"), 
  trophoblast_w = c("trophoblasts?", "trophectoderms?"), 
  cytotrophoblast_w = c("cytotrophoblasts?", "ctbs?"), 
  # abbreviation
  neuroectoderm_w = c("neuroectoderms?", "neuroectodermal"), 
  neuromesoderm_w = c("neuromesoderms?", "neuromesodermal"), 
  neural_tube_w = c("neuraltube"), 
  ganglionic_eminence_w = c("ganglionic eminences?", "m?ge"), 
  optic_vesicle_w = c("opticvesicle"), 
  # abbreviation
  paraxial_mesoderm_w = c("paraxial mesoderms?", "pm"), 
  # abbreviation
  precardiac_w = c("precardiac"), 
  foregut_w = c("foreguts?"), 
  midgut_w = c("midguts?"), 
  hindgut_w = c("hindguts?"), 
  ureteric_bud_w = c("ureteric buds?", "i?ub"), 
  # abbreviation
  aorta_gonad_mesonephros_w = c("aorta[- ]gonad[- ]mesonephros", "agm"), 
  # abbreviation
  mesonephros_w = c("mesonephros"), 
  placenta_w = c("placenta[ls]?"), 
  placental_barrier_w = c("placentalbarrier"), 
  fetal_membrane_w = c("fetalmembrane"), 
  amnion_w = c("amnions?", "amnia", "amniotic"), 
  embryonic_w = c("embryos?"), 
  # another term to capture with perl
  fetal_w = c("fo?etus", "fo?etuses", "foeti", "fo?etal"), 
  
  ### germ layer origins
  mesodermal_w = c("mesodermal", "mesoderms?"), 
  endodermal_w = c("endodermal", "endoderms?"), 
  ectodermal_w = c("ectodermal", "ectoderms?"), 
  
  ### Cell types
  epithelium_w = c("epithelial?", "epithelium", "epitheloid", "squamous", "neuroepithelial"), 
  endothelium_w = c("endothelial?", "endothelium", "endotheloid"), 
  mesenchyme_w = c("mesenchymes?", "mesenchymal"), 
  
  ### substructures
  crypt_w = c("crypts?"), 
  mucosa_w = c("mucosa[el]?"), 
  submucosa_w = c("submucosa[el]?"), 
  villus_w = c("villo?us", "villi"), 
  acinar_w = c("acinar", "acinus", "acini"), 
  follicle_w = c("follicles?", "follicular"), 
  papilla_w = c("papillae?"), 
  
  ### diseases
  ### Words in the following vectors will be captured and later assigned to corresponding organs (if there is any).
  ### For example, callus will be treated as "dermal", and "disease".
  disease_w = c("diseases?", "infections?", "syndromes?", "inflammation", "allergy", "allergies", "allergic"), 
  atherosclerosis_w = c("atheroscleros[ei]s"), 
  # vascular
  alzheimer_w = c("alzheimer'?s diseases?", "ad"), 
  # abbreviation, brain
  arrhythmia_w = c("arrhythmia"), 
  # heart
  arteriovenous_malformation_w = c("arteriovenous malformation", "avm"), 
  # blood vessel
  asd_w = c("autism spectrum disorder", "asd"), 
  # neural
  atresia_w = c("atresia"), 
  bph_w = c("benign prostatic hyperplasia", "bph"), 
  # prostate
  callus_w = c("callus", "calluses"), 
  # dermal
  cavd_w = c("calcific aortic valve diseases?", "cavd"), 
  # valve
  cmt_w = c("charcot[- ]marie[ -]tooth", "cmt", "cmt1a"), 
  # peripheral nervous
  copd_w = c("chronic obstructive pulmonary diseases?", "copd"), 
  # respiratory
  cystic_fibrosis_w = c("cystic fibrosis", "cf"), 
  ulcerative_colitis_w = c("ulcerative coliti[cs]", "uc"), 
  # abbreviation, large intestine, colitis
  colitis_w = c("coliti[cs]"), 
  # colon, colitis
  drvt_w = c("dravet syndromes?", "drvt"), 
  # neural
  endometriosis_w = c("endometriosis", "endometriotic"), 
  # endometrium
  enterocolitis_w = c("enterocoliti[cs]"), 
  # intestine, colitis
  epilepsy_w = c("epilepsy"), 
  fatal_familial_insomnia_w = c("fatal familial insomnia", "fii"), 
  # brain
  focal_cortical_dysplasia_w = c("focal cortical dysplasia", "fcd"), 
  # abbreviation, brain, disease
  fibrosis_w = c("fibrosis", "fibrotic"), 
  fxs_w = c("fragile x syndromes?", "fxs"), 
  # abbreviation
  glaucoma_w = c("glaucomas?"), 
  # ocular
  huntington_w = c("huntington'?s diseases?", "hd"), 
  # abbreviation, brain
  hofs_w = c("orbital fibroblasts?", "g?hofs"), 
  # abbreviation
  hypertonia_w = c("hypertonia"), 
  # muscle
  hyperuricemia_w = c("hyperuricemia", "hua"), 
  # abbreviation, urinary
  hypoxia_w = c("hypoxia"), 
  ibd_w = c("inflammatory bowel diseases?", "ibd"), 
  # abbreviation, intestine
  ie_w = c("infective endocarditis", "ie"), 
  # abbreviation, myocardium
  ischemia_w = c("ischemia"), 
  # cardiovascular
  long_qt_syndrome_w = c("long qt syndromes?", "lqts2"), 
  # cardiovascular
  aneurysm_w = c("aneurysms?", "microaneurysms?"), 
  # blood vessel
  multiple_sclerosis_w = c("multiple sclerosis", "ms"), 
  # neural
  nafld_w = c("non[- ]?alcoholic fatty liver disease", "nafld", "nash", "steatohepatitis"), 
  # abbreviation, liver
  nbs_w = c("nijmegen breakage syndromes?", "nbs"), 
  # abbreviation
  nephropathy_w = c("nephropathy", "nephrotoxicity"), 
  # kidney
  npc_w = c("niemann-pick diseases?", "npc"), 
  # abbreviation
  osteoarthritis_w = c("osteoarthritis", "oa"), 
  # abbreviation, cartilage
  parkinson_w = c("parkinson'?s diseases?", "pd"), 
  # abbreviation, brain
  pkd_w = c("polycystic kidney diseases?", "arpkd", "adpkd", "pkd"), 
  # abbreviation, kidney, polycystic kidney disease
  pmse_w = c("polyhydramnios, megalencephaly, symptomatic epilepsy", "pmse"), 
  progeria_w = c("progeria"), 
  pulmonary_hypertension_w = c("pulmonary[- ]hypertension", "pulmonary[- ]arterial[- ]hypertension", "pah"), 
  # abbreviation, lung
  scz_w = c("schizophrenia", "scz"), 
  # abbreviation
  tao_w = c("thyroid[- ]associated orbitopathy", "tao"), 
  # abbreviation, thyroid
  traumatic_brain_injury_w = c("traumatic brain injury", "traumatic brain injuries", "tbi"), 
  # brain
  thyroiditis_w = c("thyroiditis", "ht"), 
  # abbreviation, thyroid gland
  thrombosis_w = c("thrombos[ei]s", "atherothrombos[ei]s"), 
  # Blood vessel
  ulcer_w = c("ulcers?", "diabetic foot ulcers?", "dfu"), 
  # abbreviation, gastrointestinal
  
  ### Tumor
  tumor_w = c("tumou?rs?", "tumou?ral", "metastas[ei]s", "metastatic", "circulating tumou?r cells?", "ctcs?", 
              "tumou?r[- ]microenvironment", "tme", "tumou?roids?", "carcinomatos[ei]s"), 
  # abbreviation
  cancer_w = c("cancers?", "cancerous"), 
  adenocarcinoma_w = c("adenocarcinomas?", "ac"), 
  # abbreviation
  adenoma_w = c("adenomas?"), 
  ameloblastoma_w = c("ameloblastomas?", "am"), 
  # tooth
  carcinoma_w = c("carcinomas?"), 
  cholangiocarcinoma_w = c("cholangiocarcinomas?", "i?cca?", "ihcc"), 
  # abbreviation, bile duct, chlangiocarcinoma
  chondrosarcoma_w = c("chondrosarcomas?"), 
  # cartilage
  chordoma_w = c("chordomas?"), 
  # spine
  fibroadenoma_w = c("fibroadenomas?"), 
  # mammary
  glioblastoma_w = c("glioblastomas?", "gbm", "gb"), 
  # abbreviation, brain (may occur in spine as well), 
  glioma_w = c("gliomas?", "lgg"), 
  # abbreviation, brain tumor, lgg being "lower grade glioma
  hemangioma_w = c("hemangiomas?", "nichs?"), 
  # vascular tumor
  hepatoblastoma_w = c("hepatoblastomas?", "hb"), 
  # liver, blastoma
  hepatocarcinoma_w = c("hepatocarcinomas?"), 
  # liver, carcinoma
  hepatoma_w = c("hepatomas?"), 
  # liver 
  leukemia_w = c("leukemias?"), 
  # blood cancer but also occurs in bone marrow. Not to assign for organs.
  lymphoma_w = c("lymphomas?", "dlbcl"), 
  # abbreviation, lymphatic
  mpd_w = c("mammary paget’s diseases?", "mpd"), 
  # abbreviation, mammary
  melanoma_w = c("melanomas?"), 
  # dermal,
  medulloblastoma_w = c("medulloblastomas?", "mb"), 
  # brain, cancer
  meningioma_w = c("meningiomas?"), 
  # Brain or spine, not to classify for organs at the moment
  metastatic_niche_w = c("metastaticniche", "emns?"), 
  # abbreviation
  mesothelioma_w = c("mesothelioma", "c?mm"), 
  neoplasm_w = c("neoplastic", "neoplasms?"), 
  neuroblastoma_w = c("neuroblastomas?", "nb"), 
  # nerve tumor
  neurofibroma_w = c("neurofibromas?", "cnfs?"), 
  # nerve tumor
  osteosarcoma_w = c("osteosarcomas?", "os"), 
  # abbreviation, bone, cancer
  retinoblastoma_w = c("retinoblastomas?", "rb"), 
  # abbreviation, retina, cancer
  sarcoma_w = c("sarcomas?"), 
  # can occur in many tissues.
  raird_w = c("refractory diseases?", "raird"), 
  # abbreviation, "radioactive iodine (RAI), -refractory disease", cancer that stopped responding to treatment
  tumor_vessel_w = c("tumorvessel"), 
  
  ### abbreviations
  acc_w = c("adrenocortical carcinomas?", "accs?"), 
  # adrenal, carcinoma
  ba_w = c("biliary atresia", "ba"), 
  # bile duct, atresia
  bladder_cancer_w = c("bladder cancers?", "bc", "nmibc"), 
  # bladder, cancer
  breast_cancer_w = c("breast cancers?", "bca?", "mammary tumors?", "c?mts?", "phyllodes tumors?", "mpt"), 
  # breast, cancer
  btc_w = c("biliary tract carcinoma", "btc"), 
  # biliary carcinoma
  cac_w = c("colitis[- ]associated cancer", "cac"), 
  # colitis, cancer
  cccc_w = c("cervical clear cell carcinomas?", "cccc"), 
  # cervix, carcinoma
  ccrcc_w = c("renal cell carcinomas?", "rcc", "ccrcc", "trcc"), 
  # kidney, carcinoma
  cho_w = c("cholangiocytes?", "cho", "clcs?"), 
  # bile duct
  crc_w = c("colorectal cancers?", "[hm]?crc", "crpm", "colorectal adenomas?", "cra", "t1crc"), 
  # large intestine, cancer
  crlm_w = c("colorectal cancer liver metastasis", "crlm"), 
  # liver, cancer
  dga_w = c("diffuse-type gastric adenocarcinomas?", "dga"), 
  # gastric, adenocarcinoma
  dipg_w = c("diffuse intrinsic pontine gliomas?", "dipg"), 
  # glioma, brain (brainstem), 
  eac_w = c("o?esophageal adenocarcinomas?", "eac"), 
  # esophagus, adenocarcinoma
  endometrial_cancer_w = c("endometrial cancers?", "ecs?"), 
  fte_w = c("oviduct epithelium", "fte"), 
  # oviduct, epithelium
  ga_w = c("grade appendiceal", "hga", "lga"), 
  # cancer (low-grade and high-grade appendiceal), 
  gbc_w = c("gallbladder carcinomas?", "gbc"), 
  # gallbladder, carcinoma
  gastric_cancer_w = c("gastric cancers?", "d?gc"), 
  # stomach, cancer
  gist_w = c("gastrointestinal stromal tumors?", "gists?"), 
  # gastrointestinal, tumor
  gka_w = c("gut[- ]kidney ax[ei]s", "gka"), 
  # gut, kidney
  hcc_w = c("hepatocellular carcinomas?", "hcc","fibrolamellar carcinomas?", "flc"), 
  # liver, carcinoma
  hee_w = c("human epididymis epithelial cells?", "hee"), 
  # epididymis, epithelial
  hgsoc_w = c("high[- ]grade serous ovarian cancers?", "high[- ]grade serous ovarian carcinomas?", "hgsoc", 
              "high[- ]grade serous tubo[- ]ovarian cancers?", "high[- ]grade serous tubo[- ]ovarian carcinomas?", "hgsc"), 
  # ovary, cancer
  hne_w = c("human nasal epithelial", "hne"), 
  # nasal, epithelial
  hnscc_w = c("head and neck squamous cell carcinomas?", "hnscc"), 
  # epithelial, carcinoma
  iec_w = c("intestinal epithelial cells?", "iec"), 
  # intestine, epithelial
  isc_w = c("intestinal stem cells?", "isc"), 
  # intestine
  ipmn_w = c("intraductal papillary mucinous neoplasms?", "ipmn"), 
  # pancreatic duct, neoplasm
  lsec_w = c("liver sinusoidal endothelial cells?", "lsec"), 
  # liver, endothelial
  luad_w = c("lung adenocarcinomas?", "luad"), 
  # lung, adenocarcinoma
  lusc_w = c("lung squamous cell carcinomas?", "lusc"), 
  # lung, carcinoma, epithelium
  mec_w = c("mammary epithelial cells?", "mec"), 
  # mammary, epithelial
  mpm_w = c("malignant pleural mesothelioma", "mpm"), 
  # pleura, tumor
  mrt_w = c("malignant rhabdoid tumors?", "mrt"), 
  # kidney?, tumor
  nec_w = c("neuroendocrine carcinomas?", "nec"), 
  # neuroendocrine, carcinoma
  nem_w = c("nasal epithelial mucosa", "nem"), 
  # nasal, epithelial, mocosa
  nen_w = c("neuroendocrine neoplasms?", "nen"), 
  # neuroendocrine, neoplasm
  nepc_w = c("neuroendocrine prostate cancers?", "nepc"), 
  # neuroendocrine, prostate, cancer
  net_w = c("neuroendocrine tumors?", "net"), 
  # neuroendocrine, tumor
  nsclc_w = c("small cell lung cancers?", "sclc", "nonsmall cell lung cancers?", "nsclc"), 
  # lung cancer
  ovarian_cancer_w = c("ovarian cancers?", "oc"), 
  # ovary, cancer
  oscc_w = c("oral squamous cell carcinomas?", "oscc"), 
  # mouth, carcinoma
  pca_w = c("prostate cancers?", "pca", "pc", "m?crpc"), 
  # prostate, cancer
  pcc_w = c("pancreatic cancers?", "pancreatic intraepithelial neoplasia", "pcc", "pc", "panin"), 
  # pancreas, cancer
  pdac_w = c("pancreaticduct adenocarcinomas?", "pdac"), 
  # pancreatic duct, adenocarcinoma
  pitnet_w = c("pituitary neuroendocrine tumors?", "pitnets?"), 
  # pituitary, tumor
  plc_w = c("primary liver cancers?", "plc"), 
  # liver, cancer
  ptc_w = c("papillary thyroid cancers?", "ptc"), 
  # thyroid, cancer
  rectal_cancer_w = c("rectal cancers?", "rc"), 
  # rectum, cancer
  rpe_w = c("retinal pigment epithelial", "rpe"), 
  # retina, epithelial
  scc_w = c("squamous cell carcinomas?", "c?scc"), 
  # epithelial, tumor
  sccc_w = c("small cell[a-z -]*cervi[a-z]+", "sccc"), 
  # cancer, cervix
  sgc_w = c("salivarygland carcinomas?", "sgcs?"), 
  # salivary gland, carcinoma
  srcc_w = c("signet-ring cell carcinomas?", "srcc"), 
  # carcinoma
  sys_w = c("synovial sarcomas?", "sys"), 
  # sarcoma
  tcc_w = c("transitional cell carcinomas?", "tcc"), 
  # urinary system, carcinoma
  tnbc_w = c("triple negative breast cancers?", "tnbc"), 
  # breast, cancer
  tsa_w = c("traditional serrated adenomas", "tsa"), 
  # large intestine, adenoma
  oe_w = c("olfactory epithelium", "olfactory epithelial", "oe"), 
  utuc_w = c("urothelial carcinomas?", "utucs?"), 
  # urothelial, carcinoma
  vc_w = c("vessel co-?option", "vc")
  # blood vessel, tumor
) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))

print(all_terms)




###
### Making another nested list of organ names.
### Here, organ names and their equivalents that included abbreviations in the above nested list were selected.
### Then, abbreviations were deleted.
### So, the list basically contains full terms of abbreviations listed in the above list.
### The list will be used to consider abbreviations as organ names/equivalents only when corresponding full terms appear somewhere 
### in the combined text field.
### Also, the two nested lists were used to conditionally capture some organ names. 
### For example, "cortex" is considered to be "brain cortex" only when "brain" or similar words appear somewhere in the combined text field.
### For this reason, the "cortex_w" in the below list includes brain, cerebral and so on, instead of "cortex".
###
full_terms <- list(
  ### organs
  cortex_w = c("brains?", "cerebrums?", "cerebral?", "telencephalons?", "telencephalic", "endbrains?"), 
  basal_ganglion_w = c("basalganglion"), 
  hypothalamus_w = c("hypothalamus", "hypothalamic?"), 
  ventral_midbrain_w = c("ventral midbrains?"), 
  bbb_w = c("bloodbrainbarrier"), 
  choroid_plexus_w = c("choroidplexus", "choroidplexuses", "cerebrospinal fluids?"), 
  autonomic_ganglion_w = c("autonomic ganglion", "autonomic ganglia"), 
  dorsal_root_ganglion_w = c("dorsalrootganglion"), 
  neurovascular_unit_w = c("neurovascularunit"), 
  gliovascular_unit_w = c("gliovascular units?"), 
  enteric_nervous_system_w = c("enteric nervous systems?", "enteric neurons?"), 
  blood_retinal_barrier_w = c("bloodretinalbarrier"), 
  lacrimal_gland_w = c("lacrimalgland", "lacrimal"), 
  meibomian_gland_w = c("meibomian glands?", "meibocytes?"), 
  dental_follicle_w = c("dental follicles?"), 
  dental_papilla_w = c("dental papillae?", "apical papillae?"), 
  dental_pulp_w = c("dentalpulps?"), 
  salivary_gland_w = c("salivarygland"), 
  submandibular_gland_w = c("submandibulargland"), 
  bone_marrow_w = c("bonemarrow"),   
  germinal_center_w = c("germinal centers?"), 
  lymph_node_w = c("lymphnode"), 
  sinus_w = c("lymphatic", "lymphs?", "lympho", "lymphoids?", "lymphnode"), 
  lymphatic_vessel_w = c("lymphaticvessel"), 
  thyroid_w = c("thyroids?"), 
  valve_w = c("hearts?", "cardiac"), 
  blood_air_barrier = c("bloodairbarrier"), 
  gastroesophageal_junction_w = c("gastroesophageal junctions?"), 
  stomach_w = c("stomachs?", "gastric"), 
  gastric_antrum_w = c("stomachs?", "gastric"), 
  gastric_fundus_w = c("stomachs?", "gastric"), 
  gastric_corpus_w = c("stomachs?", "gastric"), 
  small_intestine_w = c("smallintestines?"), 
  liver_w = c("livers?", "hepatic", "hepatocytes?", "hepatostellate", "hepatic progenitor cells?"), 
  lobule_w = c("livers?", "hepatic"), 
  sinusoid_w = c("livers?", "hepatic"), 
  ihbd_w = c("intrahepatic bileduct", "intrahepatic ducts?"), 
  ehbd_w = c("extrahepatic bileduct", "extrahepatic ducts?"), 
  gallbladder_w = c("gallbladders?"), 
  oviduct_w = c("oviducts?", "oviductal", "salpinx", "salpinges", "salpinxes"), 
  collecting_duct_w = c("collectingduct"), 
  glomerulus_w = c("kidneys?", "renal", "nephrons?", "nephronal"), 
  dermal_papilla_w = c("dermal papillae?"), 
  sebaceous_gland_w = c("sebaceousgland", "sebaceous"), 
  sweat_gland_w = c("sweatgland"), 
  hair_follicle_w = c("hair follicles?", "hfscs?"), 
  anterior_cruciate_ligament_w = c("anterior cruciate ligaments?"), 
  artificial_joint_w = c("periprosthetic joints?", "artificial joints?", "prosthes[ei]s"), 
  muscle_w = c("muscles?", "myotubes?", "muscular"), 
  anulus_fibrosus_w = c("ann?ulus fibrosus"), 
  white_adipose_tissue_w = c("white adipose tissues?"), 
  
  ### Major organs
  neural_w = c("central nervous systems?", "peripheral nervous systems?", "neural"), 
  brain_w = c("brains?", "cyborg"), 
  mammary_w = c("breasts?", "mammary", "mammaries"), 
  gastrointestinal_w = c("gastrointestinal", "gastrointestines?"), 
  hpb_w = c("hepatic, pancreatic,? and biliary"), 
  urinary_w = c("urinary", "urines?", "urothelial"), 

  ### embryonic
  inner_cell_mass_w = c("inner cell mass", "inner cell masses"), 
  cytotrophoblast_w = c("cytotrophoblasts?"), 
  ganglionic_eminence_w = c("ganglionic eminences?"), 
  paraxial_mesoderm_w = c("paraxial mesoderms?"), 
  ureteric_bud_w = c("ureteric buds?"), 
  aorta_gonad_mesonephros_w = c("aorta[- ]gonad[- ]mesonephros"), 
  
  ### diseases
  alzheimer_w = c("alzheimer'?s diseases?"), 
  arteriovenous_malformation_w = c("arteriovenous malformation"), 
  asd_w = c("autism spectrum disorder"), 
  bph_w = c("benign prostatic hyperplasia"), 
  cavd_w = c("calcific aortic valve diseases?"), 
  cmt_w = c("charcot[- ]marie[ -]tooth"), 
  copd_w = c("chronic obstructive pulmonary diseases?"), 
  cystic_fibrosis_w = c("cystic fibrosis"), 
  drvt_w = c("dravet syndromes?"), 
  ulcerative_colitis_w = c("ulcerative coliti[cs]"), 
  fatal_familial_insomnia_w = c("fatal familial insomnia"), 
  focal_cortical_dysplasia_w = c("focal cortical dysplasia"), 
  fxs_w = c("fragile x syndromes?"), 
  huntington_w = c("huntington'?s diseases?"), 
  hofs_w = c("orbital fibroblasts?"), 
  hyperuricemia_w = c("hyperuricemia"), 
  ibd_w = c("inflammatory bowel diseases?"), 
  ie_w = c("infective endocarditis"), 
  long_qt_syndrome_w = c("long qt syndromes?"), 
  multiple_sclerosis_w = c("multiple sclerosis"), 
  nafld_w = c("non[- ]?alcoholic fatty liver disease", "steatohepatitis"), 
  nbs_w = c("nijmegen breakage syndromes?"), 
  npc_w = c("niemann-pick diseases?"), 
  osteoarthritis_w = c("osteoarthritis"), 
  parkinson_w = c("parkinson'?s diseases?"), 
  pkd_w = c("polycystic kidney diseases?"), 
  pmse_w = c("polyhydramnios, megalencephaly, symptomatic epilepsy"), 
  pulmonary_hypertension_w = c("pulmonary[- ]hypertension", "pulmonary[- ]arterial[- ]hypertension"), 
  scz_w = c("schizophrenia"), 
  tao_w = c("thyroid[- ]associated orbitopathy"), 
  traumatic_brain_injury_w = c("traumatic brain injury", "traumatic brain injuries"), 
  thyroiditis_w = c("thyroiditis"), 
  ulcer_w = c("ulcers?", "diabetic foot ulcers?"), 
  
  ### Tumor
  tumor_w = c("tumou?rs?", "tumou?ral", "metastas[ei]s", "metastatic", "circulating tumou?r cells?", 
              "tumou?r[- ]microenvironment", "tumou?roids?"), 
  adenocarcinoma_w = c("adenocarcinomas?"), 
  ameloblastoma_w = c("ameloblastomas?"), 
  cholangiocarcinoma_w = c("cholangiocarcinomas?"), 
  glioblastoma_w = c("glioblastomas?"), 
  glioma_w = c("gliomas?"), 
  hemangioma_w = c("hemangiomas?"), 
  hepatoblastoma_w = c("hepatoblastomas?"), 
  lymphoma_w = c("lymphomas?"), 
  mpd_w = c("mammary paget’s diseases?"), 
  medulloblastoma_w = c("medulloblastomas?"), 
  mesothelioma_w = c("mesothelioma"), 
  metastatic_niche_w = c("metastaticniche"), 
  neuroblastoma_w = c("neuroblastomas?"), 
  neurofibroma_w = c("neurofibromas?"), 
  osteosarcoma_w = c("osteosarcomas?"), 
  retinoblastoma_w = c("retinoblastomas?"), 
  raird_w = c("refractory diseases?"), 
  
  ### abbreviations
  acc_w = c("adrenocortical carcinomas?"), 
  ba_w = c("biliary atresia"), 
  bladder_cancer_w = c("bladder cancers?"), 
  breast_cancer_w = c("breast cancers?", "mammary tumors?", "phyllodes tumors?"), 
  btc_w = c("biliary tract carcinoma"), 
  cac_w = c("colitis[- ]associated cancer"), 
  cccc_w = c("cervical clear cell carcinomas?"), 
  ccrcc_w = c("renal cell carcinomas?"), 
  cho_w = c("cholangiocytes?"), 
  crc_w = c("colorectal cancers?", "colorectal adenomas?"), 
  crlm_w = c("colorectal cancer liver metastasis"), 
  dga_w = c("diffuse-type gastric adenocarcinomas?"), 
  dipg_w = c("diffuse intrinsic pontine gliomas?"), 
  eac_w = c("o?esophageal adenocarcinomas?"), 
  endometrial_cancer_w = c("endometrial cancers?"), 
  fte_w = c("oviduct epithelium"), 
  ga_w = c("grade appendiceal"), 
  gbc_w = c("gallbladder carcinomas?"), 
  gastric_cancer_w = c("gastric cancers?"), 
  gist_w = c("gastrointestinal stromal tumors?"), 
  gka_w = c("gut[- ]kidney ax[ei]s"), 
  hcc_w = c("hepatocellular carcinomas?", "fibrolamellar carcinomas?"), 
  hee_w = c("human epididymis epithelial cells?"), 
  hgsoc_w = c("high[- ]grade serous ovarian cancers?", "high[- ]grade serous ovarian carcinomas?", 
              "high[- ]grade serous tubo[- ]ovarian cancers?", "high[- ]grade serous tubo[- ]ovarian carcinomas?"), 
  hne_w = c("human nasal epithelial"), 
  hnscc_w = c("head and neck squamous cell carcinomas?"), 
  iec_w = c("intestinal epithelial cells?"), 
  isc_w = c("intestinal stem cells?"), 
  ipmn_w = c("intraductal papillary mucinous neoplasms?"), 
  lsec_w = c("liver sinusoidal endothelial cells?"), 
  luad_w = c("lung adenocarcinomas?"), 
  lusc_w = c("lung squamous cell carcinomas?"), 
  mec_w = c("mammary epithelial cells?"), 
  mpm_w = c("malignant pleural mesothelioma"), 
  mrt_w = c("malignant rhabdoid tumors?"), 
  nec_w = c("neuroendocrine carcinomas?"), 
  nem_w = c("nasal epithelial mucosa"), 
  nen_w = c("neuroendocrine neoplasms?"), 
  nepc_w = c("neuroendocrine prostate cancers?"), 
  net_w = c("neuroendocrine tumors?"), 
  nsclc_w = c("small cell lung cancers?", "nonsmall cell lung cancers?"), 
  ovarian_cancer_w = c("ovarian cancers?"), 
  oscc_w = c("oral squamous cell carcinomas?"), 
  pca_w = c("prostate cancers?"), 
  pcc_w = c("pancreatic cancers?", "pancreatic intraepithelial neoplasia"), 
  pdac_w = c("pancreaticduct adenocarcinomas?"), 
  pitnet_w = c("pituitary neuroendocrine tumors?"), 
  plc_w = c("primary liver cancers?"), 
  ptc_w = c("papillary thyroid cancers?"), 
  rectal_cancer_w = c("rectal cancers?"), 
  rpe_w = c("retinal pigment epithelial"), 
  scc_w = c("squamous cell carcinomas?"), 
  sccc_w = c("small cell[a-z -]*cervi[a-z]+"), 
  sgc_w = c("salivarygland carcinomas?"), 
  srcc_w = c("signet-ring cell carcinomas?"), 
  sys_w = c("synovial sarcomas?"), 
  tcc_w = c("transitional cell carcinomas?"), 
  tnbc_w = c("triple negative breast cancers?"), 
  tsa_w = c("traditional serrated adenomas"), 
  oe_w = c("olfactory epithelium", "olfactory epithelial"), 
  utuc_w = c("urothelial carcinomas?"), 
  vc_w = c("vessel co-?option")
)  %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))

print(full_terms)




##########
###
### 5. Capturing full terms of organ names in the combined text field.
###
##########

### Making a logical data frame showing presence/absence of full terms in the combined text fields.
### Later, organ names/their equivalents that include abbreviations are only considered to be present 
### if the corresponding value in this data frame is TRUE.
full_all_TF <- as.data.frame(sapply(full_terms, function(x) grepl(x, pre_word_corpus$text_all_lower)))






##########
###
### 6. Capturing all terms of organ names in pw_ columns (a few words before organoid/onchip) in titles
###
##########

### First, all terms in the organ name list are captured in titles.
pw_title_organ_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pre_word_corpus$pw_title_key_all))) %>% 
  ### Word groups that need perl = TRUE are captured below.
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pre_word_corpus$pw_title_key_all, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pre_word_corpus$pw_title_key_all, perl = TRUE)) %>% 
  mutate(mesonephros_w = grepl(paste0("\\b", p_mesonephros_w, "\\b", collapse = "|"), pre_word_corpus$pw_title_key_all, perl = TRUE)) %>% 
  mutate(embryonic_w = grepl(paste0("\\b", p_embryonic_w, "\\b", collapse = "|"), pre_word_corpus$pw_title_key_all, perl = TRUE))


### From this data frame of all terms, only selecting columns that exist in the data frame for full terms that was generated in the section 5.
pw_title_organ_selected <- pw_title_organ_TF %>% 
  select(colnames(full_all_TF))

### Multiplying the data frames for all terms and full terms.
### In this way, values will be changed to 0 (FALSE when converted to logical) if full terms do not appear in the combined text fields.  
pw_title_organ_adjusted <- pw_title_organ_selected * full_all_TF

### Replacing the columns of the data frame of all terms with multiplied columns.
pw_title_organ_combined <- pw_title_organ_TF %>% 
  select(!colnames(full_all_TF)) %>% 
  cbind(., (as.data.frame(lapply(pw_title_organ_adjusted, as.logical)))) %>% 
  ### Reordering the columns
  select(colnames(pw_title_organ_TF)) %>% 
  ### Changing the columns that need considerations.
  ### For example, "choroid" is only considered as choroid in the eye when the paper doesn't talk about "choroid plexsus".
  mutate(choroid_w = ifelse(full_all_TF$choroid_plexus_w == TRUE, FALSE, choroid_w)) %>% 
  mutate(aorta_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, aorta_w)) %>% 
  mutate(reproductive_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, reproductive_w)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pre_word_corpus$pw_title_key_all, perl = TRUE), TRUE, hair_follicle_w))


### Checking column names.
colnames(pw_title_organ_combined)


### Making a custom function for adjusting the logical data frame to integrate organ name equivalents into organ names.
### For example, "glioma", "glioblastoma" are interpreted as "brain" with a tumor attribute, 
### so that the columns for brain and tumor are changed to TRUE.
fn_adjust_TF <- function(x) {
  adjusted_data_frame <- x %>% 
    mutate(glioma_w = ifelse(dipg_w == TRUE, TRUE, glioma_w)) %>% 
    mutate(sarcoma_w = ifelse(sys_w == TRUE | chondrosarcoma_w == TRUE | 
                                osteosarcoma_w == TRUE, TRUE, sarcoma_w)) %>% 
    mutate(colitis_w = ifelse(cac_w == TRUE, TRUE, colitis_w)) %>% 
    mutate(neural_w = ifelse(asd_w == TRUE | multiple_sclerosis_w == TRUE | drvt_w == TRUE, TRUE, neural_w)) %>% 
    mutate(brain_w = ifelse(glioma_w == TRUE | alzheimer_w == TRUE | parkinson_w == TRUE | 
                              glioblastoma_w == TRUE | huntington_w == TRUE | focal_cortical_dysplasia_w == TRUE | 
                              medulloblastoma_w == TRUE | traumatic_brain_injury_w == TRUE | fatal_familial_insomnia_w == TRUE, 
                            TRUE, brain_w)) %>% 
    mutate(pituitary_w = ifelse(pitnet_w == TRUE, TRUE, pituitary_w)) %>% 
    mutate(nerve_w = ifelse(neuroblastoma_w == TRUE | neurofibroma_w == TRUE | cmt_w == TRUE, TRUE, nerve_w)) %>% 
    mutate(spine_w = ifelse(chordoma_w == TRUE, TRUE, spine_w)) %>%     
    mutate(ocular_w = ifelse(glaucoma_w == TRUE, TRUE, ocular_w)) %>%   
    mutate(retina_w = ifelse(retinoblastoma_w == TRUE | rpe_w == TRUE, TRUE, retina_w)) %>%   
    mutate(nasal_w = ifelse(hne_w == TRUE | nem_w == TRUE | oe_w == TRUE, TRUE, nasal_w)) %>% 
    mutate(oral_w = ifelse(oscc_w == TRUE, TRUE, oral_w)) %>% 
    mutate(tooth_w = ifelse(ameloblastoma_w == TRUE, TRUE, tooth_w)) %>% 
    mutate(salivary_gland_w = ifelse(sgc_w == TRUE, TRUE, salivary_gland_w)) %>% 
    mutate(lymphatic_w = ifelse(lymphoma_w == TRUE, TRUE, lymphatic_w)) %>% 
    mutate(neuroendocrine_w = ifelse(nec_w == TRUE | nen_w == TRUE | 
                                       net_w == TRUE | nepc_w == TRUE, 
                                     TRUE, neuroendocrine_w)) %>%   
    mutate(thyroid_w = ifelse(ptc_w == TRUE | tao_w == TRUE | thyroiditis_w == TRUE, TRUE, thyroid_w)) %>% 
    mutate(mammary_w = ifelse(breast_cancer_w == TRUE | fibroadenoma_w == TRUE | tnbc_w == TRUE | 
                                mpd_w == TRUE | mec_w == TRUE,
                              TRUE, mammary_w)) %>% 
    mutate(cardiovascular_w = ifelse(ischemia_w == TRUE | long_qt_syndrome_w == TRUE, TRUE, cardiovascular_w)) %>%    
    mutate(heart_w = ifelse(arrhythmia_w == TRUE, TRUE, heart_w)) %>%     
    mutate(myocardium_w = ifelse(ie_w == TRUE, TRUE, myocardium_w)) %>%   
    mutate(valve_w = ifelse(cavd_w == TRUE, TRUE, valve_w)) %>% 
    mutate(vascular_w = ifelse(thrombosis_w == TRUE | hemangioma_w == TRUE | atherosclerosis_w == TRUE, TRUE, vascular_w)) %>% 
    mutate(blood_vessel_w = ifelse(arteriovenous_malformation_w == TRUE | aneurysm_w == TRUE | vc_w == TRUE, 
                                   TRUE, blood_vessel_w)) %>% 
    mutate(respiratory_w = ifelse(copd_w == TRUE, TRUE, respiratory_w)) %>% 
    mutate(lung_w = ifelse(luad_w == TRUE | nsclc_w == TRUE | lusc_w == TRUE | 
                             pulmonary_hypertension_w == TRUE, 
                           TRUE, lung_w)) %>% 
    mutate(pleura_w = ifelse(mpm_w == TRUE, TRUE, pleura_w)) %>% 
    mutate(gastrointestinal_w = ifelse(ulcer_w == TRUE | gist_w == TRUE, TRUE, gastrointestinal_w)) %>%  
    mutate(esophagus_w = ifelse(eac_w == TRUE, TRUE, esophagus_w)) %>% 
    mutate(stomach_w = ifelse(dga_w == TRUE | gastric_cancer_w == TRUE, TRUE, stomach_w)) %>% 
    mutate(intestine_w = ifelse(ibd_w == TRUE | iec_w == TRUE | isc_w == TRUE | 
                                  enterocolitis_w == TRUE | gka_w == TRUE, TRUE, intestine_w)) %>% 
    mutate(large_intestine_w = ifelse(crc_w == TRUE | tsa_w == TRUE | ulcerative_colitis_w == TRUE, 
                                      TRUE, large_intestine_w)) %>% 
    mutate(colon_w = ifelse(colitis_w == TRUE, TRUE, colon_w)) %>% 
    mutate(rectum_w = ifelse(rectal_cancer_w == TRUE, TRUE, rectum_w)) %>% 
    mutate(pancreas_w = ifelse(pcc_w == TRUE, TRUE, pancreas_w)) %>%   
    mutate(pancreatic_duct_w = ifelse(pdac_w == TRUE | ipmn_w == TRUE, 
                                      TRUE, pancreatic_duct_w)) %>% 
    mutate(liver_w = ifelse(hepatoma_w == TRUE | hepatoblastoma_w == TRUE | hepatocarcinoma_w == TRUE | 
                              hcc_w == TRUE | lsec_w == TRUE | plc_w == TRUE | nafld_w == TRUE | crlm_w == TRUE, 
                            TRUE, liver_w)) %>%   
    mutate(biliary_w = ifelse(btc_w == TRUE, TRUE, biliary_w)) %>% 
    mutate(bile_duct_w = ifelse(ba_w == TRUE | cholangiocarcinoma_w == TRUE | 
                                  cho_w == TRUE, TRUE, bile_duct_w)) %>%   
    mutate(gallbladder_w = ifelse(gbc_w == TRUE, TRUE, gallbladder_w)) %>% 
    mutate(ovary_w = ifelse(hgsoc_w == TRUE | ovarian_cancer_w == TRUE, TRUE, ovary_w)) %>% 
    mutate(oviduct_w = ifelse(fte_w == TRUE, TRUE, oviduct_w)) %>%   
    mutate(endometrium_w = ifelse(endometriosis_w == TRUE | endometrial_cancer_w == TRUE, TRUE, endometrium_w)) %>%  
    mutate(cervix_w = ifelse(cccc_w == TRUE | sccc_w == TRUE, TRUE, cervix_w)) %>%  
    mutate(epididymis_w = ifelse(hee_w == TRUE, TRUE, epididymis_w)) %>%   
    mutate(prostate_w = ifelse(pca_w == TRUE | nepc_w == TRUE, 
                               TRUE, prostate_w)) %>% 
    mutate(urinary_w = ifelse(tcc_w == TRUE | hyperuricemia_w == TRUE | utuc_w == TRUE, TRUE, urinary_w)) %>%   
    mutate(kidney_w = ifelse(pkd_w == TRUE | ccrcc_w == TRUE | nephropathy_w == TRUE | 
                               gka_w == TRUE, TRUE, kidney_w)) %>% 
    mutate(bladder_w = ifelse(bladder_cancer_w == TRUE, TRUE, bladder_w)) %>%     
    mutate(dermal_w = ifelse(melanoma_w == TRUE | callus_w == TRUE, TRUE, dermal_w)) %>% 
    mutate(epidermis_w = ifelse(callus_w == TRUE, TRUE, epidermis_w)) %>% 
    mutate(bone_w = ifelse(osteosarcoma_w == TRUE, TRUE, bone_w)) %>% 
    mutate(cartilage_w = ifelse(chondrosarcoma_w == TRUE | osteoarthritis_w == TRUE, 
                                TRUE, cartilage_w)) %>% 
    mutate(muscle_w = ifelse(hypertonia_w == TRUE, TRUE, muscle_w)) %>% 
    mutate(adrenal_w = ifelse(acc_w == TRUE, TRUE, adrenal_w)) %>% 
    mutate(epithelium_w = 
             ifelse(iec_w == TRUE | hee_w == TRUE | hne_w == TRUE | oscc_w == TRUE | 
                      fte_w == TRUE | mec_w == TRUE | lusc_w == TRUE | hnscc_w == TRUE | 
                      nem_w == TRUE | rpe_w == TRUE | scc_w == TRUE | oe_w == TRUE, 
                    TRUE, epithelium_w)) %>% 
    mutate(endothelium_w = ifelse(lsec_w == TRUE, TRUE, endothelium_w)) %>% 
    mutate(follicle_w = ifelse(dental_follicle_w == TRUE | hair_follicle_w == TRUE, TRUE, follicle_w)) %>% 
    mutate(atresia_w = ifelse(ba_w == TRUE, TRUE, atresia_w)) %>%   
    mutate(neoplasm_w = ifelse(ipmn_w == TRUE | nen_w == TRUE, TRUE, neoplasm_w)) %>% 
    mutate(adenocarcinoma_w = 
             ifelse(dga_w == TRUE | pdac_w == TRUE | luad_w == TRUE | eac_w == TRUE, 
                    TRUE, adenocarcinoma_w)) %>% 
    mutate(adenoma_w = ifelse(tsa_w == TRUE, TRUE, adenoma_w)) %>% 
    mutate(carcinoma_w = 
             ifelse(acc_w == TRUE | btc_w == TRUE | hcc_w == TRUE | nec_w == TRUE | oscc_w == TRUE | sgc_w == TRUE | 
                      srcc_w == TRUE | hnscc_w == TRUE | cccc_w == TRUE | tcc_w == TRUE | 
                      lusc_w == TRUE | ccrcc_w == TRUE | cholangiocarcinoma_w == TRUE | gbc_w == TRUE | 
                      utuc_w == TRUE, TRUE, carcinoma_w)) %>% 
    mutate(cancer_w = 
             ifelse(breast_cancer_w == TRUE | bladder_cancer_w | crc_w == TRUE | 
                      ga_w == TRUE | pca_w == TRUE | tnbc_w == TRUE | ptc_w == TRUE | nsclc_w == TRUE | 
                      hgsoc_w == TRUE | nepc_w == TRUE | plc_w == TRUE | rectal_cancer_w == TRUE | 
                      pcc_w == TRUE | ovarian_cancer_w == TRUE | gastric_cancer_w == TRUE | cac_w == TRUE | 
                      endometrial_cancer_w == TRUE | crlm_w == TRUE | medulloblastoma_w == TRUE | sccc_w == TRUE, 
                    TRUE, cancer_w)) %>% 
    mutate(tumor_w = ifelse(net_w == TRUE | mrt_w == TRUE | scc_w == TRUE | gist_w == TRUE | 
                              mpm_w == TRUE | pitnet_w == TRUE | vc_w == TRUE, TRUE, tumor_w)) %>% 

    ### Below, only keeping the lowest-level categories as TRUE.
    ### For example, "brain" is changed to FALSE if the document is TRUE for cerebrum.
    ### This is because a paper may use different terms such as "brain organoid" and "cerebral organoid" to describe the same entity.
    mutate(brain_w = ifelse(rowSums(.[2:17]) > 0, FALSE, brain_w)) %>% 
    mutate(forebrain_w = ifelse(rowSums(.[3:12]) > 0, FALSE, forebrain_w)) %>% 
    mutate(cerebrum_w = ifelse(rowSums(.[4:8]) > 0, FALSE, cerebrum_w)) %>% 
    mutate(cortex_w = ifelse(corticostriatal_network_w == TRUE, FALSE, cortex_w)) %>% 
    mutate(basal_ganglion_w = ifelse(striatum_w == TRUE, FALSE, basal_ganglion_w)) %>%     
    mutate(diencephalon_w = ifelse(rowSums(.[10:12]) > 0, FALSE, diencephalon_w)) %>% 
    mutate(hypothalamus_w = ifelse(pituitary_w == TRUE, FALSE, hypothalamus_w)) %>% 
    mutate(brainstem_w = ifelse(rowSums(.[14:17]) > 0, FALSE, brainstem_w)) %>% 
    mutate(hindbrain_w = ifelse(cerebellum_w == TRUE, FALSE, hindbrain_w)) %>% 
    mutate(glymphatic_w = ifelse(gliovascular_unit_w == TRUE, FALSE, glymphatic_w)) %>% 
    mutate(nerve_w = ifelse(rowSums(.[25:31]) > 0, FALSE, nerve_w)) %>%   
    mutate(ganglion_w = ifelse(dorsal_root_ganglion_w == TRUE, FALSE, ganglion_w)) %>%   
    mutate(neurovascular_w = ifelse(neurovascular_unit_w == TRUE, FALSE, neurovascular_w)) %>% 
    mutate(neuromuscular_w = ifelse(neuromuscular_junction_w == TRUE, FALSE, neuromuscular_w)) %>%  
    mutate(spine_w = ifelse(spinal_cord_w == TRUE, FALSE, spine_w)) %>%    
    mutate(retina_w = ifelse(blood_retinal_barrier_w == TRUE, FALSE, retina_w)) %>% 
    mutate(cornea_w = ifelse(corneal_limbus_w == TRUE | corneal_barrier_w == TRUE, FALSE, cornea_w)) %>% 
    mutate(tooth_w = ifelse(rowSums(.[47:51]) > 0, FALSE, tooth_w)) %>% 
    mutate(tongue_w = ifelse(taste_bud_w == TRUE | circumvallate_papilla_w == TRUE, FALSE, tongue_w)) %>% 
    mutate(salivary_gland_w = ifelse(parotid_gland_w == TRUE | submandibular_gland_w == TRUE, FALSE, salivary_gland_w)) %>% 
    mutate(lymphoid_w = ifelse(rowSums(.[60:66]) > 0, FALSE, lymphoid_w)) %>% 
    mutate(lymph_node_w = ifelse(sinus_w == TRUE, FALSE, lymph_node_w)) %>% 
    mutate(thyroid_w = ifelse(parathyroid_w == TRUE | thyroid_gland_w == TRUE, FALSE, thyroid_w)) %>% 
    mutate(heart_w = ifelse(myocardium_w == TRUE | valve_w == TRUE, FALSE, heart_w)) %>% 
    mutate(vascular_w = ifelse(rowSums(.[79:84]) > 0, FALSE, vascular_w)) %>% 
    mutate(blood_vessel_w = ifelse(rowSums(.[81:84]) > 0, FALSE, blood_vessel_w)) %>% 
    mutate(artery_w = ifelse(aorta_w == TRUE, FALSE, artery_w)) %>%      
    mutate(airway_w = ifelse(rowSums(.[87:93]) > 0, FALSE, airway_w)) %>% 
    mutate(pharynx_w = ifelse(rowSums(.[88:90]) > 0, FALSE, pharynx_w)) %>% 
    mutate(trachea_w = ifelse(tracheosphere_w == TRUE, FALSE, trachea_w)) %>% 
    mutate(lung_w = ifelse(rowSums(.[95:98]) > 0, FALSE, lung_w)) %>% 
    mutate(stomach_w = ifelse(rowSums(.[103:107]) > 0, FALSE, stomach_w)) %>% 
    mutate(gastric_corpus_w = ifelse(gastric_gland_w == TRUE, FALSE, gastric_corpus_w)) %>%     
    mutate(intestine_w = ifelse(rowSums(.[109:117]) > 0, FALSE, intestine_w)) %>% 
    mutate(small_intestine_w = ifelse(rowSums(.[110:112]) > 0, FALSE, small_intestine_w)) %>% 
    mutate(large_intestine_w = ifelse(rowSums(.[114:117]) > 0, FALSE, large_intestine_w)) %>% 
    mutate(pancreas_w = ifelse(pancreatic_duct_w == TRUE | islet_w == TRUE, FALSE, pancreas_w)) %>% 
    mutate(liver_w = ifelse(lobule_w == TRUE | sinusoid_w == TRUE, FALSE, liver_w)) %>% 
    mutate(lobule_w = ifelse(sinusoid_w == TRUE, FALSE, lobule_w)) %>%     
    mutate(biliary_w = ifelse(rowSums(.[125:128]) > 0, FALSE, biliary_w)) %>% 
    mutate(bile_duct_w = ifelse(ihbd_w == TRUE | ehbd_w == TRUE, FALSE, bile_duct_w)) %>% 
    mutate(female_reproductive_w = ifelse(rowSums(.[130:137]) > 0, FALSE, female_reproductive_w)) %>%   
    mutate(uterus_w = ifelse(rowSums(.[133:135]) > 0, FALSE, uterus_w)) %>% 
    mutate(endometrium_w = ifelse(endometrial_gland_w == TRUE | decidua_w == TRUE, FALSE, endometrium_w)) %>% 
    mutate(kidney_w = ifelse(rowSums(.[142:147]) > 0, FALSE, kidney_w)) %>% 
    mutate(nephron_w = ifelse(rowSums(.[143:147]) > 0, FALSE, nephron_w)) %>% 
    mutate(renal_tubule_w = ifelse(rowSums(.[144:146]) > 0, FALSE, renal_tubule_w)) %>%   
    mutate(hair_follicle_w = ifelse(dermal_papilla_w == TRUE | sebaceous_gland_w == TRUE, FALSE, hair_follicle_w)) %>% 
    mutate(osteochondral_w = ifelse(bone_w == TRUE | cartilage_w == TRUE, FALSE, osteochondral_w)) %>% 
    mutate(joint_w = ifelse(rowSums(.[159:162]) > 0, FALSE, joint_w)) %>% 
    mutate(synovial_joint_w = ifelse(synovium_w == TRUE, FALSE, synovial_joint_w)) %>%  
    
    ## Changing the first-level categories to TRUE if any of the lower categories are TRUE.
    mutate(neural_w = ifelse(rowSums(.[1:34]) > 0, TRUE, neural_w)) %>% 
    mutate(otic_w = ifelse(rowSums(.[35:36]) > 0, TRUE, otic_w)) %>% 
    mutate(ocular_w = ifelse(rowSums(.[37:45]) > 0, TRUE, ocular_w)) %>% 
    mutate(oral_w = ifelse(rowSums(.[46:58]) > 0, TRUE, oral_w)) %>%  
    mutate(lymphatic_w = ifelse(rowSums(.[59:67]) > 0, TRUE, lymphatic_w)) %>% 
    mutate(endocrine_w = ifelse(rowSums(.[68:72]) > 0, TRUE, endocrine_w)) %>% 
    mutate(mammary_w = ifelse(rowSums(.[73:74]) > 0, TRUE, mammary_w)) %>% 
    mutate(cardiovascular_w = ifelse(rowSums(.[75:84]) > 0, TRUE, cardiovascular_w)) %>% 
    mutate(respiratory_w = ifelse(rowSums(.[85:99]) > 0, TRUE, respiratory_w)) %>% 
    mutate(gastrointestinal_w = ifelse(rowSums(.[100:117]) > 0, TRUE, gastrointestinal_w)) %>% 
    mutate(hpb_w = ifelse(rowSums(.[118:128]) > 0, TRUE, hpb_w)) %>% 
    mutate(reproductive_w = ifelse(rowSums(.[129:140]) > 0, TRUE, reproductive_w)) %>% 
    mutate(urinary_w = ifelse(rowSums(.[141:149]) > 0, TRUE, urinary_w)) %>% 
    mutate(dermal_w = ifelse(rowSums(.[150:154]) > 0, TRUE, dermal_w)) %>% 
    mutate(musculoskeletal_w = ifelse(rowSums(.[155:165]) > 0, TRUE, musculoskeletal_w)) %>% 
    mutate(adipose_w = ifelse(rowSums(.[166:167]) > 0, TRUE, adipose_w)) %>% 
    
    ### adjusting embryonic, disease, and tumor classifications.
    mutate(blastocyst_w = ifelse(rowSums(.[190:194]) > 0, FALSE, blastocyst_w)) %>% 
    mutate(inner_cell_mass_w = ifelse(epiblast_w == TRUE | hypoblast_w == TRUE, FALSE, inner_cell_mass_w)) %>%   
    mutate(trophoblast_w = ifelse(cytotrophoblast_w == TRUE, FALSE, trophoblast_w)) %>% 
    mutate(neuroectoderm_w = ifelse(neural_tube_w == TRUE | ganglionic_eminence_w == TRUE, FALSE, neuroectoderm_w)) %>% 
    mutate(neural_tube_w = ifelse(ganglionic_eminence_w == TRUE, FALSE, neural_tube_w)) %>% 
    mutate(aorta_gonad_mesonephros_w = ifelse(mesonephros_w == TRUE, FALSE, aorta_gonad_mesonephros_w)) %>% 
    mutate(placenta_w = ifelse(rowSums(.[209:211]) > 0, FALSE, placenta_w)) %>% 
    mutate(fetal_membrane_w = ifelse(amnion_w == TRUE, FALSE, fetal_membrane_w)) %>% 
    mutate(embryonic_w = as.logical(rowSums(.[188:212]))) %>%   
    mutate(disease_w = as.logical(rowSums(.[227:277]))) %>% 
    mutate(tumor_w = as.logical(rowSums(.[278:309])))
}

### Running the custom function on the data frame
pw_title_organ_modified <- fn_adjust_TF(pw_title_organ_combined) %>% 
  ### Assigning unique identifier numbers to each document, and also making a new column which shows what the classification is based on, 
  ### which in this case is "pw_title".
  mutate(ID = c(1:nrow(.)), 
         based_on = "pw_title")

colnames(pw_title_organ_modified)




##########
###
### 7. Capturing all terms in pw in keywords_abstracts
###
##########

### The below code is the same as above codes for pw in titles, except: 
### pw_title_ was replaced with pw_KA_ as names of data objects/columns.
###
pw_KA_organ_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pre_word_corpus$pw_KA_key_all))) %>% 
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pre_word_corpus$pw_KA_key_all, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pre_word_corpus$pw_KA_key_all, perl = TRUE)) %>% 
  mutate(mesonephros_w = grepl(paste0("\\b", p_mesonephros_w, "\\b", collapse = "|"), pre_word_corpus$pw_KA_key_all, perl = TRUE)) %>% 
  mutate(embryonic_w = grepl(paste0("\\b", p_embryonic_w, "\\b", collapse = "|"), pre_word_corpus$pw_KA_key_all, perl = TRUE))

pw_KA_organ_selected <- pw_KA_organ_TF %>% 
  select(colnames(full_all_TF)) 

pw_KA_organ_adjusted <- pw_KA_organ_selected * full_all_TF

pw_KA_organ_combined <- pw_KA_organ_TF %>% 
  select(!colnames(full_all_TF)) %>% 
  cbind(., (as.data.frame(lapply(pw_KA_organ_adjusted, as.logical)))) %>% 
  select(colnames(pw_KA_organ_TF)) %>% 
  ### Changing the columns that need consideration.
  ### For example, "choroid" is only considered as choroid in the eye when the paper doesn't talk about "choroid plexsus.
  mutate(choroid_w = ifelse(full_all_TF$choroid_plexus_w == TRUE, FALSE, choroid_w)) %>% 
  mutate(aorta_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, aorta_w)) %>% 
  mutate(reproductive_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, reproductive_w)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pre_word_corpus$pw_KA_key_all, perl = TRUE), TRUE, hair_follicle_w))

pw_KA_organ_modified <- fn_adjust_TF(pw_KA_organ_combined) %>% 
  mutate(ID = c(1:nrow(.)), 
         based_on = "pw_KA")

colnames(pw_KA_organ_modified)









##########
###
### 8. Capturing all terms in key sentences in titles.
###
##########

### Basically, the below code is the same as the above code, except: 
### 1) pw_KA_key_all was replaced with title_key_lower to capture terms in "key" titles (i.e., titles that contain the term "organoid/assembloid/onchip).
### 2) pw_KA_ in data object names was replaced with key_title_.
###
key_title_organ_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pre_word_corpus$title_key_lower))) %>% 
  ### Word groups that need perl = TRUE are captured below.
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pre_word_corpus$title_key_lower, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pre_word_corpus$title_key_lower, perl = TRUE)) %>% 
  mutate(mesonephros_w = grepl(paste0("\\b", p_mesonephros_w, "\\b", collapse = "|"), pre_word_corpus$title_key_lower, perl = TRUE)) %>% 
  mutate(embryonic_w = grepl(paste0("\\b", p_embryonic_w, "\\b", collapse = "|"), pre_word_corpus$title_key_lower, perl = TRUE))

key_title_organ_selected <- key_title_organ_TF %>% 
  select(colnames(full_all_TF)) 

key_title_organ_adjusted <- key_title_organ_selected * full_all_TF

key_title_organ_combined <- key_title_organ_TF %>% 
  select(!colnames(full_all_TF)) %>% 
  cbind(., (as.data.frame(lapply(key_title_organ_adjusted, as.logical)))) %>% 
  select(colnames(key_title_organ_TF)) %>% 
  ### Changing the columns that need consideration.
  ### For example, "choroid" is only considered as choroid in the eye when the paper doesn't talk about "choroid plexsus.
  mutate(choroid_w = ifelse(full_all_TF$choroid_plexus_w == TRUE, FALSE, choroid_w)) %>% 
  mutate(aorta_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, aorta_w)) %>% 
  mutate(reproductive_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, reproductive_w)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pre_word_corpus$title_key_lower, perl = TRUE), TRUE, hair_follicle_w))

key_title_organ_modified <- fn_adjust_TF(key_title_organ_combined) %>% 
  mutate(ID = c(1:nrow(.)), 
         based_on = "key_title")

colnames(key_title_organ_modified)






##########
###
### 9. Capturing all terms in key sentences of keywords_abstracts
###
##########

### The below code is the same as above codes for title_key, except: 
### 1) title_key_lower was replaced with KA_key_lower to capture terms in "key" keywords_abstract 
### (i.e., sentences in keywords/abstracts that contain the term "organoid/assembloid/onchip").
### 2) key_title_ in the data object names was replaced as key_KA_.
###
key_KA_organ_TF <- as.data.frame(sapply(all_terms, function(x) grepl(x, pre_word_corpus$KA_key_lower))) %>% 
  ### Word groups that need perl = TRUE are captured below.
  mutate(ganglion_w = grepl(paste0("\\b", p_ganglion_w, "\\b", collapse = "|"), pre_word_corpus$KA_key_lower, perl = TRUE)) %>% 
  mutate(epidermis_w = grepl(paste0("\\b", p_epidermis_w, "\\b", collapse = "|"), pre_word_corpus$KA_key_lower, perl = TRUE)) %>% 
  mutate(mesonephros_w = grepl(paste0("\\b", p_mesonephros_w, "\\b", collapse = "|"), pre_word_corpus$KA_key_lower, perl = TRUE)) %>% 
  mutate(embryonic_w = grepl(paste0("\\b", p_embryonic_w, "\\b", collapse = "|"), pre_word_corpus$KA_key_lower, perl = TRUE))

key_KA_organ_selected <- key_KA_organ_TF %>% 
  select(colnames(full_all_TF)) 

key_KA_organ_adjusted <- key_KA_organ_selected * full_all_TF

key_KA_organ_combined <- key_KA_organ_TF %>% 
  select(!colnames(full_all_TF)) %>% 
  cbind(., (as.data.frame(lapply(key_KA_organ_adjusted, as.logical)))) %>% 
  select(colnames(key_KA_organ_TF)) %>% 
  ### Changing the columns that need consideration.
  ### For example, "choroid" is only considered as choroid in the eye when the paper doesn't talk about "choroid plexsus.
  mutate(choroid_w = ifelse(full_all_TF$choroid_plexus_w == TRUE, FALSE, choroid_w)) %>% 
  mutate(aorta_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, aorta_w)) %>% 
  mutate(reproductive_w = ifelse(full_all_TF$aorta_gonad_mesonephros_w == TRUE, FALSE, reproductive_w)) %>% 
  mutate(hair_follicle_w = 
           ifelse(grepl(paste0("\\b", p_hair_w, "\\b", collapse = "|"), pre_word_corpus$KA_key_lower, perl = TRUE), TRUE, hair_follicle_w))

key_KA_organ_modified <- fn_adjust_TF(key_KA_organ_combined) %>% 
  mutate(ID = c(1:nrow(.)), 
         based_on = "key_KA")

colnames(key_KA_organ_modified)










##########
###
### 10. Assigning the final classification
###
### Overall, the final classification is assigned based on pw_title, pw_KA, title_key, or KA_key, 
### where earlier classification having higher priority
###
### There will be two major groups of classification: "organ" classification and "prenatal" classification.
###
##########

### As mentioned, final classification is made according to the determined priority.
###
### For this purpose, a new data frame is made.
### The data frame has all documents as rows, with columns containing logical values showing the presence of organ names, 
### just like the previously generated data frames.
### The difference here is that rows are taken from the pw_title data frame if a row has TRUE for any of the organ (or prenatal) names, 
### and if not from the pw_KA data frame if the row is TRUE for any of the organ names, and so on.

colnames(pw_title_organ_modified)


### First, organ classification is made.
###
### From the pw_title data frame, selecting rows that have "TRUE" in any of the organ names.
pw_title_organ_positive <- pw_title_organ_modified %>% 
  filter(rowSums(.[168:185]) > 0)

### From the pw_KA data frame, selecting rows that have not been picked up from the pw_title above, and have "TRUE" in any of the organs.
pw_KA_organ_positive <- pw_KA_organ_modified %>% 
  filter(rowSums(.[168:185]) > 0) %>% 
  filter(!ID %in% pw_title_organ_positive$ID)

### From key_title, selecting rows that have not been picked up above, and have "TRUE".
key_title_organ_positive <- key_title_organ_modified %>% 
  filter(rowSums(.[168:185]) > 0) %>% 
  filter(!ID %in% c(pw_title_organ_positive$ID, pw_KA_organ_positive$ID))

### From key_KA, selecting rows that have not been picked up. 
key_KA_organ_positive <- key_KA_organ_modified %>% 
  filter(!ID %in% c(pw_title_organ_positive$ID, pw_KA_organ_positive$ID, key_title_organ_positive$ID))

### Combining all the above four subset data frames, and reordering the rows.
organ_positive <- rbind(pw_title_organ_positive, pw_KA_organ_positive, key_title_organ_positive, key_KA_organ_positive) %>% 
  arrange(ID) %>% 
  ### Replacing the "assembloid_w" column, as this does not need priority-based classification.
  mutate(assembloid_w = grepl("\\bassembloids?\\b", tolower(pre_word_corpus$text_all_mod)))


### Similarly, "prenatal" classification is made.
###
### From the pw_title data frame, selecting rows that are TRUE for any of the prenatal structures.
pw_title_prenatal_positive <- pw_title_organ_modified %>% 
  filter(rowSums(.[187:213]) > 0)

### Selecting from pw_KA.
pw_KA_prenatal_positive <- pw_KA_organ_modified %>% 
  filter(rowSums(.[187:213]) > 0) %>% 
  filter(!ID %in% pw_title_prenatal_positive$ID)

### Selecting from key_title
key_title_prenatal_positive <- key_title_organ_modified %>% 
  filter(rowSums(.[187:213]) > 0) %>% 
  filter(!ID %in% c(pw_title_prenatal_positive$ID, pw_KA_prenatal_positive$ID))

### Selecting from key_KA.
key_KA_prenatal_positive <- key_KA_organ_modified %>% 
  filter(!ID %in% c(pw_title_prenatal_positive$ID, pw_KA_prenatal_positive$ID, key_title_prenatal_positive$ID))

### Combining the subset data frames, and reordering rows.
prenatal_positive <- rbind(pw_title_prenatal_positive, pw_KA_prenatal_positive, key_title_prenatal_positive, key_KA_prenatal_positive) %>% 
  arrange(ID) %>% 
  ### Replacing the "blastoid_w" and "gastruloid_w" columns as they don't need priority-based classification.
  mutate(blastoid_w = grepl("\\bblastoids?\\b", tolower(pre_word_corpus$text_all_mod)), 
         gastruloid_w = grepl("\\bgastruloids?\\b", tolower(pre_word_corpus$text_all_mod))) %>% 
  ### Changing the column "based_on" to "based_on_prenatal" so that it can be distinguised from organ classification later.
  rename(based_on_prenatal = based_on)

colnames(organ_positive)


### Below, organ classification columns are taken from the reconstructed data frame for organ classification, 
### and prenatal classification columns are taken from the reconstructed data frame for prenatal classification.
### Then, these two are combined.
### Note that "tumor_w" classification column is taken from the organ classification data frame.
all_positive <- left_join(organ_positive[, c(1:186, 278, 372, 373)], prenatal_positive[, c(187:213, 372, 373)], by = "ID")

colnames(all_positive)

### Making some adjustments to the combined data frame.
all_classified <- all_positive %>% 
  ### Making columns that show the number of organs detected.
  ### "n_organ_major" shows the number of first organ categories, and "n_organ_minor" shows the number of lower organ categories.
  mutate(n_organ_minor = rowSums(.[1:167]), 
         n_organ_major = rowSums(.[168:185])) %>% 
  ###  Summarizing whether a paper talks about blastoid and/or gastruloid.
  ### If a document talks about both blastoids and gastruloids, then the column value will be "blastoid and gastruloid".
  mutate(blastoid_gastruloid = ifelse(rowSums(.[190:191]) > 1, "blastoid and gastruloid", 
                                      ifelse(rowSums(.[190:191]) == 1, colnames(.[190:191])[max.col(.[190:191])], NA))) %>% 
  mutate(blastoid_gastruloid = gsub("_w$", "", blastoid_gastruloid)) %>% 
  ### Summarizing prenatal organoid types.
  ### If a paper studies both embryonic and fetal organoid, then it is classified as "prenatal".
  ### If a paper studies multiple embryonic organoids, then it is classified as "multiple embryonic".
  ### If a paper studies only one embryonic organoid type, then the corresponding embryonic organoid types is used for classification.
  mutate(prenatal = 
           ifelse(rowSums(.[215:216]) > 1, "prenatal", 
                  ifelse(rowSums(.[192:214]) > 1, "multiple_embryonic", 
                         ifelse(rowSums(.[192:214]) == 1, colnames(.[192:214])[max.col(.[192:214])], 
                                ifelse(rowSums(.[215:216]) == 1, colnames(.[215:216])[max.col(.[215:216])], NA))))) %>% 
  ### Adjusting writing styles.
  mutate(prenatal = gsub("_w$", "", prenatal)) %>% 
  mutate(prenatal = gsub("_", " ", prenatal)) %>% 
  ### Combining prenatal and blastoid_gastruloid classification.
  ### If a paper uses blastoid/gastruloid, it is classified according to it.
  ### If not, classification is based on prenatal types.
  mutate(prenatal_BG_type = ifelse(!is.na(blastoid_gastruloid), blastoid_gastruloid, prenatal)) 

### May save the data frame.
save(all_classified, file = paste0(root_path, "R_temps/all_classified"))




### For organ classification, a document that is positive for only one organ can be classified accordingly.
### It is more tricky to classify documents that use more than one organ/substructure models.
###
### Two types of classifications are made to deal with these documents using multiple organ models.
### 1) classify them as "multiple organs"
### 2) make a column that lists all detected organs.
###

colnames(all_classified)

minor_organs <- all_classified %>% 
  ### Converting the data frame to the long format based on the columns for non-first level organ categories.
  pivot_longer(., c(1:167), names_to = "minor_organ") %>% 
  filter(value == TRUE) %>% 
  ### Adjusting writing styles
  mutate(minor_organ = gsub("_w$", "", minor_organ)) %>% 
  mutate(minor_organ = gsub("_", " ", minor_organ)) %>% 
  mutate(minor_organ = gsub("bbb", "blood-brain barrier", minor_organ)) %>% 
  mutate(minor_organ = gsub("blood retinal barrier", "blood-retinal barrier", minor_organ)) %>% 
  mutate(minor_organ = gsub("blood air barrier", "blood-air barrier", minor_organ)) %>% 
  mutate(minor_organ = gsub("ehbd", "extrahepatic bile duct", minor_organ)) %>% 
  mutate(minor_organ = gsub("ihbd", "intrahepatic bile duct", minor_organ)) %>% 
  ### Making a column that lists all the detected non-first level organ categories in the same document.
  group_by(ID) %>% 
  mutate(all_minor_organ = paste0(minor_organ, collapse = ";")) %>% 
  ungroup() %>% 
  select(!c(minor_organ, value)) %>% 
  distinct() %>% 
  ### Duplicating the generated column.
  mutate(all_minor_organ2 = all_minor_organ) %>% 
  ### Storing the detected organ category names in columns, one organ category per column.
  separate(all_minor_organ2, paste0("minor_organ_", c(1:max(all_classified$n_organ_minor))), sep = ";", fill = "right") %>% 
  ### Below, the final organ classification is made.
  ### If there is only one detected organ name in a document, then the document is classified according to the organ name.
  ### If there is more than one detected organ names, then the document is classified as "multiple organs".
  mutate(organ_type = ifelse(is.na(minor_organ_2), minor_organ_1, "multiple organs")) 

### Doing the same for the first-level organ category.
major_organs <- all_classified %>% 
  pivot_longer(., c(168:185), names_to = "major_organ") %>% 
  filter(value == TRUE) %>% 
  mutate(major_organ = gsub("_w$", "", major_organ)) %>% 
  mutate(major_organ = gsub("_", " ", major_organ)) %>% 
  mutate(major_organ = gsub("hpb", "hepatic, pancreatic, biliary", major_organ)) %>% 
  group_by(ID) %>% 
  mutate(all_major_organ = paste0(major_organ, collapse = ";")) %>% 
  ungroup() %>% 
  select(!c(major_organ, value)) %>% 
  distinct() %>% 
  mutate(all_major_organ2 = all_major_organ) %>% 
  separate(all_major_organ2, paste0("major_organ_", c(1:max(all_classified$n_organ_major))), sep = ";", fill = "right") %>% 
  ### Below, the final classification of the first-level organ category is made (often referred to as the "major organ" category below).
  mutate(major_organ = ifelse(is.na(major_organ_2), major_organ_1, "multiple organs")) 



colnames(pre_word_corpus)

colnames(all_classified)

colnames(minor_organs)

colnames(major_organs)


### Combining the modified corpus data frame, organ/prenatal classification data frame, 
### and final classifications for non-first level and first level organ categories.
pre_organ_classification <- pre_word_corpus %>% 
  ### Adding the reconstructed logical data frame of organ/prenatal classifications
  left_join(., all_classified, by = "ID") %>% 
  ### Adding the non-first level organ category classification.
  left_join(minor_organs %>% select(ID, contains("minor_organ"), organ_type), by = "ID") %>% 
  ### Adding the first-level organ category classification.
  left_join(major_organs %>% select(ID, contains("major_organ")), by = "ID") %>% 
  ### If no organ name was detected in a document, then the organ classification is changed to "unidentified".
  mutate(organ_type = ifelse(is.na(organ_type), "unidentified", organ_type), 
         major_organ = ifelse(is.na(major_organ), "unidentified", major_organ)) %>% 
  ### Some documents have major organ category, but lack non-first level organ categories.
  ### For example, a paper talking about "neural organoids" have "neural" in the major_organ column, but "unidentified" in the "organ_type" column.
  ### In such cases, the "organ_type" category is changed to the values in the "major_organ" column.
  mutate(organ_type = ifelse(major_organ == "multiple organs", "multiple organs", 
                             ifelse(organ_type %in% c("multiple organs", "unidentified"), major_organ, organ_type)))


### May save the result.
save(pre_organ_classification, 
     file = paste0(root_path, "R_temps/pre_organ_classification"))

colnames(pre_organ_classification)


### !!! Note !!!
### The following lines undergo manual inspection of the data to check the organ model classification.
### If you want to skip this step, ignore the following lines and instead execute; 
### pre_organ_classification2 <- pre_organ_classification
### and resume at: 10.2. Assigning hierarchy to organ classification.
###
### Manually checking  rare organ models (mentioned in less than 5 documents) to avoid reporting non-existent organ model categories.
###
### Identifying organ categories that appear in less than 5 documents.
rare_organs <- pre_organ_classification %>% 
  count(organ_type, sort = TRUE) %>% 
  filter(n < 5)

### Selecting papers classified as using one of the rare organ models
rare_organ_papers <- pre_organ_classification %>% 
  filter(organ_type %in% rare_organs$organ_type) %>% 
  select(ID, author, text_all, title_key_lower, KA_key_lower, organ_type)

### Saved as csv.
write.csv(rare_organ_papers, file = paste0(root_path, "csv/temps/rare_organ_papers.csv"), row.names = FALSE)
### The above file was manually checked, and saved as "./csv/rare_organ_papers_checked.csv".

### The below lines are to manually check classifications when the corpus is updated.
###
### Loading the manually corrected file
### rare_organ_papers_checked <- read.csv(paste0(root_path, "csv/rare_organ_papers_checked.csv"))
### rare_organ_papers_combined <- rbind(rare_organ_papers_checked, 
###                                     rare_organ_papers %>% 
###                                       mutate(checked = "", major_organ = "")) %>% 
###   arrange(ID)
### write.csv(rare_organ_papers_combined, file = paste0(root_path, "csv/temps/rare_organ_papers_combined.csv"), row.names = FALSE)

### The above file was manually checked, and saved as "./csv/rare_organ_papers_checked.csv".
### Loading the manually corrected file
rare_organ_papers_checked <- read.csv(paste0(root_path, "csv/rare_organ_papers_checked.csv"))

### Selecting papers that needs manual correction.
organ_type_to_correct <- rare_organ_papers_checked %>% 
  filter(!checked == "OK")

### Selecting the documents whose organ type classification needs correcting, and then correct the classification.
organ_type_corrected <- pre_organ_classification %>% 
  filter(ID %in% organ_type_to_correct$ID) %>% 
  mutate(organ_type = organ_type_to_correct$checked, 
         major_organ = organ_type_to_correct$major_organ)

### Replacing the documents with manually adjusted documents.
pre_organ_classification2 <- pre_organ_classification %>% 
  filter(!ID %in% organ_type_to_correct$ID) %>% 
  rbind(., organ_type_corrected) %>% 
  arrange(ID)

colnames(pre_organ_classification2)






###
### 10.2. Assigning hierarchy to organ classification.
###

### Making a new column "organ_type_unspecified, based on the organ_type column.
### Here, if the value in the organ_type columns corresponds to first-level organ categories, 
### or second-level categories of gastrointestinal or neural categories, then "unspecified" is added before the value.
### For example, "brain" is changed to "unspecified brain".
### This is because in a subsequent graph, the number of papers on "brain" (or other) organoids is determined by 
### the sum of the number of papers in its lower categories. 
### Therefore, papers on unspecified part of brain need to have their own category under "brain" for this graph.
pre_organ_classification2_unspecifying <- pre_organ_classification2 %>% 
  mutate(organ_type_unspecified = 
           ifelse(is.na(organ_type), NA, 
                  ifelse(organ_type %in% c("multiple organs", "nasal", "venom gland", "unidentified"), organ_type, 
                         ifelse(organ_type %in% c(major_organ, "brain", "nerve", "spine", "intestine"), paste0("unspecified ", organ_type), 
                                organ_type)))) %>% 
  mutate(prenatal_BG_type_unspecified = gsub("^embryonic", "unspecified embryonic", prenatal_BG_type))


### Loading an edge list, where organ names are listed (in the "to" column) along with their immediately above categories ("from" column).
### The file also shows the first-level organ categories to which an organ category belongs ("major_organ" column), which we don't need right now.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv")) %>% 
  select(from, to)

### Extracting all organ names appearing in the edge list
edge_all_unique <- unique(c(edge_all$from, edge_all$to))

### Making a vector containing all detected organ types.
detected_organs <- unique(pre_organ_classification2$organ_type)

### Checking organ_type categories that do not appear in the edge list.
setdiff(detected_organs, edge_all_unique)

### Checking prenatal types that do not appear in the edge list.
setdiff(pre_organ_classification2$prenatal_BG_type, edge_all_unique)




###
### Assigning hierarchy to organoid classification
###
### The goal of this step is so that a paper is counted for all higher categories as well.
### For example, in the "neural brain - forebrain - cerebrum - cortex" hierarchical branch, 
### a paper on cortex is counted towards cerebrum, forebrain, brain, and neural.
###
### To this end, the above edge list is added to the classification data frame repeatedly.
pre_organ_classification_OR <- pre_organ_classification2_unspecifying %>% 
  mutate(from_OR = organ_type_unspecified) %>% 
  left_join(., edge_all, by = c("from_OR" = "to")) %>% 
  rename(from_OR5 = from) %>% 
  left_join(., edge_all, by = c("from_OR5" = "to")) %>% 
  rename(from_OR4 = from) %>%   
  left_join(., edge_all, by = c("from_OR4" = "to")) %>% 
  rename(from_OR3 = from) %>%   
  left_join(., edge_all, by = c("from_OR3" = "to")) %>% 
  rename(from_OR2 = from) %>% 
  left_join(., edge_all, by = c("from_OR2" = "to")) %>% 
  rename(from_OR1 = from) %>% 
  left_join(., edge_all, by = c("from_OR1" = "to")) %>% 
  rename(from_OR0 = from)

### Checking that the latest column (from_OR1) only contains the zero-level category of "body"
pre_organ_classification_OR %>% count(from_OR0)

### Assigning hierarchy to embryonic classification.
pre_organ_types <- pre_organ_classification_OR %>% 
  mutate(from_EM = prenatal_BG_type_unspecified) %>% 
  left_join(., edge_all, by = c("from_EM" = "to")) %>% 
  rename(from_EM7 = from) %>%   
  left_join(., edge_all, by = c("from_EM7" = "to")) %>% 
  rename(from_EM6 = from) %>%   
  left_join(., edge_all, by = c("from_EM6" = "to")) %>% 
  rename(from_EM5 = from) %>%   
  left_join(., edge_all, by = c("from_EM5" = "to")) %>% 
  rename(from_EM4 = from) %>%     
  left_join(., edge_all, by = c("from_EM4" = "to")) %>% 
  rename(from_EM3 = from) %>%   
  left_join(., edge_all, by = c("from_EM3" = "to")) %>% 
  rename(from_EM2 = from) %>% 
  left_join(., edge_all, by = c("from_EM2" = "to")) %>% 
  rename(from_EM1 = from)

pre_organ_types %>% count(from_EM1)

### Adding a corpus_F column, based on the existing corpus column.
### Here, the category "organoid" is split into "non-tumor" and "tumor" depending on whether the paper studies tumor organoid, 
### based on the column "tumor_type".
### Also including the "phase" column which shows whether or not the paper is from a recent period.
organ_types_F <- pre_organ_types %>% 
  mutate(corpus_F = ifelse(corpus == "organoid" & tumor_w == TRUE, "tumor_organoid", 
                           ifelse(corpus == "OoC" & tumor_w == TRUE, "ToC", corpus)))


### Saving the result.
save(organ_types_F, file = paste0(root_path, "R_results/organ_types_F"))

### Removing text fields.
organ_types_G <- organ_types_F %>% 
  select(!c(7:25, 28:34))

save(organ_types_G, file = paste0(root_path, "R_results/organ_types_G"))

### Removing text fields.
### Also removing tumor organoids and ToC corpora.
organ_types_P <- organ_types_F %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  select(!c(7:25, 28:34))

save(organ_types_P, file = paste0(root_path, "R_results/organ_types_P"))





##########
###
### Writing supplementary csv file showing organ types of publications.
###
##########

organ_models_organoid <- organ_types_F %>% 
  filter(type == "Research article", 
         corpus_F == "organoid", 
         !major_organ == "unidentified") %>% 
  select(author, year, title, doi, major_organ, organ_type) %>% 
  arrange(major_organ, organ_type, year, author)

write.csv(organ_models_organoid, file = paste0(root_path, "results/csv/table_S1_organ_models_organoid.csv"), row.names = FALSE)


organ_models_OoC <- organ_types_F %>% 
  filter(type == "Research article", 
         corpus_F == "OoC", 
         !major_organ == "unidentified") %>% 
  select(author, year, title, doi, major_organ, organ_type) %>% 
  arrange(major_organ, organ_type, year, author)

write.csv(organ_models_OoC, file = paste0(root_path, "results/csv/table_S2_organ_models_OoC.csv"), row.names = FALSE)
