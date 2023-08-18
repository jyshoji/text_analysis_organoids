### This R script file is for detecting organisms used in research.
###
### The input file is "./R_results/all_corpus"
### 
### The file includes following steps: 
### 1. Identifying phrases containing organism names that have to be excluded.
### 2. Deleting phrases containing animal names that have to be excluded.
### 3. Making a nested list of organisms' names to capture
### 4. Capturing organisms' names
### 
### The output of the codes are saved as:
### "./R_results/organisms_F"
### And fed to ./R/research_topics.R

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file
load(paste0(root_path, "R_results/all_corpus"))

### Making a vector containing relevant animal names.
animals_w <- c("canines?", "dogs?", "bovines?", "cows?", "calf", 
               "calves", "porcines?", "swines?", "pigs?", 
               "chickens?", "poultry", "avians?", "birds?", "felines?", "zebrafish", "fish", "fly", "flies", 
               "drosophila", "horses?", "equines?", "monkeys?", "macaques?", 
               "primates?", "catarrhines?", "apes?", "gorillas?", "chimpanzees?", 
               "orangutans?", "rabbits?", "cone?ys?", "snakes?", "ophidia", "hares?", "turtles?", "bats?")






##########
###
### 1. Identifying phrases containing animal names that have to be excluded.
###
##########

### !!! Note !!!
### This step is to manually identify phrases that include animal names that should not be considered as research organisms.
### If you want to skip this step, ignore the following lines and resume 
### at: 2. Deleting phrases containing animal names that have to be excluded.

### Animal names that appear in specific phrases (e.g., bovine serum albumin) should not be considered as research organisms.
### This section is for identifying such phrases, in order to later ignore them before determining research organisms.

### Loading a package.
library(tidytext)

### Loading stop words.
data("stop_words")

### Splitting sentences to chunks.
phrases <- all_corpus %>% 
  filter(!is.na(text_all_mod)) %>% 
  select(ID, text_all_mod) %>% 
  unnest_tokens(phrase, text_all_mod, token = stringr::str_split, pattern = "\\.|;|:|,") 

### Extracting pentagrams (i.e., five words occurring together), and selecting those that contain animal names.
pentagrams <- phrases %>% 
  unnest_tokens(pentagram, phrase, token = "ngrams", n = 5) %>% 
  filter(grepl(paste0("\\b", animals_w, "\\b", collapse = "|"), pentagram)) %>% 
  separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")  %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  unite(pentagram, word1, word2, word3, word4, word5, sep = " ") %>% 
  count(pentagram, sort = TRUE)
### The above pentagrams (> 1 occurrence) were manually checked, and phrases to be ignored were determined.

### Extracting tetragrams, and selecting those that contain animal names.
tetragrams <- phrases %>% 
  unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
  filter(grepl(paste0("\\b", animals_w, "\\b", collapse = "|"), tetragram)) %>% 
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ")  %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  count(tetragram, sort = TRUE)
### Tetragrams with > 2 occurrence were checked. 


### Extracting trigrams, and selecting those that contain animal names.
trigrams <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
  filter(grepl(paste0("\\b", animals_w, "\\b", collapse = "|"), trigram)) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ")  %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  count(trigram, sort = TRUE)
### Trigrams with > 2 occurrence were checked. 

### Extracting bigrams, and selecting those that contain animal names.
bigrams <- phrases %>% 
  unnest_tokens(bigram, phrase, token = "ngrams", n = 2) %>% 
  filter(grepl(paste0("\\b", animals_w, "\\b", collapse = "|"), bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")  %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE)
### Bigrams with > 3 occurrence were checked. 








##########
###
### 2. Deleting phrases containing animal names that have to be excluded.
###
##########

### The previous step identified phrases containing animal names that need to be ignored.
### Making a vector containing such phrases.
animals_exclude_w <- c("madin[- ]darby canine kidney", "bovine serum", "bovine aortic endothelial cells?", 
                       "porcine liver extracellular matrix", "hybridization \\(fish", "avian influenza", "calf serum", 
                       "decellularized [a-z]* ?porcine", "acellular [a-z]* ?porcine", "chicken serum", "immortalized bovine", 
                       "bovine[- ]human reassortant [a-z]* ?vaccine", "bovine G6P\\[5\\] WC3", "simian [a-z]* ?virus", 
                       "trojan horse")

### In the corpus data frame, new columns (ending with "_aex") of text fields were made, in which the above phrases were deleted.
### Also, "guinea pig" was changed to "guineapig" so that it's not captured as "pig".
aex_species <- all_corpus %>% 
  mutate(title_key_aex = gsub(
    paste0("\\b", animals_exclude_w, "\\b", collapse = "|"), "", title_key, ignore.case = TRUE)) %>% 
  mutate(KA_key_aex = gsub(
    paste0("\\b", animals_exclude_w, "\\b", collapse = "|"), "", keywords_abstract_key, ignore.case = TRUE)) %>% 
  mutate(title_mod_aex = gsub(
    paste0("\\b", animals_exclude_w, "\\b", collapse = "|"), "", title_mod, ignore.case = TRUE)) %>% 
  mutate(KA_mod_aex = gsub(
    paste0("\\b", animals_exclude_w, "\\b", collapse = "|"), "", keywords_abstract_mod, ignore.case = TRUE)) %>% 
  mutate(across(c(28:31), ~ gsub("\\b[Gg]uinea [Pp]igs?\\b", "guineapig", .)))






##########
###
### 3. Making a nested list of organisms' names to capture
###
##########
organisms_names <- list(
  human_w = c("[Hh]umans?", "[Pp]atients?", "[Pp]eople"), 
  murine_w = c("[Mm]ouse", "[Mm]ice", "[Mm]urines?", "[Rr]odents?", "[Rr]ats?"), 
  macaque_w = c("[Mm]onkeys?", "[Mm]acaques?", "[Rr]hesus"), 
  ape_w = c("[Aa]pes?", "[Gg]orillas?", "[Cc]himpanzees?", "[Oo]rangutans?"),   
  canine_w = c("[Cc]anines?", "[Dd]ogs?"), 
  feline_w = c("[Ff]elines?", "[Cc]ats?"),   
  bovine_w = c("[Bb]ovines?", "[Cc]ows?", "[Cc]alf", "[Cc]alves"), 
  swine_w = c("[Pp]orcines?", "[Ss]wines?", "[Pp]igs?"), 
  equine_w = c("[Hh]orses?", "[Ee]quines?"),  
  sheep_w = c("[Ss]heep", "[Oo]vines?"), 
  rabbit_w = c("[Rr]abbits?", "[Cc]one?ys?", "[Hh]ares?"), 
  bat_w = c("[Bb]ats?", "[Cc]arollia", "[Rr]ousettus", "[Rr]hinolophus", "[Ee]onycteris"), 
  avian_w = c("[Cc]hickens?", "[Pp]oultry", "[Aa]vians?", "[Bb]irds?", "[Cc]hicks?"), 
  snake_w = c("[Ss]nakes?", "[Oo]phidia"), 
  zebrafish_w = c("[Zz]ebrafish", "[Ff]ish"), 
  fly_w = c("[Ff]ly", "[Ff]lies", "Drosophila"), 
  xenopus_w = c("[Xx]enopus", "[Ff]rogs?"), 
  turtle_w = c("[Tt]urtles?"), 
  plant_w = c("[Bb]rassica", "[Pp]lants?")
  ) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))

### Checking the list of organisms' names.
print(organisms_names)






##########
###
### 4. Capturing organisms' names
###
##########

colnames(aex_species)

### Detecting organisms' names in title_key (i.e, titles containing the term "organoid"/"onchip")
TF_key_title <- as.data.frame(sapply(organisms_names, function(x) grepl(x, aex_species$title_key_aex))) %>% 
  rename_with(~ gsub("_w", "_key_title", .))

### Detecting organisms' names in KA_key (i.e, keywords_abstract containing the term "organoid"/"onchip")
TF_key_KA <- as.data.frame(sapply(organisms_names, function(x) grepl(x, aex_species$KA_key_aex))) %>% 
  rename_with(~ gsub("_w", "_key_KA", .))

### Detecting organisms' names in titles 
TF_mod_title <- as.data.frame(sapply(organisms_names, function(x) grepl(x, aex_species$title_mod_aex))) %>% 
  rename_with(~ gsub("_w", "_mod_title", .))

### Detecting organisms' names in keywords_abstracts
TF_mod_KA <- as.data.frame(sapply(organisms_names, function(x) grepl(x, aex_species$KA_mod_aex))) %>% 
  rename_with(~ gsub("_w", "_mod_KA", .))

### Adding the above four logical data frames to the aex_species data frame.
TF_organisms <- aex_species %>% 
  cbind(TF_key_title, TF_key_KA, TF_mod_title, TF_mod_KA)

colnames(TF_organisms)

### Modifying the logical columns.
### Basically, classification is made with priority in the order of key_title > key_KA > mod_title > mode_KA.
### If there is TRUE for a higher priority classification, then lower classifications are all changed to FALSE.
modified_TF_organisms <- TF_organisms %>% 
  mutate(across(c(51:107), ~ ifelse(rowSums(across(ends_with("_key_title"))) > 0, FALSE, .))) %>% 
  mutate(across(c(70:107), ~ ifelse(rowSums(across(ends_with("_key_KA"))) > 0, FALSE, .))) %>% 
  mutate(across(c(89:107), ~ ifelse(rowSums(across(ends_with("_mod_title"))) > 0, FALSE, .)))

### Making summary TF_ columns, where the value is TRUE if any of the columns for the corresponding organism is TRUE.
pre_organism <- modified_TF_organisms %>% 
  mutate(TF_human = rowSums(across(starts_with("human_")))) %>% 
  mutate(TF_mouse = rowSums(across(starts_with("murine_")))) %>% 
  mutate(TF_monkey = rowSums(across(starts_with("macaque_")))) %>% 
  mutate(TF_ape = rowSums(across(starts_with("ape_")))) %>% 
  mutate(TF_dog = rowSums(across(starts_with("canine_")))) %>% 
  mutate(TF_cat = rowSums(across(starts_with("feline_")))) %>% 
  mutate(TF_cow = rowSums(across(starts_with("bovine_")))) %>% 
  mutate(TF_pig = rowSums(across(starts_with("swine_")))) %>% 
  mutate(TF_horse = rowSums(across(starts_with("equine_")))) %>% 
  mutate(TF_sheep = rowSums(across(starts_with("sheep_")))) %>%   
  mutate(TF_rabbit = rowSums(across(starts_with("rabbit_")))) %>% 
  mutate(TF_bat = rowSums(across(starts_with("bat_")))) %>% 
  mutate(TF_bird = rowSums(across(starts_with("avian_")))) %>% 
  mutate(TF_snake = rowSums(across(starts_with("snake_")))) %>% 
  mutate(TF_fish = rowSums(across(starts_with("zebrafish_")))) %>% 
  mutate(TF_fly = rowSums(across(starts_with("fly_")))) %>% 
  mutate(TF_frog = rowSums(across(starts_with("xenopus_")))) %>% 
  mutate(TF_turtle = rowSums(across(starts_with("turtle_")))) %>% 
  mutate(TF_plant = rowSums(across(starts_with("plant_")))) 

colnames(pre_organism)



### !!! Note !!!
### This step is for manually checking and correcting organism classification for minor research organisms.
### If you want to skip this step, ignore the following line, and instead run the below line.
### pre_organism_replaced <- pre_organism
### Then resume at: 4.2. Summarizing the organism classification.
###
###
### Below, the logical columns for minor organisms (organisms which are not human nor mouse) are manually checked.
minor_organisms <- pre_organism %>% 
  ### Selecting research articles
  filter(type == "Research article") %>% 
  ### Selecting papers that are TRUE for minor organisms
  filter(rowSums(.[, c(110:126)]) > 0) %>% 
  ### selecting relevant columns
  select(1, 10, 6, 110:126) %>% 
  ### Making a new column
  mutate(checked = "") 

### Saving as a csv file.
### write.csv(minor_organisms, file = paste0(root_path, "csv/temps/minor_organisms.csv"), row.names = FALSE)
### This file was manually checked, and TRUE/FALSE were modified so that the paper is TRUE only when 
### the organism mentioned is used as a source for organoid/onchip.
### The file was saved as "./csv/minor_organisms_F.csv"

### The lines below are to manually check the classifications when the corpus is updated.
minor_organisms_F <- read.csv(paste0(root_path, "csv/minor_organisms_F.csv")) 

minor_organisms_previous <- minor_organisms %>% 
  filter(ID %in% minor_organisms_F$ID)

minor_organisms_update <- minor_organisms_F %>% 
  mutate(TF_bat = minor_organisms_previous$TF_bat, 
         TF_turtle = minor_organisms_previous$TF_turtle, 
         TF_plant = minor_organisms_previous$TF_plant) %>% 
  select(any_of(colnames(minor_organisms_previous))) %>% 
  rbind(., 
        minor_organisms %>% filter(!ID %in% minor_organisms_F$ID)) %>% 
  arrange(ID)
  
write.csv(minor_organisms_update, file = paste0(root_path, "csv/temps/minor_organisms_update.csv"), row.names = FALSE)

minor_organisms_F <- read.csv(paste0(root_path, "csv/minor_organisms_F.csv"))

colnames(pre_organism)
colnames(minor_organisms_F)

### Checking the number of articles where the classifications were manually changed.
### Skip this line if you did not manually correct the classification above.
changed_papers <- data.frame(before = rowSums(minor_organisms[, 4:20]), after = rowSums(minor_organisms_F[, 4:20])) %>% 
  filter(!before == after)

### Replacing the corresponding columns/rows with the manually checked data frame.
### For this purpose, the corresponding papers (rows) are first selected from the pre_organism data frame.
### TF_" columns for minor organisms are then replaced with the manually checked TF_" columns.
minor_organisms_replaced <- pre_organism %>% 
  filter(ID %in% minor_organisms_F$ID) %>% 
  select(!c(110:126)) %>% 
  left_join(., minor_organisms_F[, c(1, 4:20)], by = "ID")

### Then, the corresponding rows in the pre_organism data frame are replaced with the above data frame.
pre_organism_replaced <- pre_organism %>% 
  filter(!ID %in% minor_organisms_F$ID) %>% 
  rbind(., minor_organisms_replaced) %>% 
  arrange(ID)



###
### 4.2. Summarizing the organism classification.
###

colnames(pre_organism_replaced)

### Making a summary data frame.
organisms_F <- pre_organism_replaced %>% 
  ### "TF_" columns are changed from integer to logical
  mutate(across(c(108:126), ~ as.logical(.))) %>% 
  ### Making a summary column "organism".
  mutate(organism = 
           ifelse(TF_human == TRUE & rowSums(.[, 109:126]) > 0, "mixed (human and non-human)", 
                  ifelse(rowSums(.[, 109:126]) > 1, "mixed (non-human)", 
                         ifelse(rowSums(.[, 108:126]) == 1, colnames(.[, 108:126])[max.col(.[, 108:126])], NA)))) %>% 
  mutate(organism = gsub("TF_", "", organism))

### Saving the result.
save(organisms_F, file = paste0(root_path, "R_results/organisms_F"))

### Loading an R data file that shows which publications are on organoids/OoCs, rather than on tumor organoids/ToCs.
load(paste0(root_path, "R_results/organ_types_P"))

### Removing text fields.
### Also removing tumor organoids and ToC corpora.
organisms_P <- organisms_F %>% 
  filter(ID %in% organ_types_P$ID) %>%  
  select(!c(7:25, 28:31))

save(organisms_P, file = paste0(root_path, "R_results/organisms_P"))
