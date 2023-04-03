### This R script file is for identifying research topics that appear in academic publications on 
### organoid or organ-on-a-chip.
###
### !!! Note !!!
### This step is for manually checking and picking up research topics from frequently occurring phrases.
### If you want to skip this step, ignore the entire code of this script file, and go ahead to "./R/research_topics.R".
###
### Overall, frequently occurring phrases were extracted and checked manually, and key research topics were selected.
###
### The outcomes of the code here were used to make lists of research topics to capture in "./R/research_topics.R".

### Loading packages
library(tidyverse)
library(tidytext)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.
### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/all_corpus"))

### Loading stop words.
### "Stop words" here mean very common words such as "in", "a", etc.
data(stop_words)

### Splitting sentences into phrases
phrases <- all_corpus %>% 
  select(ID, text_all_mod) %>% 
  ### As "cell/cells" and "organoid/organoids" frequently appear, they are all changed to singular forms.
  mutate(text_all_mod = gsub("\\bcells\\b", "cell", text_all_mod, ignore.case = TRUE)) %>% 
  mutate(text_all_mod = gsub("\\borganoids\\b", "organoid", text_all_mod, ignore.case = TRUE)) %>%   
  ### Splitting sentences into phrases at either full stop, colon, semicolon, opening and closing parentheses.
  unnest_tokens(phrase, text_all_mod, token = stringr::str_split, pattern = "\\.|;|:|\\(|\\)")




##########
###
### Pentagrams
###
##########

### Splitting phrases into pentagrams.
pentagrams <- phrases %>% 
  unnest_tokens(pentagram, phrase, token = "ngrams", n = 5) %>% 
  separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") 

### Discarding pentagrams that include stop words.
pentagrams_filtered <- pentagrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word & 
           !word4 %in% stop_words$word & 
           !word5 %in% stop_words$word)

### Counting the number of papers that mention the pentagrams.
pentagrams_counted <- pentagrams_filtered %>% 
  unite(pentagram, word1, word2, word3, word4, word5, sep = " ") %>% 
  distinct(.) %>% 
  count(pentagram, sort = TRUE) %>% 
  filter(n > 1)
### Pentagrams that occur in > 2 papers were manually checked.

### Counting the number of papers that mention pentagrams related to disease.
pentagrams_disease <- pentagrams_filtered %>% 
  mutate(word5 = gsub("diseases\\b", "disease", word5)) %>% 
  mutate(word5 = gsub("disorders\\b", "disorder", word5)) %>% 
  mutate(word5 = gsub("syndromes\\b", "syndrome", word5)) %>%  
  mutate(word5 = gsub("infections\\b", "infection", word5)) %>% 
  mutate(word5 = gsub("injuries\\b", "injury", word5)) %>% 
  mutate(word5 = gsub("failures\\b", "failure", word5)) %>% 
  mutate(word5 = gsub("dystrophies\\b", "dystrophy", word5)) %>% 
  filter(word5 %in% c("disease", "disorder", "syndrome", "infection", "injury", "failure", "dystrophy")) %>% 
  unite(pentagram, word1, word2, word3, word4, word5, sep = " ") %>% 
  distinct(.) %>% 
  count(pentagram, sort = TRUE) %>% 
  filter(n > 1)





##########
###
### Tetragrams
###
##########

### Splitting phrases into tetragrams.
tetragrams <- phrases %>% 
  unnest_tokens(tetragram, phrase, token = "ngrams", n = 4) %>% 
  separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") 

### Discarding tetragrams that include stop words
tetragrams_filtered <- tetragrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word & 
           !word4 %in% stop_words$word)

### Counting the number of papers that mention the tetragrams.
tetragrams_counted <- tetragrams_filtered %>% 
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  distinct(.) %>% 
  count(tetragram, sort = TRUE) %>% 
  filter(n > 1)
### Tetragrams that occur in > 4 papers were manually checked.

### Counting the number of papers that mention tetragrams related to disease.
tetragrams_disease <- tetragrams_filtered %>% 
  mutate(word4 = gsub("diseases\\b", "disease", word4)) %>% 
  mutate(word4 = gsub("disorders\\b", "disorder", word4)) %>% 
  mutate(word4 = gsub("syndromes\\b", "syndrome", word4)) %>%  
  mutate(word4 = gsub("infections\\b", "infection", word4)) %>% 
  mutate(word4 = gsub("injuries\\b", "injury", word4)) %>% 
  mutate(word4 = gsub("failures\\b", "failure", word4)) %>% 
  mutate(word4 = gsub("dystrophies\\b", "dystrophy", word4)) %>% 
  filter(word4 %in% c("disease", "disorder", "syndrome", "infection", "injury", "failure", "dystrophy")) %>% 
  unite(tetragram, word1, word2, word3, word4, sep = " ") %>% 
  distinct(.) %>% 
  count(tetragram, sort = TRUE) %>% 
  filter(n > 1)




##########
###
### trigrams
###
##########

### Splitting phrases into trigrams.
trigrams <- phrases %>% 
  unnest_tokens(trigram, phrase, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") 

### Discarding trigrams that include stop words 
trigrams_filtered <- trigrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word & 
           !word3 %in% stop_words$word)

### Counting the number of papers that mention the trigrams.
trigrams_counted <- trigrams_filtered %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  distinct(.) %>% 
  count(trigram, sort = TRUE) %>% 
  filter(n > 1)
### Trigrams that occur in > 9 papers were manually checked.

### Counting the number of papers that mention trigrams related to disease.
trigrams_disease <- trigrams_filtered %>% 
  mutate(word3 = gsub("diseases\\b", "disease", word3)) %>% 
  mutate(word3 = gsub("disorders\\b", "disorder", word3)) %>% 
  mutate(word3 = gsub("syndromes\\b", "syndrome", word3)) %>%  
  mutate(word3 = gsub("infections\\b", "infection", word3)) %>% 
  mutate(word3 = gsub("injuries\\b", "injury", word3)) %>% 
  mutate(word3 = gsub("failures\\b", "failure", word3)) %>% 
  mutate(word3 = gsub("dystrophies\\b", "dystrophy", word3)) %>% 
  filter(word3 %in% c("disease", "disorder", "syndrome", "infection", "injury", "failure", "dystrophy")) %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  distinct(.) %>% 
  count(trigram, sort = TRUE) %>% 
  filter(n > 1)





##########
###
### bigrams
###
##########

### Splitting phrases into bigrams.
bigrams <- phrases %>% 
  unnest_tokens(bigram, phrase, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") 

### Discarding bigrams that include stop words 
bigrams_filtered <- bigrams %>% 
  filter(!is.na(word1)) %>% 
  filter(!word1 %in% stop_words$word & 
           !word2 %in% stop_words$word)

### Counting the number of papers that mention the bigrams.
bigrams_counted <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  distinct(.) %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 1)
### Bigrams that occur in > 30 papers were manually checked.

### Counting the number of papers that mention bigrams related to disease.
bigrams_disease <- bigrams_filtered %>% 
  mutate(word2 = gsub("diseases\\b", "disease", word2)) %>% 
  mutate(word2 = gsub("disorders\\b", "disorder", word2)) %>% 
  mutate(word2 = gsub("syndromes\\b", "syndrome", word2)) %>%  
  mutate(word2 = gsub("infections\\b", "infection", word2)) %>% 
  mutate(word2 = gsub("injuries\\b", "injury", word2)) %>% 
  mutate(word2 = gsub("failures\\b", "failure", word2)) %>% 
  mutate(word2 = gsub("dystrophies\\b", "dystrophy", word2)) %>% 
  filter(word2 %in% c("disease", "disorder", "syndrome", "infection", "injury", "failure", "dystrophy")) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  distinct(.) %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 1)




##########
###
### words
###
##########

### Splitting phrases into words.
words <- phrases %>% 
  unnest_tokens(word, phrase, token = "words") 

### Discarding words 
words_filtered <- words %>% 
  filter(!is.na(word)) %>% 
  filter(!word %in% stop_words$word) 

### Counting the number of papers that mention the words.
words_counted <- words_filtered %>% 
  mutate(word = gsub("s$", "", word)) %>% 
  distinct(.) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 1)
### Words that occur in > 50 papers were manually checked.




##########
###
### Only looking at the keyword field
###
##########

keywords <- all_corpus %>% 
  select(ID, keywords) %>% 
  mutate(keywords = gsub("\\bcells\\b", "cell", keywords, ignore.case = TRUE)) %>% 
  mutate(keywords = gsub("\\borganoids\\b", "organoid", keywords, ignore.case = TRUE)) %>%   
  unnest_tokens(phrase, keywords, token = stringr::str_split, pattern = "\\.|,|;|:|\\(|\\)|\\band\\b") %>% 
  filter(!is.na(phrase)) %>% 
  mutate(phrase = gsub("^ +| +$", "", phrase)) %>% 
  distinct(.) %>% 
  count(phrase, sort = TRUE) %>% 
  filter(n > 1)
### Keywords that occur in > 4 papers were manually checked.



##########
###
### Identifying words to be considered for organ development
###
##########

### Extracting words that appear before "development"
dev_words <- unlist(str_extract_all(tolower(all_corpus$text_all_mod), "[a-z]+\\)?[- ]developments?\\b"))

pre_dev <- unique(gsub("\\)?[- ]developments?", "", dev_words))

### Making a data frame containing the list of detected words.
pre_dev_df <- data.frame(pre_dev = pre_dev, include = "")

### Saving as a csv file.
write.csv(pre_dev_df, file = paste0(root_path, "csv/temps/pre_dev_df.csv"), row.names = FALSE)
### The csv file was manually checked, and either "y" or "n" was assigned to the "include" column.
### If words with "y" in the "include" column appear before "development", then they are considered as "organ development".
### The resulting file was saved as "./csv/pre_dev_F.csv"






##########
###
### Identifying words ending with "-oma" to consider as tumor.
###
##########

### Making a vector containing potential tumor-related words.
tumor_words <- c("cancers?", "tumou?rs?", "[a-z]*omas?", "neoplasms?", "neoplastic", 
                 "metastas[ei]s", "metastatic", "tumorigenesis", "carcinogenesis", 
                 "oncogenesis", "oncology", "carcinoids?")

### Capturing the words in the above vector
tumor_detected <- unique(unlist(str_extract_all(tolower(all_corpus$text_all_mod), paste0("\\b", tumor_words, "\\b", collapse = "|"))))

### Making a data frame containing the captured words.
tumor_detected_df <- data.frame(tumor = tumor_detected, include = "")

### Saving the data frame.
write.csv(tumor_detected_df, file = paste0(root_path, "csv/temps/tumor_detected_df.csv"), row.names = FALSE)

### The file was manually checked so that words to be considered as tumor have "y" in the "include" column.
### The file was saved as "./csv/tumor_detected_F.csv".