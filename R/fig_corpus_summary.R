### This R scripts are to draw graphs summarizing publication counts in organoid and organ-on-a-chip corpora.
###
### The scripts use "./R_results/organ_types_G" as an input file.
###
### The scripts include following parts.
### 1. Making a table showing the number of publications.
### 2. Calculating average yearly growth of publication counts on 3D culture mode systems, as compared with tumor research.
### 3. Plotting publication counts of 3D culture model systems.
###

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/organ_types_G"))





##########
###
### 1. Making a table showing the number of publications.
###
##########
table_publication_counts <- organ_types_G %>% 
  group_by(type) %>% 
  count(corpus_F) %>% 
  rename(Publication_counts = n) %>% 
  mutate(corpus_F = gsub("_", " ", corpus_F)) %>% 
  pivot_wider(names_from = type, values_from = Publication_counts) %>% 
  arrange(factor(corpus_F, levels = c("organoid", "tumor organoid", "OoC", "ToC")))

write.csv(table_publication_counts, file = paste0(root_path, "results/csv/table_publication_counts.csv"), row.names = FALSE)







##########
###
### 2. Calculating average yearly growth of publication counts on 3D culture mode systems, as compared with tumor research.
###
##########

### Importing the publication count data of tumor research.
tumor_publication <- read.csv(paste0(root_path, "csv/PubMed_Timeline_Results_by_Year.csv"), skip = 1)

### Adjusting the publication count data.
tumor_publication_count <- tumor_publication %>% 
  # Assigning the corpus name "tumor research" to the data.
  mutate(corpus_F = "tumor research", 
         type = "Research article") %>% 
  # changing column names to be consistent with the organoid/organ-on-a-chip corpus.
  rename(year = Year, 
         n = Count) %>% 
  # reordering rows by year
  arrange(year) %>% 
  # changing the column order.
  select(4, 3, 1, 2)

### Calculating publication counts of organoid/organ-on-a-chip publications in each corpus.
organoid_publication_count <- organ_types_G %>% 
  # calculating publication counts in each year for each corpus.
  group_by(type, corpus_F) %>% 
  count(year) %>% 
  ungroup() %>% 
  # Combining with the above publication counts for tumor research.
  rbind(., tumor_publication_count)

### Calculating average growth of publication counts
organoid_average_growth <- organoid_publication_count %>% 
  # First making the "year_plus_one" column, which contains publication counts in the year after.
  mutate(year_plus_one = n[c(2:nrow(.), 1)]) %>% 
  # calculating "growth", which is annual growth in publication counts, calculated as 
  # (publication counts in a "year + 1" / publication counts in a year).
  mutate(growth = year_plus_one / n) %>% 
  # Only selecting the years between 2011 and 2020.
  filter(year %in% c(2011:2021)) %>% 
  # calculating average growth for each corpus.
  group_by(type, corpus_F) %>% 
  mutate(average_growth = mean(growth)) %>% 
  ungroup()

### Saving the results as a csv file.
write.csv(organoid_average_growth, file = paste0(root_path, "results/csv/organoid_average_growth.csv"), row.names = FALSE)








##########
###
### 3. Plotting publication counts of 3D culture model systems.
###
##########

### Loading the input data.
load(paste0(root_path, "R_results/organ_types_G"))

### Plotting the number of publications on organoids and OoC
publication_by_corpus <- organ_types_G %>% 
  #filter(corpus_F %in% c("organoid", "OoC")) %>% 
  mutate(type = factor(type, levels = c("Preprint", "Research article", "Review"))) %>% 
  mutate(corpus_F = gsub("tumor_organoid", "tumor organoid", corpus_F)) %>% 
  mutate(corpus_F = factor(corpus_F, levels = c("organoid", "tumor organoid", "OoC", "ToC"))) %>% 
  ggplot(aes(x = year, fill = type)) + 
  geom_bar() + 
  facet_wrap(~ corpus_F, ncol = 4) + 
  labs(#title = "The number of academic publications on 3D culture model systems", 
       y = "The number of publications") + 
  scale_fill_manual(values = c("royalblue3", "gold2", "green4"), name = "Publication types") + 
  scale_x_discrete(breaks = c(2012, 2014, 2016, 2018, 2020, 2022)) + 
  theme(text = element_text(size = 7), 
        axis.text.x.bottom = element_text(size = 5), 
        #legend.title = element_text(size = 5), 
        legend.key.size = unit(2, "mm")) 

### Saving as a png file.
ggsave(publication_by_corpus, filename = paste0(root_path, "results/corpus_summary/publication_by_corpus.pdf"), 
       width = 178, height = 50, units = "mm")


