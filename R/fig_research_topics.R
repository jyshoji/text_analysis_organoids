### The R scripts are to draw figures for research topics, including cell sources and organisms.
###
### The scripts include following parts.
### 1. Plotting trends of cell source usage.
### 2. Plotting trends of research organisms
### 3. Making a table of minor research organisms.
### 4. Plotting organs/substructures modelled for coronavirus research
###
### The graphs were saved at: 
### ./results/research_topics






##########
###
### 1. Plotting trends of cell source usage.
###
##########
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

load(paste0(root_path, "R_results/organ_types_P"))

load(paste0(root_path, "R_results/cell_types_P"))

colnames(cell_types_P)

### Same as above, except that cell sources are classified as "embryonic stem cell", "induced pluripotent stem cell", "unspecified pluripotent stem cell", 
### "adult cell", and "other.
cell_types_simplified <- cell_types_P %>% 
  mutate(corpus_F = organ_types_P$corpus_F) %>% 
  filter(!is.na(cell_type)) %>% 
  mutate(cell_type3 = ifelse(cell_type %in% c("mixed", "tumor cell", "unspecified stem cell"), "other",  
                             ifelse(cell_type %in% c("adult stem cell", "progenitor cell", "tissue"), "adult cell", 
                                    ifelse(cell_type == "pluripotent stem cell", "PSC", 
                                           ifelse(cell_type == "induced pluripotent stem cell", "iPSC", 
                                                  ifelse(cell_type == "embryonic stem cell", "ESC", NA)))))) %>% 
  mutate(cell_type3 = factor(cell_type3, 
                             levels = c("ESC", "iPSC", "PSC", 
                                        "adult cell", "other"))) %>% 
  group_by(year, corpus_F, type, .drop = FALSE) %>% 
  count(cell_type3) %>% 
  mutate(n_year = sum(n)) %>% 
  mutate(percentage_year = n / n_year) 

cell_sources_area <- cell_types_simplified %>% 
  filter(type == "Research article") %>% 
  ggplot(aes(x = year, y = percentage_year, fill = cell_type3, group = cell_type3)) + 
  geom_area() + 
  scale_fill_manual(values = c("deepskyblue3", "turquoise2", "royalblue3", "gold2", "grey60")) + 
  scale_x_discrete(breaks = c(2012, 2014, 2016, 2018, 2020, 2022)) + 
  labs(title = "B. Stem cell sources", y = "Ratio", fill = "Stem cell types") + 
  theme(text = element_text(size = 7), 
        plot.title = element_text(face = "bold"),
        legend.key.size = unit(3, "mm")) + 
  facet_wrap(~ factor(corpus_F, levels = c("organoid", "OoC")))

ggsave(cell_sources_area, 
       filename = paste0(root_path, "results/research_topics/cell_sources_area.png"),  
       width = 120, height = 60, units = "mm")




##########
###
### 2. Plotting trends of research organisms
###
##########
library(tidyverse)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/organ_types_P"))

load(paste0(root_path, "R_results/organisms_P"))

colnames(organisms_P)

### Same as above, except that only non-tumor organoid is plotted with "multiple organisms" removed.
###
### Calculating the number of publications and proportions of organisms studied with 3D culture model systems.
organisms_count <- organisms_P %>% 
  mutate(corpus_F = organ_types_P$corpus_F) %>% 
  filter(type == "Research article") %>% 
  filter(!is.na(organism)) %>% 
  filter(!grepl("\\bmixed\\b", organism)) %>% 
  mutate(organism2 = ifelse(organism %in% c("human", "mouse"), organism,"other")) %>% 
  mutate(organism2 = factor(organism2, 
                            levels = c("human", "mouse", "other"))) %>% 
  group_by(year, corpus_F, .drop = FALSE) %>% 
  count(organism2) %>% 
  mutate(n_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(ratio = n / n_total) 

organisms_area <- organisms_count %>% 
  ggplot(aes(x = year, y = ratio, fill = organism2, group = organism2)) + 
  geom_area() + 
  scale_fill_manual(values = c("royalblue3", "gold2", "grey60")) + 
  scale_x_discrete(breaks = c(2012, 2014, 2016, 2018, 2020, 2022)) + 
  labs(title = "A. Research organisms", y = "Ratio", fill = "Organisms") + 
  theme(text = element_text(size = 7), 
        plot.title = element_text(face = "bold"),
        legend.key.size = unit(3, "mm")) + 
  facet_wrap(~ factor(corpus_F, levels = c("organoid", "OoC")))

ggsave(organisms_area, 
       filename = paste0(root_path, "results/research_topics/organisms_area.png"),  
       width = 120, height = 60, units = "mm")





##########
###
### 3. Making a table of minor research organisms.
###
##########
library(tidyverse)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/organ_types_P"))

load(paste0(root_path, "R_results/organisms_P"))

colnames(organisms_P)

organisms_counts <- organisms_P %>% 
  select(ID, type, starts_with("TF_")) %>% 
  mutate(corpus_F = organ_types_P$corpus_F) %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  filter(type == "Research article") %>% 
  pivot_longer(starts_with("TF_"), names_to = "research_organisms") %>% 
  filter(value == TRUE) %>% 
  group_by(corpus_F) %>% 
  count(research_organisms) %>% 
  pivot_wider(names_from = "corpus_F", values_from = "n") %>% 
  arrange(factor(research_organisms, levels = colnames(organisms_P %>% select(starts_with("TF_"))))) %>% 
  mutate(OoC = replace_na(OoC, 0)) %>% 
  select(1, 3, 2)
  
write.csv(organisms_counts, file = paste0(root_path, "results/csv/organisms_counts.csv"), row.names = FALSE)




##########
###
### 4. Plotting organs/substructures modelled for coronavirus research
###
### The graph aims to show organ types researched in coronavirus research using 3D culture model systems.
###
##########

library(tidyverse)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/organ_types_P"))

load(paste0(root_path, "R_results/research_topics_P"))

colnames(organ_types_P)

colnames(research_topics_P)

### Selecting papers studying coronavirus
covid_papers <- organ_types_P %>% 
  mutate(TF_coronavirus = research_topics_P$TF_coronavirus) %>% 
  filter(TF_coronavirus == TRUE) 

### Checking what kind of organs are studied.
covid_papers %>% filter(type == "Research article") %>% 
  count(organ_type, sort = TRUE)
covid_papers %>% filter(type == "Research article") %>% 
  count(major_organ, sort = TRUE)
covid_papers %>% filter(type == "Research article") %>% 
  count(year)


### Based on the above, following categories are used for plots: 
### "lung", "gastrointestinal", "airway", "respiratory", "brain", "kidney", "hepatic, pancreatic, biliary"
### First choosing major organ types to plot

### Changing classifications to include "airway" and "lung" in addition to major organ types.
covid_papers_recategorized <- covid_papers %>% 
  mutate(major_organ_covid = 
           ifelse(organ_type %in% c("alveolus", "bronchus", "bronchioles", "lung"), "lung", 
                  ifelse(organ_type %in% c("trachea", "tracheosphere", "airway"), "airway", 
                         ifelse(organ_type %in% c("brain", "kidney"), organ_type, 
                                ifelse(major_organ %in% c("cardiovascular", "gastrointestinal", "hepatic, pancreatic, biliary", "ocular", "respiratory"), 
                                       major_organ, "other"))))) %>% 
  mutate(major_organ_covid = factor(major_organ_covid, 
                                    levels = c("cardiovascular", "gastrointestinal", "hepatic, pancreatic, biliary", "brain", "ocular", 
                                               "airway", "lung", "respiratory", "kidney", "other"))) 

covid_papers_recategorized %>% count(major_organ_covid)

covid_organs_yearly <- covid_papers_recategorized %>% 
  filter(type == "Research article") %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  mutate(corpus_F = factor(corpus_F, levels = c("organoid", "OoC"))) %>% 
  ggplot(aes(x = year, fill = major_organ_covid)) + 
  geom_bar(color = "black", size = 0.1) + 
  labs(title = "Organ models for coronavirus research", 
       x = "Year", 
       y = "The number of research articles", 
       fill = "Organ models") + 
  scale_x_discrete(limits = factor(c(2017:2022))) + 
  scale_fill_manual(values = c("seagreen", "orange","red",  "blue", "springgreen3", "mediumpurple", "magenta3", "purple", "pink", "grey60")) + 
  facet_wrap(~ corpus_F) + 
  theme(text = element_text(size = 8), 
        plot.title = element_text(size = 9, face = "bold"), 
        legend.key.size = unit(2.5, "mm"))

ggsave(covid_organs_yearly, 
       filename = paste0(root_path, "results/research_topics/covid_organs_yearly.png"),  
       width = 210, height = 70, units = "mm")
