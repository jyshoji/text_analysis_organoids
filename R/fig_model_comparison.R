### This R script file is to draw graphs to compare organoid and OoC model systems.
###
### The script include the following steps.
### 1. Comparing organ/substructures modelled as organoids, OoCs, and organoids-on-a-chip.
### 2. Comparison of research topics in organoid and OoC research.
###
### The figures were saved at: 
### ./results/model_comparison/






##########
###
### 1. Comparing organ/substructures modelled as organoids, OoCs, and organoids-on-a-chip
###
##########

library(tidyverse)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/organ_types_P"))


### Identifying publications that mention organoids-on-a-chip (i.e., being present in both "organoid" and "OoC" corpora), 
corpus_overlap <- organ_types_P %>% 
  ### adding a new column, showing the number of documents that have the identical doi.
  group_by(doi) %>% 
  mutate(no_copy = n()) %>% 
  ungroup() %>% 
  ### The documents on organoid-on-chip are present as two copies in the entire corpus, 
  ### one in the organoid corpus and the other in the OoC corpus.
  ### Note that organ type classifications in the pair of copies are different, as the organ classification in organoid corpus 
  ### is based on organ names that occur with "organoid", whereas that in OoC corpus is based on organ names that occur with "onchip".
  ### As we are interested in types of "organoids" researched in OoC setups, we will select the copy in the orgnaoid corpus, and use 
  ### its organ type classification for subsequent analysis.
  ###
  ### Making a new column (corpus_D) which shows if a document is on organoid-on-chip.
  ### If a document is in the "organoid" corpus (including tumor organoid) and has an extra copy in the "OoC" corpus, the column value 
  ### is "organoid-on-chip".
  ### If a document is in the "OoC" corpus (including ToC) and has an extra copy in the "organoid" corpus, the column value 
  ### is "duplicate".  
  mutate(corpus_D = ifelse(no_copy == 2 & corpus == "organoid", "organoid-on-chip", 
                           ifelse(no_copy == 2 & corpus == "OoC", "duplicate", corpus)))

colnames(corpus_overlap)

### Loading an edge list file.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv")) 

### Counting the number of publications on each organ category per article type, phase, and corpus (corpus_D).
organ_count_overlap <- corpus_overlap %>% 
  ### Counting based on "from_OR" columns so that an organ category is counted for all the upper categories as well.
  pivot_longer(starts_with("from_OR"), values_drop_na = TRUE) %>% 
  group_by(type, phase, corpus_F, corpus_D) %>% 
  count(value) %>% 
  ungroup() %>% 
  ### Combining with the edge list to include, for each organ category, an immediate upper category and the 1st-level category.
  ### This is in order to later select level 2 organ categories for plots.
  left_join(., edge_all[, 1:3], by = c("value" = "to")) %>% 
  ### Excluding tumor organoid and ToC publications.
  filter(corpus_F %in% c("organoid", "OoC"))

### Making a custom function for calculating trends of publication counts.
### This is the function that we use throughout the analysis to calculate trends of publication counts.
fn_calculate_trends <- function(publication_count_data, output_prefix = "") {
  trend_data <- publication_count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}

### Counting the number of papers of each article type in organoid and OoC corpus.
type_counts <- organ_types_P %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  group_by(phase) %>% 
  count(type)

### Calculating trends of publication counts per type.
### This will be used to adjust the trends of publication counts on each organ.
type_trends <- fn_calculate_trends(type_counts, output_prefix = "type") 

### Calculating trends of publication counts on each organ categories.
organ_trend_overlap <- fn_calculate_trends(organ_count_overlap, output_prefix = "organ") %>% 
  ### Adding the trends of article types.
  left_join(., type_trends, by = "type") %>% 
  ### Adjusting the organ trends by article type trends.
  mutate(adjusted_trend = organ_trend / type_trend) %>% 
  ### Setting the upper limit to 3 of the adjusted trends.
  mutate(adjusted_trend2 = ifelse(is.nan(adjusted_trend), 0, 
                                  ifelse(adjusted_trend > 3, 3, adjusted_trend))) %>% 
  ### Only selecting research articles.
  filter(type == "Research article") %>% 
  ### Removing duplicate entries.
  filter(!corpus_D == "duplicate")

### Adjusting the trend data frame by selecting required organ categories, reordering them.
adjusted_trend_overlap <- organ_trend_overlap %>% 
  ### Selecting 2nd-level organ categories, in addition to "nasal" which is a 1st-level organ category without lower categories.
  filter(from %in% unique(major_organ) | value == "nasal") %>% 
  ### Removing unnecessary organ categories.
  filter(!value %in% c("body", "unidentified")) %>% 
  filter(!grepl("unspecified", value)) %>% 
  ### Reordering rows by major_organ (1st level categories) and then by 2nd level categories.
  arrange(major_organ, value) %>% 
  ### Adding the "multiple organs" category.
  rbind(., organ_trend_overlap %>% filter(value == "multiple organs")) %>% 
  ### Changing the major_organ column to factor, so that the organ categories are plotted later in a desired order. 
  mutate(major_organ = factor(major_organ, levels = unique(major_organ))) %>% 
  ### Changing the corpus_D to the factor class to order the figure columns.
  mutate(corpus_D = factor(corpus_D, levels = c("OoC", "organoid-on-chip", "organoid"))) 

### Plotting a bubble plot to compare organid and OoC models of organs.
organ_model_comparison <- adjusted_trend_overlap %>% 
  ggplot(aes(x = corpus_D, y = fct_rev(value), size = organ_total, fill = adjusted_trend2)) + 
  geom_point(shape = 21, color = "grey60") + 
  scale_fill_gradient2(low = "darkblue", mid = "paleturquoise", high = "orange", midpoint = 1, guide = "colorbar", name = "Trend") + 
  scale_x_discrete(labels = c("OoC", "Organoid \n+ OoC", "Organoid")) + 
  scale_size_continuous(range = c(0.5, 4), breaks = c(10, 100, 1000)) + 
  labs(x = "", y = "", size = "Publication counts") + 
  facet_grid(rows = vars(major_organ), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 9, face = 1), 
        strip.text.y.left = element_text(angle = 360, vjust = 0.5, hjust = 1, face = 2), 
        legend.key = element_rect(fill = "grey80"), 
        legend.key.size = unit(3, "mm"),
        legend.title = element_text(size = 6, face = 2), 
        legend.text = element_text(size = 6))

### Saving
ggsave(organ_model_comparison, 
       filename = paste0(root_path, "results/model_comparison/organ_model_comparison.pdf"),  
       width = 140, height = 190, units = "mm")


















##########
###
### 2. Comparison of research topics in organoid and OoC research
###
##########
library(tidyverse)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/research_topics_P"))

### Identifying the frequently researched organ model groups.
selected_organ <- research_topics_P %>% 
  ### Only considering research articles.
  filter(type == "Research article") %>% 
  ### Only considering organoid and OoC corpora.
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  ### Counting the number of publications on each of 1st-level organ categories.
  count(major_organ) %>% 
  ### Removing major_organ categories that do not represent organ groups.
  filter(!is.na(major_organ)) %>% 
  filter(!major_organ %in% c("multiple organs", "unidentified")) %>% 
  ### Sorting the rows based on the publication count.
  arrange(- n) %>% 
  ### Selecting top 13 1st-level organ categories.
  top_n(., 13)

### Calculating the ratio of the number of publications on organoids and OoCs per type.
### This will be later used to adjust ratio of publications on each organ category/research topic combination.
corpus_ratio <- research_topics_P %>% 
  ### Counting the number of publications of each article type in each corpus.
  group_by(type) %>% 
  count(corpus_F) %>% 
  ungroup() %>% 
  ### Only selecting organoid and OoC corpora.
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  rename(corpus_count = n) %>% 
  ### Calculating the ratio of publications as "publication counts on organoids" divided by "publication counts on OoCs".
  pivot_wider(names_from = corpus_F, values_from = corpus_count) %>% 
  mutate(corpus_ratio = organoid / OoC)

### Counting the number of publications on each research topic.
topic_total_counts <- research_topics_P %>% 
  ### There are too many 1st-level organ categories to plot.
  ### For this reason, the top 13 1st-level organ categories (determined above) will be plotted, whereas publications on other 
  ### 1st-level organ categories are grouped together as "other".
  mutate(top_major_organ = ifelse(major_organ %in% selected_organ$major_organ, major_organ, "other")) %>% 
  ### Counting the numbers of publications on each research topic, per corpora, 1st-level organ categories, and article types.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  group_by(type, corpus_F, top_major_organ) %>% 
  count(topic) %>% 
  ungroup() %>% 
  rename(topic_counts_total = n) 

### Loading the csv file of research topic categories.
### The file includes research topic writing styles to be used for figure labels, names of research topics group categories, and 
### filter of which research topics to plot.
category_names_F <- read.csv(paste0(root_path, "csv/category_names_F.csv")) 

### extracting category names (to be used as figure labels), and converting them to factors to properly order them.
category_level <- factor(category_names_F$category_renamed)

### Extracting category group names and converting them to factors.
category_group_level <- unique(category_names_F$group_name)

### Calculating model system preference for topics.
topic_total_counts_selected <- topic_total_counts %>% 
  ### Selecting required corpora and the article type.
  filter(type == "Research article") %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  ### Transforming the data frame to calculate ratio of publication counts.
  pivot_wider(names_from = corpus_F, values_from = topic_counts_total) %>% 
  mutate(across(c(4, 5), ~ replace_na(., 0))) %>% 
  ### Adding the corpus ratio data frame.
  left_join(., corpus_ratio[, c(1, 4)], by = "type") %>% 
  ### Calculating the ratio of organoid/OoC publications that mention each of the research topics.
  ### Also calculating the sum of organoid/OoC publications that mention research topics.
  mutate(topic_ratio = organoid / OoC, 
         topic_total = organoid + OoC) %>% 
  ### Adjusting the ratios of publication counts on each topic by the ratio of total organoid/OoC publications.
  mutate(adjusted_topic_ratio = topic_ratio / corpus_ratio) %>% 
  ### Setting upper and lower limits of the adjusted ratio to 10 and 0.1, respectively.
  ### Also converting the ratio to a log10 scale.
  mutate(adjusted_topic_ratio2 = ifelse(adjusted_topic_ratio > 10, 1, 
                                        ifelse(adjusted_topic_ratio < 0.1, - 1, log10(adjusted_topic_ratio)))) %>% 
  ### Adding the columns of research topic category names, as well as a topic filter columns showing which topics to plot.
  left_join(., category_names_F[, c(1:3, 11)], by = c("topic" = "category_colname")) %>% 
  ### Changing columns to the factor class.
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(top_major_organ = factor(top_major_organ, levels = c(sort(selected_organ$major_organ), "other"))) %>% 
  ### Selecting research topics to plot.
  filter(A_filter == "A")
  
### Plotting a bubble graph.
topic_model_point <- topic_total_counts_selected %>% 
  ggplot(aes(x = top_major_organ, y = category_renamed)) +
  geom_point(aes(color = adjusted_topic_ratio2, size = sqrt(topic_total)), shape = 16) + 
  scale_color_gradient2(low = "red", mid = "grey80", high = "darkblue", midpoint = 0, na.value = "black", 
                       breaks = c(-1, 0, 1), labels = c("OoC", "", "organoid"), name = "Model system preference") + 
  scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top") + 
  scale_size_continuous(range = c(0.5, 3), breaks = c(sqrt(10), sqrt(100), sqrt(1000)), labels = c(10, 100, 1000), 
                        name = "Total publication counts") + 
  facet_grid(rows = vars(group_name), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 7), 
        axis.text.x.top = element_text(angle = 90, face = 2, vjust = 0.3, hjust = 0, size = 7), 
        strip.text.y.left = element_text(angle = 360, vjust = 0.5, hjust = 1, face = 2), 
        legend.key = element_rect(fill = "grey80"), 
        legend.key.size = unit(3, "mm"),
        legend.title = element_text(size = 6, face = 2, vjust = 2), 
        legend.text = element_text(size = 6), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_model_point, 
       filename = paste0(root_path, "results/model_comparison/topic_model_point.pdf"),  
       width = 178, height = 270, units = "mm")









##########
###
### 3. Similar comparison of research topics in organoid and OoC research, but with trends.
###
##########

### The code is mostly same as the previous section, except that trends of publication counts are calculated instead of 
### ratios of publication counts between organoids and OoCs.

### Making a custom function for counting publication counts and their trend.
fn_calculate_trends <- function(publication_count_data, output_prefix = "") {
  trend_data <- publication_count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}


RT_top_major_organs <- research_topics_P %>% 
  ### There are too many 1st-level organ categories to plot.
  ### For this reason, the top 13 1st-level organ categories (determined above) will be plotted, whereas publications on other 
  ### 1st-level organ categories are grouped together as "other".
  mutate(top_major_organ = ifelse(major_organ %in% selected_organ$major_organ, major_organ, "other"))



### Calculating publication trends of each organ group.
### This is later used to adjust the research topic trends. 
group_trends <- RT_top_major_organs %>% 
  group_by(type, phase, corpus_F) %>% 
  count(top_major_organ) %>% 
  ungroup() %>% 
  fn_calculate_trends(., output_prefix = "group")

### Calculating trends in research topics.
topic_trends <- RT_top_major_organs %>% 
  ### Counting the number of publications on each combination of research topic, phase, corpora, 1st-level organ categories, and article types.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  group_by(type, phase, corpus_F, top_major_organ) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Calculating trends.
  fn_calculate_trends(., output_prefix = "TF") %>% 
  ### Adding trends of the corpus.
  left_join(group_trends, by = c("type", "corpus_F", "top_major_organ")) %>% 
  ### Calculating adjusted trends by dividing the topic trends of each combination by corpus trends.
  mutate(adjusted_trend = TF_trend / group_trend) %>% 
  ### Changing the trends to "high", "moderate", and "low".
  mutate(adjusted_trend2 = ifelse(adjusted_trend > 1.4, "high", 
                                         ifelse(adjusted_trend < 0.6, "low", "moderate"))) %>% 
  left_join(., category_names_F, by = c("topic" = "category_colname")) %>% 
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(top_major_organ = factor(top_major_organ, levels = c(sort(selected_organ$major_organ), "other"))) %>% 
  ### Grouping the data so that each group is plotted in a column.
  mutate(x_group = paste(corpus_F, top_major_organ, sep = "_")) %>% 
  ### Selecting required corpora, the article type, and topics.
  filter(type == "Research article") %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  filter(A_filter == "A")

topic_trends_bubble <- topic_trends %>% 
  ggplot(aes(x = x_group, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_trend2, size = sqrt(TF_total)), shape = 16) + 
  scale_color_manual(values = c("high" = "orange", "moderate" = "grey65", "low" = "deepskyblue1"), 
                     breaks = c("high", "moderate", "low"), name = "Publication trends") + 
  #labs(color = "Model preference") + 
  #scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top", labels = function(x) gsub("_.*$", "", x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100), sqrt(1000)), labels = c(10, 100, 1000), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(top_major_organ), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 7), 
        axis.text.x.top = element_text(angle = 90, vjust = 0.3, hjust = 0, size = 7), 
        strip.text.x.top = element_text(angle = 90, face = 2, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(angle = 360, vjust = 0.5, hjust = 1, face = 2), 
        legend.key.size = unit(3, "mm"),
        legend.title = element_text(size = 6, face = 2), 
        legend.text = element_text(size = 6), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_trends_bubble, 
       filename = paste0(root_path, "results/model_comparison/topic_trends_bubble.pdf"),  
       width = 178, height = 270, units = "mm")
  




topic_trends_all_disease <- RT_top_major_organs %>% 
  ### Counting the number of publications on each combination of research topic, phase, corpora, 1st-level organ categories, and article types.
  pivot_longer(starts_with("TF_"), names_to = "topic") %>% 
  filter(value == TRUE) %>% 
  group_by(type, phase, corpus_F, top_major_organ) %>% 
  count(topic) %>% 
  ungroup() %>% 
  ### Calculating trends.
  fn_calculate_trends(., output_prefix = "TF") %>% 
  ### Adding trends of the corpus.
  left_join(group_trends, by = c("type", "corpus_F", "top_major_organ")) %>% 
  ### Calculating adjusted trends by dividing the topic trends of each combination by corpus trends.
  mutate(adjusted_trend = TF_trend / group_trend) %>% 
  ### Changing the trends to "high", "moderate", and "low".
  mutate(adjusted_trend2 = ifelse(adjusted_trend > 1.4, "high", 
                                  ifelse(adjusted_trend < 0.6, "low", "moderate"))) %>% 
  left_join(., category_names_F, by = c("topic" = "category_colname")) %>% 
  mutate(group_name = factor(group_name, levels = category_group_level)) %>% 
  mutate(category_renamed = factor(category_renamed, levels = category_level)) %>% 
  mutate(top_major_organ = factor(top_major_organ, levels = c(sort(selected_organ$major_organ), "other"))) %>% 
  ### Grouping the data so that each group is plotted in a column.
  mutate(x_group = paste(corpus_F, top_major_organ, sep = "_")) %>% 
  ### Selecting required corpora, the article type, and topics.
  filter(type == "Research article") %>% 
  filter(corpus_F %in% c("organoid", "OoC")) %>% 
  filter(!grepl("techniques", group_name), 
         !group_name %in% c("cell biology", "physiology", "tumor physiology", "oncology"))

topic_trends_bubble_all_disease <- topic_trends_all_disease %>% 
  ggplot(aes(x = x_group, y = fct_rev(category_renamed))) + 
  geom_point(aes(color = adjusted_trend2, size = sqrt(TF_total)), shape = 16) + 
  scale_color_manual(values = c("high" = "orange", "moderate" = "grey65", "low" = "deepskyblue1"), 
                     breaks = c("high", "moderate", "low"), name = "Publication trends") + 
  #labs(color = "Model preference") + 
  #scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top", labels = function(x) gsub("_.*$", "", x)) + 
  scale_size_continuous(range = c(0.3, 3), breaks = c(sqrt(10), sqrt(100), sqrt(1000)), labels = c(10, 100, 1000), name = "Publication counts") + 
  facet_grid(rows = vars(group_name), cols = vars(top_major_organ), scales = "free", space = "free", switch = "y") + 
  theme(strip.placement = "outside", 
        text = element_text(size = 7), 
        axis.text.x.top = element_text(angle = 90, vjust = 0.3, hjust = 0, size = 7), 
        strip.text.x.top = element_text(angle = 90, face = 2, vjust = 0.3, hjust = 0), 
        strip.text.y.left = element_text(angle = 360, vjust = 0.5, hjust = 1, face = 2), 
        legend.key.size = unit(3, "mm"),
        legend.title = element_text(size = 6, face = 2), 
        legend.text = element_text(size = 6), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank())

ggsave(topic_trends_bubble_all_disease, 
       filename = paste0(root_path, "results/model_comparison/topic_trends_bubble_all_disease.pdf"),  
       width = 178, height = 550, units = "mm")
