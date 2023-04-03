### This R script file is to draw figures of the global research trend.
###
###
### The file includes following steps: 
### 1. Identifying well-researched major organ types in research articles of each corpus.
### 2. Calculating the number and trends of publications.
### 3. Making a table showing countries' contributions
### 4. Drawing pie charts showing researched organ types in top research countries.
### 5. Drawing world maps showing fractional contribution counts of countries in organoid/organ-on-chip research.
###

### Loading a package.
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading required files.
load(paste0(root_path, "R_results/countries_P"))
load(paste0(root_path, "R_results/organ_types_P"))

countries_O <- countries_P %>% 
  mutate(corpus_F = organ_types_P$corpus_F, 
         major_organ = organ_types_P$major_organ)




##########
###
### 1. Identifying well-researched major organ types in research articles of each corpus.
###
##########

### Calculating countries' total contributions as percentages of total research articles for each corpus.
countries_contributions <- countries_O %>% 
  ### Only considering research articles
  filter(type == "Research article") %>% 
  ## If main_country is NA, then it's changed to "undetermined".
  mutate(main_country = replace_na(main_country, "undetermined")) %>% 
  mutate(main_country = as.factor(main_country)) %>% 
  group_by(corpus_F, .drop = FALSE) %>% 
  count(main_country) %>% 
  ### Calculating the total number of research articles per corpus.
  mutate(corpus_total_RA = sum(n)) %>% 
  ungroup() %>% 
  ### Dividing the sum of countries' contributions by the total number of research articles.
  mutate(percentage_contributions = n*100 / corpus_total_RA) 

### Counting the number of research articles on each major organ type (i.e., first-level organ categories, 
### largely corresponding to the organ systems) for each corpus.
major_organ_all <- countries_O %>% 
  ### Only considering research articles.
  filter(type == "Research article") %>% 
  group_by(corpus_F) %>% 
  ### Counting publication counts on major_organ categories.
  count(major_organ) %>% 
  ### Removing major_organ categories that do not represent organ groups.
  filter(!is.na(major_organ)) %>% 
  filter(!major_organ %in% c("multiple organs", "unidentified")) %>% 
  ### Sorting the rows based on the publication count.
  arrange(corpus_F, - n)

### Selecting top 7 well-researched major organ types for each corpus.
organoid_top7_organ <- major_organ_all %>% 
  filter(corpus_F == "organoid") %>% 
  top_n(7, n)

tumor_top7_organ <- major_organ_all %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  top_n(7, n)

OoC_top7_organ <- major_organ_all %>% 
  filter(corpus_F == "OoC") %>%    
  top_n(7, n)

ToC_top7_organ <- major_organ_all %>% 
  filter(corpus_F == "ToC") %>%    
  top_n(7, n)


### Adjusting the data frame "countries_O" for plotting
###
### Making a new column "major_organ7" that shows top 7 major organ types.
### The column is similar to major_organ, except that all major_organ categories that are NOT among the top 7 major organ types 
### (determined above) are now classified as "other".
organ7 <- countries_O %>% 
  mutate(major_organ7 = 
           ifelse(major_organ == "unidentified", major_organ, 
                  ifelse((corpus_F == "organoid" & major_organ %in% organoid_top7_organ$major_organ) | 
                           (corpus_F == "tumor_organoid" & major_organ %in% tumor_top7_organ$major_organ) | 
                           (corpus_F == "OoC" & major_organ %in% OoC_top7_organ$major_organ) | 
                           (corpus_F == "ToC" & major_organ %in% ToC_top7_organ$major_organ), major_organ, "other"))) %>% 
  mutate(main_country = replace_na(main_country, "undetermined"))









##########
###
### 2. Calculating the number and trends of publications.
###
##########

### To later show publication counts of EU (as European research area, Horizon 2020 to be more precise) as a whole, 
### a character vector is made of names of countries belonging to ERA.
ERA_countries_w <- c("Albania", "Armenia", "Austria", "Belgium", "Bosnia and Herzegovina", 
                    "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                    "Estonia", "Faroe Islands", "Finland", "France", "Georgia", 
                    "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                    "Israel", "Italy", "Kosovo", "Latvia", "Lithuania", 
                    "Luxembourg", "Malta", "Moldova", "Montenegro", "Morocco", 
                    "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                    "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", 
                    "Sweden", "Switzerland", "Tunisia", "Turkey", "UK", 
                    "Ukraine")


### Making a custom function for counting publication counts and their trend.
### The input data frame is expected to have the "phase" column which shows either "early" or "later" phase, 
### and "n" column which shows the number of publication in the corresponding phase.
### The "output_prefix" argument shows a prefix used for output column names.
fn_calculate_trends <- function(publication_count_data, output_prefix = "") {
  trend_data <- publication_count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}

### Calculating publication counts per corpus, article types, and phase.
corpus_counts <- organ7 %>% 
  group_by(corpus_F, type) %>% 
  count(phase)

### Calculating publication count trends of corpus per article types.
corpus_trends <- fn_calculate_trends(corpus_counts, output_prefix = "corpus")

### Calculating publication counts per country, corpus, article types, and phase.
country_counts <- organ7 %>% 
  group_by(corpus_F, type, main_country) %>% 
  count(phase)

### Calculating publication count trends of countries per corpus per article types.
country_trends <- fn_calculate_trends(country_counts, output_prefix = "country")

### Calculating publication counts per country, corpus, article types, and phase.
### This time, EU countries are first grouped as "ERA".
ERA_counts <- organ7 %>% 
  mutate(main_country = ifelse(main_country %in% ERA_countries_w, "ERA", main_country)) %>% 
  group_by(corpus_F, type, main_country) %>% 
  count(phase)

### Calculating publication count trends of countries per corpus per article types.
### EU countries are grouped as "ERA".
ERA_trends <- fn_calculate_trends(ERA_counts, output_prefix = "country")

### Combining the country_trends and ERA_trends.
### From the ERA_trends, only rows corresponding to ERA are used.
all_country_trends <- ERA_trends %>% 
  filter(main_country == "ERA") %>% 
  rbind(., country_trends) 

### Counting the number of publications on major_organ categories per country, corpus_F, and article types.
organ_counts <- organ7 %>% 
  group_by(corpus_F , type, main_country) %>% 
  count(major_organ7)

### Counting the number of publication on major_organ categories again.
### This time, EU countires are changed to "ERA".
ERA_organ_counts <- organ7 %>% 
  mutate(main_country = ifelse(main_country %in% ERA_countries_w, "ERA", main_country)) %>% 
  group_by(corpus_F , type, main_country) %>% 
  count(major_organ7) %>% 
  ungroup()

### Combining the above two data frames of publication counts on major_organs.
all_organ_counts <- ERA_organ_counts %>% 
  filter(main_country == "ERA") %>% 
  rbind(., organ_counts)

### Combining the organ_counts, country trends, and corpus trend data frames.
adjusted_trends <- all_organ_counts %>% 
  rename(organ_total = n) %>% 
  left_join(., all_country_trends, by = c("corpus_F", "type", "main_country")) %>% 
  left_join(., corpus_trends[, c(1, 2, 5, 6)], by = c("corpus_F", "type")) %>% 
  ### normalizing the country trends by corpus trends.
  ### Also, making a new column for showing arrows in later figures that show trends.
  mutate(adjusted_trend = country_trend / corpus_trend) %>% 
  mutate(trend_arrow = ifelse(adjusted_trend < 0.6, "\u2193", 
                              ifelse(adjusted_trend < 1.4, "\u2192", "\u2191"))) %>% 
  ### Making a figure label column.
  mutate(country_n = paste0(main_country, " (", country_total, ")  ", trend_arrow))

colnames(adjusted_trends)







##########
###
### 3. Making a table showing countries' contributions
###
##########

countries_contributions <- adjusted_trends %>% 
  select(corpus_F, type, main_country, country_total, corpus_total) %>% 
  distinct() %>% 
  mutate(country_percentage = country_total / corpus_total) %>% 
  select(!corpus_total) %>% 
  pivot_wider(names_from = corpus_F, values_from = c("country_total", "country_percentage")) %>% 
  arrange(type, desc(country_total_organoid))

write.csv(countries_contributions, 
          file = paste0(root_path, "results/csv/countries_contributions.csv"), row.names = FALSE)








##########
###
### 4. Drawing pie charts showing researched organ types in top research countries.
###
### Note that the diameter of pies are in the cubic root scale rather than the square root scale for better visibility.
###
##########


### Selecting columns for identifying top research countries.
selected_trends <- adjusted_trends %>% 
  select(corpus_F, type, main_country, country_total) %>% 
  distinct() %>% 
  ### Only considering research articles. Discarding "undetermined" as main_country.
  filter(type == "Research article") %>% 
  filter(!main_country == "undetermined")

### Selecting top 20 countries (and "ERA") for each corpus.
organoid_top20_country <- selected_trends %>% 
  filter(corpus_F == "organoid") %>% 
  top_n(21, country_total)

tumor_top20_country <- selected_trends %>% 
  filter(corpus_F == "tumor_organoid") %>% 
  top_n(21, country_total)

OoC_top20_country <- selected_trends %>% 
  filter(corpus_F == "OoC") %>% 
  top_n(21, country_total)

ToC_top20_country <- selected_trends %>% 
  filter(corpus_F == "ToC") %>% 
  top_n(21, country_total)


### Making a custom function for drawing pie charts.
### type_index accepts an integer between 1 to 3, with 1 corresponding to "Research article", 
### 2 corresponding to "Review", and 3 corresponding to "Preprint".
fn_country_pies <- function(x, corpus_type, article_type = "Research article") {
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor", 
                                  ifelse(corpus_type %in% c("organoid", "OoC", "ToC"), corpus_type, NA))
  top20_country <- eval(parse(text = paste0(corpus_type_converted, "_top20_country")))
  top7_organ <- eval(parse(text = paste0(corpus_type_converted, "_top7_organ")))
  country_pie <- x %>% 
    filter(type == article_type) %>% 
    filter(corpus_F == corpus_type) %>% 
    filter(main_country %in% top20_country$main_country) %>% 
    mutate(major_organ7 = factor(major_organ7, 
                                 levels = c(sort(unique(top7_organ$major_organ)), "other", "unidentified"))) %>% 
    ggplot(aes(x = country_total^(1/3)/2, y = organ_total, fill = major_organ7, width = country_total^(1/3))) + 
    geom_bar(position = "fill", stat = "identity", color = "black", linewidth = 0.1) + 
    coord_polar("y") + 
    labs(fill = "Organ models") + 
    facet_wrap(~ country_n) + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.background = element_rect(fill = "transparent"), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.background = element_rect(fill = "transparent"), 
      legend.title = element_text(size = 8, face = 2), 
      legend.text = element_text(size = 8), 
      strip.text.x = element_text(size = 7, face = "bold"), 
      strip.background = element_rect(fill = "transparent"), 
      legend.key.size = unit(5, "mm")
    )
}

### For organoids
fig_organoid_organ7_country20 <- fn_country_pies(adjusted_trends, corpus_type = "organoid") + 
  scale_fill_manual(values = c("orange", "red", "blue", "green", "magenta", "purple", "pink", "grey75", "grey50"))

ggsave(fig_organoid_organ7_country20, 
       filename = paste0(root_path, "results/global_trends/pie_charts/fig_organoid_organ7_country20.png"),  
       bg = "transparent", width = 210, height = 210, units = "mm")

### For OoCs
fig_OoC_organ7_country20 <- fn_country_pies(adjusted_trends, corpus_type = "OoC")  + 
  scale_fill_manual(values = c("seagreen", "orange", "red", "aquamarine", "blue", "purple", "pink", "grey75", "grey50"))

ggsave(fig_OoC_organ7_country20, 
       filename = paste0(root_path, "results/global_trends/pie_charts/fig_OoC_organ7_country20.png"),  
       bg = "transparent", width = 210, height = 210, units = "mm")






##########
###
### 5. Drawing world maps showing fractional contribution counts of countries in organoid/organ-on-chip research.
###
##########


### Calculating the sum of fractional contribution counts for each country per corpus_F and article types.
countries_longer <- countries_O %>% 
  pivot_longer(ends_with("_p"), names_to = "country", values_drop_na = TRUE) %>% 
  mutate(country = gsub("_p", "", country)) %>% 
  mutate(country = gsub("_", " ", country)) %>% 
  group_by(corpus_F, type, country) %>% 
  summarize(number = sum(value, na.rm = TRUE)) %>% 
  ungroup()

### Saving the list of country names as a csv file.
data.frame(country = unique(countries_longer$country), population = "") %>% 
  write.csv(., file = paste0(root_path, "csv/temps/all_countries_df.csv"), row.names = FALSE)
### Outside R, the "population" column was filled with the number of population of each country.
### Then the csv file was saved as "./csv/all_countries_F.csv".

all_countries_F <- read.csv(paste0(root_path, "csv/all_countries_F.csv"))

### Fractional contribution counts of countries in the countries_longer data frame are adjusted by populations.
countries_pop <- left_join(countries_longer, all_countries_F, by = "country") %>% 
  mutate(count_per_million = number * 10^6 / population)  %>% 
  mutate(count_per_million = ifelse(count_per_million == 0, NA, count_per_million))

counts_per_capita <- countries_pop %>% 
  select(corpus_F, type, country, count_per_million) %>% 
  pivot_wider(names_from = corpus_F, values_from = count_per_million) %>% 
  select(1, 2, 4, 3) %>% 
  arrange(type, desc(organoid))

write.csv(counts_per_capita, 
          file = paste0(root_path, "results/csv/counts_per_capita.csv"), row.names = FALSE)


### Loading a package 
library(rworldmap)

### Assigning map_data
world_map <- map_data(map = "world")

### Checking that all countries from publications exist in the world_map data.
setdiff(unique(countries_pop$country), unique(world_map$region))

### Making a custom function for plotting world map of fractional contributions to organoid/Organ-on-a-chip research.
fn_country_map <- function(x, y, corpus_type, article_type = "Research article") {
  countries_pop_selected <- x %>% 
    filter(corpus_F == corpus_type)
  pop_map <- left_join(y, countries_pop_selected[, c(3:6)], by = c("region" = "country")) 
  pop_map %>% 
    ggplot() + 
    labs(fill = "Publications per million") + 
    geom_map(data = pop_map, map = pop_map, aes(map_id = region, fill = sqrt(count_per_million))) + 
    expand_limits(x = pop_map$long, y = pop_map$lat) + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      panel.background = element_rect(fill = "transparent"), 
      legend.title = element_text(size = 7, face = 2, vjust = 2), 
      legend.text = element_text(size = 7), 
      legend.key.size = unit(4, "mm")
    ) + 
    coord_equal()
  
}


### Plotting countries' contributions to organoid research.
organoid_world_sqrt <- fn_country_map(countries_pop, world_map, corpus_type = "organoid") +
  scale_fill_gradient(low = "grey70", high = "black", na.value = "grey90", guide = "colourbar", 
                      limits = c(0, 5), breaks = c(0, 2, 4), labels = c(0, 4, 16)) 

ggsave(organoid_world_sqrt, 
       filename = paste0(root_path, "results/global_trends/world_maps/organoid_world_sqrt.png"),  
       bg = "transparent", width = 210, height = 100, units = "mm")

### Plotting the same graph, without the legend.
organoid_world_sqrt_NL <- organoid_world_sqrt + 
  guides(fill = "none")

ggsave(organoid_world_sqrt_NL, 
       filename = paste0(root_path, "results/global_trends/world_maps/organoid_world_sqrt_NL.png"),  
       bg = "transparent", width = 210, height = 100, units = "mm")


### OoC research
OoC_world_sqrt <- fn_country_map(countries_pop, world_map, corpus_type = "OoC") + 
  scale_fill_gradient(low = "grey70", high = "black", na.value = "grey90", guide = "colourbar", 
                      limits = c(0, 2.7), breaks = c(0, 1, 2), labels = c(0, 1, 4))

ggsave(OoC_world_sqrt, 
       filename = paste0(root_path, "results/global_trends/world_maps/OoC_world_sqrt.png"),  
       bg = "transparent", width = 210, height = 100, units = "mm")


### OoC research, without a legend.
OoC_world_sqrt_NL <- OoC_world_sqrt + 
  guides(fill = "none")

ggsave(OoC_world_sqrt_NL, 
       filename = paste0(root_path, "results/global_trends/world_maps/OoC_world_sqrt_NL.png"),  
       bg = "transparent", width = 210, height = 100, units = "mm")
