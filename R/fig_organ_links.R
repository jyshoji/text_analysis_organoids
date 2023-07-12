### This R script file is to draw network graphs of hierarchical organ classifications, 
### along with links showing co-researched organ/substructures as organoid/OoC models.
###
### The script include the following steps.
### 1. Counting the number of academic publications which were classified as being using multiple organ models.
### 2. Identifying organs/substructures simultaneously researched as organoid/OoC models.
### 3. Drawing network graphs with links.
###
### The figures were saved at: 
### ./results/organ_links/

### Loading a package.
library(tidyverse)
library(ggraph)
library(igraph)
library(ggpp)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### Loading required files.
load(paste0(root_path, "R_results/organ_types_P"))







##########
###
### 1. Counting the number of academic publications which were classified as using multiple organ models.
###
##########

### Counting the number of publications using multiple organ models for each article type.
multi_organ_papers <- organ_types_P %>% 
  filter(n_organ_minor > 1 | n_organ_major > 1) %>% 
  group_by(type) %>% 
  count(corpus_F) %>% 
  ungroup() %>% 
  rename(multi = n)

### Calculating the proportions of publications using multiple organ models in each article type.
multi_organ_ratio <- organ_types_P %>% 
  group_by(type) %>% 
  count(corpus_F) %>% 
  ungroup() %>% 
  rename(total = n) %>% 
  left_join(., multi_organ_papers, by = c("corpus_F", "type")) %>% 
  mutate(ratio = multi / total)







##########
###
### 2. Identifying organs/substructures co-researched as organoid/OoC models.
###
##########

edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv")) %>% 
  rename(major_organ2 = major_organ)

colnames(organ_types_P)

### The goal here is to connect two organs that are studied together in single articles.
### For this purpose, pairwise combinations of organs will be linked in the generated figure. 
### However, "reproductive" and "prostate", for example, shouldn't be connected because "prostate" is a subcategory of 
### "reproductive", such that these two organs represent a single organoid/OoC entity.
### Therefore, first, "major organ" categories are deleted when corresponding organs exist in the "minor organ" categories.
###
### For this,the "minor organ" categories are converted into corresponding "major organ" categories.
### The converted minor organ categories will be called as "major_organ2" categories to distinguish them from the 
### existing "major organ" categories.

### The organ_types data frame is changed to a long format, so that each row contains an organ type researched in a paper.
### Corresponding major organ categories (i.e. major_organ2) are added to each row from the edge_all file.
minor_major_organs <- organ_types_P %>% 
  select(ID, contains("minor_organ"), organ_type, contains("major_organ")) %>% 
  pivot_longer(., starts_with("minor_organ_"), values_drop_na = TRUE) %>% 
  left_join(., edge_all[, 2:3], by = c("value" = "to")) 

### Changing "major_organ_" columns to NA when the same category also occurs in the "major_organ2", because they are redundant.
minor_major_organs_adjusted <- minor_major_organs %>% 
  group_by(ID) %>% 
  mutate(across(starts_with("major_organ_"), ~ ifelse(. %in% major_organ2, NA, .))) %>% 
  ungroup() %>% 
  ### Removing unrequired columns.
  select(!c(name, value, major_organ2))

colnames(minor_major_organs_adjusted)

### In the above data frame, publications without "minor organ" categories" have been removed, even if they had "major organ" 
### categories (e.g., where the organ_type == "reproductive").
### Below, these publications are selected
minor_major_organs_longer <- organ_types_P %>% 
  ### Selecting columns that are present in the minor_major_organs_adjusted data frame.
  select(colnames(minor_major_organs_adjusted)) %>% 
  ### Selecting publications that are absent in the minor_major_organs_adjusted data frame.
  filter(!ID %in% minor_major_organs_adjusted$ID) %>% 
  ### And combined it with the minor_major_organs_adjusted data frame.
  rbind(., minor_major_organs_adjusted) %>% 
  arrange(ID) %>% 
  ### The data frame is changed to a long format, so that each row contains a major_organ_ category of a paper.
  ### Along the way, publications without major_organ_ categories are dropped.
  ### Therefore this steps only keeps publications that have major_organ_ categories that are not redundant with minor_organ categories.
  pivot_longer(., starts_with("major_organ_"), names_to = "major_organ_columns", values_to = "major_organ_values", values_drop_na = TRUE) %>% 
  ### Selecting the required columns.
  select(ID, major_organ_values) %>% 
  distinct() 

### Counting the number of major organ categories for each paper.
minor_major_organs_counts <- minor_major_organs_longer %>% 
  count(ID)

### Changing the data frame to a wide format, so that each publication has one row, and major organ categories are shown across several columns.
minor_major_organs_separated <- minor_major_organs_longer %>% 
  group_by(ID) %>% 
  ### Concatenating all major_organ categories of the same publication in a single column.
  mutate(major_organ_all = paste(major_organ_values, collapse = ";")) %>% 
  ungroup() %>% 
  select(1, 3) %>% 
  distinct() %>% 
  ###  Separating the concatenated major_organ categories across columns. 
  separate(major_organ_all, paste0("major_organ_", c(1:max(minor_major_organs_counts$n))), sep = ";", fill = "right")
### Overall, this data frame represents publications whose major_organ categories are not redundant and thus have to be considered as co-researched 
### organ models.

### Making a data frame of all organ types that need to be considered as co-researched organ models.
multiple_organ_types <- organ_types_P %>% 
  ### Selecting minor_organ_ columns.
  select(ID, starts_with("minor_organ_")) %>% 
  ### Combining with the above data frame of non-redundant major_organ_ categories.
  left_join(., minor_major_organs_separated, by = "ID") %>% 
  mutate(across(- "ID", ~ replace_na(., "")))

### calculating all pair-wise combinations of organs that are co-researched in single papers.
### Co-researched organs will be stored as a value, separated by ";".
combinations <- as.data.frame(t(apply(multiple_organ_types %>% select(!ID), 1, function(x) {
  as.data.frame(combn(x, 2)) %>% 
    apply(., 2, paste, collapse = ";")
})))

### Making a data frame containing publication ID, type, phase, corpus_F, along with combinations of organs.
combined_organs <- organ_types_P %>% 
  select(ID, type, phase, corpus_F) %>% 
  cbind(combinations)

### Only selecting publications that co-researched multiple organ models.
combined_counted <- combined_organs %>% 
  ### Converting to a long format.
  pivot_longer(., starts_with("V")) %>% 
  select(!name) %>% 
  ### Here, organ combinations are shown as "organ1;organ2".
  ### Single organs (i.e., not being combined with another organ) lack alphabetical characters on either side of ";", and are removed.
  filter(grepl("[a-z];[a-z]", value)) %>% 
  group_by(corpus_F, type, value, phase) %>% 
  ### Counting the number of occurrences of each organ combination per corpus and article type.
  mutate(n = n()) %>% 
  ungroup() %>% 
  ### Separating two organs of combinations into two columns.
  separate(value, c("organ_1", "organ_2"), sep = ";")

### Only selecting organ model combinations in organoid research.
### Note that names of the following data objects will be called by later custom functions.
organoid_links <- combined_counted %>% 
  select(!ID) %>% 
  distinct() %>% 
  filter(corpus_F == "organoid") %>% 
  filter(type == "Research article") %>% 
  filter(n > 1)

### Only selecting organ models combinations in organoid research in the recent period.
organoid_links_later <- organoid_links %>% 
  filter(phase == "later")

### Only selecting organ models combinations in organoid research in the earlier period.
organoid_links_early <- organoid_links %>% 
  filter(phase == "early")

### Only selecting organ model combinations in OoC research.
OoC_links <- combined_counted %>% 
  select(!ID) %>% 
  distinct() %>% 
  filter(corpus_F == "OoC") %>% 
  filter(type == "Research article") %>% 
  filter(n > 1)

### Only selecting organ models combinations in OoC research in the recent period.
OoC_links_later <- OoC_links %>% 
  filter(phase == "later")

### Only selecting organ models combinations in OoC research in the earlier period.
OoC_links_early <- OoC_links %>% 
  filter(phase == "early")







##########
###
### 3. Drawing network graphs with links.
###
##########

### Counting the number of publications on each level of the hierarchical organ classification, per corpus and article types.
organ_counts <- organ_types_P %>% 
  ### Converting to a long format, so that each row contains one level of the hierarchical organ classification of a paper.
  pivot_longer(starts_with("from_OR"), values_to = "hierarchical_organ_type", values_drop_na = TRUE) %>% 
  mutate(hierarchical_organ_type = factor(hierarchical_organ_type)) %>% 
  group_by(type, corpus_F, .drop = FALSE) %>% 
  ### Counting the number of publications for each level of the hierarchical organ classification.
  count(hierarchical_organ_type) %>% 
  ungroup() %>% 
  rename(total = n) %>% 
  mutate(hierarchical_organ_type = as.character(hierarchical_organ_type)) %>% 
  mutate(hierarchical_organ_type = gsub("body", " ", hierarchical_organ_type)) 

### From the edge list, organ categories (i.e., excluding prenatal categories) are selected. 
edge_organ <- edge_all %>% 
  filter(!is.na(major_organ2)) %>% 
  select(from, to) %>% 
  filter(!grepl("unspecified", to)) %>% 
  filter(!to %in% c("multiple organs", "venom gland", "unidentified")) %>% 
  mutate(from = gsub("body", " ", from))

### In order to facilitate figure label customization (e.g., tilting text labels), the order of the organ categories have to match 
### the order to be plotted.
### To this end, each organ category needs to be sorted based on their levels in the hierarchical classification.
### For this reason, a data frame is made that shows category levels and corresponding organ categories. 
edge_network <- edge_organ %>% 
  filter(from == " ") %>% 
  rename(lvl0 = from, 
         lvl1 = to) %>% 
  left_join(., edge_organ, by = c("lvl1" = "from")) %>% 
  rename(lvl2 = to) %>% 
  mutate(lvl2 = ifelse(is.na(lvl2), lvl1, lvl2)) %>% 
  left_join(., edge_organ, by = c("lvl2" = "from")) %>% 
  rename(lvl3 = to) %>% 
  mutate(lvl3 = ifelse(is.na(lvl3), lvl2, lvl3)) %>% 
  left_join(., edge_organ, by = c("lvl3" = "from")) %>% 
  rename(lvl4 = to) %>% 
  mutate(lvl4 = ifelse(is.na(lvl4), lvl3, lvl4)) %>% 
  left_join(., edge_organ, by = c("lvl4" = "from")) %>% 
  rename(lvl5 = to) %>% 
  mutate(lvl5 = ifelse(is.na(lvl5), lvl4, lvl5)) %>% 
  left_join(., edge_organ, by = c("lvl5" = "from")) %>% 
  rename(lvl6 = to) %>% 
  mutate(lvl6 = ifelse(is.na(lvl6), lvl5, lvl6))

### Making a data frame to adjust figure labels.
edge_network_arranged <- edge_network %>% 
  ### Adjusting, so that the row order matches the plot order.
  arrange(lvl1, lvl2, lvl3, lvl4, lvl5, lvl6) %>% 
  ### Removing unrequired categories.
  filter(!lvl6 %in% c("multiple organs", "unidentified", "venom gland")) %>% 
  ### Determining the angles of each text labels in the figure.
  mutate(leaf_ID = c(1:nrow(.))) %>% 
  mutate(angle = 360 * leaf_ID / max(leaf_ID)) %>% 
  mutate(hjust = ifelse(90 < angle & angle < 270, -0.1, 1.1)) %>% 
  mutate(angle = ifelse(90 < angle & angle < 270, angle + 180, angle))


### Making a custom function for generating vertice data that will be used for plotting.
fn_network_vertice <- function(count_data, corpus_type, article_type = "Research article", position_adjustment = FALSE) {
  ### Preparing for using the corpus_type argument to specify data objects.
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor", 
                                  ifelse(corpus_type %in% c("organoid", "OoC", "ToC"), corpus_type, NA))
  ### Specifying the data object for organ links depending on the chosen corpus_type.
  corpus_links <- eval(parse(text = paste0(corpus_type_converted, "_links")))
  ### Selecting publication counts of the corresponding corpus.
  corpus_counts <- count_data %>% 
    filter(type == article_type) %>% 
    filter(corpus_F == corpus_type) %>% 
    select(hierarchical_organ_type, total) 
  ### Making the vertice data object.
  ### First, making a data frame listing all organs.
  corpus_vertice <- data.frame(name = unique(c(edge_organ$from, edge_organ$to))) %>% 
    ### Removing unnecessary organ categories.
    filter(!name %in% c("multiple organs", "venom gland", "unidentified")) %>% 
    ### Adding the publication counts.
    left_join(., corpus_counts, by = c("name" = "hierarchical_organ_type")) %>% 
    ### NA in the publication counts is replaced with 0.
    mutate(total = replace_na(total, 0)) %>% 
    arrange(name) %>% 
    ### Adding and adjusting the data frame for figure label adjustments.
    left_join(., edge_network_arranged[, 7:10], by = c("name" = "lvl6")) %>% 
    mutate(angle = replace_na(angle, 0), 
           hjust = replace_na(hjust, 0.5)) %>% 
    mutate(vjust = ifelse(angle == 0, 1.2, 0)) %>% 
    ### major_organ2 category is added, which will be used for grouping organ categories.
    left_join(., edge_all[, 2:3], by = c("name" = "to")) %>%  
    ### Organ categories are assigned to one of four categories, depending on their levels and whether or not they are leaf 
    ### (i.e., the lowest) categories.
    ### These four categories will be labeled with different text styles.
    mutate(lvl1 = ifelse(is.na(leaf_ID) & name %in% edge_all$major_organ2, name, ""), 
           lvl1_leaf = ifelse(!is.na(leaf_ID) & name %in% edge_all$major_organ2, name, ""), 
           lvl2 = ifelse(is.na(leaf_ID) & !name %in% edge_all$major_organ2, name, ""), 
           lvl2_leaf = ifelse(!is.na(leaf_ID) & !name %in% edge_all$major_organ2, name, "")) %>% 
    ### Counting the number of organ categories belonging to each major_organ2 category.
    ### At a later step, major_organ2 categories with small numbers of included organ categories are not color-coded and instead shown in grey.
    group_by(major_organ2) %>% 
    mutate(n = n()) %>% 
    ### Recategorizing the major_organ categories. 
    ### Basically, important major_organ categories will be color-coded, unimportant (e.g., with low publication count) will be in grey, 
    ### and organ categories that are not modelled will be in lighter grey.
    mutate(major_organ3 = ifelse(total == 0 & !name %in% c(corpus_links$organ_1, corpus_links$organ_2), "not modelled", 
                                 ifelse(is.na(major_organ2) | n < 5, "other", major_organ2))) %>% 
    ### The above major_organ3 column is converted to the factor class.
    ### However, in the plot, the factor levels were ignored for some reason.
    mutate(major_organ3 = factor(major_organ3, 
                                 levels = c(sort(unique(.$major_organ2)), "other", "not modelled")))
  ### "position_adjustment" argument is TRUE, the output vertice object is also saved as a csv file, so that the positions of the text labels
  ### can be adjusted outside the R.
  ### If you do not want to manually adjust the figure label positions, keep the position_adjustment = FALSE, which is the default argument.
  if(position_adjustment) {
    write.csv(corpus_vertice, 
              file = paste0(root_path, "csv/temps/", corpus_type_converted, "_vertice.csv"), row.names = FALSE)
  } 
  return(corpus_vertice)
}

### Applying the custom function to the organoid corpus.
### If you do not want to manually adjust the positions of text labels, change "position_adjustment = TRUE" to 
### "position_adjustment = FALSE".
### Also, delete the following line to read csv.
organoid_vertice <- fn_network_vertice(organ_counts, corpus_type = "organoid", position_adjustment = TRUE)

### After manually adjusting the coordinates for text positions, the csv file was loaded onto R again and used as the vertice data.
organoid_vertice <- read.csv(paste0(root_path, "csv/organoid_vertice_F.csv"))

### Making a custom function for plotting dendrogram with links.
fn_draw_network_links <- function(vertice_data, edge_list, corpus_type, size_max) {
  ### Preparing for using the corpus_type argument to specify data objects.
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor", 
                                  ifelse(corpus_type %in% c("organoid", "OoC", "ToC"), corpus_type, NA))
  ### Specifying the data objects for organ links depending on the chosen corpus_type.
  links_later <- eval(parse(text = paste0(corpus_type_converted, "_links_later")))
  links_early <- eval(parse(text = paste0(corpus_type_converted, "_links_early")))
  links_from_later <- match(links_later$organ_1, vertice_data$name)
  links_to_later <- match(links_later$organ_2, vertice_data$name)
  links_from_early <- match(links_early$organ_1, vertice_data$name)
  links_to_early <- match(links_early$organ_2, vertice_data$name)
  ### Making an ggraph object from the vertice data and the edge list.
  graph_object <- graph_from_data_frame(edge_list, vertices = vertice_data)  
  ### Plotting.
  graph_network_links <- ggraph(graph_object, layout = "dendrogram", circular = TRUE) + 
    geom_edge_link(color = "grey50", alpha = 0.1, linetype = "longdash", width = 0.3) +
    geom_node_point(aes(size = ifelse(total == 0, 0.1, sqrt(total)), color = major_organ3), alpha = 0.6) + 
    geom_conn_bundle(data = get_con(from = links_from_early, to = links_to_early), 
                     alpha = 0.3, colour = "royalblue2", tension = 0.65, edge_width = log10(rep(links_early$n, each = 100)) * 1.2) +  
    geom_conn_bundle(data = get_con(from = links_from_later, to = links_to_later), 
                     alpha = 0.3, colour = "gold2", tension = 0.7, edge_width = log10(rep(links_later$n, each = 100)) * 1.2) +  
    geom_node_text(aes(label = lvl1, hjust = hjust, vjust = vjust), size = 2.5, fontface = 4, color = "black") + 
    geom_node_text(aes(label = lvl1_leaf, angle = angle, hjust = hjust, vjust = vjust), size = 2.2, fontface = 4, color = "black") + 
    geom_node_text(aes(label = lvl2, hjust = hjust, vjust = vjust), size = 2, fontface = 2, color = "black") + 
    geom_node_text(aes(label = lvl2_leaf, angle = angle, hjust = hjust, vjust = vjust), size = 2.2, color = "black") + 
    scale_size_continuous(range = c(0.5, 6), limits = c(0.1, sqrt(size_max)), breaks = c(sqrt(10), sqrt(100), sqrt(1000)), 
                          labels = c(10, 100, 1000), name = "Publication counts") + 
    guides(size = "none") + 
    labs(color = NULL) + 
    coord_flip(clip = "off") + 
    scale_x_reverse() + 
    scale_y_reverse(expand = expansion(mult = 0.1)) + 
    theme(panel.background = element_rect(fill = "transparent"), 
          legend.box = "horizontal", 
          legend.position = c(-0.04, 0.97), 
          legend.title = element_text(size = 6, face = 2), 
          legend.text = element_text(size = 5), 
          legend.key.size = unit(2.5, "mm"), 
          plot.margin = margin(2, 2, 2, 2, "cm"), 
          plot.title = element_text(hjust = 0, vjust = 16)
    )
}

### Plotting and saving organ links of organoid research.
organoid_network_links <- fn_draw_network_links(organoid_vertice, edge_organ, corpus_type = "organoid", size_max = 8000) + 
  scale_color_manual(values = c("seagreen", "deepskyblue2", "maroon4", "orange", "red", "steelblue3", "aquamarine3", "blue", "grey80", "springgreen3", 
                                "violetred1", "grey40", "magenta", "purple", "pink2"))

ggsave(organoid_network_links, 
       filename = paste0(root_path, "results/organ_links/organoid_network_links.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 178, units = "mm")




### Preparing, plotting and saving organ links of OoC research.
### If you do not want to manually adjust the positions of text labels, change "position_adjustment = TRUE" to 
### "position_adjustment = FALSE".
### Also, delete the following line to read csv.
OoC_vertice <- fn_network_vertice(organ_counts, corpus_type = "OoC", position_adjustment = TRUE)

OoC_vertice <- read.csv(paste0(root_path, "csv/OoC_vertice_F.csv"))

OoC_network_links <- fn_draw_network_links(OoC_vertice, edge_organ, corpus_type = "OoC", size_max = 2500) + 
  scale_color_manual(values = c("seagreen", "deepskyblue2", "maroon4", "orange", "red", "steelblue3", "aquamarine3", "blue", "grey80", "springgreen3", 
                                "violetred1", "grey40", "magenta", "purple", "pink2"))

ggsave(OoC_network_links, 
       filename = paste0(root_path, "results/organ_links/OoC_network_links.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 178, units = "mm")






##########
###
### Writing a supplementary csv file showing publications on multiple organ models.
###
########## 
load(paste0(root_path, "R_results/organ_types_F"))

multi_model_metadata <- left_join(combined_counted, 
                                  organ_types_F %>% select(ID, author, year, title, doi), 
                                  by = "ID")

multi_models <- multi_model_metadata %>% 
  filter(type == "Research article", 
         n > 1) %>% 
  select(author, year, title, doi, phase, corpus_F, organ_1, organ_2, n) %>% 
  arrange(corpus_F, - n, organ_2, organ_1, year, author)

write.csv(multi_models, file = paste0(root_path, "results/csv/multi_models.csv"), row.names = FALSE)
