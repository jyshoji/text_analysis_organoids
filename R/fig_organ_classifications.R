### This R script file is to draw circular tree maps and network graphs for hierarchical organ classifications.
###
### The script include the following steps.
### 1. Preparing for circular tree maps and network graphs.
### 2. Plotting Circular packing graphs
### 3. Plotting network graph of hierarchical organ classification
###
### The figures were saved at: 
### ./results/organ_classifications/



### Loading a package.
library(tidyverse)
library(ggraph)
library(igraph)
library(ggpp)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading required files.
load(paste0(root_path, "R_results/organ_types_P"))




##########
###
### 1. Preparing for circular tree maps and network graphs.
###
##########

### Loading the edge list.
### This is used to provide higher organ categories of an organ category.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv")) 
  

### Calculating the number of publications per corpus and article types, in which organ type classification was made.
### The "plot_total" column shows the total number of publications to be plotted.
identified_total <- organ_types_P %>% 
  filter(!organ_type == "unidentified") %>% 
  group_by(corpus_F, type) %>% 
  summarize(plot_total = n()) %>% 
  ungroup() 

### Making a custom function for calculating trends of publication counts.
### "Trend" here means the relative increase of publication counts in the "later" phase as compared with the "early" phase.
### The input data frames will be count data, with "phase" column showing either "early" or "later" phase, 
### and "n" column showing the number of publication in the corresponding phase.
### The "output_prefix" argument is used to add a prefix for output column names.
fn_calculate_trends <- function(count_data, output_prefix = "") {
  trend_data <- count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}


### Counting the number of publications per corpus and article types, and using the custom function to calculate the trend.
corpus_trends <- organ_types_P %>% 
  group_by(corpus_F, type) %>% 
  count(phase) %>% 
  ungroup() %>% 
  fn_calculate_trends(., "corpus") %>% 
  ### Adding the above-generated plot_total column.
  left_join(., identified_total, by = c("corpus_F", "type"))




### Calculating the publication counts of each organ category, as well as their trends.
### For this purpose, "from_OR" columns are used which include the identified organ category, 
### as well all the upper-level organ categories of the hierarchical organ classification tree.
### In this way, a paper on e.g., colon will be also counted as "intestine", and "gastrointestinal."
organ_trends <- organ_types_P %>% 
  pivot_longer(starts_with("from_OR"), values_to = "hierarchical_organ_type", values_drop_na = TRUE) %>% 
  mutate(hierarchical_organ_type = factor(hierarchical_organ_type)) %>% 
  group_by(corpus_F, type, phase, .drop = FALSE) %>% 
  count(hierarchical_organ_type) %>% 
  ungroup() %>% 
  fn_calculate_trends(., "organ")


### Combining the corpus_trends and organ_trends.
combined_trends <- organ_trends %>% 
  left_join(., corpus_trends, by = c("corpus_F", "type")) %>% 
  ### Adjusting the organ trends by corpus trends, and setting the upper limit for the adjusted trends to be 3.
  mutate(adjusted_trend = organ_trend / corpus_trend) %>% 
  mutate(adjusted_trend2 = ifelse(is.nan(adjusted_trend), 0, 
                                  ifelse(adjusted_trend > 3, 3, adjusted_trend))) %>% 
  ### Adding the corresponding 1st-level organ categories (major_organ column) to each of organ categories.
  ### The 1st-level organ categories will be used for grouping the publications.
  mutate(hierarchical_organ_type2 = gsub("unspecified ", "", hierarchical_organ_type)) %>% 
  left_join(., edge_all[, 2:3], by = c("hierarchical_organ_type2" = "to")) %>% 
  mutate(hierarchical_organ_type = as.character(hierarchical_organ_type)) 








##########
###
### 2. Plotting Circular packing graphs
###
##########

### Below, organs will be plotted as two-level hierarchical categories, with an extra level included for intestinal and neural organs.
### For this purpose, making a vector containing the names of organs, whose lower categories are to be plotted.
lvl2_5_organs <- c("brain", "barriers", "nerve", "intestine")

### Making a custom function to make edge lists, which show connections between organs 
### (e.g., showing lung is a lower category of respiratory).
### This step is needed because when plotting, organs in the trend data and the edge list have to match.
fn_edge_lvl3 <- function(trend_data, edge_list, corpus_type, article_type = "Research article") {
  ### From the trend data, selecting article types, corpus, and organs to plot.
  modified_trends <- trend_data %>% 
    filter(corpus_F == corpus_type) %>% 
    filter(type == article_type) %>% 
    filter(!organ_total == 0) %>% 
    filter(!hierarchical_organ_type == "unidentified")
  ### From the edge list, only selecting organs that appear in the selected trend data.
  edge_lvl3 <- edge_list %>% 
    filter(to %in% modified_trends$hierarchical_organ_type) %>% 
    ### Only selecting organ categories that have the following organ categories as the upper-level categories.
    ### In this way, two-level organ categories will be plotted, with an extra level of categories for lvl2_5_organs.
    filter(from %in% c(modified_trends$major_organ, lvl2_5_organs, "body")) 
}



fn_count_lvl3 <- function(trend_data, edge_list, corpus_type, article_type = "Research article") {
  ### This part is same as the above custom function.
  ### From the trend data, selecting article types, corpus, and organs to plot.
  modified_trends <- trend_data %>% 
    filter(corpus_F == corpus_type) %>% 
    filter(type == article_type) %>% 
    filter(!organ_total == 0) %>% 
    filter(!hierarchical_organ_type == "unidentified")
  ### Further modifying the trend data.
  modified_lvl3 <- modified_trends %>% 
    ### Removing organs that do not exist in the edge list.
    filter(hierarchical_organ_type %in% unique(c(edge_list$from, edge_list$to))) %>% 
    ### Assigning organ names in different columns based on their relative levels.
    ### lvl1 corresponds to the body, lvl2 corresponds to the organ systems, and lvl3 shows other organs.
    ### UN means unspecified part of an organ. 
    ### This category is needed to more accurately size the circles in the plots. 
    ### For example the number of publications on brain will be calculated as the sum of publication counts on it lower categories
    ### which are "brainstem", "forebrain", and "UN".
    ### These columns are used for formatting text labels in the plots.
    mutate(lvl2 = ifelse(hierarchical_organ_type %in% major_organ, hierarchical_organ_type, " ")) %>% 
    mutate(lvl2_5 = ifelse(hierarchical_organ_type %in% lvl2_5_organs, hierarchical_organ_type, " ")) %>%   
    mutate(lvl3 = ifelse(hierarchical_organ_type %in% c("body", major_organ, lvl2_5_organs) | grepl("unspecified", hierarchical_organ_type), " ", 
                         hierarchical_organ_type)) %>% 
    mutate(UN = ifelse(grepl("unspecified", hierarchical_organ_type), "UN", " ")) %>% 
    ### changing the column order so that the plot function can find the required columns.
    select(3:20, 1:2)
}

### Writing a (csv) file of text position nudging.
### The csv file will be used to manually adjust the positions of the texts to avoid text overlaps.
fn_write_nudge_positions <- function(edge_list, position_adjustment = FALSE) {
  ### Converting the data object names to character strings so that it can be used as a part of the name of the csv file.
  edge_file_name <- deparse(substitute(edge_list))
  ### Reordering rows in the edge list, so that the order matches the plot order.
  ### In this way, the plot function can use the coordinate of the corresponding texts.
  edge1 <- edge_list %>% 
    filter(from == "body") %>% 
    arrange(to)
  edge2 <- edge_list %>% 
    filter(from %in% edge_all$major_organ) %>% 
    arrange(to)
  edge3 <- edge_list %>% 
    filter(!from %in% c("body", edge_all$major_organ)) %>% 
    arrange(to)
  ### Making a data frame for adjusting positions of figure labels.
  nudge_positions <- data.frame(organ = c("", edge1$to, edge2$to, edge3$to), 
                                x = 0, 
                                y = 0)
  ### Saving as a csv for manually adjusting figure label positions.
  ### If you do not want to manually adjust them, keep "position_adjustment = FALSE" which is the default argument.
  if(position_adjustment) {
    write.csv(nudge_positions, 
              file = paste0(root_path, "csv/temps/", edge_file_name, "_nudge.csv"), row.names = FALSE)
  }
  return(nudge_positions)
}


### Making a custom function for drawing circular packing graphs. 
fn_celtic_circles <- function(edge_list, vertice_data, nudge_file, random_seed = 1) {
  ### Determining the corpus type based on the input data frame.
  corpus_type <- unique(vertice_data$corpus_F)
  ### Converting corpus names to the writing styles used as data object name prefix.
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor organoids", 
                                  ifelse(corpus_type == "organoid", "organoids", 
                                         ifelse(corpus_type %in% c("OoC", "ToC"), corpus_type, NA)))
  ### Making a figure title, based on the corpus type and the total number of publications to be plotted.
  title_label <- paste0("Organs and substructures modelled as ", corpus_type_converted, " (", 
                        unique(vertice_data$plot_total), " ", tolower(unique(vertice_data$type)), "s)")
  ### Setting a random seed for reproducibility.
  set.seed(random_seed)
  ### Making an igraph object
  graph_object <- graph_from_data_frame(edge_list, vertices = vertice_data)
  ### Plotting
  celtic_circle <- ggraph(graph_object, layout = "circlepack", weight = organ_total) + 
    geom_node_circle(aes(fill = adjusted_trend2), size = 0.05, color = "grey30", alpha = 0.7) + 
    ### Adding texts showing organ names.
    geom_node_text(aes(label = lvl2), size = 3.2, fontface = "bold.italic", color = "black", show.legend = FALSE, 
                   position = position_nudge_center(x = nudge_file$x, y = nudge_file$y, direction = "none")) + 
    geom_node_text(aes(label = lvl2_5), size = 2.8, fontface = "bold", color = "grey10", show.legend = FALSE, 
                   position = position_nudge_center(x = nudge_file$x, y = nudge_file$y, direction = "none")) +   
    geom_node_text(aes(label = lvl3), size = 2.5, color = "grey10", show.legend = FALSE, 
                   position = position_nudge_center(x = nudge_file$x, y = nudge_file$y, direction = "none")) + 
    geom_node_text(aes(label = UN), size = 2, color = "grey10", show.legend = FALSE, 
                   position = position_nudge_center(x = nudge_file$x, y = nudge_file$y, direction = "none")) +  
    scale_fill_gradient2(low = "darkblue", mid = "paleturquoise", high = "orange", midpoint = 1, guide = "colorbar", name = "Trends") + 
    #labs(title = title_label) + 
    theme_void() + 
    theme(
      plot.title = element_text(size = 8, face = 2, vjust = - 5), 
      legend.position = c(0.92, 0.15), 
      legend.title = element_text(size = 6, face = 2, vjust = 0), 
      legend.text = element_text(size = 5), 
      legend.key.size = unit(4, "mm")
    ) + 
    coord_fixed() 
}




### Plotting for organoids.


organoid_edge_lvl3 <- fn_edge_lvl3(combined_trends, edge_all[, 1:2], corpus_type = "organoid")

organoid_organ_lvl3_count <- fn_count_lvl3(combined_trends, organoid_edge_lvl3, corpus_type = "organoid")

organoid_nudge <- fn_write_nudge_positions(organoid_edge_lvl3, position_adjustment = TRUE)
### If you do not want to manually adjust the positions of text labels, change "position_adjustment = TRUE" to 
### "position_adjustment = FALSE".
### Also, delete the following line to read csv.

organoid_nudge <- read.csv(paste0(root_path, "csv/organoid_edge_lvl3_nudge_F.csv"))

organoid_bubble <- fn_celtic_circles(organoid_edge_lvl3, organoid_organ_lvl3_count, organoid_nudge, random_seed = 8) 


ggsave(organoid_bubble, 
       filename = paste0(root_path, "results/organ_classifications/organoid_bubble.pdf"), 
       device = cairo_pdf, 
       width = 178, height = 178, units = "mm")




###
### Organ-on-a-chip
###
OoC_edge_lvl3 <- fn_edge_lvl3(combined_trends, edge_all[, 1:2], corpus_type = "OoC")

OoC_organ_lvl3_count <- fn_count_lvl3(combined_trends, OoC_edge_lvl3, corpus_type = "OoC")

OoC_nudge <- fn_write_nudge_positions(OoC_edge_lvl3, position_adjustment = TRUE)
### If you do not want to manually adjust the positions of text labels, change "position_adjustment = TRUE" to 
### "position_adjustment = FALSE".
### Also, delete the following line to read csv.

OoC_nudge <- read.csv(paste0(root_path, "csv/OoC_edge_lvl3_nudge_F.csv"))

OoC_bubble <- fn_celtic_circles(OoC_edge_lvl3, OoC_organ_lvl3_count, OoC_nudge, random_seed = 5) 


ggsave(OoC_bubble, 
       filename = paste0(root_path, "results/organ_classifications/OoC_bubble.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 178, units = "mm")








### Plotting for organoids, preprints
PP_organoid_edge_lvl3 <- fn_edge_lvl3(combined_trends, edge_all[, 1:2], corpus_type = "organoid", article_type = "Preprint")

PP_organoid_organ_lvl3_count <- fn_count_lvl3(combined_trends, PP_organoid_edge_lvl3, corpus_type = "organoid", article_type = "Preprint")

PP_organoid_nudge <- fn_write_nudge_positions(PP_organoid_edge_lvl3, position_adjustment = TRUE)
### If you do not want to manually adjust the positions of text labels, change "position_adjustment = TRUE" to 
### "position_adjustment = FALSE".
### Also, delete the following line to read csv.

PP_organoid_nudge <- read.csv(paste0(root_path, "csv/PP_organoid_edge_lvl3_nudge_F.csv"))

PP_organoid_bubble <- fn_celtic_circles(PP_organoid_edge_lvl3, PP_organoid_organ_lvl3_count, PP_organoid_nudge, random_seed = 1) 


ggsave(PP_organoid_bubble, 
       filename = paste0(root_path, "results/organ_classifications/PP_organoid_bubble.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 178, units = "mm")









##########
###
### 3. Plotting network graph of hierarchical organ classification
###
##########

### Making a custom function 
fn_count_SP <- function(trend_data, corpus_type, article_type = "Research article") {
  ### From the trend data, selecting article types, corpus, and organs to plot.
  ### Different from the above custom functions for circular packing graphs, "unspecified" organ categories are removed.
  ### This is because in network graphs, the number of publication does not have to be determined by the sum of lower categories.
  trend_SP <- trend_data %>% 
    filter(corpus_F == corpus_type) %>% 
    filter(type == article_type) %>% 
    filter(!organ_total == 0) %>% 
    filter(!grepl("unspecified", hierarchical_organ_type)) %>% 
    mutate(hierarchical_organ_type = gsub("body", " ", hierarchical_organ_type)) %>% 
    arrange(hierarchical_organ_type) %>% 
    ### Making a new column to show organ categories and corresponding publication counts to use as figure labels.
    mutate(name = paste0(hierarchical_organ_type, " (", organ_total, ")")) %>% 
    select(c(3:17, 1:2))
  ### Reordering the rows.
  trend_SP_reordered <- trend_SP %>% 
    filter(!hierarchical_organ_type %in% c("multiple organs", "venom gland", "unidentified")) %>% 
    rbind(., trend_SP %>% filter(hierarchical_organ_type == "multiple organs")) %>% 
    rbind(., trend_SP %>% filter(hierarchical_organ_type == "venom gland")) %>% 
    rbind(., trend_SP %>% filter(hierarchical_organ_type == "unidentified"))
}

### Making a custom function to make edge lists.
### Essentially, organ categories that are present in the input modified trend data are selected.
fn_edge_SP <- function(count_data, edge_list) {
  edge_SP <- edge_list %>% 
    filter(to %in% count_data$hierarchical_organ_type) %>% 
    mutate(from = gsub("body", " ", from)) 
}

### Making a custom function to draw network graphs.
fn_organ_network <- function(vertice_data, edge_list) {
  ### Making an igraph object
  g_network <- graph_from_data_frame(edge_list, vertices = vertice_data)
  ### Determining the corpus_F based on the input data frame.
  corpus_type <- unique(vertice_data$corpus_F)
  ### Changing the writing style of the corpus.
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor organoids", 
                                  ifelse(corpus_type == "organoid", "organoids", 
                                         ifelse(corpus_type %in% c("OoC", "ToC"), corpus_type, NA)))
  ### Making the figure titles.
  title_label <- paste0("Organs and substructures modelled as ", corpus_type_converted, " (", 
                        unique(vertice_data$corpus_total), " ", tolower(unique(vertice_data$type)), "s)")
  ### Drawing.
  organ_network <- ggraph(g_network, layout = "igraph", algorithm = "tree") + 
    geom_edge_link(color = "grey50", alpha = 0.15) +
    geom_node_point(aes(size = sqrt(organ_total), color = adjusted_trend2), alpha = 0.8) + 
    geom_node_text(aes(label = name), size = 2.1, fontface = 2, vjust = -0.5) + 
    scale_color_gradient2(low = "darkblue", mid = "paleturquoise", high = "orange", midpoint = 1, guide = "colorbar", name = "Trends") + 
    labs(#title = title_label, 
         size = "Publication counts") + 
    coord_flip() + 
    scale_x_reverse() + 
    scale_y_reverse(expand = expansion(mult = 0.1))
}


### Drawing network graph of organoid
organoid_counts_SP <- fn_count_SP(combined_trends, corpus_type = "organoid")

organoid_edge_SP <- fn_edge_SP(organoid_counts_SP, edge_all[, 1:2])

organoid_network <- fn_organ_network(organoid_counts_SP, organoid_edge_SP) + 
  scale_size_continuous(limits = c(0, sqrt(10000)), breaks = c(sqrt(10), sqrt(100), sqrt(1000), sqrt(10000)), labels = c(10, 100, 1000, 10000)) + 
  theme(panel.background = element_rect(fill = "transparent"), 
        legend.position = c(0.08, 0.17), 
        legend.title = element_text(size = 7, face = 2), 
        legend.text = element_text(size = 6), 
        legend.key.size = unit(5, "mm"))

ggsave(organoid_network, 
       filename = paste0(root_path, "results/organ_classifications/organoid_network.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 250, units = "mm")





OoC_counts_SP <- fn_count_SP(combined_trends, corpus_type = "OoC")

OoC_edge_SP <- fn_edge_SP(OoC_counts_SP, edge_all[, 1:2])

OoC_network <- fn_organ_network(OoC_counts_SP, OoC_edge_SP) + 
  scale_size_continuous(limits = c(0, sqrt(2500)), breaks = c(sqrt(10), sqrt(100), sqrt(1000)), labels = c(10, 100, 1000)) + 
  theme(
    panel.background = element_rect(fill = "transparent"), 
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.07, 0.20), 
    legend.title = element_text(size = 7, face = 2), 
    legend.text = element_text(size = 6), 
    legend.key.size = unit(5, "mm"))

ggsave(OoC_network, 
       filename = paste0(root_path, "results/organ_classifications/OoC_network.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 190, units = "mm")

