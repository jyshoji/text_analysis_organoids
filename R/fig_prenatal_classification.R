### This R script file is to draw circular tree maps and network graphs for prenatal classifications.
###
### The script include the following steps.
### 1. Preparing for circular tree maps and network graphs.
### 2. Drawing circular packing graphs of prenatal models.
### 3. Drawing network graphs of prenatal models.
###
### The figures were saved at: 
### ./results/prenatal_classification/


### Loading packages
library(tidyverse)
library(ggraph)
library(igraph)

root_path <- "~/Research_data/Hybrida/final_analysis/"

load(paste0(root_path, "R_results/organ_types_P"))
load(paste0(root_path, "R_results/organisms_P"))





##########
###
### 1. Preparing for circular tree maps and network graphs.
###
##########

### Making a custom function for calculating trends of publication counts.
fn_calculate_trends <- function(publication_count_data, output_prefix = "") {
  trend_data <- publication_count_data %>% 
    pivot_wider(names_from = phase, values_from = n) %>% 
    mutate(across(c("early", "later"), ~ replace_na(., 0))) %>% 
    mutate(total = later + early, 
           trend = later / early) %>% 
    rename_with(~ gsub("^", paste0(output_prefix, "_"), .), .cols = c("early", "later", "total", "trend"))
}


### Selecting academic publications classified as using prenatal models.
### Only publications on either human or mouse will be plotted.
prenatal_corpus <- organ_types_P %>% 
  ### Adding the research organism column, and selecting publications that used either human or mouse.
  mutate(organism = organisms_P$organism) %>% 
  filter(organism %in% c("human", "mouse")) %>% 
  ### Only selecting papers classified as using prenatal models
  filter(!is.na(prenatal_BG_type_unspecified)) 

### Calculating the publication counts, as well as their trends of prenatal models.
prenatal_corpus_trends <- prenatal_corpus %>% 
  group_by(type, corpus_F) %>% 
  count(phase) %>% 
  ungroup() %>% 
  fn_calculate_trends(., "corpus")

### Calculating the publication counts and trends of prenatal models, separately for human and mouse.
prenatal_HM_trends <- prenatal_corpus %>% 
  pivot_longer(starts_with("from_EM"), values_to = "hierarchical_prenatal_type", values_drop_na = TRUE) %>% 
  mutate(hierarchical_prenatal_type = factor(hierarchical_prenatal_type)) %>% 
  group_by(organism, phase, corpus_F, type, .drop = FALSE) %>% 
  count(hierarchical_prenatal_type) %>% 
  ungroup() %>% 
  fn_calculate_trends(., "prenatal")


### Calculating the publication counts and trends of prenatal models, NOT separately for human and mouse.
prenatal_both_trends <- prenatal_corpus %>% 
  pivot_longer(starts_with("from_EM"), values_to = "hierarchical_prenatal_type", values_drop_na = TRUE) %>% 
  mutate(hierarchical_prenatal_type = factor(hierarchical_prenatal_type)) %>% 
  group_by(phase, corpus_F, type, .drop = FALSE) %>% 
  count(hierarchical_prenatal_type) %>% 
  ungroup() %>% 
  fn_calculate_trends(., "prenatal") %>% 
  mutate(organism = "both") %>% 
  select(8, 1:7) 


### Combining the above three data frames of publication counts and trends.
combined_prenatal_trends <- rbind(prenatal_HM_trends, prenatal_both_trends) %>% 
  left_join(., prenatal_corpus_trends, by = c("corpus_F", "type")) %>% 
  ### Adjusting the trends of prenatal models by trends of the corpus, 
  ### and setting the upper limit for the adjusted trends to be 3.
  mutate(adjusted_trend = prenatal_trend / corpus_trend) %>% 
  mutate(adjusted_trend2 = ifelse(is.nan(adjusted_trend), 0, 
                                  ifelse(adjusted_trend > 3, 3, adjusted_trend))) %>% 
  ### Removing "body" as hierarchical prenatal_type
  filter(!hierarchical_prenatal_type == "body") %>% 
  mutate(hierarchical_prenatal_type = as.character(hierarchical_prenatal_type)) %>% 
  group_by(organism, corpus_F, type) %>% 
  ### Determining the maximal number of publications in prenatal_total column.
  ### This will give the total number of publications on prenatal models, which will later be used as a figure label.
  mutate(organism_total = max(prenatal_total)) 




##########
###
### 2. Drawing circular packing graphs of prenatal models.
###
##########

### Loading the edge list.
edge_all <- read.csv(paste0(root_path, "csv/edge_all.csv"))

### Making a custom function for making edge lists for the plot.
fn_prenatal_edge_lvl3 <- function(trend_data, edge_list, corpus_type = "organoid", organism_type = "both", article_type = "Research article") {
  ### Selecting rows to plot.
  filtered_trend <- trend_data %>% 
    filter(corpus_F %in% corpus_type) %>% 
    filter(type %in% article_type) %>% 
    filter(organism == organism_type) %>% 
    filter(!prenatal_total == 0)
  ### Making an edge list by selecting prenatal categories that are present in the above filtered_trend
  prenatal_edge_lvl3 <- edge_list %>% 
    filter(to %in% filtered_trend$hierarchical_prenatal_type) %>% 
    filter(from %in% c("prenatal", "embryonic", "fetal", "blastoid and gastruloid")) 
}


### Making a custom function for making a vertice data frame for the plot.
fn_prenatal_counts_lvl3 <- function(trend_data, edge_list, corpus_type = "organoid", organism_type = "both", article_type = "Research article") {
  ### Selecting rows to plot.
  prenatal_counts_lvl3 <- trend_data %>% 
    filter(corpus_F %in% corpus_type) %>% 
    filter(type %in% article_type) %>% 
    filter(organism == organism_type) %>% 
    filter(!prenatal_total == 0) %>% 
    filter(hierarchical_prenatal_type %in% unique(c(edge_list$from, edge_list$to))) %>% 
    ### Assigning prenatal category names in different columns based on their relative levels.
    ### lvl1 corresponds to "prenatal, lvl2 are either "embryonic" or "fetal", and lvl3 are the lower categories.
    ### For blastoid and gastruloid, and extra level is included.
    ### For this reason, the "blastoid and gastruloid" category is made to be the lvl2_5.
    mutate(lvl2 = ifelse(hierarchical_prenatal_type %in% c("embryonic", "fetal"), hierarchical_prenatal_type, " ")) %>% 
    mutate(lvl2_5 = ifelse(hierarchical_prenatal_type == "blastoid and gastruloid", hierarchical_prenatal_type, " ")) %>% 
    mutate(lvl3 = ifelse(hierarchical_prenatal_type %in% c("embryonic", "fetal", "blastoid and gastruloid", "unspecified embryonic", "prenatal"), " ", 
                         hierarchical_prenatal_type)) %>% 
    mutate(UN = ifelse(hierarchical_prenatal_type == "unspecified embryonic", "UN", " ")) %>% 
    select(c(4:19, 1:3))
}


### Making a custom function for drawing circular packing graphs for prenatal models.
fn_prenatal_celtic_circles <- function(edge_list, vertice_data, random_seed = 1) {
  ### Making an igraph object.
  prenatal_g_lvl3 <- graph_from_data_frame(edge_list, vertices = vertice_data)
  ### Determining the corpus based on the input data.
  corpus_type <- unique(vertice_data$corpus_F)
  ### Changing the writing styles of the corpus.
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor organoids", 
                                  ifelse(corpus_type == "organoid", "organoids", 
                                         ifelse(corpus_type %in% c("OoC", "ToC"), corpus_type, NA)))
  ### Making character strings to use in figure labels as organism names.
  organism_type <- ifelse(unique(vertice_data$organism) == "both", "human and mouse", unique(vertice_data$organism))
  ### Making a character strings for figure titles
  title_label <- paste0("Prenatal structures modelled as ", corpus_type_converted, " in ",  organism_type, 
                        " (", vertice_data$organism_total, " ", tolower(unique(vertice_data$type)), "s)")
  ### Setting a random seed.
  set.seed(random_seed)
  ### Drawing
  prenatal_bubble <- ggraph(prenatal_g_lvl3, layout = "circlepack", weight = sqrt(prenatal_total)) + 
    geom_node_circle(aes(fill = adjusted_trend2), size = 0.02, alpha = 0.7) + 
    geom_node_text(aes(label = lvl2), size = 1.5, fontface = "bold.italic", show.legend = FALSE) + 
    geom_node_text(aes(label = lvl2_5), size = 1.2, fontface = "bold", show.legend = FALSE) +     
    geom_node_text(aes(label = lvl3), size = 1, color = "grey10", check_overlap = TRUE, show.legend = FALSE) +  
    geom_node_text(aes(label = UN), size = 0.8, color = "grey10", check_overlap = TRUE, show.legend = FALSE) +    
    scale_fill_gradient2(low = "darkblue", mid = "paleturquoise", high = "orange", midpoint = 1, guide = "colorbar", name = "Trends", limits = c(0, 3), 
                         breaks = c(0, 1, 2, 3)) + 
    labs(title = title_label) + 
    theme_void() + 
    theme(
      plot.title = element_text(size = 3.5, face = 2, vjust = - 5), 
      legend.position = c(0.96, 0.18), 
      legend.title = element_text(size = 3, face = 2, vjust = 0), 
      legend.text = element_text(size = 2.5), 
      legend.key.size = unit(2.5, "mm")
    ) + 
    coord_fixed() 
}


### Drawing a graph of human organoid prenatal models.
### Not saved at the moment, as the figure won't be included in the paper.
### organoid_prenatal_human_edge_lvl3 <- fn_prenatal_edge_lvl3(combined_prenatal_trends, edge_list = edge_all[, 1:2], organism_type = "human")
### organoid_prenatal_human_counts_lvl3 <- fn_prenatal_counts_lvl3(combined_prenatal_trends, organoid_prenatal_human_edge_lvl3, organism_type = "human")
### organoid_EM_human_bubble <- fn_prenatal_celtic_circles(organoid_prenatal_human_edge_lvl3, organoid_prenatal_human_counts_lvl3)
### ggsave(organoid_EM_human_bubble, 
###        filename = paste0(root_path, "results/prenatal_classifications/organoid_EM_human_bubble.png"),  
###        width = 60, height = 60, units = "mm")

### mouse organoid prenatal models.
### organoid_prenatal_mouse_edge_lvl3 <- fn_prenatal_edge_lvl3(combined_prenatal_trends, edge_list = edge_all[, 1:2], organism_type = "mouse")
### organoid_prenatal_mouse_counts_lvl3 <- fn_prenatal_counts_lvl3(combined_prenatal_trends, organoid_prenatal_mouse_edge_lvl3, organism_type = "mouse")
### organoid_EM_mouse_bubble <- fn_prenatal_celtic_circles(organoid_prenatal_mouse_edge_lvl3, organoid_prenatal_mouse_counts_lvl3, random_seed = 3)
### ggsave(organoid_EM_mouse_bubble, 
###        filename = paste0(root_path, "results/prenatal_classifications/organoid_EM_mouse_bubble.png"),  
###        width = 60, height = 60, units = "mm")

### OoC prenatal models including both human and mouse
### OoC_prenatal_HM_edge_lvl3 <- fn_prenatal_edge_lvl3(combined_prenatal_trends, edge_list = edge_all[, 1:2], corpus_type = "OoC", organism_type = "both")
### 
### OoC_prenatal_HM_counts_lvl3 <- fn_prenatal_counts_lvl3(combined_prenatal_trends, OoC_prenatal_HM_edge_lvl3, corpus_type = "OoC", organism_type = "both")
### OoC_EM_HM_bubble <- fn_prenatal_celtic_circles(OoC_prenatal_HM_edge_lvl3, OoC_prenatal_HM_counts_lvl3)
### ggsave(OoC_EM_HM_bubble, 
###        filename = paste0(root_path, "results/prenatal_classifications/OoC_EM_HM_bubble.png"),  
###        width = 60, height = 60, units = "mm")

















##########
###
### 3. Drawing network graphs of prenatal models.
###
##########


### Making a custom function for making a vertice data frame for the plot.
fn_prenatal_counts_SP <- function(trend_data, corpus_type = "organoid", organism_type = "both", article_type = "Research article") {
  trend_data_SP <- trend_data %>% 
    filter(corpus_F == corpus_type) %>% 
    filter(organism == organism_type) %>% 
    filter(type == article_type) %>% 
    filter(!grepl("unspecified", hierarchical_prenatal_type)) %>% 
    filter(!prenatal_total == 0) %>% 
    arrange(hierarchical_prenatal_type) %>% 
    mutate(name = paste0(hierarchical_prenatal_type, " (", prenatal_total, ")")) %>% 
    select(4:16, 1:3)
}

### Making a custom functin for making edge lists.
fn_prenatal_edge_SP <- function(trend_data_SP, edge_list) {
  edge_SP <- edge_all[, 1:2] %>% 
    filter(to %in% trend_data_SP$hierarchical_prenatal_type) %>% 
    filter(!to == "prenatal")
}

### Making a custom function to draw network graphs of prenatal models.
fn_prenatal_network <- function(edge_list, vertice_data) {
  prenatal_g_lvl3 <- graph_from_data_frame(edge_list, vertices = vertice_data)
  corpus_type <- unique(vertice_data$corpus_F)
  corpus_type_converted <- ifelse(corpus_type == "tumor_organoid", "tumor organoids", 
                                  ifelse(corpus_type == "organoid", "organoids", 
                                         ifelse(corpus_type %in% c("OoC", "ToC"), corpus_type, NA)))
  organism_type <- ifelse(unique(vertice_data$organism) == "both", "human and mouse", unique(vertice_data$organism))
  title_label <- paste0("Embryonic / extraembryonic structures modelled as ", corpus_type_converted, " in ",  organism_type)
  prenatal_network <- ggraph(prenatal_g_lvl3, layout = "igraph", algorithm = "tree") + 
    geom_edge_link(color = "grey50", alpha = 0.15) +
    geom_node_point(aes(size = sqrt(prenatal_total), color = adjusted_trend2), alpha = 0.8) + 
    geom_node_text(aes(label = name), size = 1.6, fontface = 2, vjust = -0.5) + 
    scale_size_continuous(limits = c(0, 20), breaks = c(sqrt(10), sqrt(50), sqrt(200)), labels = c(10, 50, 200)) + 
    scale_color_gradient2(low = "darkblue", mid = "paleturquoise", high = "orange", midpoint = 1, guide = "colorbar", name = "Trends", 
                          breaks = c(0, 1, 2, 3)) + 
    labs(#title = title_label, 
         size = "Publication counts") + 
    coord_flip() + 
    scale_x_reverse() + 
    scale_y_reverse(expand = expansion(mult = 0.1)) + 
    theme(
      panel.background = element_rect(fill = "transparent"), 
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = c(0.1, 0.14), 
      legend.title = element_text(size = 7, face = 2), 
      legend.text = element_text(size = 6), 
      legend.key.size = unit(3, "mm"), 
      legend.box = "horizontal"
    )
}


### Drawing a network graph of human organoid prenatal models.
organoid_prenatal_human_counts_SP <- fn_prenatal_counts_SP(combined_prenatal_trends, organism_type = "human")

organoid_prenatal_human_edge_SP <- fn_prenatal_edge_SP(organoid_prenatal_human_counts_SP, edge_all[, 1:2])

organoid_EM_human_network <- fn_prenatal_network(organoid_prenatal_human_edge_SP, organoid_prenatal_human_counts_SP) + 
  labs(title = "A. Organoids (human)")

ggsave(organoid_EM_human_network, 
       filename = paste0(root_path, "results/prenatal_classifications/organoid_EM_human_network.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 70, units = "mm")




### Drawing a network graph of mouse organoid prenatal models.
organoid_prenatal_mouse_counts_SP <- fn_prenatal_counts_SP(combined_prenatal_trends, organism_type = "mouse")

organoid_prenatal_mouse_edge_SP <- fn_prenatal_edge_SP(organoid_prenatal_mouse_counts_SP, edge_all[, 1:2])

organoid_EM_mouse_network <- fn_prenatal_network(organoid_prenatal_mouse_edge_SP, organoid_prenatal_mouse_counts_SP) + 
  labs(title = "B. Organoids (mouse)")

ggsave(organoid_EM_mouse_network, 
       filename = paste0(root_path, "results/prenatal_classifications/organoid_EM_mouse_network.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 70, units = "mm")



### Drawing a network graph of OoC prenatal models including both human and mouse.
OoC_prenatal_HM_counts_SP <- fn_prenatal_counts_SP(combined_prenatal_trends, corpus_type = "OoC", organism_type = "both")

OoC_prenatal_HM_edge_SP <- fn_prenatal_edge_SP(OoC_prenatal_HM_counts_SP, edge_all[, 1:2])

OoC_EM_HM_network <- fn_prenatal_network(OoC_prenatal_HM_edge_SP, OoC_prenatal_HM_counts_SP) + 
  labs(title = "C. OoC (human and mouse)")

ggsave(OoC_EM_HM_network, 
       filename = paste0(root_path, "results/prenatal_classifications/OoC_EM_HM_network.pdf"),  
       device = cairo_pdf, 
       width = 178, height = 70, units = "mm")
