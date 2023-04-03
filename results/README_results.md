# Summary

This folder contains subfolders to save graphical figures or .csv files generated through the analysis. 

# Folders and files within

## corpus_summary

Containing figures showing the number of publications in the corpora.

### publication_by_corpus.png

A figure showing the number of publications in organoid, tumor organoid, OoC, and ToC corpora.

## csv

Containing tables in the .csv formats sumamrizing numerical data.

### countries_contributions.csv

Showing the number and proportions of research articles by countries. Generated in *./R/fig_global_trends.R*.

### counts_per_capita.csv

Showing the number of research articles by countries per million people. Generated in *./R/fig_global_trends.R*.

### organisms_counts.csv

Showing the number of research articles on different organisms. Generated in *./R/fig_research_topics.R*.

### organoid_average_growth.csv

Showing the average growth of the number of research articles per corpus. Generated in *./R/fig_corpus_summary.R*.

### table_publication_counts.csv

A table showing the number of academic publications per corpus, for each article type. Generated in *./R/fig_corpus_summary.R*.

## global_trends

Contains figures showing global research trends. Generated in *./R/fig_global_trends.R*.

### pie_charts/

A subfolder containing pie charts that show proportions of research articles on organ types in top 20 countries. Drawn separately for each of the organoid and OoC corpora.

### world_maps/

A subfolder containing world maps that show adjusted fractional contributions of each country. Drawn separately for each corpus, with or without legends.

## model_comparison

Figures for comparing organoid and OoC model systems. Generated in *./R/fig_model_comparison.R*.

### organ_model_comparison.png

A bubble plot showing organs/substructures modelled as organoids, OoCs, and organoid-on-a-chip.

### topic_model_point.png

A bubble plot showing preference of model systems in relation to research topics. 

### topic_trends_bubble.png

A bubble plot showing publication counts and trends of research artciles on research topics.

### topic_trends_bubble_all_disease.png
Similar to the above, but includes all identified diseases in the Y-axis.

## organ_classifications

Circular packing graphs and dendrograms of organ classifications. Generated in *./R/fig_organ_classifications.R*.

### OoC_bubble.png
### organoid_bubble.png
### PP_organoid_bubble.png

Circular packing graphs showing two-level organ classifications. The first two graphs are of research articles, and the last graph (starting with "PP_") is of preprint publications.

### OoC_network.png
### organoid_network.png

Dedrograms showing hierarchical organ classifications. 

## organ_links

### OoC_network_links.png
### OoC_network_links.png

Figures showing dendtograms of hierarchical organ classification, with links connecting organs/substructures that are researched in combination. Generated in *fig_organ_links.R*.

## prenatal_classifications

Dendrograms of prenatal classifications. Generated in *./R/fig_prenatal_classifications.R*.

### OoC_EM_HM_network.png
Dendrograms of prenatal OoC models, including both human and mouse models.

### organoid_EM_human_network.png
Dendrograms of human prenatal organoid models.

### organoid_EM_mouse_network.png
Dendrograms of mouse prenatal organoid models.

## research_topics

Miscellaneous graphs of researched topics. Generated in *./R/fig_research_topics.R*. 

### cell_sources_area.png
Area graph of cell source usage.

### covid_organs_yearly.png
Bar chart showing the number of research articles on SARS-CoV-2 per year.

### organisms_area.png
Area graph showing the ratio of human/mouse research.

## README_results.md

This file.

