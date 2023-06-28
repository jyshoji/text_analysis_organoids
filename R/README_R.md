# Summary

This folder contains R script files which are to be opened and run on RStudio.

# Files

## README_R.md

This file.

## accuracy_tests.R

Containing code used for determining accuracy of computational classifications as compared to manual classifications.

## capturing_topics.R

Code used for capturing research topics. Key research topics were identified by this code, and were captured in the *./R/research_topics.R*. 

## cell_types_F.R

Code used for identifying cell sources. The code uses *./R_results/all_corpus* and *./R_results/organ_types_P* as inputs, and includes following steps.

1. Identifying terms to capture.
2. Listing terms to capture.
3. Capturing the terms in titles.
4. Capturing the terms in keywords_abstract 
5. Assigning the cell source classification.

The end result was saved as *./R_results/cell_types_F* and *./R_results/cell_types_P*.

## country_F.R

Code used for identifying countries of research. The code uses *./R_results/all_corpus* and *./R_results/organ_types_P* as inputs, and includes following steps.
1. Listing names of countries.
2. Restyling the address field.
3. Identifying countries involved in research

The end result was saved as *./R_results/countries_F* and *./R_results/countries_P*.

## fig_corpus_summary.R

Code used for drawing graphs/tables summarizing the number of publications. The code uses *./R_results/organ_types_G* as an input, and includes following steps; 
1. Making a table showing the number of publications.
2. Calculating average yearly growth of publication counts on 3D culture mode systems, as compared with tumor research.
3. Plotting publication counts of 3D culture model systems.

The generated graphs were saved in *./results/corpus_summary/*, and the tables were saved in *./results/csv/*

## fig_global_trends.R

Code used for drawing graphs of global research trends. The code uses *./R_results/organ_types_P*, and *./R_results/countries_P* as inputs, and includes following steps.
1. Identifying well-researched major organ types in research articles of each corpus.
2. Calculating the number and trends of publications.
3. Making a table showing countries' contributions
4. Drawing pie charts showing researched organ types in top research countries.
5. Drawing world maps showing fractional contribution counts of countries in organoid/OoC research.

The plotted graphs were saved at *./results/global_trends/*

## fig_model_comparison.R
Code used for drawing graphs for comparing organoid and OoC models. The code uses *./R_results/organ_types_P*, and *./R_results/research_topics_P* as inputs, and includes following steps.
1. Comparing organ/substructures modelled as organoids, OoCs, and organoids-on-a-chip.
2. Comparison of research topics in organoid and OoC research.

The plotted graphs were saved at *./results/model_comparison/*.

## fig_organ_classifications.R
Code used for drawing graphs of organ classifications. The code uses *./R_results/organ_types_P* as an input, and includes following steps.
1. Preparing for circular tree maps and network graphs.
2. Plotting Circular packing graphs
3. Plotting network graph of hierarchical organ classification

The plotted graphs were saved at *./results/organ_classifications/*.

## fig_organ_links.R
Code used for drawing graphs of multi-organ models. The code uses *./R_results/organ_types_P* as an input, and includes following steps.
1. Counting the number of academic publications which were classified as being using multiple organ models.
2. Identifying organs/substructures simultaneously researched as organoid/OoC models.
3. Drawing network graphs with links.

The plotted graphs were saved at *./results/organ_links/*.

## fig_prenatal_classification.R

Code used for drawing graphs of prenatal classifications. The code uses *./R_results/organ_types_P* as an input, and includes following steps.
1. Preparing for circular tree maps and network graphs.
2. Drawing circular packing graphs of prenatal models.
3. Drawing network graphs of prenatal models.

The plotted graphs were saved at *./results/prenatal_classifications/*.

## fig_research_topics.R

Code used for drawing graphs of research topics. The code uses *./R_results/organ_types_P*, *./R_results/cell_types_P*, *./R_results/organisms_P*, and *./R_results/research_tipcs_P* as inputs, and includes following steps.
1. Plotting trends of cell source usage.
2. Plotting trends of research organisms
3. Making a table of minor research organisms.
4. Plotting organs/substructures modelled for coronavirus research

The plotted graphs were saved at *./results/research_topics/*.

## formatting.R

Code used for importing, deduplicating and formatting literature metadata for all the subsequent analysis. The code uses literature metadata stored in *./raw_data2/*, and includes following steps.
1. Importing literature metadata of microphysiological systems.
2. Deduplicating and reformatting metadata on microphysiological systems.
3. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
4. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
5. Combining the organ-on-a-chip and microphysiological system corpora.
6. Deduplicating the combined organ-on-a-chip corpus
7. Reformatting the organ-on-a-chip corpus
8. Importing, reformatting, and deduplicating literature metadata on organoids.
9. Combining orgnaoid and organ-on-a-chip corpora

The end result is a corpus of academic publications on organoids and OoCs (the latter including microphysiological systems), which was saved as *./R_results/all_corpus*, which was used for all the subsequent analysis. 

## investigating.R

Code and instructions on how to identify academic publications on specific topics using *./R_results/research_topics_P*. Not needed for the analysis, but instead shows how to use the generated data. 

## organ_types.R

Code used for identifying organs/substructures modelled as organoids/OoCs. The code uses *./R_results/all_corpus* as an input, and includes following steps.
1. Identifying organ names that appear immediately before "organoid" or "onchip"
2. Reshaping the "all_corpus" data frame to facilitate later extraction of organ names.
3. Extracting a few words before "organod", "assembloid", or "onchip"
4. Listing words to capture
5. Capturing full terms of organ names in the combined text field.
6. Capturing all terms (i.e., full terms and abbreviations) of organ names in pw (a few words before organoid/onchip) in titles
7. Capturing all terms in pw in keywords_abstracts
8. Capturing all terms in key sentences in titles (i.e., titles that includes the term "organoid" or "onchip").
9. Capturing all terms in key sentences of keywords_abstracts
10. Assigning the final classification

The result of the code was saved as *./R_results/organ_types_F*, *./R_results/organ_types_G*, and *./R_results/organ_types_P*/.

## organism_F.R
Code used for identifying research organisms. The code uses *./R_results/all_corpus* and *./R_results/organ_types_P* as inputs, and includes following steps.
1. Identifying phrases containing organism names that have to be excluded.
2. Deleting phrases containing organism names that have to be excluded.
3. Making a nested list of organisms' names to capture
4. Capturing organisms' names

The result of the analysis was saved as *./R_results/organisms_F*, and *./R_results/organisms_P*.

## research_topics.R
Code used for identifying research topics. The code uses *./R_results/all_corpus*, *./R_results/organ_types_P*, *./R_results/cell_types_P*, *./R_results/organisms_P*, and *./R_results/countries_P* as inputs, and includes following steps.
1. Identifying research topics to capture. (the code for this step is now in the *./R/capturing_topics.R*)
2. Making lists of research topics to capture
3. Capturing research topics.
4. Making a summary data frame of research topics and other research themes.

The result of the analysis was saved as: *./R_results/research_topics_F*, and *./R_results/research_topics_P*.

## updating_corpus.R
Code used for updating the literature corpus. The code uses *./R_results/all_corpus* and generates an R object file with the same name with new publications being added. The code is mostly identical to that in *./R/formatting.R* with a few modifications. There is a 9-month overlap of the publication period between the existing corpus and newly added publications, which was introduced to handle changes in the publication year which may happen when publication status changes from electronic to printed version. Thus, the main objectives of the code is to add new publications, update the publication year when applicable, and removed duplicate publications that already existed in the existing corpus. The code includes following steps.
1. Saving the existing corpus as a back-up.
2. Importing literature metadata of microphysiological systems.
3. Deduplicating and reformatting metadata on microphysiological systems.
4. Importing, reformatting, and deduplicating literature metadata on on-chip technology.
5. Selecting organ-on-a-chip documents by identifying words that occur before "on-a-chip".
6. Combining the organ-on-a-chip and microphysiological system corpora.
7. Deduplicating the combined organ-on-a-chip corpus. 
8. Reformatting the organ-on-a-chip corpus and adding it to the existing corpus
9. Importing, reformatting, and deduplicating literature metadata on organoids, and adding it to the existing corpus
10. Combining orgnaoid and organ-on-a-chip corpora
