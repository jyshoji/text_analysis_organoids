# Summary

This file contains basic information on how to investigate the R data files. It is recommended to use *./R_results/research_topics_P* to investigate the data as this R data file contains most of the important classifications that were assigned through the analysis. This file has a tabular structure, with each row containing one publication and columns showing various classifications. The file can be used to find publications that meet certain criteria (e.g., finding publications using cerebral models in human, published after 2021), by subsetting the data by column values. The section below shows what each column of the data represents. 

In order to investigate the data, you will want to download *./R_results/research_topics_P* as a data file and *./R/investigating.R* for code and instructions. To download them, click the names of corresponding files at the repository, and click "download" on the right side of the browser window. 

For more instructions and examples of code, see *./R/investigating.R*, which can be opened on RStudio, as well as on text editors.

# Column names

## ID
Unique identification numbers assigned in the analysis to each of the documents.

## author
List of authors of the publication.

## title
Title of the publication.

## type
Article type, either "Review", "Research article", or "Preprint".

## year
Year of publication.

## doi
DOI

## corpus
Either "organoid" or "OoC", showing if the copy of the document is from the organoid corpus or the on-chip/microphysiological system corpus.

## phase
Either "early" or "later", showing if the publication is from the early (2011 - 2019) or later (2020 - ) period.

## TF_ columns (from TF_organ_development to TF_Toxoplasma)
Either TRUE or FALSE, showing whether the publication mentions the corresponding research topic/disease type.

## TF_columns (TF_human to TF_frog)
Either TRUE or FALSE, showing whether the publication is on the corresponding organism. These columns are originally generated in the *./R/organisms_F.R*. 

## TF columns (TF_induced_pluripotent_stem_cell to TF_stem_cell)
Either TRUE or FALSE, showing whether the publication mentions culture models generated from the corresponding cell cource. Originally generated in the *./R/cell_types_F.R*. 

## organ_type
Describing the organ type of the culture model of the publication. The values in this column for example include "forebrain", "intestine", and "liver". Originally generated in the *./R/organ_types.R*.

## major_organ
Describing the organ type of the culture model, but with categories at the organ system-level. Includes for example "neural", "gastrointestinal", and "hepatic, pancreatic, biliary". Originally generated in the *./R/organ_types.R*.

## tumor_type
Used to determine if the publication is on tumor models or not. If the value here was TRUE, the publication was excluded from the analysis. Originally generated in the *./R/organ_types.R*.

## prenatal_type
Showing prenatal structures modeled in the publication, if applicable. Originally generated in the *./R/organ_types.R*.

## blastoid_gastruloid
Either "blastoid", "gastruloid", "blastoid and gastruloid", or NA, and shows if the publication is on blastoids/gastruloids. Originally generated in the *./R/organ_types.R*.

## main_country
Showing the main country of research. Originally generated in the *./R/country_F.R*.

## organism
Showing the final classification for organisms, generated in the *./R/organisms_F.R* by summarizing the "TF_" columns.

## cell_type 
Showing the final classification for cell sources, generated in the *./R/cell_types_F.R* by summarizing the "TF_" columns.

## corpus_F
Showing the corpus to which the publication belongs. The value of this column was either "organoid", "tumor_organoid", "OoC", or "ToC"; however, publications in "tumor_organoid" and "ToC" corpora were removed from the analysis and the data at the repository.
