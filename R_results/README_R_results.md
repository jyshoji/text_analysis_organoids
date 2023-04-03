# Summary

This folder contains R data files that are generated through R scripts.

Note that each R script file typically generates two R data files, ending with either "_F" or "_P". Files with "_F" include text fields and bibliometric fields such as abstracts and journal names of publications, whereas files with "_P" lack them. As we are not allowed to publish all metadata, we are only publishing files ending with "_P" in the repository.

# Files

## README_R_results.md

This file.

## cell_types_P

Includes the cell source classification of documents. Generated in *./R/cell_types_F.R*. 

## countries_P

Includes country classifications of documents. Generated in *./R/country_F.R*. 

## organ_types_G

Includes the organ type classifications. Includes publications on tumor organoids and ToCs as well. Generated in *./R/organ_types.R*. 

## organ_types_P

Same as the above, except that the file does not include publications on tumor organoids and ToCs.

## organisms_P

Includes the research organism classifications. Generated in *./R/organisms_F.R*.

## research_topics_P 

Includes research topics classifications. Also includes classifications for cell sources, countries, organ types, and organisms that are imported from other R data files. Generated in *./R/research_topics.R*.