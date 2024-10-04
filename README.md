# Summary

This repository is for mapping academic literature on 3D culture model systems (organoid and organ-on-chip systems), which is published as Shoji et al., 2023 (see below). The repository contains literature metadata , R code, categorization results, and csv files that were used for the mapping analysis, as well as generated figures. Note that, due to licensing issues, we only distribute metadata of literature under Creative Commons Attribute licenses. If you want to reproduce the analysis, see *./README_reproduction.md* for more details.

The repsitory can be used to: 
1. investigate the data (see *./README_investigating.md*)
2. reproducing the analysis (see *./README_reproduction.md*).

!! Note !!
The read_bibliography function of the revtools package gives an error on R version 4.3.0. and later.
See *./R/for_newer_R_versions.R* for a workaround.

# Citation

The paper on this analysis is published as below. 

Shoji JY, Davis RP, Mummery CL, Krauss S. 
Global Literature Analysis of Organoid and Organ-on-Chip Research. 
Adv Healthc Mater. 2024 Aug;13(21):e2301067. 
doi: 10.1002/adhm.202301067. Epub 2023 Aug 7. PMID: 37479227.

# Terminology

- **Repository** means the GitHub repository *https://github.com/jyshoji/text_analysis_organoids*
- **R script files** mean files containing R code in *./R/* folder of the repository, with the extension *.R*.

# Folders and files

## ./R/

The folder contains R script files used for the analysis.


## ./R_results/

Containing R data files that were generated through the analysis by the R scripts.

## ./csv/

Containing csv files that were generated by/for the R code. Typically, the subfolder *./csv/temps/* contains csv files generated by R code, which were then manually adjusted on Excel and saved in *./csv/*.

## ./csv2/

Containing csv files that can be readily used to reproduce the analysis. 

## ./miscs/

Containing miscellaneous files for the analysis. 

## ./results/

Graphical figures generated in the analysis.

## ./results2/

Contains the same folders and subfolders as the above *./results/*, but no figure files. Can be used to easily setup folders in which R saves graphical files.  

## LICENSE.md

License statement.

## README.md

This file.

## README_investigating.md
Instruction on how to investigate the data in the repository.

## README_raw_data.md
Instuction on the metadata corpora that were/can be used for the analysis. It also includes details of how academic publications were searched, retrieved, and processed. 

## README_reproduction.md
Instruction on how to reproduce the analysis.


## R_scripts_flowchart.pdf

A PDF file showing a flowchart of the analysis, showing how each of the R script files and R data files relate to each other.


## raw_data3.zip

Containing metadata of academic publications under the Creative Commons Attribute Licenses. May be used for partial reproduction of the analysis.