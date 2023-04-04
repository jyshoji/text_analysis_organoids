# Summary

This file contains instruction on how to reproduce the meta-analysis of oranoid and organ-on-chip publications. 

In short, the analysis consists of two parts: 
1. Categorization of academic publications. 
2. Visualization of the categorization.

Due to legal restrictions posed by license holders of the academic publications, publishers, and/or literature databases, we are not allowed to publicly distribute the original publication metadata that we used for the analysis. Thus, the repository allows only limited reproduction of the analysis, as allows: 
1. Reproduction of both categorization and visualization of the open-access subset of the academic publications (i.e., those under the Creative Commons Attribute Licences).
2. Reproduction of only the vizualization part of all the academic publications that we used for the analysis. 

If you want to reproduce the categorization of all the academic publications, you will need to either collect academic publications yourself, or to obtain the corpus from the lead contact. You may read more at the "Reproduction" section. 

If you want to reproduce the analysis using the open-access subset of academic publications, you may jump to the section "Easy-start guide".

In addition, guides are provided on how we manually adjusted the data (see the "manual adjustments of the data"). 

# Reproduction

We consider three potential approaches for reproducing the analysis.

1. Partial reproduction using the open-access subset of the academic publications.

- We publish metadata of academic publications under the Creative Commons Attribute licencing, which encompasses about one-third of publications that we used for the analysis. The metadata can be used to run the code with minimal modifications. The metadata is stored as the zip file *./raw_data3.zip*.
- See the next section **Easy-start guide** for details.

2. Full reproduction of both categorization and visualization of all the academic publications, using our corpus

- Please contact the lead contact, Stefan Krauss (s.j.k.krauss@medisin.uio.no), and we will provide you with our corpus.

3. Full reproduction of both categorization and visualization of all the academic publications, without using our corpus

- This approach requires readers to obtain publication metadata from EMBASE, PubMed, Scopus, Web of Science and/or bioRxiv.
- It can be time-consuming, and may take a day or two just to collect the required metadata.
- Upon import into R, file names of metada are expected to include database types ("em_" for EMBASE, "pubmed" for PubMed, "scopus" for Scopus, "savedrecs" for Web of Science, and "BR_" for bioRxiv) and article types ("_RV" for reviews, "_RA" for non-reviews, and "BR_" for preprints). 
- This approach will allow almost full reproduction of the analysis. However, due to how literature databases handle publication dates before and after the actual print date, you may have slightly more/less publications compared to our corpus.
- Depending on combinations of databases that publications are retrieved from, the code may need adjustments. 
- You may also need to perform manual adjustments and quality checking of the data as we did. When an R script file includes a manual adjustment step, follow the instruction there.

# Easy-start guide

This section provides instruction on an easy start of the analysis. This approach skips all the manual adjustments and quality checking. If you want to perform manual adjustments as well, see the next section "Manual adjustments of the data" as well.

In order to run the analysis, you will need to; (1) make a root folder of the analysis, (2) Store required files in the above root folder, (3) install R packages, (4) set the right file path in each R script file, and (5) run the code in each R script file sequentially. 

1. Make a root folder of the analysis.

- You can make a root folder at your desired location. Once you set the right file path in R script files, the code should work without problems. 
- For example, if you use a mac, you may make a folder "organoid_meta_analysis" in your home directory (i.e., /Users/*username*/organoid_meta_analysis)
- If you use a Windows, you may make a folder "organoid_meta_analysis" folder on the C drive. You will also need to replace all occurrences of "/" in file paths with double backslash.
- If you use a Mac, the easiest way may be to make a folder "~/Research_data/Hybrida/final_analysis/" and use it as the root folder. In this way, the default file path in R script files can be used without changes. 

2. Storing required files in the above root folder.

- Download all the folders and files of the repository, and save them in your root folder.
- The easiest way to download them may be to go to the repository, click the green button "Code" in the upper part of the page, slightly to the right, and choose "Download ZIP". This will download the entire repository. After downloading, unzip it if necessary. You may then move the entire folder to where you want to make the root folder (e.g., ~/), rename it (e.g., organoid_meta_analysis), and use it as the root folder. 
- Then, make *./R_temps/* folder in your root folder. This folder is used to save intermediate R data files which can be used for quality control purposes.
- Note that you do not actually need all the files to run the code. If you want to avoid storing unnecessary files, just keep the files and folders listed below. 
- *./R/* folder and all its contents.
- *./R_results/* folder, which can stay empty. 
- *./csv2/* folder and its contents. Delete the *./csv/* folder, and rename the *csv2* folder as *csv*. 
- *./results2/* and all its contents. Delete the *./results/* folder and rename the *results2* folder as *results*. 
- If you want partial reproduction using the open-access subset of the metadata, download and unzip the *./raw_data3.zip* file, and rename the resulting folder as *./raw_data2/*. If you have collected your own corpus from the databases, make a folder "raw_data2" in your root folder. Then, make "ms", "onchip", and "organoid" folders in *./raw_data2/*, and store publications on microphysiological systems, on-chip technology, and organoids in each folder, respectively. 

3. Installing packages

- If you use RStudio, go to "tools" in the menu bar, choose "install packages". Write down a name of a package in the "packages" box.
- You will need "ggpp", "ggraph", "igraph", "revtools", "rworldmap", "tidytext", and "tidyverse".
- Alternatively, you can run the following lines on RStudio to install these packages.

install.packages("ggpp")

install.packages("ggraph")

install.packages("igraph")

install.packages("revtools")

install.packages("rworldmap")

install.packages("tidytext")

install.packages("tidyverse")

4. Setting file paths in each R script file.

- Follow the instruction in R script files to set the file path, so that R can save at and load from your root folder.
- You will essentially only need to change one line of code per an R script file where the root path is set, except for some of the R script files starting with *fig_* where root path is set multiple times to facilitate our quality control. 
- For example, if your root folder is *~/organoid_meta_analysis/*, change the line:
- root_path <- "~/Research_data/Hybrida/final_analysis/"
- into 
- root_path <- "~/organoid_meta_analysis/"

5. Running the code in R script files.

- See *./R_scripts_flowchart/* for details.
- You will first need to run the code in *./R/formatting.R*, and then *./R/organ_types.R*. Then, you may run the code in *./R/cell_types_F.R*, *./R/country_F.R*, and *./R/organisms_F.R*. After this, you may run *./R/research_topics.R*.
- After the above, you may run code in any of the R script files starting with *fig_*, which generate graphical figures in the *./results/* folder.
- *./R/accuracy_tests.R*, *./R/capturing_topics.R*, and *./R/investigating.R* are for quality cotrol, for identifying research topics to capture, and for investigating the data, respectively, and are not needed for generating graphical figures.
- Once you change the line of code for setting file path (as described in 4. Setting file paths in each R script file), the code should work without further changes except below; 
- In *./R/fig_organ_classifications.R*, change all occurrences of; 
- **position_adjustment = TRUE** to 
- **position_adjustment = FALSE** 
- and delete the lines; 
- **organoid_nudge <- read.csv(paste0(root_path, "csv/organoid_edge_lvl3_nudge_F.csv"))** 
- **OoC_nudge <- read.csv(paste0(root_path, "csv/OoC_edge_lvl3_nudge_F.csv"))**
- Also in *./R/fig_organ_links.R*, change all occurrences of; 
- **position_adjustment = TRUE** to
- **position_adjustment = FALSE**
- and delete the lines; 
- **organoid_vertice <- read.csv(paste0(root_path, "csv/organoid_vertice_F.csv"))**
- **OoC_vertice <- read.csv(paste0(root_path, "csv/OoC_vertice_F.csv"))**

# Manual adjustments of the data

In some places, we saved intermediate data as csv files, manually adjusted them, and used them to run the subsequent code. The manual adjustments were performed to for example identify duplicate academic publications in the corpus, or to correct algorithm-based classifications. We typically saved such intermediate data files (called "output csv files" below) in *./csv/temps/*, and saved the manually adjusted data files (called "input csv files" below) in *./csv/* which was then loaded onto R. 

## How to handle manual adjustment steps

As manual adjustments of the data can be time-consuming, we assume that readers may want to skip these steps. We consider the following three options for how readers may deal with manual adjustments. 

1. Perform manual adjustments

In this case, the code can be run as is. However, readers will have to manually adjust the output csv files and save them as input csv files in */csv/*, typically with "_F" added to the end of the file names. 

2. Skip the manual adjustments

The R script files include comments saying that manual adjustments will be performed, along with quick instruction on how to skip that part. Readers may just follow the instruction. Note that this will require small changes to the code.

3. Use the dummy csv files.

Readers may use dummy csv files as input csv files. These files will not do anything, and the code will continue with no manual adjustments being made. To take this approach, copy the files in the *./csv2/* folder and save them in the *./csv/*, replacing any existing csv files. The advantage of this approach is that the code can be run as is, but the downside is that it still performs some computations prior to manual adjustments, and therefore may take longer than the above approach 2. In addition, you will still have to make following changes; 
- In *./R/fig_organ_classifications.R*, change all occurrences of; 
- **position_adjustment = TRUE** to 
- **position_adjustment = FALSE** 
- and delete the lines; 
- **organoid_nudge <- read.csv(paste0(root_path, "csv/organoid_edge_lvl3_nudge_F.csv"))** 
- **OoC_nudge <- read.csv(paste0(root_path, "csv/OoC_edge_lvl3_nudge_F.csv"))**
- Also in *./R/fig_organ_links.R*, change all occurrences of; 
- **position_adjustment = TRUE** to
- **position_adjustment = FALSE**
- and delete the lines; 
- **organoid_vertice <- read.csv(paste0(root_path, "csv/organoid_vertice_F.csv"))**
- **OoC_vertice <- read.csv(paste0(root_path, "csv/OoC_vertice_F.csv"))**


