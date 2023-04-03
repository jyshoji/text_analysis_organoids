##########
###
### Installing R and RStudio
###
##########

### RStudio provides graphical user interface for coding with R.
###
### You can download R and RStudio from below.
### https://posit.co/download/rstudio-desktop/




##########
###
### Installing a package
###
##########

### After installing R and RStudio, open this file on RStudio.
###
### Then, install the package "tidyverse" by running the line of code below.
install.packages("tidyverse")



##########
###
### Making a root folder for the analysis.
###
##########

### You can make a root folder at your desired location. 
### 
### For example, if you use a mac, you may make a folder "organoid_meta_analysis" in your home directory 
### (i.e., /Users/*username*/organoid_meta_analysis).
### If you use a Windows, you may make a folder "organoid_meta_analysis" folder on the C drive.
###
### Then, make the folders "R_results" and "R" in your root folder. 
### Download and save the ./R_results/research_topics_P of the repository in the R_results folder.
### Download and save the ./R/investigating.R in the R folder.



##########
###
### Loading the data file and the package.
###
########## 

### Setting the file path to your root folder.
### If your root folder is ~/organoid_meta_analysis/, run the below.
root_path <- "~/organoid_meta_analysis/"

### If you use Windows, and your root folder is C:\\organoid_meta_analysis\\, run the below instead (after removing ### at the start of the line).
### root_path <- "C:\\organoid_meta_analysis\\"

### Loading the research_topics_P.
### If you use Windows, replace "/" below with "\\".
load(paste0(root_path, "R_results/research_topics_P"))

### Loading the required package.
library(tidyverse)




##########
###
### Subsetting the data
###
########## 

### The data can be subset by column values.
### To do this, you will first want to known what column values to choose.
### See ./README_investigating.md to see what columns mean.
### You can also check the column names as below.
colnames(research_topics_P)

### Showing the values in the column "type".
unique(research_topics_P$type)
### This basically means showing unique values (i.e., deduplicated values) of the column "type" of the data object "research_topics_P".
### This will give you:
### [1] "Preprint"         "Research article" "Review"     
### which means that the column contains three values, "Preprint", "Research article", and "Review".

### Selecting research articles.
research_articles <- research_topics_P %>% 
  filter(type == "Research article")
### The pipe operator %>% basically means "use the former as an input for the latter".
### So, the above lines mean "making a data object "research_articles" by applying the second line of code to the data object "research_topics_P".
### The second line of the code means "filter the data object to select rows where the values in the "type" column is "Research article".
### Note that you need to write the value ("Research article" here) exactly as it appeared following the line "unique(research_topics_P$type)" above.

### Taking a look at the TF_alzheimers_disease column.
unique(research_topics_P$TF_alzheimers_disease)
### This will give: 
### [1] FALSE  TRUE
### which means the column value is either FALSE or TRUE.

### Collectively, you can choose research articles that mention Alzheimer's disease as below.
alzheimers_disease <- research_topics_P %>% 
  filter(type == "Research article") %>% 
  filter(TF_alzheimers_disease == TRUE)

### Checking the titles of the selected publications.
alzheimers_disease[, "title"]
### This means "subset the data object alzheimers_disease by choosing the column "title".
### Similarly, you can see the DOIs of the selected publications.
alzheimers_disease[, "doi"]
### You can also select rows of the data object, for example to show the first 10 publications.
alzheimers_disease[1:10, "doi"]

### Overall, through selecting combinations of column values, you can subset the data to find publications of your interest.




##########
###
### Counting the number of publications in a subset of the corpus.
###
##########

### You can for example count the number of papers in the subset of the corpus.
### The below lines show the countries that performed research on Alzheimer's disease using neural organoids, 
### along with the number of research articles.
AD_countries <- research_topics_P %>% 
  ### Subsetting the corpus
  filter(type == "Research article") %>% 
  filter(TF_alzheimers_disease == TRUE) %>% 
  filter(corpus_F == "organoid") %>% 
  filter(major_organ == "neural") %>% 
  ### Counting the numbers of occurrences of each value in the main_country column.
  count(main_country, sort = TRUE)

### Below lines show the numbers of review publications in each year of cardiovascular OoC models.
cardiovascular_OoC_year <- research_topics_P %>% 
  filter(type == "Review") %>% 
  filter(corpus_F == "OoC") %>% 
  filter(major_organ == "cardiovascular") %>% 
  ### Counting the numbers of occurrences of each value in the year column.
  count(year)





##########
###
### Some more useful tips.
###
##########

### Exclusion
### You can use ! to select a complement.
### For example, below will exclude reviews, instead of selecting them.
t3 <- research_topics_P %>% 
  filter(!type == "Review")

### Temporarily disable a line of code.
### 
### If you use #, R does not run the code after the symbol in the same line.
### For example, below, the line for filtering based on the year is ignored. 
t4 <- research_topics_P %>% 
  filter(type == "Research article") %>% 
  #filter(phase == "later") %>% 
  filter(corpus_F == "non-tumor") %>% 
  filter(major_organ == "ocular") %>% 
  filter(TF_gene_editing == TRUE)



### Filtering with multiple values
### If you want to filter based on more than one values, for example to select papers using either oral or otic organoids, below will do it.
t5 <- research_topics_P %>% 
  filter(major_organ %in% c("oral", "otic"))
### Also, if you want to see papers studying either Alzheimer's disease or Parkinson's disease, below will do it.
t6 <- research_topics_P %>% 
  filter(TF_alzheimers_disease == TRUE | TF_parkinsons_disease == TRUE)
### Above, "|" means "or".


### Selecting papers that mention certain term.
###
### You can for example select papers that mention the term "liver" in the title.
t7 <- research_topics_P %>% 
  filter(grepl("\\blivers?\\b", title, ignore.case = TRUE) == TRUE)
### Above, the second line look for the character sting "liver" in the "title" column.
### "ignore.case = TRUE" means case-insensitive search.
### "\\b" is word boundary, and "?" means that the character immediately before is optional.
### So it selects papers that mention either "liver" or "livers", but exclude "delivery" for example.

