### This R script file is for testing accuracy of algorithm-based classifications 
### by comparing them with manual classifications. 

### Loading a package and the data.
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/organ_types_F"))
load(paste0(root_path, "R_results/organisms_F"))
load(paste0(root_path, "R_results/cell_types_F"))

accuracy_test_OR <- organ_types_F %>% 
  mutate(organism = organisms_F$organism, 
         cell_type = cell_types_F$cell_type) %>% 
  select(ID, text_all, organ_type, tumor_w, prenatal_BG_type, organism, cell_type, corpus_F) %>% 
  filter(corpus_F == "organoid")

### Setting random seed for the first subset of documents.
set.seed(46)

### Shuffling the row order.
nrow1 <- sample(nrow(accuracy_test_OR))

at_OR1 <- accuracy_test_OR[nrow1, ]

### Outcomes of manual classification could also vary widely depending on how much a human reader would read between lines.
### For example, in sentences saying 
### "Gene expression during brain development is not well known. Thus, we investigated the gene expression using an organoid model." 
### a human reader may assume that the authors used a brain organoid, which may or may not be correct.
### In order to deal with such a grey zone of manual classification, comparison between the manual and computational classifications are 
### made into three categories below: 1) OK means a match between the manual and computational classifications, 2) CON means a conditinal match 
### where the two classifications match when a human reader does not read too much between lines, and 3) NO means unmatch where the compuational 
### classification is unambiguously wrong.
###
### Below, the comment in each of the three lines shows:
### 1) A comment 
### 2) manual classification of organ type, tumor type, embryonic type, research organism, cell sources, 
### 3) algorithm-based classification of organ type, tumor type, embryonic type, research organism, cell sources 
### 4) three-category comparison of the two classifications for organ type (including the organ type, tumor type, and embryonic type classification), 
### research organisms, and cell sources.
at_OR1[1, "text_all"]
at_OR1[1, 3:7]
## 
## cardiovascular, NA, NA, human, NA
## cardiovascular   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR1[2, "text_all"]
at_OR1[2, 3:7]
## 
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    mouse      <NA>
## OK, NO, OK

at_OR1[3, "text_all"]
at_OR1[3, 3:7]
## 
## NA, NA, NA, human, iPSC
## unidentified   FALSE             <NA>    human      <NA>
## OK, OK, CON

at_OR1[4, "text_all"]
at_OR1[4, 3:7]
##
## NA, NA, NA, human, iPSC
## unidentified   FALSE             <NA>    human      <NA>
## OK, OK, CON

at_OR1[5, "text_all"]
at_OR1[5, 3:7]
## 
## intestine, NA, NA, human, mouse, and others, PSCs
## intestine   FALSE             <NA> mixed (human and non-human) pluripotent stem cell
## OK, CON, OK

at_OR1[6, "text_all"]
at_OR1[6, 3:7]
## The paper doesn't say "organoid".
## testes?, NA, NA, mouse?, stem cell? 
## unidentified   FALSE             <NA>    mouse      <NA>
## CON, OK, CON

at_OR1[7, "text_all"]
at_OR1[7, 3:7]
## 
## nasal, NA, NA, human, primary cell
## nasal   FALSE             <NA>    human unspecified stem cell
## OK, OK, NO

at_OR1[8, "text_all"]
at_OR1[8, 3:7]
## This is a review papers on organoids.
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    mouse      <NA>
## OK, NO, OK

at_OR1[9, "text_all"]
at_OR1[9, 3:7]
## 
## liver, NA, NA, human?, iPSC
## liver   FALSE        embryonic    human induced pluripotent stem cell
## OK, OK, OK

at_OR1[10, "text_all"]
at_OR1[10, 3:7]
## 
## liver, NA, NA, human, NA
## liver   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR1[11, "text_all"]
at_OR1[11, 3:7]
## The paper is on thyroid spheroids, but doesn't use the word "organoid".
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    human      <NA>
## OK, NO, OK

at_OR1[12, "text_all"]
at_OR1[12, 3:7]
## 
## brain, NA, NA, human?, NA
## brain   FALSE             <NA>     <NA>      <NA>
## OK, CON, OK

at_OR1[13, "text_all"]
at_OR1[13, 3:7]
## 
## intestine, NA, NA, human, NA
## intestine   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR1[14, "text_all"]
at_OR1[14, 3:7]
## 
## striatum, NA, NA, human, iPSCs
## striatum   FALSE             <NA> mixed (human and non-human) induced pluripotent stem cell
## OK, NO, OK

at_OR1[15, "text_all"]
at_OR1[15, 3:7]
##
## HPB, NA, NA, mouse, NA
## hepatic, pancreatic, biliary   FALSE             <NA>    mouse      <NA>
## OK, OK, OK

at_OR1[16, "text_all"]
at_OR1[16, 3:7]
## 
## oviduct, NA, NA, mouse, epithelial cell
## oviduct   FALSE             <NA>    mouse      <NA>
## OK, OK, NO

at_OR1[17, "text_all"]
at_OR1[17, 3:7]
## 
## kidney, NA, NA, human, NA
## kidney   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR1[18, "text_all"]
at_OR1[18, 3:7]
## The paper says "organoid formation of HLCs" where HLCs mean hepatocyte-like cells.
## liver, NA, NA, NA, Stem cell
## unidentified   FALSE             <NA> mixed (human and non-human)      <NA>
## NO, NO, NO

at_OR1[19, "text_all"]
at_OR1[19, 3:7]
##
## liver, NA, NA, mouse, progenitor cell (adult stem cell)
## liver   FALSE             <NA>    mouse      <NA>
## OK, OK, NO

at_OR1[20, "text_all"]
at_OR1[20, 3:7]
##
## liver, NA, NA, human, iPSC
## liver   FALSE             <NA>    human induced pluripotent stem cell
## OK, OK, OK

at_OR1[21, "text_all"]
at_OR1[21, 3:7]
## 
## stomach, NA, NA, human, NA
## stomach   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR1[22, "text_all"]
at_OR1[22, 3:7]
##
## liver, NA, NA, NA, tissue
## liver   FALSE            fetal    mouse      <NA>
## OK, NO, NO

at_OR1[23, "text_all"]
at_OR1[23, 3:7]
## 
## alveolus, NA, NA, human, iPSC
## alveolus   FALSE             <NA>    human induced pluripotent stem cell
## OK, OK, OK

at_OR1[24, "text_all"]
at_OR1[24, 3:7]
## 
## cerebrum, NA, NA, human, iPSC
## cerebrum   FALSE             <NA>    human induced pluripotent stem cell
## OK, OK, OK


## Summary so far in the order of: organ type match, research organism match, cell source match
## The first line is without accepting conditinal matches, and the second line is with accepting conditinal matches.
## 22/24, 16/24, 16/24
## 23/24, 18/24, 19/24





### Second subset
set.seed(914)

### Shuffling the row order.
nrow2 <- sample(nrow(accuracy_test_OR))

at_OR2 <- accuracy_test_OR[nrow2, ]

at_OR2[1, "text_all"]
at_OR2[1, 3:7]
## 
## taste bud, NA, NA, mouse, NA
## taste bud   FALSE             <NA>    mouse      <NA>
## OK, OK, OK

at_OR2[2, "text_all"]
at_OR2[2, 3:7]
##
## small intestine, NA, NA, mouse, stem cell?
## small intestine   FALSE             <NA>    mouse unspecified stem cell
## OK, OK, CON

at_OR2[3, "text_all"]
at_OR2[3, 3:7]
## The paper is on developing bioink, and doesn't use the word "organodi"
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR2[4, "text_all"]
at_OR2[4, 3:7]
## A review on intestinal organoids.
## intestine, NA, NA,  NA, adult, embryonic, and induced pluripotent stem cell.
## intestine   FALSE             <NA>     <NA> embryonic stem cell
## OK, OK, NO

at_OR2[5, "text_all"]
at_OR2[5, 3:7]
## The authors say "previous studies derived pituitary lineages from organoid cultures", and 
## "we derived pituitary lineages from hPSCs using monolayer culture. Thus, it seems this study didn't use organoids.
## NA, NA, NA, human, PSC 
## pituitary   FALSE             <NA> mixed (human and non-human) embryonic stem cell
## CON, CON, CON

at_OR2[6, "text_all"]
at_OR2[6, 3:7]
## A review on brain damages by Zika virus, without using the word "organoid".
## brain? NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## CON, OK, OK

at_OR2[7, "text_all"]
at_OR2[7, 3:7]
## Only one sentence abstract, probably an editorial piece or conference abstract.
## tonsil, NA, NA, human, NA
## tonsil   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR2[8, "text_all"]
at_OR2[8, 3:7]
##
## cartilage, NA, NA, human, primary cell
## cartilage   FALSE             <NA>    human      <NA>
## OK, OK, NO

at_OR2[9, "text_all"]
at_OR2[9, 3:7]
## The paper is on a microfluidic system for drug screening.
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR2[10, "text_all"]
at_OR2[10, 3:7]
## 
## lacrimal gland, NA, NA, mouse and porcines but not human?, primary cell
## lacrimal gland   FALSE             <NA> mixed (human and non-human) adult stem cell
## OK, CON, NO

at_OR2[11, "text_all"]
at_OR2[11, 3:7]
##
## forebrain and midbrain, NA, NA, human, NA
## brain   FALSE             <NA>    human progenitor cell 
## OK, OK, NO

at_OR2[12, "text_all"]
at_OR2[12, 3:7]
## The paper doesn't seem to be on organoids.
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR2[13, "text_all"]
at_OR2[13, 3:7]
##
## intestine, NA, NA, NA, NA
## intestine   FALSE             <NA> mixed (human and non-human)      <NA>
## OK, CON, OK

at_OR2[14, "text_all"]
at_OR2[14, 3:7]
## The paper says "mouse PSCs ... allowing generation of mouse PSC-derived kidney" 
## where the authors probably meant kidney organoids rather than kidney itself.
## kidney, NA, NA, mouse, NA
## unidentified   FALSE             <NA>    mouse      <NA>
## CON, OK, OK

at_OR2[15, "text_all"]
at_OR2[15, 3:7]
##
## small intestine, NA, NA, human, NA
## small intestine   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR2[16, "text_all"]
at_OR2[16, 3:7]
##
## NA, NA, NA, human, NA
## unidentified   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR2[17, "text_all"]
at_OR2[17, 3:7]
## On developing computational approach to assess brain organoids.
## brain, NA, NA, human?, stem cell?
## brain   FALSE             <NA>    human unspecified stem cell
## OK, CON, CON

at_OR2[18, "text_all"]
at_OR2[18, 3:7]
##
## cochlea, NA, NA, mouse? NA
## cochlea   FALSE             <NA>    mouse      <NA>
## OK, CON, OK

at_OR2[19, "text_all"]
at_OR2[19, 3:7]
##
## mammary gland, NA, NA, human, NA
## mammary gland   FALSE             <NA>    human      <NA>
## OK, OK, OK

at_OR2[20, "text_all"]
at_OR2[20, 3:7]
## On development of hydrogels for organoid cultures.
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    human      <NA>
## OK, NO, OK

at_OR2[21, "text_all"]
at_OR2[21, 3:7]
##
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR2[22, "text_all"]
at_OR2[22, 3:7]
##
## cerebrum, NA, NA, human, NA
## cerebrum   FALSE             <NA> mixed (human and non-human)      <NA>
## OK, NO, OK

at_OR2[23, "text_all"]
at_OR2[23, 3:7]
##
## Oral, NA, NA, NA, stem cell
## multiple organs   FALSE             <NA>     <NA> unspecified stem cell
## CON, OK, OK

at_OR2[24, "text_all"]
at_OR2[24, 3:7]
##
## intestine, NA, NA, NA, adult stem cell
## intestine   FALSE             <NA>    mouse adult stem cell
## OK, NO, OK

##
## 20/24, 16/24, 17/24
## 24/24, 21/24, 20/24





### The third subset.
set.seed(16)

### Shuffling the row order.
nrow3 <- sample(nrow(accuracy_test_OR))

at_OR3 <- accuracy_test_OR[nrow3, ]

at_OR3[1, "text_all"]
at_OR3[1, 3:7]
## 
## reprudictive, NA, NA, NA, NA
## unidentified   FALSE             <NA>    mouse      <NA>
## CON, NO, OK

at_OR3[2, "text_all"]
at_OR3[2, 3:7]
##
## nerve NA, NA, human, iPSC
## nerve   FALSE             <NA>    human induced pluripotent stem cell
## OK, OK, OK

at_OR3[3, "text_all"]
at_OR3[3, 3:7]
##
## retina, NA, NA, mouse, ESC
## retina   FALSE             <NA>    mouse embryonic stem cell
## OK, OK, OK

at_OR3[4, "text_all"]
at_OR3[4, 3:7]
## A review on CRISPR on organoids and other approaches.
## unidentified, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA> pluripotent stem cell
## OK, OK, NO

at_OR3[5, "text_all"]
at_OR3[5, 3:7]
##
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR3[6, "text_all"]
at_OR3[6, 3:7]
##
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR3[7, "text_all"]
at_OR3[7, 3:7]
##
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>      <NA>
## OK, OK, OK

at_OR3[8, "text_all"]
at_OR3[8, 3:7]
##
## cerebrum, NA, NA, human, iPSC, PSC
## cerebrum   FALSE             <NA>    human     mixed
## OK, OK, CON

at_OR3[9, "text_all"]
at_OR3[9, 3:7]
## 
## prostate cancer?, tumor?, NA, human? tumor cell?
## unidentified   FALSE             <NA>    human      <NA>
## CON, CON, CON

at_OR3[10, "text_all"]
at_OR3[10, 3:7]
## A review on gastric glands in health and disease
## gastric glands?, NA, NA, NA, stem cell 
## gastrointestinal   FALSE             <NA>    human unspecified stem cell
## CON, CON, OK

at_OR3[11, "text_all"]
at_OR3[11, 3:7]
##
## small intestine, NA, NA, mouse, NA
## small intestine   FALSE             <NA>    mouse      <NA>
## OK, OK, OK

at_OR3[12, "text_all"]
at_OR3[12, 3:7]
## This paper summarizes three key studies of a conference, one on alveolus and another on organoids.
## NA, NA, NA, NA, NA
## alveolus   FALSE             <NA>    human      <NA>
## NO, NO, OK

at_OR3[13, "text_all"]
at_OR3[13, 3:7]
## The paper is about use of decellularized porcine brain ECM for culturing human brain organoids.
## brain, NA, NA, human, ESCs
## brain   FALSE             <NA>    human embryonic stem cell
## OK, OK, OK

at_OR3[14, "text_all"]
at_OR3[14, 3:7]
## The paper is on nasal spheroids.
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    human      <NA>
## OK, NO, OK

at_OR3[15, "text_all"]
at_OR3[15, 3:7]
##
## alveolus, NA, NA, mouse, adult stem cell
## alveolus   FALSE             <NA>    mouse      <NA>
## OK, OK, NO

at_OR3[16, "text_all"]
at_OR3[16, 3:7]
##
## NA, NA, NA, NA, NA
## unidentified   FALSE             <NA>    human      <NA>
## OK, NO, OK

at_OR3[17, "text_all"]
at_OR3[17, 3:7]
##
## NA, NA, blastoid, mouse, stem cell
## uterus   FALSE         blastoid    mouse unspecified stem cell
## NO, OK, OK

at_OR3[18, "text_all"]
at_OR3[18, 3:7]
##
## intestine, NA, NA, NA, adult stem cell
## intestine   FALSE             <NA>     <NA> adult stem cell
## OK, OK, OK

at_OR3[19, "text_all"]
at_OR3[19, 3:7]
## 
## small intestine, NA, NA, mouse, NA
## small intestine   FALSE             <NA>    mouse tumor cell
## OK, OK, NO

at_OR3[20, "text_all"]
at_OR3[20, 3:7]
##
## Lung, NA, NA, NA, adult stem cell
## alveolus   FALSE             <NA>    human adult stem cell
## CON, NO, OK

at_OR3[21, "text_all"]
at_OR3[21, 3:7]
##
## small intestine, NA, NA, pig, NA
## small intestine   FALSE             <NA>      pig      <NA>
## OK, OK, OK

at_OR3[22, "text_all"]
at_OR3[22, 3:7]
## The paper is on spheroids.
## NA, NA, NA, fish, NA
## unidentified   FALSE             <NA>     fish      <NA>
## OK, OK, OK

at_OR3[23, "text_all"]
at_OR3[23, 3:7]
##
## renal tubule, NA, NA, human, adult stem cell
## renal tubule   FALSE             <NA>    human adult stem cell
## OK, OK, OK

at_OR3[24, "text_all"]
at_OR3[24, 3:7]
##  The paper is about developing decellularized pig testicular tissue for potential 
## use for generating human teticular organoids.
## testes, NA, NA, human, NA
## testes   FALSE             <NA>    human      <NA>
## OK, OK, OK

## Summary
## 18/24, 17/24, 19/24
## 22/24, 19/24, 21/24






### Calculating averages
###
### Without accepting conditional matches
###
### organ type
mean(c(22/24, 20/24, 18/24))
## [1] 0.8333333
sd(c(22/24, 20/24, 18/24))
## [1] 0.08333333

### organisms
mean(c(16/24, 16/24, 17/24))
## [1] 0.6805556
sd(c(16/24, 16/24, 17/24))
## [1] 0.02405626

### cell sources
mean(c(16/24, 17/24, 19/24))
## [1] 0.7222222
sd(c(16/24, 17/24, 19/24))
## [1] 0.1339396


### With accepting conditional matches
###
### organ type
mean(c(23/24, 24/24, 22/24))
## [1] 0.9583333
sd(c(23/24, 24/24, 22/24))
## [1] 0.04166667

### organisms
mean(c(18/24, 21/24, 19/24))
## [1] 0.8055556
sd(c(18/24, 21/24, 19/24))
## [1] 0.06364688

### cell sources
mean(c(19/24, 20/24, 21/24))
## [1] 0.8333333
sd(c(19/24, 20/24, 21/24))
## [1] 0.02405626


### To summarize; 
### organ types
### 83 +- 8% (96 +- 4%)
###
### organisms
### 68 +- 2% (81 +- 6%)
###
### cell sources
### 72 +- 13% (83 +- 2%)










##########
###
### Accuracy test for OoC publications.
###
##########

### This code is for measuring accuracy of computational classifications on OoC publications.
### The cell source classification for OoC publications is not extensitve as most of OoC platforms 
### use primary cells rather than stem cells.
### Therefore, the cell source classification is not published, thus was not tested here.

### Loading a package and the data.
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

load(paste0(root_path, "R_results/organ_types_F"))
load(paste0(root_path, "R_results/organisms_F"))
load(paste0(root_path, "R_results/cell_types_F"))

accuracy_test_OoC <- organ_types_F %>% 
  mutate(organism = organisms_F$organism, 
         cell_type = cell_types_F$cell_type) %>% 
  select(ID, text_all, organ_type, tumor_w, prenatal_BG_type, organism, cell_type, corpus_F) %>% 
  filter(corpus_F == "OoC")



###
### Below, the classifications are shown from the left in the order of: 
### organ type, tumor type, embryonic type, organisms.
###


###
### Subset 1
###

### Setting random seed for the first subset of documents.
set.seed(225)

### Shuffling the row order.
nrow1 <- sample(nrow(accuracy_test_OoC))

at_OoC1 <- accuracy_test_OoC[nrow1, ]

at_OoC1[1, "text_all"]
at_OoC1[1, 3:6]
##
## bone marrow, NA, NA, human
## bone marrow   FALSE             <NA>    human
## OK, OK

at_OoC1[2, "text_all"]
at_OoC1[2, 3:6]
## This paper is on embryoid body on chip for reproductive medicine.
## NA, NA, NA, human?
## reproductive   FALSE             <NA>    human
## NO, CON

at_OoC1[3, "text_all"]
at_OoC1[3, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>    human
## OK, NO

at_OoC1[4, "text_all"]
at_OoC1[4, 3:6]
## 
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[5, "text_all"]
at_OoC1[5, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[6, "text_all"]
at_OoC1[6, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[7, "text_all"]
at_OoC1[7, 3:6]
##
## cartilage, NA, NA, NA
## cartilage   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[8, "text_all"]
at_OoC1[8, 3:6]
## 
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[9, "text_all"]
at_OoC1[9, 3:6]
##
## NA, NA, NA, human and mouse
## unidentified   FALSE             <NA> mixed (human and non-human)
## OK, OK

at_OoC1[10, "text_all"]
at_OoC1[10, 3:6]
##
## lung, NA, NA, NA
## lung   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[11, "text_all"]
at_OoC1[11, 3:6]
##
## NA, NA, NA, human
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC1[12, "text_all"]
at_OoC1[12, 3:6]
##
## brain, NA, NA, human?
## brain   FALSE             <NA>    human
## OK, CON

at_OoC1[13, "text_all"]
at_OoC1[13, 3:6]
##
## BBB, NA, NA, human
## blood-brain barrier   FALSE             <NA>    human
## OK, OK

at_OoC1[14, "text_all"]
at_OoC1[14, 3:6]
## 
## decidua, NA, fetal membrane, human?
## decidua   FALSE   fetal membrane    human
## OK, OK

at_OoC1[15, "text_all"]
at_OoC1[15, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[16, "text_all"]
at_OoC1[16, 3:6]
##
## brain, NA, NA, human
## brain   FALSE             <NA>    human
## OK, OK

at_OoC1[17, "text_all"]
at_OoC1[17, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[18, "text_all"]
at_OoC1[18, 3:6]
##
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC1[19, "text_all"]
at_OoC1[19, 3:6]
##
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC1[20, "text_all"]
at_OoC1[20, 3:6]
##
## liver, NA, NA, human
## liver   FALSE             <NA>    human
## OK, OK

at_OoC1[21, "text_all"]
at_OoC1[21, 3:6]
##
## cartilate, NA, NA, NA
## cartilage   FALSE             <NA>     <NA>
## OK, OK

at_OoC1[22, "text_all"]
at_OoC1[22, 3:6]
##
## liver, NA, NA, human
## liver   FALSE             <NA>    human
## OK, OK

at_OoC1[23, "text_all"]
at_OoC1[23, 3:6]
##
## NA, NA, NA, human
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC1[24, "text_all"]
at_OoC1[24, 3:6]
##
## NA, NA, NA, human
## unidentified   FALSE             <NA>    human
## OK, OK

## Summary
## 23/24, 21/24
## 23/24, 23/24



###
### Subset 2
###

### Setting random seed for the first subset of documents.
set.seed(80)

### Shuffling the row order.
nrow2 <- sample(nrow(accuracy_test_OoC))

at_OoC2 <- accuracy_test_OoC[nrow2, ]

at_OoC2[1, "text_all"]
at_OoC2[1, 3:6]
## The paper is on gut-skin axis.
## gut and skin, NA, NA, human
## multiple organs   FALSE             <NA>    human
## OK, OK

at_OoC2[2, "text_all"]
at_OoC2[2, 3:6]
##
## NA, NA, NA, mouse and human and others
## unidentified   FALSE             <NA> mixed (non-human)
## OK, OK

at_OoC2[3, "text_all"]
at_OoC2[3, 3:6]
##
## pancreas, NA, NA, human
## pancreas   FALSE             <NA>    human
## OK, OK

at_OoC2[4, "text_all"]
at_OoC2[4, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[5, "text_all"]
at_OoC2[5, 3:6]
## 
## liver, NA, NA, human
## liver   FALSE             <NA>    human
## OK, OK

at_OoC2[6, "text_all"]
at_OoC2[6, 3:6]
##
## muscle, NA, NA, NA
## muscle   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[7, "text_all"]
at_OoC2[7, 3:6]
##
## kidney, NA, NA, NA
## kidney   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[8, "text_all"]
at_OoC2[8, 3:6]
## 
## renal proximal tubule and vessels, NA, NA, human
## multiple organs   FALSE             <NA>    human
## OK, OK

at_OoC2[9, "text_all"]
at_OoC2[9, 3:6]
##
## intestine, NA, NA, human?
## intestine   FALSE             <NA>    human
## OK, CON,

at_OoC2[10, "text_all"]
at_OoC2[10, 3:6]
##
## brain, NA, NA, human
## brain   FALSE             <NA>    human
## OK, OK

at_OoC2[11, "text_all"]
at_OoC2[11, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[12, "text_all"]
at_OoC2[12, 3:6]
## 
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human?
## OK, CON

at_OoC2[13, "text_all"]
at_OoC2[13, 3:6]
##
## liver, NA, NA, NA
## liver   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[14, "text_all"]
at_OoC2[14, 3:6]
## 
## heart, NA, NA, human?
## heart   FALSE             <NA>    human
## OK, CON

at_OoC2[15, "text_all"]
at_OoC2[15, 3:6]
##
## pancreas, NA, NA, NA
## pancreas   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[16, "text_all"]
at_OoC2[16, 3:6]
##
## microvascular, NA, NA, NA
## microvascular   FALSE             <NA>     <NA>
## OK, OK

at_OoC2[17, "text_all"]
at_OoC2[17, 3:6]
## The paper just says "body-on-a-chip" without elaborating.
## NA, NA, NA, human
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC2[18, "text_all"]
at_OoC2[18, 3:6]
## The paper is on "thrombosis-on-a-chip".
## vascular, NA, NA, human
## vascular   FALSE             <NA>    human
## OK, OK

at_OoC2[19, "text_all"]
at_OoC2[19, 3:6]
##
## intestine, NA, NA, human
## intestine   FALSE             <NA>    human?
## OK, CON

at_OoC2[20, "text_all"]
at_OoC2[20, 3:6]
##
## sinus, NA, NA, human
## sinus   FALSE             <NA>    human?
## OK, CON

at_OoC2[21, "text_all"]
at_OoC2[21, 3:6]
##
## islet, NA, NA, mouse
## islet   FALSE             <NA>    mouse
## OK, OK

at_OoC2[22, "text_all"]
at_OoC2[22, 3:6]
##
## intestine, NA, NA, human
## intestine   FALSE             <NA>    human
## OK, OK

at_OoC2[23, "text_all"]
at_OoC2[23, 3:6]
##
## NA, NA, placenta, human
## unidentified   FALSE         placenta    human
## OK, OK

at_OoC2[24, "text_all"]
at_OoC2[24, 3:6]
##
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human
## OK, CON


## Summary
## 24/24, 18/24
## 24/24, 24/24





###
### Subset 3
###

### Setting random seed for the first subset of documents.
set.seed(101)

### Shuffling the row order.
nrow3 <- sample(nrow(accuracy_test_OoC))

at_OoC3 <- accuracy_test_OoC[nrow3, ]

at_OoC3[1, "text_all"]
at_OoC3[1, 3:6]
##
## breast, cancer, NA, NA
## unidentified   FALSE             <NA>    human
## NO, NO

at_OoC3[2, "text_all"]
at_OoC3[2, 3:6]
## They say "nervous system" on-a-chip and "nerve-on-a-chip.
## neural?, NA, NA, human
## nerve   FALSE             <NA>    human
## OK, OK

at_OoC3[3, "text_all"]
at_OoC3[3, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC3[4, "text_all"]
at_OoC3[4, 3:6]
##
## dermal, NA, NA, human?
## dermal   FALSE             <NA>    human
## OK, CON

at_OoC3[5, "text_all"]
at_OoC3[5, 3:6]
##
## heart, NA, NA, human?
## heart   FALSE             <NA>    human
## OK, CON

at_OoC3[6, "text_all"]
at_OoC3[6, 3:6]
##
## joint?, NA, NA, human and mouse?
## cartilage   FALSE             <NA> mixed (human and non-human)
## CON, CON

at_OoC3[7, "text_all"]
at_OoC3[7, 3:6]
##
## BBB (brain and microvascular), NA, NA, human
## multiple organs   FALSE             <NA>    human
## CON, OK

at_OoC3[8, "text_all"]
at_OoC3[8, 3:6]
##
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human
## OK, CON

at_OoC3[9, "text_all"]
at_OoC3[9, 3:6]
##
## heart, NA, NA, human
## heart   FALSE             <NA>    human
## OK, OK

at_OoC3[10, "text_all"]
at_OoC3[10, 3:6]
##
## heart, NA, NA, human
## heart   FALSE             <NA>    human
## OK, OK

at_OoC3[11, "text_all"]
at_OoC3[11, 3:6]
## 
## blood, NA, NA, NA
## blood   FALSE             <NA>     <NA>
## OK, OK

at_OoC3[12, "text_all"]
at_OoC3[12, 3:6]
##
## multple organoids, NA, NA, human
## unidentified   FALSE             <NA>    human
## NO, OK

at_OoC3[13, "text_all"]
at_OoC3[13, 3:6]
##
## NA, NA, NA, human?
## unidentified   FALSE             <NA>    human
## OK, CON

at_OoC3[14, "text_all"]
at_OoC3[14, 3:6]
##
## liver? NA, NA, human
## liver   FALSE             <NA>    human
## CON, OK

at_OoC3[15, "text_all"]
at_OoC3[15, 3:6]
##
## heart, NA, NA, human
## heart   FALSE             <NA>    human
## OK, OK

at_OoC3[16, "text_all"]
at_OoC3[16, 3:6]
##
## nerve, NA, NA, mouse
## nerve   FALSE             <NA>    mouse
## OK, OK

at_OoC3[17, "text_all"]
at_OoC3[17, 3:6]
## This is a review 
## multiple organs, NA, NA, human?
## unidentified   FALSE             <NA>    human
## NO, CON

at_OoC3[18, "text_all"]
at_OoC3[18, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK

at_OoC3[19, "text_all"]
at_OoC3[19, 3:6]
##
## NA, NA, NA, human
## unidentified   FALSE             <NA>    human
## OK, OK

at_OoC3[20, "text_all"]
at_OoC3[20, 3:6]
##
## blood vessel, NA, NA, human
## blood vessel   FALSE             <NA>    human
## OK, OK

at_OoC3[21, "text_all"]
at_OoC3[21, 3:6]
##
## blood and muscle?, NA, NA, NA
## blood   FALSE             <NA>     <NA>
## CON, OK

at_OoC3[22, "text_all"]
at_OoC3[22, 3:6]
##
## oviduct, NA, NA, dog
## oviduct   FALSE             <NA>      dog
## OK, OK

at_OoC3[23, "text_all"]
at_OoC3[23, 3:6]
##
## heart, NA, NA, NA
## heart   FALSE             <NA>     <NA>
## OK, OK

at_OoC3[24, "text_all"]
at_OoC3[24, 3:6]
##
## NA, NA, NA, NA
## unidentified   FALSE             <NA>     <NA>
## OK, OK


## Summary
## 17/24, 17/24
## 21/24, 23/24



### Calculating averages
###
### Without accepting conditional matches
###
### organ type
mean(c(23/24, 24/24, 17/24))
## [1] 0.8888889
sd(c(23/24, 24/24, 17/24))
## [1] 0.1577475

### organisms
mean(c(21/24, 18/24, 17/24))
## [1] 0.7777778
sd(c(21/24, 18/24, 17/24))
## [1] 0.08673608



### With accepting conditional matches
###
### organ type
mean(c(23/24, 24/24, 21/24))
## [1] 0.9444444
sd(c(23/24, 24/24, 21/24))
## [1] 0.06364688

### organisms
mean(c(23/24, 24/24, 23/24))
## [1] 0.9722222
sd(c(23/24, 24/24, 23/24))
## [1] 0.02405626





