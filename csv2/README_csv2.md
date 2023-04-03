# Summary

This folder contains .csv files that can be readily used to reproduce the analysis without manual adjustments of the data. Many of the .csv files are identical to the files under the same name in the *./csv/* folder, except the below. 

# Files that are different from those in the *./csv/* folder

## README_csv2.md

This file.

## duplicates_ooc_checked.csv

The file does not have any rows. The file is supposed to be used in *./R/formatting.R* to remove duplicate publications from the organ-on-a-chip corpus by matching unique identification numbers which are assigned to documents; however, using a different corpus as an input will remove wrong publications as the unique identification numbers are assigned to different publications. For this reason, the file has no documents such that the code accepts the file but does not remove any publications.

## duplicates_or_checked.csv

Same as above, but for the organoid corpus.

## minor_organisms_F.csv

The file is supposed to be used in *./R/organisms_F.R* to correct organism classifications by matching unique identification number of documents. As this will match wrong publications when the input corpus is different, the file contains no rows such that the code accepts the file but does not correct classifications.

## rare_organ_papers_checked.csv
The file is supposed to be used in *./R/organ_types.R* to correct organ classifications. The file contains no rows such that the code accepts the file but does not correct classifications.



