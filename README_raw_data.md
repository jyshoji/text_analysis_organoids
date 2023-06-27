# Summary

We collected two sets of metadata of academic publications on organoids, organ-on-chip (abbreviated as either OoC or onchip), and microphysiological systems (abbreviated as MS or ms). The first set houses metadata of all academic publications, and was stored in the "raw_data2" folder (not published in the repository). The second set houses only a subset of metadata of academic publications that are under the Creative Commons Attritution Licenses, and was stored in the "raw_data3" folder. Due to the copyright issues, we only distribute metadata in the zipped "raw_data3". Please contact the lead contact, Stefan Krauss (s.j.k.krauss@medisin.uio.no) if you wish to run the full analysis on the all academic publications.

The metadata files in each folder of “raw_data2” and "raw_data3" include following changes compared to the original metadata files obtained from the literature databases. 

1. All occurrences of non-breaking space (typed as option + space) was replaced with standard space.
2. All occurrences of em-dash (shift + option + dash) and en-dash (option + dash) were replaced with standard hyphen.

These metadata files were imported into R using the code in *./R/formatting.R* and used for the analysis.


# General conditions for the literature search

1. Academic publications were collected from EMBASE, PubMed, Scopus, Web of Science, and bioRxiv.
3. The literature search was performed on 6 September, 2022.
3. Metadata of academic publications published between 2011 - present was collected. (For publication from bioRxiv, metadata were collected in February, 2022, and then metadata published after January, 2022 were recollected on 6 September).
4. Only academic publications in English were collected.
5. *research articles*, and *reviews* were collected separately, while excluding *comments*, *editorial*, *retracted publications*, and *retraction of publication* when possible.
6. However, research articles on organoid from PubMed and research articles on blastoids from EMBASE could not be imported to R for unknown reasons. Therefore, research articles and reviews were collectively imported, and review articles were later marked as such.
7. Publications from bioRxiv were defined as *preprints*.
8. Metadata of academic publications under Creative Commons Attribution Licenses was also collected from PubMed, by inserting **AND ("pmc cc license"[Filter])** at the end of each search string.
9. The literature corpus was updated upon revision of our submitted paper. For this, metadata of the academic publications was collected again on the 5th June, 2023, with the same search conditions, except that the date range was set to Jan-01-2022 to present. The collected metadata documents were stored in "raw_data4" folder. Duplicate publications (published between Jan-01-2022 and Sep-06-2022) were later removed on R. 

# Organoid literature

Query words included; organoid, enteroid, gastruloid, colonoid, assembloid, iblastoid, tumoroid, tumouroid, as well as their plural forms. The query word "blastoid" was considered only for publications published after 2017.

## EMBASE

1. Query words were searched in tw, kw
2. DID NOT map the term on Subject Headings
3. Selected the following fields for .ris export: ab, ad, au, do, ga, gi, ji, kw, lg, ti, yr
4. The file for research articles for blastoids was unable to be imported into R. Therefore instead, research articles and reviews were stored in a single file which was then imported into R.

### Research articles (organoids)

(organoid or organoids or enteroid or enteroids or gastruloid or gastruloids or colonoid or colonoids or assembloid or assembloids or iblastoid or iblastoids or tumoroid or tumoroids or tumouroid or tumouroids).kw,tw.

limit 1 to (english language and yr="2011 -Current")

limit 2 to article

### Reviews (organoids)

limit 2 to "review"

### Research articles + Reviews (blastoids)

(blastoid or blastoids).kw,tw.

(embyo or embryonic).kw,tw.

6 and 7

limit 8 to (english language and yr="2018 -Current")

limit 9 to (article or "review")

### Reviews (blastoids)

limit 9 to "review"


## PubMed

1. Query words were searched in *Text Word* field of the database.
2. Search builder was used.
3. For unknown reason, the metadata of research articles were unable to be imported into R. Therefore, metadata of both research articles and reviews were collectively imported instead, after which metadata of reviews was deduplicated.
4. Exported through "Save to", "Citation manager", "All results".

### Research articles + reviews (organoids)

((((organoid[Text Word] OR organoids[Text Word] OR enteroid[Text Word] OR enteroids[Text Word] OR gastruloid[Text Word] OR gastruloids[Text Word] OR colonoid[Text Word] OR colonoids[Text Word] OR assembloid[Text Word] OR assembloids[Text Word] OR iblastoid[Text Word] OR iblastoids[Text Word] OR tumoroid[Text Word] OR tumoroids[Text Word] OR tumouroid[Text Word] OR tumouroids[Text Word]) AND ("english"[Language])) AND (("2011/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("journal article"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])

### Reviews only (organoids)

((((organoid[Text Word] OR organoids[Text Word] OR enteroid[Text Word] OR enteroids[Text Word] OR gastruloid[Text Word] OR gastruloids[Text Word] OR colonoid[Text Word] OR colonoids[Text Word] OR assembloid[Text Word] OR assembloids[Text Word] OR iblastoid[Text Word] OR iblastoids[Text Word] OR tumoroid[Text Word] OR tumoroids[Text Word] OR tumouroid[Text Word] OR tumouroids[Text Word]) AND ("english"[Language])) AND (("2011/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("review"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])


### Research articles (blastoids)

(((((blastoid[Text Word] OR blastoids[Text Word]) AND (embryo[Text Word] OR embryonic[Text Word])) AND ("english"[Language])) AND (("2018/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("journal article"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "review"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])

### Reviews (blastoids)

(((((blastoid[Text Word] OR blastoids[Text Word]) AND (embryo[Text Word] OR embryonic[Text Word])) AND ("english"[Language])) AND (("2018/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("review"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])


## Scopus

1. Only up to 2,000 papers can be exported as a csv file from a search. Therefore, documents were filtered by year and country to make the number of hits to be smaller than 2,000.
2. Query words were searched in the *TITLE-ABS-KEY* field.
3. BibTeX export, including "Citation information", "Bibliographical information", "Abstract & keywords", and "Funding details".

### Research articles (organoids)

( TITLE-ABS-KEY ( organoid  OR  organoids  OR  enteroid  OR  enteroids  OR  gastruloid  OR  gastruloids  OR  colonoid  OR  colonoids  OR  assembloid  OR  assembloids  OR  iblastoid  OR  iblastoids  OR  tumoroid  OR  tumoroids  OR  tumouroid  OR  tumouroids )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" ) ) 

### Reviews (organoids)

( TITLE-ABS-KEY ( organoid  OR  organoids  OR  enteroid  OR  enteroids  OR  gastruloid  OR  gastruloids  OR  colonoid  OR  colonoids  OR  assembloid  OR  assembloids  OR  iblastoid  OR  iblastoids  OR  tumoroid  OR  tumoroids  OR  tumouroid  OR  tumouroids )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "re" ) ) 

### Research articles (blastoids)

( TITLE-ABS-KEY ( ( blastoid  OR  blastoids )  AND  ( embryo  OR  embryonic ) )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2017  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" ) ) 

### Reviews (blastoids)

( TITLE-ABS-KEY ( ( blastoid  OR  blastoids )  AND  ( embryo  OR  embryonic ) )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2017  AND  ( LIMIT-TO ( DOCTYPE ,  "re" ) ) 


## Web of Science

1. Query words were searched in “topic”.
2. Using only “Web of Science Core Collection”
3. "Full record" (without cited references) of the metadata was exported in a "BibTeX" format.

### Research articles (organoids)

((#1) AND DT=(Article)) NOT DT=(Review OR Editorial Material OR Retraction OR Retracted Publication)

where 1 is;

((TS=(organoid OR organoids OR enteroid OR enteroids OR gastruloid OR gastruloids OR colonoid OR colonoids OR assembloid OR assembloids OR iblastoid OR iblastoids OR tumoroid OR tumoroids OR tumouroid OR tumouroids)) AND LA=(English)) AND DOP=(2011-01-01/2023-12-31)

### Reviews (organoids)

((#1) AND DT=(Review)) NOT DT=(Retracted Publication OR Retraction)

### Research articles (blastoids)

((#1) AND DT=(Article)) NOT DT=(Review OR Editorial Material OR Retraction OR Retracted Publication)

Where 1 is;

((TS=((blastoid OR blastoids) AND (embryo OR embryonic))) AND LA=(English)) AND DOP=(2018-01-01/2023-12-31)

### Reviews (blastoids)

((#1) AND DT=(Review)) NOT DT=(Retracted Publication OR Retraction)



## bioRxiv

1. Searched both “bioRxiv” and “medRxiv” (NOT “bioRxiv and medRxiv”)
2. Advanced search -> Abstract or Title -> any (check box next to search box.)
3. As the search box had a stingy word limit, the search string was split into two.
4. Exported as BibTeX.

### Preprints - 1 (organoids)

for abstract or title "organoid organoids enteroid enteroids gastruloid gastruloids colonoid colonoids assembloid assembloids" (match any words) and posted between "01 Jan, 2011

### Preprints - 2 (organoids)

iblastoid iblastoids tumoroid tumoroids tumouroid tumouroids
for abstract or title "iblastoid iblastoids tumoroid tumoroids tumouroid tumouroids" (match any words) and posted between "01 Jan, 2011

### Preprints - blastoids

for abstract or title "blastoid blastoids" (match any words) and posted between "01 Jan, 2018






# organ-on-chip literature

Query words: either,
1. on-chip OR on-chips OR on-a-chip (PubMed style)
2. “on chip” OR “on chips” OR “on a chip” (Web of Science style)


## EMBASE

1. Query words were searched in tw, kw
2. DID NOT map the term on Subject Headings
3. Selected the following fields for .ris export: ab, ad, au, do, ga, gi, ji, kw, lg, ti, yr

### Research articles

(on chip or on chips or on a chip).kw,tw.

limit 1 to (english language and yr="2011 -Current")

limit 2 to article

### Reviews

limit 2 to "review"




## PubMed

1. Query words were searched in *Text Word* field of the database.
2. Search builder was used
3. “-“ between "on" and "chip" wasn’t really necessary, as phrase search is automatically performed when search field is specified.
4. Exported through "Save to", "Citation manager", "All results".


### Research articles 

((((on-chip[Text Word] OR on-chips[Text Word] OR on-a-chip[Text Word]) AND ("english"[Language])) AND (("2011/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("journal article"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "review"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])

### Reviews 

((((on-chip[Text Word] OR on-chips[Text Word] OR on-a-chip[Text Word]) AND ("english"[Language])) AND (("2011/01/01"[Date - Publication] : "3000"[Date - Publication]))) AND ("review"[Publication Type])) NOT ("comment"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])


## Scopus

1. Only up to 2,000 papers can be exported as csv from a search. Therefore, documents were filtered by year and country to make the number of hits to be smaller than 2,000.
2. Query words were searched in the *TITLE-ABS-KEY* field.
3. BibTeX export, including "Citation information", "Bibliographical information", "Abstract & keywords", and "Funding details".

### Research articles

( TITLE-ABS-KEY ( "on chip"  OR  "on chips"  OR  "on a chip" )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" ) ) 

### Reviews

( TITLE-ABS-KEY ( "on chip"  OR  "on chips"  OR  "on a chip" )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "re" ) ) 


## Web of Science

1. Query words were searched in “topic”.
2. Using only “Web of Science Core Collection”
3. "Full record" (without cited references) of the metadata was exported in a "BibTeX" format.

### Research articles

((#1) AND DT=(Article)) NOT DT=(Review OR Editorial Material OR Retraction OR Retracted Publication)

where 1 is:

((TS=("on chip" OR "on chips" OR "on a chip")) AND LA=(English)) AND DOP=(2011-01-01/2023-12-31)

### Reviews

((#1) AND DT=(Review)) NOT DT=(Retracted Publication OR Retraction)



## bioRxiv

1. Searched both “bioRxiv” and “medRxiv” (NOT “bioRxiv and medRxiv”)
2. Advanced search -> Abstract and Title -> phrase (check box next to search box.)
3. The search box doesn’t accept Boolean operators. For this reason, “on-chip”, “on-chips” and “on-a-chip” were searched for separately.
4. Apparently, the search is special character insensitive,
as on-chip and on chip (phrase) gave the same results.
5. Exported as BibTeX.

### Preprints

for abstract or title "on-chip" (match phrase words) and posted between “01 Jan, 2011 and"

for abstract or title "on-chips" (match phrase words) and posted between “01 Jan, 2011 and"

for abstract or title "on-a-chip" (match phrase words) and posted between “01 Jan, 2011 and"





# Microphysiological systems (ms)

Query words (PubMed stye)

("microphysiological system*"[Text Word]) OR ("microphysiology system*"[Text Word]) OR ("microphysiologic system*"[Text Word]) OR ("micro physiological system*"[Text Word]) OR ("micro physiology system*"[Text Word]) OR ("micro physiologic system*"[Text Word])

Query words (Web of Science Style)

"microphysiolog* system*” OR “micro physiolog* system*”


## EMBASE

1. Query words were searched in tw, kw
2. DID NOT map the term on Subject Headings
3. Selected the following fields for .ris export: ab, ad, au, do, ga, gi, ji, kw, lg, ti, yr

### Research articles

(microphysiolog* system* or micro physiolog* system*).kw,tw.

limit 1 to (english language and yr="2011 -Current")

limit 2 to article

### Reviews

limit 2 to "review"

## PubMed

1. Query words were searched in *Text Word* field of the database.
2. Search builder was used. 
3. Exported through "Save to", "Citation manager", "All results".

### Research articles

((("microphysiological system*"[Text Word] OR "microphysiology system*"[Text Word] OR "microphysiologic system*"[Text Word] OR "micro physiological system*"[Text Word]) AND "english"[Language] AND 2011/01/01:3000/12/31[Date - Publication]) AND ("journal article"[Publication Type])) NOT ("comment"[Publication Type] OR "editorial"[Publication Type] OR "review"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])

### Reviews

((("microphysiological system*"[Text Word] OR "microphysiology system*"[Text Word] OR "microphysiologic system*"[Text Word] OR "micro physiological system*"[Text Word]) AND "english"[Language] AND 2011/01/01:3000/12/31[Date - Publication]) AND ("review"[Publication Type])) NOT ("comment"[Publication Type] OR "retracted publication"[Publication Type] OR "retraction of publication"[Publication Type])


## Scopus

1. Only up to 2,000 papers can be exported as csv from a search. Therefore, documents were filtered by year to make the number of hits to be smaller than 2,000.
2. Query words were searched in the *TITLE-ABS-KEY* field.
3. BibTeX export, including "Citation information", "Bibliographical information", "Abstract & keywords", and "Funding details".

### Research articles

( TITLE-ABS-KEY ( "microphysiolog* system*"  OR  "micro physiolog* system*" )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" ) ) 

### Reviews

( TITLE-ABS-KEY ( "microphysiolog* system*"  OR  "micro physiolog* system*" )  AND  LANGUAGE ( english ) )  AND  PUBYEAR  >  2010  AND  ( LIMIT-TO ( DOCTYPE ,  "re" ) ) 


## Web of Science

1. Query words were searched in “topic”.
2. Using only “Web of Science Core Collection”
3. "Full record" (without cited references) of the metadata was exported in a "BibTeX" format.

### Research articles

((#1) AND DT=(Article)) NOT DT=(Review OR Editorial Material OR Retraction OR Retracted Publication)
where 1 is
((TS=("microphysiolog* system*” OR “micro physiolog* system*”)) AND LA=(English)) AND DOP=(2011-01-01/2023-12-31)

### Reviews

((#1) AND DT=(Review)) NOT DT=(Retracted Publication OR Retraction)


## bioRxiv

1. Searched both “BioRxiv” and “MedRxiv” (NOT “bioRxiv and Medrxiv”)
2. Advanced search -> Abstract and Title -> phrase 
3. The search box doesn’t accept Boolean operators. For this reason, each phrase was searched for separately, one by one.

