### This R script file is for identifying and quantifying the occurrence of country names in the address field of the corpus.
###
### The files uses "./R_results/all_corpus" as an input.
###
### The code includes following steps;
### 1. Listing names of countries.
### 2. Restyling the address field.
### 3. Identifying countries involved in research
### 
### The outcome of the file is saved as:
### ~/Research_data/Hybrida/final_analysis/R_results/countries_F
### And fed to **purpose.R** and **main_figures.R**

### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file
load(paste0(root_path, "R_results/all_corpus"))



##########
###
### 1. Listing names of countries.
###
########## 


### Making a vector containing names of almost all countries/areas.
all_country_w <- c("Palestine", "Pakistan", "Kuwait", "Somalia", "Iraq", 
               "Saudi Arabia", "Syria", "Israel", "Iran", "Jordan", 
               "United Arab Emirates", "Lebanon", "Ethiopia", "Bahrain", 
               "Afghanistan", "Djibouti", "Yemen", "Sudan", "Tunisia", 
               "Algeria", "Mauritania", "Oman", "Spain", "Portugal", 
               "Norway", "Netherlands", "Denmark", "Germany", "Switzerland", 
               "Estonia", "Belgium", "Finland", "Hungary", "Nigeria", 
               "Brazil", "Paraguay", "Russia", "Niger", "Turkey", 
               "Peru", "Italy", "Kiribati", "Bolivia", "Georgia_c", "Japan", 
               "Mexico", "Kazakhstan", "France", "Ireland", "Canada", 
               "Chad", "Colombia", "Ivory Coast", "UK", "United Kingdom", "England", 
               "Scotland", "Wales", "Northern Ireland", 
               "Indonesia", "Czech Republic", "Bangladesh", "USA", "United States", "United States of America", 
               "Egypt", "India", "Benin", "Cameroon", "Ghana", "Jamaica", "Armenia", 
               "Cuba", "Haiti", "Romania", "Austria", "Qatar", "Philippines", "Gambia", 
               "Luxembourg", "El Salvador", "Venezuela", "Guatemala", "Uruguay", "Cyprus", 
               "China", "Honduras", "Nicaragua", "Panama", "Equatorial Guinea", "Mayotte", 
               "San Marino", "Serbia", "Montenegro", "Greece", "Croatia", "Eritrea", 
               "Australia", "South Africa", "Uganda", "Tajikistan", 
               "Marshall Islands", "South Korea", "Samoa", "Morocco", "Azerbaijan", 
               "Maldives", "Dominican Republic", "Kenya", "New Zealand", "Bulgaria", 
               "East Timor", "Latvia", "Palau", "Papua New Guinea", "Libya", "Slovenia", 
               "Kyrgyzstan", "Turkmenistan", "Sweden", "Congo Democratic Republic", 
               "Lithuania", "Iceland", "Uzbekistan", "Myanmar", "Costa Rica", "Ecuador", 
               "Bahamas", "Suriname", "Mauritius", "Ukraine", "Poland", "Sierra Leone", 
               "Central African Republic", "Antigua and Barbuda", "Niue", "Malaysia", 
               "Argentina", "Mongolia", "Nepal", "Sri Lanka", "Madagascar", "Togo", 
               "Thailand", "Cook Islands", "North Korea", "Chile", "Andorra", "Moldova", 
               "Mozambique", "Guyana", "Saint Lucia", "Netherlands", "Comoros", 
               "North Macedonia", "Namibia", "Mali", "Trinidad and Tobago", "Botswana", "Tanzania", 
               "Tuvalu", "Belarus", "Cape Verde", "Tokelau", "Dominica", "Malta", 
               "Solomon Islands", "Bhutan", "Fiji", "Vietnam", "Guinea-Bissau", "Albania", 
               "Senegal", "Malawi", "Liechtenstein", "Laos", "Brunei", "Burkina Faso", 
               "Bosnia and Herzegovina", "Zimbabwe", "Cambodia", "Slovakia", 
               "Saint Kitts and Nevis", "Barbados", "Belize", "Angola", "Liberia", 
               "Guinea", "Swaziland", "Saint Vincent and The Grenadines", "Gabon", 
               "Congo", "Western Sahara", "Burundi", "Rwanda", "Lesotho", "Isle of Man", 
               "Zambia", "Taiwan", "Micronesia", "Monaco", "Grenada", "Tonga", 
               "Vanuatu", "Sao Tome and Principe", "Aruba", "Singapore", "Vatican City", 
               "Seychelles", "Nauru")
### Note that Georgia is written as "Georgia_c" instead, in order to distinguish it from Georgia as the USA state..
### The vector doesn't include "Jersey", and other territories.

### Making a vector containing names and abbreviations of USA states.
US_states_w <- c("Alabama", "AL", "Alaska", "AK", "Arizona", 
               "AZ", "Arkansas", "AR", "California", "CA", "Colorado", "CO", 
               "Connecticut", "CT", "Delaware", "DE", 
               "Florida", "FL", "Georgia", "GA", "Hawaii", "HI", 
               "Idaho", "ID", "Illinois", "IL", "Indiana", "IN", "Iowa", "IA", 
               "Kansas", "KS", "Kentucky", "KY", "Louisiana", "LA",  "Maine", "ME", 
               "Maryland", "MD", "Massachusetts", "MA", "Michigan", "MI", "Minnesota", 
               "MN", "Mississippi", "MS", "Missouri", "MO", "Montana", "MT", "Nebraska", 
               "NE", "Nevada", "NV", "New Hampshire", "NH", "New Jersey", "NJ", 
               "New Mexico", "NM",  "New York", "NY", "North Carolina", "NC", 
               "North Dakota", "ND", "Ohio", "OH", "Oklahoma", "OK", "Oregon", 
               "OR", "Pennsylvania", "PA", "Rhode Island", "RI", "South Carolina", 
               "SC", "South Dakota", "SD", "Tennessee", "TN",  "Texas", "TX", 
               "Utah", "UT", "Vermont", "VT", "Virginia", "VA", "Washington", "WA", 
               "West Virginia", "WV", "Wisconsin", "WI", "Wyoming", "WY", "Puerto Rico")
### Including "Puerto Rico".
### Note that "New Mexico" has to be dealt with before capturing "Mexico".

### A few things to note are:
###
### 1) In almost all documents (except ~200 documents, plus preprints that lack the address field), 
### country names are followed by either ., ;, or $ (i.e., end of the field) when they appear as country names.
### For example, "Netherlands" will match "The Royal Institute of the Netherlands", but "Netherlands[.;]" and "Netherlands$" will only 
### match the Netherlands when it appears as a country name.
### 2) Republic of China and People's Republic of China have to be distinguished.
### 3) Some papers by South Korean authors simply say "Korea". No paper from North Korea.
### 4) So far, all papers by Georgian Scientists seem to be from Tbilisi, written as either 
### A. Tbilisi, 0160, Georgia; B. Tbilisi, 0160, Georgia$, C. Tbilisi, 0141, Georgia, and D. Tbilisi, and Georgia
### 5) Papers from Web of Science always contain the country name "USA" when authors are from there, whereas "USA" is often omitted in 
### papers from other databases, such that an address ends with a state name (or a zip code.)





##########
###
### 2. Restyling the address field.
###
##########

### Adjusting the address field and then capturing country names.
###
### Making the "address2" column, where modified address is stored.
### "People's Republic of China" is changed to "China"
### "Republic of China" is changed to "Taiwan"
### "Northern Ireland" is changed to "UK"
### Georgia as a country name is changed to "Georgia_c"
### "Korea" is changed to "South Korea".
### For papers not from Web of Science, state names are converted to "USA"
###
### Then, extracting country names followed by either . or $ from the address field.
###
### Also, papers from Web of Science have the address of the corresponding author as a duplicate at the start of the address field, 
### which is removed below.
extract_countries <- all_corpus %>% 
  ### Making a new column to store the modified address.
  ### hanging country names to homogenize the style.
  mutate(address2 = gsub("People's Republic of China[.;]|People's Republic of China$", "China.", address)) %>% 
  mutate(address2 = gsub("Republic of China\\)?[.;]|Republic of China\\)?$", "Taiwan.", address2)) %>% 
  mutate(address2 = gsub("Northern Ireland[.;]|Northern Ireland$", "UK.", address2)) %>% 
  mutate(address2 = gsub("Russian Federation[.;]|Russian Federation$", "Russia.", address2)) %>% 
  mutate(address2 = gsub(
    "Tbilisi, Georgia[.;]|Tbilisi, Georgia$|Tbilisi, [0-9]{4}, Georgia[.;]|Tbilisi, [0-9]{4}, Georgia$|Tbilisi, and Georgia[.;]|Tbilisi, and Georgia$", 
    "Georgia_c.", address2)) %>% 
  mutate(address2 = gsub("Korea[.;]|Korea$", "South Korea.", address2)) %>% 
  ### Changing state names of the USA to "USA."
  mutate(address2 = 
           ifelse(database == "Web of Science", address2, 
                  gsub(paste0("\\b", US_states_w, "[.;]", collapse = "|"), "USA.", address2))) %>% 
  mutate(address2 = 
           ifelse(database == "Web of Science", address2, 
                  gsub(paste0("\\b", US_states_w, "$", collapse = "|"), "USA.", address2))) %>% 
  mutate(address2 = 
           ifelse(database == "Web of Science", address2, 
                  gsub(paste0("\\b", US_states_w, " [0-9]{5}[.;]", collapse = "|"), "USA.", address2))) %>%   
  ### Capturing all country names that appear in the above vector of country names, and storing them in two columns.
  mutate(country_names1 = 
           apply(
             str_extract_all(
               address2, pattern = paste0("\\b", all_country_w, "[.;]", collapse = "|"), simplify = TRUE), 
             1, paste, collapse = " ")) %>% 
  mutate(country_names2 = 
           apply(
             str_extract_all(
               address2, pattern = paste0("\\b", all_country_w, "$", collapse = "|"), simplify = TRUE), 
             1, paste, collapse = " ")) %>%  
  ### Combining the two columns that store the country names.
  mutate(country_names_all = paste(country_names1, country_names2, sep = " ")) %>% 
  ### Adjusting the writing styles.
  mutate(country_names_all = gsub("\\.|;", "", country_names_all)) %>% 
  mutate(country_names_all = gsub("United States of America", "USA", country_names_all)) %>% 
  mutate(country_names_all = gsub("United States", "USA", country_names_all)) %>% 
  mutate(country_names_all = gsub("United Kingdom|England|Wales|Scotland", "UK", country_names_all)) %>% 
  ### In documents from Web of Science, the first address is the corresponding author's address, which appears again 
  ### later as a standard author.
  ### This makes the address duplicate, so the first address is removed.
  mutate(country_names_all = 
           ifelse(database == "Web of Science", str_replace(country_names_all, "[A-Za-z]* ", ""),  
                  country_names_all))




##########
###
### 3. Identifying countries involved in research
###
##########

### Making a vector of country names that were detected in the address field of the corpus.
involved_countries <- unique(unlist(str_extract_all(extract_countries$country_names_all, pattern = paste(all_country_w, collapse = "|"))))

### Counting the number of occurrence of each country name
country_count <- as.data.frame(sapply(involved_countries, function(x) str_count(extract_countries$country_names_all, x))) %>% 
  ### Ordering the columns alphabetically
  select(order(colnames(.))) %>% 
  ### Changing the column names so that they end with "_c"
  rename_with(~ gsub(" ", "_", .)) %>%  
  rename_with(~ gsub("$", "_c", .)) %>% 
  rename(Georgia_c = Georgia_c_c)

### Calculating the fractional count (i.e., proportion) of each country's contribution
country_proportion <- data.frame(country_count / rowSums(country_count)) %>% 
  ### Changing the column names so that they end with "_p".
  rename_with(~ gsub("_c", "_p", .))

### Combining the corpus data frame with data frames of country counts and proportions
country_combined <- extract_countries %>% 
  cbind(., country_count) %>% 
  cbind(country_proportion)

colnames(country_combined)

### Selecting "first_country" which is the first country name in the address field, 
### which normally corresponds to the address of the first author. 
### Selecting "main_country" which is the most frequently occurring country name.
### If more than one country have the same number of occurrence, "first_country" is used for "main_country" instead.
countries_F <- country_combined %>% 
  mutate(first_country = str_extract(country_names_all, paste(all_country_w, collapse = "|"))) %>% 
  mutate(first_country = gsub("Georgia_c", "Georgia", first_country)) %>%  
  ### Selecting the most frequently occurring country name.
  ### If more than one country have the same occurrence, the country name with earlier appearance is selected.
  mutate(main_country_F = ifelse(rowSums(country_count) == 0, NA, 
                                 colnames(country_count)[max.col(country_count, ties.method = "first")])) %>% 
  ### Same as above, except that the country name with later appearance is selected.
  mutate(main_country_L = ifelse(rowSums(country_count) == 0, NA, 
                                 colnames(country_count)[max.col(country_count, ties.method = "last")])) %>%  
  ### Selecting the "main_country".
  ### If the above two columns have the identical country name, then the corresponding country name is selected.
  ### If not, "first_country" is selected.
  mutate(main_country = ifelse(rowSums(country_count) == 0, NA, 
                               ifelse(main_country_F == main_country_L, main_country_F, first_country))) %>% 
  ### Adjusting the writing style.
  mutate(main_country = gsub("_c", "", main_country)) %>% 
  mutate(main_country = gsub("_", " ", main_country)) %>% 
  ### Removing the unnecessary columns.
  select(!c(main_country_F, main_country_L))

### Saving the result.
save(countries_F, file = paste0(root_path, "R_results/countries_F"))

### Loading an R data file that shows which publications are on organoids/OoCs, rather than on tumor organoids/ToCs.
load(paste0(root_path, "R_results/organ_types_P"))

### Removing text fields.
### Also removing tumor organoids and ToC corpora.
countries_P <- countries_F %>% 
   filter(ID %in% organ_types_P$ID) %>%  
  select(!c(7:25))

save(countries_P, file = paste0(root_path, "R_results/countries_P"))
