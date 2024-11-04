# ------------------------------ README ------------------------------------- #
#
# This scrip shows the general workflow used for the three classification
# approaches. As an illustrative example, the frequently detected chemicals 
# shown in the SI are used. Based on their CAS numbers, a chemical presence 
# in a list was checked to assign a class.
#
# --------------------------------------------------------------------------- #

# Install relevant packages if not installed

# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("readxl")
# install.packages("rvest")
# install.packages("janitor")

# Load relevant packages

library(tidyverse)
library(xlsx)
library(readxl)
library(rvest)
library(janitor)

# Read in example data ####
example_data <- read_excel("Data/Supplementary_Data_Resubmission.xlsx",
  sheet = 14
) %>% select(1, 2)


# Approach I) - Data driven #### 

# Retrieve data from the NORMAN Network

# Read in NORMAN webpage 
norman_url <- read_html('https://www.norman-network.net/?q=node/81')

# Locate tables
tables_all <- norman_url %>% html_elements(xpath = '//table') %>% html_table()

# Select table with CAS numbers and individual substances. Convert to df
norman_table <- tables_all[2] %>% as.data.frame()

# Change empty columns to true NAs. Otherwise NA will not work
norman_table <- norman_table %>% mutate(X1 = case_when(
  X1 == "" ~ NA,
  TRUE ~ as.character(X1)
))

# Fill the X1 column and adapt column names
df_norman <- norman_table %>% fill(X1, .direction = "down")
colnames(df_norman) <- as.character(unlist(df_norman[1,]))
df_norman <- df_norman[-1,] %>% rename(CAS = `CAS#`, Class_NORMAN = `Class/category I`)

# Combine tables
df_assigned_classes_NORMAN <- left_join(example_data, df_norman, by = "CAS")

# Update classification
df_assigned_classes_NORMAN <- df_assigned_classes_NORMAN %>% mutate(Class_updated = case_when(
  Class_NORMAN %in% c(
    "Perfluoroalkylated substances and their transformation products"
  ) ~ "PFAS",
  Class_NORMAN %in% c("Personal care products",
                      "Personal care products / Biocides",
                      "Personal care products / Food additives"
  ) ~ "Personal care product",
  Class_NORMAN %in% c("Pharmaceuticals") ~ "Pharmaceutical",
  Class_NORMAN %in% c("Disinfection by-products (drinking water)"
  ) ~ "TP",
  Class_NORMAN %in% c(
    "Industrial chemicals", "Industrial chemicals / Flame retardants", 
    "Flame retardants", "Plasticisers"
  ) ~ "Industrial chemical",
  Class_NORMAN %in% c(
    "PPP", "Plant protection products / Biocides", "Biocides",
    "Plant protection products"
  ) ~ "Plant protection product",
  Class_NORMAN %in% c("Drugs of abuse") ~ "Illicit drug",
  Class_NORMAN %in% c("Food additives") ~ "Sweetener",
  Class_NORMAN %in% c("Industrial chemicals / Biocides",
                      "Moth repellent / Antimicrobial agent"
  ) ~ "Ambiguous",
  Class_NORMAN %in% c("Other") ~ "TP", # This regards the cocain's metabolite cotinine
  TRUE ~ Class_NORMAN)
) %>% select(1,2,5)

# 99 compounds were classified
df_assigned_classes_NORMAN_present <- df_assigned_classes_NORMAN %>% filter(!is.na(Class_updated))  

# 76 compounds remain unclassified
df_assigned_classes_NA <- df_assigned_classes_NORMAN %>% filter(is.na(Class_updated)) 

# Unclassified compounds were classified manually using publicly available data-
# bases such as ECHA, NORMAN, PPDB, pubchem, or literature, and compiled in an excel file 
classification_manually <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 19)

# Combine tables
df_assigned_classes_previously_NA <- left_join(df_assigned_classes_NA, classification_manually, by = "CAS") %>% select(-4) %>% rename(Chemical = Chemical.x)

# Update classification
df_assigned_classes_previously_NA <- df_assigned_classes_previously_NA %>% mutate(Class_updated = Class) %>% select(1:3)

# Combine the two dfs that contain classification
df_classification <- rbind(df_assigned_classes_NORMAN_present, df_assigned_classes_previously_NA)

# Count each class
df_classification %>% group_by(Class_updated) %>% summarise(n = n()) %>% arrange(desc(n))


# Approach II) - SIN List #####
# The SIN list can be found under: https://sinlist.chemsec.org/

# Read in data
List_SIN_raw <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 20)

# Clean up SIN list, add column SIN_compound indicating if a compound is
# classified as a SIN compound (1) or not (0)
List_SIN <- clean_names(List_SIN_raw) %>% select(c(1,3,4,5)) %>% rename(CAS = cas_number) %>% mutate(SIN_compound = 1)

# Combine data frames
Classified_SIN <- left_join(example_data, List_SIN, by = "CAS")

# Clean up df, transform NAs to 0,
Classified_SIN <- Classified_SIN %>% select(1,2,6) %>% mutate(SIN_compound = ifelse(is.na(SIN_compound), 0, SIN_compound))

# How many compounds were classified based on the SIN list
Classified_SIN %>% reframe(n_SIN_compound = sum(SIN_compound),
                           n_SIN_percentage = sum(SIN_compound)/length(Classified_SIN$SIN_compound))

# Approach III) - van Dijk et al. (2021) doi: 10.1016/j.jenvman.2020.111692 ####

# Read in data from van Dijk et al. (2021)
List_van_Dijk <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 21)

# CLean names
List_van_Dijk <- clean_names(List_van_Dijk)

# Update df so that the presence in a framework is binary
# Present = 1, Absent = 0 
List_van_Dijk <- List_van_Dijk %>% rowwise() %>% 
  mutate(biocide_present = if_else(!is.na(biocide_pt),1,0),
         Human_medicines_present = if_else(!is.na(medicines_for_human_use_registration_type),1,0),
         pesticide_present = if_else(!is.na(pesticide_type), 1,0),
         industrial_present = if_else(!is.na(industrial_chemicals_tonnage_band),1,0),
         veterinary_present = if_else(!is.na(veterinary_medcines_registration_type),1,0)
  )

# Select newly generated columns
List_van_Dijk <- List_van_Dijk %>% select(c(1,2,9,10:13)) %>% 
  rowwise() %>% 
  mutate(n_frameworks = sum(c_across(3:7), na.rm = T))

# Combine data frames
classified_van_Dijk <- left_join(example_data, List_van_Dijk, by = c("CAS" = "cas"))

# How many compounds were classified based on van Dijk et al. (2021)
classified_van_Dijk %>% pivot_longer(cols = c(4:8), names_to = "framework", values_to = "values") %>% 
  group_by(framework) %>% summarise(n = sum(values, na.rm = T)) %>% arrange(desc(n))

# How many compounds are registered under multiple frameworks
classified_van_Dijk %>% filter(n_frameworks > 1) %>% nrow()
