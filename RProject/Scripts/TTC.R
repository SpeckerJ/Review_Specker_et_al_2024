# ------------------------------- README ------------------------------------- #
#
# This script shows the workflow using the Threshold of Toxicological Concern
# (TTC) as approach to assess potential human health risks.
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("xlsx")
# install.packages("janitor")

# Load relevant packages
library(tidyverse)
library(readxl)
library(janitor)

# Read in raw data ####

# Data for which removal was reported or could be calculated
raw_data_removal <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 3)

# Data for which only effluent/final concentrations were reported. 
raw_data_concentration <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 4)

# Toxtree output
raw_data_toxtree <- read.csv("Data/Toxtree_results_Extension_Cramer.csv") # Read in using read.csv
raw_data_toxtree <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 22) # OR read in using read_excel

# # Assign to a working df
df_removal <- raw_data_removal
df_concentration <- raw_data_concentration
df_toxtree <- raw_data_toxtree


# Generate df with TTC values ####
TTC_values <- data.frame(
  Class = c(1, 2, 3),
  DW_Target = c(37.3, 4.5, 4.0),
  TTC_Unit = c("µg/L", "µg/L", "µg/L"),
  Source = c("Baken_2018", "Dieter_2014", "Baken_2018")
)

# Data wrangling ####

# Toxtree_output_extension
df_toxtree <- df_toxtree %>% rename(CanonicalSMILES = with.extensions)
df_toxtree <- df_toxtree %>%
  mutate(across(where(is.character), ~ na_if(.,"")))
df_toxtree <- df_toxtree %>% filter(!is.na(CanonicalSMILES))
df_toxtree <- df_toxtree %>% select(c("Cramer.rules", "CanonicalSMILES"))

# Combine removal and concentration data
df_removal <- df_removal %>% select(c(6, 11,15,18,19)) %>% rename(Concentration = Concentration_Eff,
                                                               Unit = Concentration_Eff_Unit)
df_concentration <- df_concentration %>% select(c(6,11,15,16,17)) 
df_TTC <- rbind(df_removal, df_concentration) %>% mutate(rowid = row_number())

# Combine with df_toxtree to assign Cramer classes
df_TTC <- left_join(df_TTC, df_toxtree, by = "CanonicalSMILES") %>% distinct(rowid, .keep_all = T)

# Add column that shows the Cramer Class
df_TTC <- df_TTC %>% 
  mutate(Class = case_when(
    str_detect(Cramer.rules, "Class III") ~ 3,
    str_detect(Cramer.rules, "Class II") ~ 2,
    str_detect(Cramer.rules, "Class I") ~ 1,
    TRUE ~ NA_real_
  ))

# Add TTC values, relocate Class, and clean up names
df_TTC <- left_join(df_TTC, TTC_values, by = "Class")

# Change DW_Target from µg/L to ng/L
df_TTC <- df_TTC %>% mutate(DW_Target = DW_Target * 1000,
                            TTC_Unit = "ng/L")

# Data Analysis ####

# Calculate RQ_TTC
df_TTC_RQ <- df_TTC %>% mutate(TTC_RQ = Concentration/DW_Target)

# Calculate median, means etc
df_TTC_RQ <- df_TTC_RQ %>% 
  group_by(CanonicalSMILES) %>% 
  mutate(mean_TTC_RQ = mean(TTC_RQ, na.rm = T),
         median_TTC_RQ = median(TTC_RQ, na.rm = T),
         min_TTC_RQ = min(TTC_RQ, na.rm = F),
         max_TTC_RQ = max(TTC_RQ, na.rm = F),
         Count_1 = sum(TTC_RQ > 1, na.rm = T),
         Percentage_1 = Count_1 / n() * 100,
         n = n())

# Which compounds have TTC_RQ over 1?
df_TTC_RQ %>% filter(TTC_RQ > 1) %>%
  select(Chemical, TTC_RQ, Class, DW_Target, Count_1, n, Percentage_1) %>% 
  arrange(desc(TTC_RQ)) %>% 
  distinct(CanonicalSMILES, .keep_all = T) %>% 
  print(n = 22)

# Which compounds have TTC_RQ over 1 and were measured during potable reuse
df_TTC_RQ %>% filter(TTC_RQ > 1 & Reuse_Application_General == "Potable") %>%
  select(Chemical, TTC_RQ, Class, DW_Target, Count_1, n, Percentage_1) %>% 
  arrange(desc(TTC_RQ)) %>% 
  distinct(CanonicalSMILES, .keep_all = T) %>% 
  print(n = 22)

