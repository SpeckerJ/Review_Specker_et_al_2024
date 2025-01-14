# ------------------------------- README ------------------------------------- #
#
# This script shows the workflow used to assess environmental and
# human health risks and the plot used to generate Figures 5,6, S2, and S3.
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")
# install.packages("patchwork")
# install.packages("scales")
# install.packages("RColorBrewer")

# Load relevant packages
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
library(scales)
library(RColorBrewer)

# Read in raw data ####

# Data for which removal was reported or could be calculated
raw_data_removal <- read_excel("Data/Supplementary_Data.xlsx", sheet = 3)

# Data for which only effluent/final concentrations were reported. 
raw_data_concentration <- read_excel("Data/Supplementary_Data.xlsx", sheet = 4)

# Data from the NORMAN database
raw_data_NORMAN <- read_csv("Data/NORMAN_database_2024-02-16.csv")

# Chemicals for which (preliminary) guideline values (pgv) could be retrieved
raw_data_pgv <- read_excel("Data/Supplementary_Data.xlsx", sheet = 5)

# --------------------------------------------------------------------------- #

# Assign to a working df, clean up and select headers
df_removal <- raw_data_removal %>% clean_names() %>% select(c(1,11,12,18,19,21,22))
df_concentration <- raw_data_concentration %>% clean_names()
df_NORMAN <- raw_data_NORMAN %>% clean_names(replace=janitor:::mu_to_u) %>% select(c(2:4)) 
df_pgv <- raw_data_pgv %>% clean_names()

# Data Wrangling - NORMAN database ####

# Remove "CAS_RN: "
df_NORMAN$cas_no <- gsub("CAS_RN: ", "", df_NORMAN$cas_no)
df_NORMAN$cas_no <- gsub("CAS_RN:", "", df_NORMAN$cas_no)

# Change data types to numeric
df_NORMAN$lowest_pnec_freshwater_ug_l <- as.numeric(df_NORMAN$lowest_pnec_freshwater_ug_l)

# Data wrangling - Removal data ####

# Update concentration column
df_removal <- df_removal %>%
  
  # Calculate adjusted concentrations with LOD/2
  mutate(concentration_eff_adjusted = case_when(
    set_t_lod == "1" ~ concentration_eff / 2,
    set_t_loq == "1" ~ concentration_eff / 2,
    set_t_lod == "0" ~ concentration_eff,
    set_t_loq == "0" ~ concentration_eff,
    TRUE ~ concentration_eff
  )) %>%
  
  relocate(concentration_eff_adjusted, .after = concentration_eff) %>%
  
  # Rename columns so that dfs can be combined later
  rename(
    concentration = concentration_eff, concentration_adjusted = concentration_eff_adjusted,
    conc_unit = concentration_eff_unit
  ) %>%
  
  # Combine set_t_lod and set_t_loq to one column so that dfs can be combined later
  mutate(set_t_lod = case_when(
    set_t_lod == 1 | set_t_loq == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  select(-set_t_loq)

# Data Wrangling - Concentration data ####

# Update concentration column
df_concentration <- df_concentration %>% mutate(concentration_adjusted = case_when(
  set_t_lod == "1" ~ concentration/2,
  set_t_lod == "0" ~ concentration,
  TRUE ~ concentration
)) %>% rename(conc_unit = unit)


# Filter NORMAN database ####

# Generate an object to select the NORMAN data for chemicals that are present
# in the current data set and which have a cas number for filtering NORMANs df.
selected_analytes1 <- df_removal %>% filter(!is.na(cas)) %>% pull(cas) 
selected_analytes2 <- df_concentration %>% filter(!is.na(cas)) %>% pull(cas) 
selected_analytes <- c(selected_analytes1, selected_analytes2) %>% as.data.frame()
selected_analytes <- selected_analytes %>% rename(cas = ".") %>% distinct(cas)

# # Filter NORMAN data for the selected analytes
NORMAN_filtered <- df_NORMAN %>% filter(cas_no %in% selected_analytes$cas)

# Remove Toluensulphonamide. No freshwater PNEC reported
NORMAN_filtered <- NORMAN_filtered %>% filter(substance != 'Toluenesulphonamide')

# Combine now  df_removal and df_concentration with NORMAN data
removal_norman <- left_join(df_removal, NORMAN_filtered, by = c("cas" = "cas_no"))
concentration_norman <- left_join(df_concentration, NORMAN_filtered, by = c("cas" = "cas_no"))

# Define columns to select for and check if they are identical
desired_columns <- c("cas", "chemical", "conc_unit", "concentration", "concentration_adjusted", "lowest_pnec_freshwater_ug_l", "paper")

# Select columns
removal_norman <- removal_norman %>% select(all_of(desired_columns))
concentration_norman <- concentration_norman %>% select(all_of(desired_columns))
identical(names(removal_norman), names(concentration_norman)) # TRUE

# Combine treatment and occurence df
data_norman <- rbind(removal_norman, concentration_norman)

# Update Metropolol's PNEC manually due to outdated CAS
data_norman <- data_norman %>% mutate(lowest_pnec_freshwater_ug_l = ifelse(cas == "51384-51-1", 8.6, lowest_pnec_freshwater_ug_l))

data_norman %>% filter(!is.na(cas)) %>% distinct(cas) %>% nrow() # 493
data_norman %>% filter(is.na(cas)) %>% nrow() # 129

# Data wrangling - (Preliminary) Guideline Values ####

# Combine pgv with removal and concentration df
removal_pgv <- left_join(df_removal, df_pgv, by = "cas")
concentration_pgv <- left_join(df_concentration, df_pgv, by = "cas")

# Filter pgv values ####

# Define columns to select for and check if they are identical
desired_columns <- c("cas", "chemical.x", "conc_unit", "concentration", "concentration_adjusted", "paper", "unit", "value")

# Select columns
removal_pgv <- removal_pgv %>% select(all_of(desired_columns))
concentration_pgv <- concentration_pgv %>% select(all_of(desired_columns))
identical(names(removal_pgv), names(concentration_pgv)) # TRUE

# Combine both dfs, clean up columns, and filter NA value
data_pgv <- rbind(removal_pgv, concentration_pgv) %>%
  relocate(chemical.x, .before = cas) %>%
  relocate(conc_unit, .after = concentration_adjusted) %>%
  relocate(paper, .before = chemical.x) %>%
  relocate(unit, .after = value) %>% 
  rename(chemical = chemical.x,
         unit_gv = unit,
         gv_value = value) %>% 
  #filter(!is.na(p_gv)) %>% 
  mutate(gv_value = ifelse(cas == "NA", NA, gv_value))

# Number of available pgvs
data_pgv %>% filter(!is.na(gv_value)) %>% distinct(cas) %>% nrow() # 167
data_pgv %>% filter(!is.na(cas)) %>% distinct(cas) %>% nrow() # 493
data_pgv %>% filter(is.na(cas)) %>% distinct(chemical) %>%  nrow() # 111
round(167/(493+111), digits = 2)

# Calculate RQs - Ecological Risks #####

# Calculate HQ
data_norman <- data_norman %>%
  mutate(
    HQ = (concentration / 1000) / lowest_pnec_freshwater_ug_l, # /1000 as pnec is in µgl
    HQ_DF10 = (concentration / 10000) / lowest_pnec_freshwater_ug_l, # Convert from ng to µg and include DF = 10
    HQ_adjusted = concentration_adjusted / 1000 / lowest_pnec_freshwater_ug_l,
    HQ_DF10_adjusted = (concentration_adjusted / 10000) / lowest_pnec_freshwater_ug_l
  )

# Calculate RQ median
data_norman <- data_norman %>%
  filter(concentration > 0) %>%
  group_by(cas) %>%
  mutate(
    median_HQ = median(HQ, na.rm = T),
    median_HQ_adjusted = median(HQ_adjusted, na.rm = T),
    median_HQ_DF10 = median(HQ_DF10, na.rm = T),
    median_HQ_DF10_adjusted = median(HQ_DF10_adjusted, na.rm = T),
    min_HQ = min(HQ, na.rm = F),
    max_HQ = max(HQ, na.rm = F),
    Count_1 = sum(HQ > 1, na.rm = T),
    Percentage_1 = Count_1 / n() * 100,
    n = n()
  ) %>%
  mutate(n = ifelse(cas == "NA", NA, n)) %>% 
  arrange(desc(median_HQ)) 

# Plots - Ecological Risk Assessment ####

# Plot manuscript ----------------------------------------------------------- #
data_norman %>%
  group_by(chemical) %>%
  filter(n > 10, HQ != 0, !is.na(HQ), median_HQ > 0.1) %>%
  mutate(chemical = case_when(
    chemical == "Chlorpyriphos" ~ "Chlorpyrifos",
    chemical == "Ciprofloxacine" ~ "Ciprofloxacin",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    chemical == "Perfluorooctanoic Acid" ~ "PFOA",
    TRUE ~ chemical
  )) %>%
  ggplot(aes(y = reorder(chemical, median_HQ), x = HQ)) +
  geom_boxplot(width = 0.5) +
  geom_text(aes(y = chemical, x = 1100, label = n), colour = "black", size = 3.4) +
  geom_vline(xintercept = 1, colour = "red", linetype = "longdash") +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Ecological Risks\nResulting From Water Reuse") +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = c(10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12)
  )

# With dilution ---------------------------------------------------------- #
# Note: The resulting plot in RStudio will look distorted. Use Zoom to look
# at the resulting plot. Outliers look smaller than they actually are 
# after exporting them as a .pdf

my_colors <- brewer.pal(8, "Set2")
data_norman %>%
  group_by(chemical) %>%
  filter(n > 10, HQ != 0, !is.na(HQ), median_HQ > 0.1, HQ_DF10 != 0, !is.na(HQ_DF10)) %>%
  mutate(chemical = case_when(
    chemical == "Chlorpyriphos" ~ "Chlorpyrifos",
    chemical == "Ciprofloxacine" ~ "Ciprofloxacin",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    chemical == "Perfluorooctanoic Acid" ~ "PFOA",
    TRUE ~ chemical
  )) %>%
  pivot_longer(
    cols = c(HQ, HQ_DF10),
    names_to = "HQ_type",
    values_to = "HQ_value"
  ) %>%
  mutate(HQ_type = factor(HQ_type, levels = c("HQ_DF10", "HQ"))) %>%
  ggplot(aes(y = reorder(chemical, median_HQ), x = HQ_value, fill = HQ_type, linetype = HQ_type)) +
  geom_boxplot(aes(color = HQ_type),
    width = 0.75, position = position_dodge(width = 0.9), linewidth = 0.4, outlier.size = 0.75,
    outlier.stroke = 0.5
  ) +
  geom_text(aes(y = chemical, x = 1000, label = n), colour = "black", size = 5) +
  geom_vline(xintercept = 1, colour = "red", linetype = "longdash") +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(0.00001, 0.0001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = c(10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Ecological Risks\nResulting From Water Reuse") +

  # Customize  colours, linetypes, and labels
  scale_fill_manual(
    values = c("HQ_DF10" = my_colors[1], "HQ" = my_colors[2]), 
    labels = c(expression(RQ["DF = 10"]), expression(RQ["DF = 0"])), 
    name = "HQ Type" 
  ) +
  scale_color_manual(
    values = c("HQ_DF10" = "black", "HQ" = "black"), 
    labels = c(expression(RQ["DF = 10"]), expression(RQ["DF = 0"])), 
    name = "HQ Type" 
  ) +
  scale_linetype_manual(
    values = c("HQ_DF10" = "longdash", "HQ" = "solid"), 
    labels = c(expression(RQ["DF = 10"]), expression(RQ["DF = 0"])),
    name = "HQ Type" 
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.size = unit(0.75, "cm"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    title = element_text(size = 16)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE),
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

# ggsave(filename=paste0("Output/YOUR_FILENAME", Sys.Date(), ".pdf"),
#        plot = last_plot(),
#        device = cairo_pdf,
#        width = 9,
#        height = 7.5,
#        dpi = 300,
#        units = "in")

# With adjusted concentrations ----------------------------------------------- #
data_norman %>%
  group_by(chemical) %>%
  filter(n > 10, HQ != 0, !is.na(HQ), median_HQ > 0.1, HQ_DF10 != 0, !is.na(HQ_DF10)) %>%
  mutate(chemical = case_when(
    chemical == "Chlorpyriphos" ~ "Chlorpyrifos",
    chemical == "Ciprofloxacine" ~ "Ciprofloxacin",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    chemical == "Perfluorooctanoic Acid" ~ "PFOA",
    TRUE ~ chemical
  )) %>%
  pivot_longer(
    cols = c(HQ, HQ_adjusted),
    names_to = "HQ_type",
    values_to = "HQ_value"
  ) %>%
  mutate(HQ_type = factor(HQ_type, levels = c("HQ_adjusted", "HQ"))) %>%
  ggplot(aes(y = reorder(chemical, median_HQ), x = HQ_value, fill = HQ_type, linetype = HQ_type)) +
  geom_boxplot(width = 0.5, aes(color = HQ_type)) +
  geom_text(aes(y = chemical, x = 1100, label = n), colour = "black", size = 3.4) +
  geom_vline(xintercept = 1, colour = "red", linetype = "longdash") +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = c(10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Ecological Risks\nResulting From Water Reuse") +
  
  # Customize colors, linetypes, and labels
  scale_fill_manual(
    values = c("HQ_adjusted" = my_colors[1], "HQ" = my_colors[2]), 
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])), 
    name = "HQ Type" 
  ) +
  scale_color_manual(
    values = c("HQ_adjusted" = "black", "HQ" = "black"), 
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])), 
    name = "HQ Type" 
  ) +
  scale_linetype_manual(
    values = c("HQ_adjusted" = "longdash", "HQ" = "solid"), 
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])), 
    name = "HQ Type" 
  ) +
  
  theme_bw() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE),
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

# With adjusted concentrations and dilution --------------------------------- #
data_norman %>%
  group_by(chemical) %>%
  filter(n > 10, HQ != 0, !is.na(HQ), median_HQ > 0.1, HQ_DF10 != 0, !is.na(HQ_DF10)) %>%
  mutate(chemical = case_when(
    chemical == "Chlorpyriphos" ~ "Chlorpyrifos",
    chemical == "Ciprofloxacine" ~ "Ciprofloxacin",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    TRUE ~ chemical
  )) %>%
  pivot_longer(
    cols = c(HQ_DF10_adjusted, HQ_adjusted),
    names_to = "HQ_type",
    values_to = "HQ_value"
  ) %>%
  mutate(HQ_type = factor(HQ_type, levels = c("HQ_DF10_adjusted", "HQ_adjusted"))) %>% # Change the order of levels
  
  ggplot(aes(y = reorder(chemical, median_HQ), x = HQ_value, fill = HQ_type, linetype = HQ_type)) +
  geom_boxplot(width = 0.5, aes(color = HQ_type)) +
  geom_text(aes(y = chemical, x = 1100, label = n), colour = "black", size = 3.4) +
  geom_vline(xintercept = 1, colour = "red", linetype = "longdash") +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = c(10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Ecological Risks\nResulting From Water Reuse") +
  
  # Customize colors, linetypes, and labels
  scale_fill_manual(
    values = c("HQ_DF10_adjusted" = my_colors[1], "HQ_adjusted" = my_colors[2]), 
    labels = c(expression(RQ["LOD/2 + DF10"]), expression(RQ["LOD/2"])), 
    name = "HQ Type" 
  ) +
  scale_color_manual(
    values = c("HQ_DF10_adjusted" = "black", "HQ_adjusted" = "black"), 
    labels = c(expression(RQ["LOD/2 + DF10"]), expression(RQ["LOD/2"])), 
    name = "HQ Type" 
  ) +
  scale_linetype_manual(
    values = c("HQ_DF10_adjusted" = "longdash", "HQ_adjusted" = "solid"), 
    labels = c(expression(RQ["LOD/2 + DF10"]), expression(RQ["LOD/2"])), 
    name = "HQ Type" 
  ) +
  
  theme_bw() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE),
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

# With (un)adjusted concentrations and dilution ------------------------------ #
# See Figure S3
data_norman %>%
  group_by(chemical) %>%
  filter(n > 10, HQ != 0, !is.na(HQ), median_HQ > 0.1, HQ_DF10 != 0, !is.na(HQ_DF10)) %>%
  mutate(chemical = case_when(
    chemical == "Chlorpyriphos" ~ "Chlorpyrifos",
    chemical == "Ciprofloxacine" ~ "Ciprofloxacin",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    TRUE ~ chemical
  )) %>%
  pivot_longer(
    cols = c(HQ_DF10_adjusted, HQ_adjusted, HQ, HQ_DF10),
    names_to = "HQ_type",
    values_to = "HQ_value"
  ) %>%
  mutate(HQ_type = factor(HQ_type, levels = c("HQ_DF10_adjusted", "HQ_DF10", "HQ_adjusted", "HQ"))) %>% 
  
  ggplot(aes(y = reorder(chemical, median_HQ), x = HQ_value, fill = HQ_type)) +
  geom_boxplot(width = 0.725) +
  geom_text(aes(y = chemical, x = 1100, label = n), colour = "black", size = 5) +
  geom_vline(xintercept = 1, colour = "red") +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = c(10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Ecological Risks\nResulting From Water Reuse") +
  
  # Customizing fill colors and legend labels
  scale_fill_manual(
    values = my_colors,
    name = "HQ Type",
    labels = c(
      "HQ_DF10_adjusted" = expression(RQ["DF10, LOD/2"]),
      "HQ_DF10" = expression(RQ["DF10"]),
      "HQ_adjusted" = expression(RQ["LOD/2"]),
      "HQ" = "RQ"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.size = unit(0.75, 'cm'),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    title = element_text(size = 14)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE),
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

# ggsave(filename=paste0("Output/YOUR_FILENAME", Sys.Date(), ".jpg"),
#        plot = last_plot(), # Ensure that the desired plot is displayed 
#        width = 10,
#        height = 12,
#        dpi = 300,
#        units = "in")


# Calculate RQs - Human Health Risks ####

# Calculate RQ
data_pgv <- data_pgv %>%
  filter(concentration > 0) %>%
  group_by(cas) %>%
  mutate(
  RQ = concentration / gv_value,
  RQ_adjusted = concentration_adjusted / gv_value) %>% 
  group_by(cas) %>% mutate(
    median_RQ = median(RQ, na.rm = T),
    median_RQ_adjusted = median(RQ_adjusted, na.rm = T),
    min_RQ = min(RQ, na.rm = F),
    max_RQ = max(RQ, na.rm = F),
    Count_1 = sum(RQ > 1, na.rm = T),
    Percentage_1 = Count_1 / n() * 100,
    n = n()) %>%
  mutate(n = ifelse(cas == "NA", NA, n)) %>% 
  arrange(desc(median_RQ))

# Plots = Human Health Risk Assessment ####

# Plot manuscript ------------------------------------------------------------ #
# See Figure 6
data_pgv %>% 
  filter(n > 10, RQ != 0, !is.na(RQ), max_RQ > 1) %>% 
  mutate(chemical = case_when(
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    chemical == "Perfluorobutanesulfonic acid" ~ "PFBS",
    chemical == "Perfluorohexanesulfonic acid" ~ "PFHxS",
    chemical == "Perfluorononanoic acid" ~ "PFNA",
    chemical == "(2,4-Dichlorophenoxy)acetic acid" ~ "2,4-D",
    chemical == "4-nonylphenol" ~ "4-Nonylphenol",
    chemical == "Nonylphenol" ~ "4-Nonylphenol",
    chemical == "Beta-estradiol" ~ "Estradiol",
    TRUE ~ chemical)
  )  %>% 
  ggplot(aes(y = reorder(chemical, median_RQ), x = RQ#,
  )) +
  geom_boxplot() +
  geom_text(aes(y = chemical, x = 200, label = n), colour = "black", size = 5) + 
  coord_trans(x = "log10") +
  scale_x_continuous(breaks = c(1e-4, 1e-3, 1e-2, 1e-1, 1,10,100),
                     minor_breaks = c(100,10,0.00001, 0.000001,0.0000001,0.00000001),
                     labels = trans_format("log10", math_format(10^.x))
  ) +
  geom_vline(
    xintercept = 1,
    colour = "red",
    linetype = "longdash"
  ) +
  xlab("Risk Quotient") +
  ylab("") + 
  ggtitle("Human Health Risks\nResulting From Potable Reuse") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size=16),
    legend.key.size = unit(0.75, 'cm'),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    title = element_text(size = 16)
  )

# Save plot if desired -------- #

# ggsave(
#   filename = paste0("Output/YOUR_FILENAME", Sys.Date(), ".pdf"),
#   plot = last_plot(), # Ensure that the desired plot is displayed
#   device = cairo_pdf,
#   width = 9,
#   height = 7.5,
#   dpi = 300,
#   units = "in"
# )

# Plot with adjusted concentrations ----------------------------------------- #
# See Figure S2 
my_colors <- brewer.pal(8, "Set2")
data_pgv %>%
  filter(n > 10, RQ != 0, !is.na(RQ), max_RQ > 1) %>%
  mutate(chemical = case_when(
    chemical == "(2,4-Dichlorophenoxy)acetic acid" ~ "2,4-D",
    chemical == "Perfluorooctanesulfonic acid" ~ "PFOS",
    chemical == "4-nonylphenol" ~ "4-Nonylphenol",
    chemical == "Nonylphenol" ~ "4-Nonylphenol",
    chemical == "Beta-estradiol" ~ "Estradiol",
    chemical == "Perfluorooctanoic acid" ~ "PFOA",
    chemical == "Perfluorobutanesulfonic acid" ~ "PFBS",
    chemical == "Perfluorohexanesulfonic acid" ~ "PFHxS",
    chemical == "Perfluorononanoic acid" ~ "PFNA",
    TRUE ~ chemical
  )) %>%
  pivot_longer(
    cols = c(RQ, RQ_adjusted), names_to = "RQ_type",
    values_to = "RQ_value"
  ) %>%
  mutate(RQ_type = factor(RQ_type, levels = c("RQ_adjusted", "RQ"))) %>%
  ggplot(aes(y = reorder(chemical, median_RQ), x = RQ_value, fill = RQ_type, colour = RQ_type, linetype = RQ_type)) +
  geom_boxplot(aes(color = RQ_type), width = 0.75, position = position_dodge(width = 0.9), linewidth = 0.4, outlier.size = 0.75,
               outlier.stroke = 0.5) +
  geom_text(aes(y = chemical, x = 200, label = n), colour = "black", size = 5) +
  coord_trans(x = "log10") +
  scale_x_continuous(
    breaks = c(1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100),
    minor_breaks = c(100, 10, 0.00001, 0.000001, 0.0000001, 0.00000001),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  geom_vline(
    xintercept = 1,
    colour = "red",
    linetype = "longdash"
  ) +
  xlab("Risk Quotient") +
  ylab("") +
  ggtitle("Human Health Risks\nResulting From Potable Reuse") +
  
  # Customize fill colors, line types, and legend labels
  scale_fill_manual(
    values = c("RQ_adjusted" = my_colors[1], "RQ" = my_colors[2]), 
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])), 
    name = "RQ Type" 
  ) +
  scale_color_manual(
    values = c("RQ_adjusted" = "black", "RQ" = "black"), 
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])), 
    name = "RQ Type" 
  ) +
  scale_linetype_manual(
    values = c("RQ_adjusted" = "longdash", "RQ" = "solid"),
    labels = c(expression(RQ["LOD/2"]), expression(RQ["LOD"])),
    name = "RQ Type" 
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=16),
    legend.key.size = unit(0.75, 'cm'),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    title = element_text(size = 16)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE),
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

# Save plot if desired -------- #

# ggsave(
#   filename = paste0("Output/YOUR_FILENAME_", Sys.Date(), ".jpg"),
#   plot = last_plot(), # Ensure that the desired plot is displayed
#   width = 9,
#   height = 7.5,
#   dpi = 300,
#   units = "in"
# )
