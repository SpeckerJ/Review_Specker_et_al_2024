# ------------------------------- README ------------------------------------- #
#
# This script shows how to generate Fig. 4.
# Fig. 4 is showing reported CEC removal rates and final CEC concentration
# using the reported limit of detection/quantification as conservative value for
# reported non-detects.
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("scales")
# install.packages("patchwork")
# install.packages("RColorBrewer")

# Load relevant packages
library(tidyverse)
library(readxl)
library(scales) 
library(patchwork)
library(RColorBrewer)

# Read in raw data
raw_data <- read_excel("Data/Supplementary_Data.xlsx", sheet = 3)

# Assign raw data to a new df for data wrangling
df <- raw_data

# ---------------------------------------------------------------------------- #
# Data wrangling ####

# Add a "treatment" that contains all data and combine with initial df
df_all <- df %>% mutate(Additional_Treatment = "All Data")
df <- rbind(df, df_all)

# Add a column with number of observations per treatment
df <- df %>% group_by(Additional_Treatment) %>% mutate(n = sum(!is.na(Concentration_Eff)))

# Generate a df that contains the number of observations (or counts)
# per treatment. Used for plotting
df_counts <- df %>%
  group_by(Additional_Treatment) %>%
  summarise(n = sum(!is.na(Concentration_Eff))) %>%
  arrange(desc(n))

# Adjust effluent concentration for compounds where max LOD was used to LOD/2
df <- df %>%
  mutate(Concentration_Eff_Adjusted = case_when(
    Set_t_LOD == 1 ~ Concentration_Eff / 2,
    Set_t_LOQ == 1 ~ Concentration_Eff / 2,
    TRUE ~ Concentration_Eff
  )) %>%
  relocate(Concentration_Eff_Adjusted, .after = Concentration_Eff)

# Calculate removal with adjusted concentrations
df <- df %>%
  mutate(
    Removal = 
      100 - (Concentration_Eff / Concentration_Inf) * 100,
    Removal_Adjusted =
      100 - (Concentration_Eff_Adjusted / Concentration_Inf) * 100
  ) %>%
  relocate(Removal_Adjusted, .after = Removal)

# Calculate median removal per treatment. Used for plotting
df_medians <- df %>%
  group_by(Additional_Treatment) %>%
  summarise(median_rem = median(Removal, na.rm = TRUE)) %>% 
  arrange(desc(median_rem))



# ---------------------------------------------------------------------------- #
# Plot - Figure 4  ####


# Generate subfigure A)
subfigure_a <- df %>% group_by(Additional_Treatment) %>% 
  ggplot(aes(
    y = reorder(Additional_Treatment, -Removal, na.rm = T, FUN = median),
    x = Removal,
  )) +
  geom_boxplot() +
  xlab("CEC Removal (%)\n ") +
  ylab("") +
  ggtitle("A)") +
  scale_x_continuous(limits = c(-100, 100)) +
  coord_cartesian(xlim = c(-100, 100)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )
subfigure_a

# Generate subfigure B)
subfigure_b <- df %>% group_by(Additional_Treatment) %>% 
  group_by(Additional_Treatment) %>%
  ggplot(aes(
    y = reorder(Additional_Treatment, -Removal, na.rm = T, FUN = median),
    x = Concentration_Eff,
  )) +
  geom_boxplot() +
  geom_text(data = df_counts, aes(y = Additional_Treatment, x = 80000, label = n), colour = "black") +
  xlab("CEC Concentration Effluent (ng/L)\n log scale") +
  ylab("") +
  ggtitle("B)") +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  theme_bw() +
  geom_vline(xintercept = 100, linetype = "dashed", color = "#FC8D62", size = 0.75) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
subfigure_b

# Plot side by side
combined_figure <- subfigure_a + subfigure_b &
  theme(plot.margin = margin(0, 0, 0, 0)) # Adjust overall plot margins
combined_figure

# Save file if desired.

# ggsave(filename = paste0("Output/YOUR_FILENAME_", Sys.Date(), ".pdf"),
#        plot = combined_figure,
#        device = cairo_pdf,
#        width = 9,
#        height = 6.5,
#        dpi = 300,
#        units = "in"
#        )


