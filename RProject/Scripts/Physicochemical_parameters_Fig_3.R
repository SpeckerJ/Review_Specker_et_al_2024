# ------------------------------- README ------------------------------------- #
#
# This script shows how to generate Figure 3 showing selected physicochemical
# parameters.
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("cowplot")
# install.packages("forcats")
# install.packages("RColorBrewer")

# Load relevant packages
library(tidyverse)
library(readxl)
library(cowplot)
library(forcats)
library(RColorBrewer)

# Read in raw data
raw_data_props_Freq <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 15)
raw_data_props_all <- read_excel("Data/Supplementary_Data_Resubmission.xlsx", sheet = 9)

# Select relevant columns, convert to numeric, assign a set column
df_Freq <- raw_data_props_Freq %>% select(all_of(c(9:13))) %>%
  mutate(across(everything(), as.numeric)) %>% mutate(set = "Freq")

df_All <- raw_data_props_all %>% select(all_of(c(6,10,12:14))) %>% 
  mutate(across(everything(), as.numeric)) %>% mutate(set = "All")

# Combine df_Freq and df_All into one df_combined
df_combined <- rbind(df_Freq, df_All)

# Define columns and specify which need log transformation
columns <- names(df_Freq[c(1:5)]) # Define columns and specify which need log transformation
log_transform_columns <- names(df_Freq[c(4,5)])


# Group by 'set' and calculate means/medians etc. for each group
results <- df_combined %>%
  group_by(set) %>%
  group_modify(~ {
    map_dfr(columns, function(column_name) {
      data <- .x[[column_name]]
      
      # Check if log transformation is needed
      if (column_name %in% log_transform_columns) {
        data <- log10(data)
        column_name <- paste0("log10_", column_name)
      }
      
      # Return summary for each column
      c(
        Column = column_name,
        Mean = round(mean(data, na.rm = TRUE), 1),
        Median = round(median(data, na.rm = TRUE), 1),
        Max = round(max(data, na.rm = TRUE), 1),
        Min = round(min(data, na.rm = TRUE), 1),
        n = sum(!is.na(data))
      )
    })
  })
results

# Creater colours
my_colors <- brewer.pal(3, "Set2")

# Segments is for the two lines for mobile and very mobile
segments <- data.frame(value = c(3,4), boxplot.nr = c(4,4),
                       Group = c("vm", "m"), color = c("#E41A1C", "black"), linetype = c("solid", "longdash"))


# Create topfigure
values_order  <- c("ACD_BCF_pH_7.4", "ACD_LogD_pH_7.4", "XLogP", "ACD_KOC_pH_7.4")
topfigure <- df_combined %>% pivot_longer(cols = c(2:5), names_to = "variable", values_to = "value") %>% 
  mutate(value = 
           ifelse(variable %in% c("ACD_BCF_pH_7.4", "ACD_KOC_pH_7.4"), log10(value), value)
  ) %>%
  mutate(variable = factor(variable, levels = c(values_order))) %>% 
  ggplot(aes(y = variable, x = value, fill = set, linetype = set)) + 
  geom_boxplot() +
  xlab("(dimensionless)") +
  ylab("") +
  
  # Add a second x axis
  scale_x_continuous(sec.axis = trans ~.*1) + 
  
  # Rename labels
  scale_y_discrete(labels = c(
    "XLogP" = "Log Kow",
    "ACD_LogD_pH_7.4" = "Log D \n(pH = 7.4)",
    "ACD_KOC_pH_7.4" = "Log Koc \n(pH = 7.4)",
    "ACD_BCF_pH_7.4" = "Log BCF  \n(pH = 7.4)"
  )) +
  
  # Add lines to indicate mobility
  geom_segment(data = segments,
               aes(y = boxplot.nr - 0.385, yend = boxplot.nr + 0.4,
                   x = value, xend = value,
                   color = Group,
                   linetype = Group), linewidth = 1, inherit.aes = FALSE) +
  
  # Change boxlot colour, outline, and legend labels
  scale_color_manual(
    values = c("All" = my_colors[1], "Freq" = my_colors[2], "m" = "#E41A1C", "vm" = "black"),
    labels = c("All reported compounds", "Frequently detected compounds"),
    name = "Set"
  ) +
  scale_linetype_manual(
    values = c("All" = "solid", "Freq" = "longdash", "m" = "solid", "vm" = "longdash"),
    labels = c("All reported compounds", "Frequently detected compounds"),
    name = "Set"
  ) +
  scale_fill_manual(
    values = c("All" = my_colors[1], "Freq" = my_colors[2]),
    labels = c("All reported compounds", "Frequently detected compounds"),
    name = "Set"
  ) +
  
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
  )
topfigure

# Create bottomfigure
bottomfigure <- df_combined %>% pivot_longer(cols = 1, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(y = variable, x = value, fill = set, linetype = set)) + 
  geom_boxplot() +
  xlab("(g/mol)") +
  ylab("") +
  
  # Rename label
  scale_y_discrete(labels = c(
    "MolecularWeight" = "Mol. Weight"
    )) +
  
  scale_fill_manual(values = c("All" = my_colors[1], "Freq" = my_colors[2]), 
                    labels = c("All reported compounds", "Frequently detected compounds"),
                    
                    name = "Set"
  ) +
  scale_linetype_manual(values = c("solid", "longdash"),
                        labels = c("All reported compounds", "Frequently detected compounds"),
                        name = "Set"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(0.75, 'cm')
  ) +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE),
         linetype = guide_legend(reverse = TRUE)
  )
bottomfigure
plot_grid(topfigure, bottomfigure, nrow = 2, align = "v", rel_heights = c(3/5, 2/5))


# Save plot if desired -------- #

# ggsave(
#   filename = paste0("Output/YOUR_FILENAME_", Sys.Date(), ".pdf"),
#   plot = last_plot(), # Ensure that the desired plot is displayed
#   device = cairo_pdf,
#   width = 16,
#   height = 24,
#   units = "cm",
#   dpi = 300
# )
