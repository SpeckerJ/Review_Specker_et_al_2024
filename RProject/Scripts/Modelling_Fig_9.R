# ------------------------------- README ------------------------------------- #
#
# This script shows how to model the data for Fig. 9.
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("RColorBrewer")

# Load relevant packages
library(tidyverse)
library(RColorBrewer)

# Generate theoretical time window ####

# How many days are there in our respective time window?
# Define the start and end dates as vectors
start_dates <- as.Date(c(
  "2021-06-01",
  "2021-06-01",
  "2021-09-01",
  "2022-06-01",
  "2022-09-01",
  "2023-06-01"
))

end_dates <- as.Date(c(
  "2025-06-01",
  "2021-09-01",
  "2022-06-01",
  "2022-09-01",
  "2023-06-01",
  "2023-09-01"
))

# Calculate the difference in days between the start and end dates
days_differences <- as.numeric(end_dates - start_dates)

# Create a data frame to display the start date, end date, and the difference in days
result_table_dates <- data.frame(
  Start_Date = start_dates,
  End_Date = end_dates,
  Days_Difference = days_differences
)
result_table_dates

# Function to model increase/decrease assuming first-order kinetics ####

# Function to calculate expected mass loads under given input values
generate_data <- function(initial_value, increase, k_values, max_time) {
  all_data <- data.frame()

  for (k in k_values) {
    time <- 0:max_time

    concentration <- numeric(length(time))
    concentration[1] <- initial_value

    # Calculate concentration for each time point based on k
    for (i in 2:length(time)) { # first element is at time point 0. Start at time point 1 
      if (time[i] <= 92) { # First increase phase with simultaneous decrease
        concentration[i] <- concentration[i - 1] + increase # -1 to use concentration of time point 0
        concentration[i] <- concentration[i] * exp(-k) # Apply decrease on the updated concentration
        
      } else if (time[i] <= 92 + 273) { # First decrease phase
        concentration[i] <- concentration[i - 1] * exp(-k)
        
      } else if (time[i] <= 92 + 273 + 92) { # Second increase phase with simultaneous decrease
        concentration[i] <- concentration[i - 1] + increase
        concentration[i] <- concentration[i] * exp(-k) # Apply decrease on the updated concentration
        
      } else if (time[i] <= 92 + 273 + 92 + 273) { # Second decrease phase
        concentration[i] <- concentration[i - 1] * exp(-k)
        
      } else if (time[i] <= 92 + 273 + 92 + 273 + 92) { # Third increase phase
        concentration[i] <- concentration[i - 1] + increase
        concentration[i] <- concentration[i] * exp(-k) # Apply decrease on the updated concentration
        
      } else {
        concentration[i] <- concentration[i - 1] * exp(-k)
      }
    }

    # Store current iteration
    k_data <- data.frame(time, concentration, k = rep(k, length(time)))
    all_data <- rbind(all_data, k_data)
  }

  return(all_data)
}

# Generate input values #####

# Assume different k values with DT50 of 30, 60, 90 etc.
k_values <- c(
  log(2) / 30,
  log(2) / 60,
  log(2) / 90,
  log(2) / 120,
  log(2) / 200,
  log(2) / 2000,
  log(2) / 20000000000000000000000000000000000000000000000000000000000
)

# Generate df
# Median CEC is 17 ng/L of all concentrations
data <- generate_data(
  initial_value = 17,
  increase = 17,
  k = k_values,
  max_time = result_table_dates[1, 3]
)

# Add DT50 time and unit
data <- data %>%
  mutate(
    DT50 = log(2) / k,
    unit = "ng"
  ) %>%
  relocate(unit, .after = concentration)

# Reorder DT50
order <- c(
  unique(data$DT50)[7],
  unique(data$DT50)[6],
  unique(data$DT50)[5],
  unique(data$DT50)[4],
  unique(data$DT50)[3],
  unique(data$DT50)[2],
  unique(data$DT50)[1]
)
data$DT50 <- factor(data$DT50, levels = order)

# Max values for each year of reuse for each k value
Max_conc_first_year <- data %>%
  group_by(DT50) %>%
  filter(time <= 92) %>%
  slice_max(concentration) %>%
  select(concentration) %>%
  mutate(year = 1)
Max_conc_second_year <- data %>%
  group_by(DT50) %>%
  filter(between(time, 93, 457)) %>%
  slice_max(concentration) %>%
  select(concentration) %>%
  mutate(year = 2)
Max_conc_third_year <- data %>%
  group_by(DT50) %>%
  filter(time >= 457) %>%
  slice_max(concentration) %>%
  select(concentration) %>%
  distinct(concentration, .keep_all = T) %>%
  mutate(year = 3)

# Combine dfs
Max_conc_all_years <- rbind(Max_conc_first_year, Max_conc_second_year, Max_conc_third_year)

# Is the max concentration over the years de/increasing
Max_conc_all_years %>%
  group_by(DT50) %>%
  summarize(trend = ifelse(all(diff(concentration) < 0), "Decreasing",
    ifelse(all(diff(concentration) > 0), "Increasing", "Mixed")
  ))

# Plot ####
Sys.setlocale("LC_ALL", "English") # Set language to English for ggplot. Otherwise, months on the x-axis might be in the local system language

# Generate months & years. Starting date: June 2021
date_sequence <- seq(from = as.Date("2021-06-01"), to = as.Date("2025-06-01"), by = "day")
date_sequence <- rep(date_sequence, each = 7) # "each" refers here to number of k values
data <- data %>%
  arrange(time) %>%
  mutate(MY = date_sequence)

# Figure 6
ggplot(data, aes(x = MY, y = concentration, color = as.factor(DT50))) +
  geom_line(linewidth = 1.25) +
  theme_bw() +
  scale_color_brewer(
    labels = c(
      expression(paste("DT"[50], " = ", infinity)),
      expression(paste("DT"[50], " = ", 2000)),
      expression(paste("DT"[50], " = ", 200)),
      expression(paste("DT"[50], " = ", 120)),
      expression(paste("DT"[50], " = ", 90)),
      expression(paste("DT"[50], " = ", 60)),
      expression(paste("DT"[50], " = ", 30))
    ),
    type = "seq",
    palette = "Set2"
  ) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "3 month"
  ) +
  labs(
    x = "",
    y = "Mass (ng)",
    color = "Half-life in days "
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.2)
  )

