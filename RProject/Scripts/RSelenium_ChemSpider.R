# ------------------------------- README ------------------------------------- #
#
# This script shows the general workflow that was used to collect data from
# https://www.chemspider.com/
# The workflow here described depends on the ChemSpider webpage and may 
# therefore be outdated in the future after the structure of the website has 
# changed. In fact, during the course of preparing the present review, 
# ChemSpider changed the structure of their website. Therefore, the old script
# which was compatible with the legacy version of ChemSpider, accessible under
# https://legacy.chemspider.com/, has been updated. However, errors in the
# script due to the updated structure of ChemSpider, cannot be ruled out.
#
# More details on RSelenium can be found in the official vignette at:
# https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
#
# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("RSelenium")
# install.packages("netstat")
# install.packages("robotstxt")
# install.packages("janitor")
# install.packages("xlsx")

# Load relevant packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(robotstxt)
library(janitor)
library(xlsx)


# Check permissions
url <- "https://www.chemspider.com/"  
paths_allowed(url)

# Open client and webbrowser
rs_driver_object <- rsDriver(browser = "firefox",
                             verbose = FALSE,
                             port = free_port(),
                             chromever = NULL)
remDr <- rs_driver_object$client

# Object containing inchikeys
items <- c("HEFNNWSXXWATRW-UHFFFAOYSA-N", # Ibuprofen
           "FFGPTBGBLSHEPO-UHFFFAOYSA-N", # Carbamazepine
           "WRONG_INCHI", # Example
           "WHKUVVPPKQRRBV-UHFFFAOYSA-N") # MCPA



# Initialize lists to store hits and misses separately
props_hits <- list()
props_misses <- list()

# Navigate to ChemSpider (handle cookies manually)
remDr$navigate("https://www.chemspider.com")  


# For-loop to extract data from chemspider
for (i in items) {
  # Locate and enter search term
  search_box_Ch <- remDr$findElement(using = "class", "input-left-icon")
  search_box_Ch$sendKeysToElement(list(paste(i), key = "enter"))
  Sys.sleep(runif(1, 4, 7))  # Pause for processing
  
  # Try to find and click properties tab
  properties <- try(remDr$findElement(using = "id", "tab1"))
  if (!inherits(properties, "try-error")) {
    properties$clickElement()
    Sys.sleep(runif(1, 4, 7))
    
    # Find and click predicted properties section
    predicted <- remDr$findElement(using = "id", "accordion-acd/labs-title")
    predicted$clickElement()
    Sys.sleep(runif(1, 4, 6))
    
    # Extract table headers (th) and data (td)
    headers_elements <- remDr$findElements(using = 'css selector', "tr[data-v-72ba9020] th")
    headers <- sapply(headers_elements, function(element) element$getElementText())
    
    data_elements <- remDr$findElements(using = 'css selector', "tr[data-v-72ba9020] td")
    data <- sapply(data_elements, function(element) element$getElementText())
    
    # Combine headers and data into a data frame and add to props_hits list
    table_data <- data.frame(Headers = unlist(headers), Data = unlist(data), Search = i)
    props_hits[[i]] <- table_data  # Store each table in props_hits list for each item
    
    Sys.sleep(runif(1, 1, 3))  # Additional pause
  } else {
    # If properties not found, assign NA with target headers as placeholders
    # Using generic headers like "Property1", "Property2" etc. if specific headers aren't known
    props_misses[[i]] <- data.frame(Headers = "NA", Data = NA, Search = i)
  }
  
  Sys.sleep(runif(1, 2, 3))  # Pause between items
  print(paste("Index:", which(i == items), "completed for item:", i))
  
  # Go back to ChemSpider homepage
  if (i == items[length(items)]) {
    print("Loop finished")
  }
  remDr$navigate("https://www.chemspider.com")
  Sys.sleep(runif(1, 5, 8))
}

# Relevent headers
target_headers <- props_hits$`HEFNNWSXXWATRW-UHFFFAOYSA-N`$Headers[c(12,16:18)]

# Generate final_df from props_hits
final_df_hits <- props_hits %>%
  map(~ .x %>%
        # Filter for desired headers
        
        filter(Headers %in% target_headers) %>% 
        # Transform to wide data
        
        pivot_wider(names_from = Headers, values_from = Data) %>%
        # Add search string (inchikey)
        
        mutate(Search = .x$Search[1])) %>%
  bind_rows()

# Create a placeholder NA df for missing or wrong items
final_df_misses <- bind_rows(props_misses) %>% select(3)

# Combine final_df_hits and final_df_misses
final_df <- bind_rows(final_df_hits, final_df_misses)

# Final combined data frame
final_df

# Write df_results to a .xlsx file if desired
# write.xlsx(as.data.frame(final_df), paste0("Output/YOUR_FILENAME_", Sys.Date(), ".xlsx"), row.names = F)

# Close the webbrowser
remDr$close()

# Close the client 
system("taskkill /im java.exe /f")
