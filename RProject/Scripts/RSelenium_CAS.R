# ------------------------------------- README ------------------------------- #

# This script shows the general workflow that was used to collect CAS numbers 
# from: https://commonchemistry.cas.org/
# The workflow described here depends on the commonchemistry webpage and may
# become outdated if the website's layout changes.
# More details on RSelenium can be found in the official vignette at:
# https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html

# ---------------------------------------------------------------------------- #


# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("RSelenium")
# install.packages("netstat")
# install.packages("robotstxt")

# Load relevant packages
library(tidyverse)
library(xlsx)
library(RSelenium)
library(netstat)
library(robotstxt)


# Vector with CAS numbers
example_names <- c(
  "Ibuprofen", 
  "Carbamazepine",
  "MCPA", 
  "Perfluorooctanesulfonicacid", # Example: Missing space for Perfluorooctanesulfonic acid.
  "Perfluorooctanesulfonic acid" 
)

# Check permissions
url <- "https://commonchemistry.cas.org/"
paths_allowed(url) # TRUE 

# Open client and webbrowser
rs_driver_object <- rsDriver(browser = "firefox",
                             verbose = FALSE,
                             port = free_port(),
                             chromever = NULL)

remDr <- rs_driver_object$client

# Generate an empty list to store results
numbers <- list() 

# Assign names to a new object. Required data type: Character
items <- example_names 


# for-loop to retrieve CAS numbers
for (i in items) {
  
  # Navigate to the website
  remDr$navigate("https://commonchemistry.cas.org/")
  
  # Random pause
  Sys.sleep(runif(1, 2, 3))
  
  # Identify the search field/box
  search_box <- remDr$findElement(using = 'class', 'search-input')
  
  # Send item to the search box and click enter
  search_box$sendKeysToElement(list(paste(i), key = 'enter'))
  
  # Random pause
  Sys.sleep(runif(1, 5, 8))
  
  # Identify results
  result <- try(remDr$findElement(using = "class", "result-content"))
  
  # If results were found:
  if (!inherits(result, "try-error")) {
    
    # Click results
    result$clickElement()
    
    # Random pause
    Sys.sleep(runif(1, 2, 3))
    
    # Identify CAS number
    cas <- remDr$findElements(using = 'class', 'cas-registry-number')
    
    # Extract CAS number
    cas_n <- lapply(cas, function(x) x$getElementText()) 
    
    # Write CAS number to numbers object
    numbers[[i]] <- unlist(cas_n)
    
  } else {
    
    # Assign NA if no results were found
    numbers[[i]] <- NA
  }
  
  # Random pause
  Sys.sleep(runif(1, 2, 3))
  
  # Output for the user to indicate current position or end of the loop
  print(paste("Index", which(i == items), "finished for item:", i))
  if (i == last(items)) {
    print("Loop finished!")
    remDr$navigate("https://commonchemistry.cas.org/")
  }
}

# Unlist the numbers object and transform it to a df
df_results <- as.data.frame((unlist(numbers))) %>% 
  rownames_to_column("Chemical") %>%
  rename(CAS = `(unlist(numbers))`) %>% 
  print()

# Write df_results to a .xlsx file if desired
# write.xlsx(as.data.frame(df_results), paste0("Output/YOUR_FILENAME_", Sys.Date(), ".xlsx"), row.names = F)

# Close the webbrowser
remDr$close()

# Close the client 
system("taskkill /im java.exe /f")
