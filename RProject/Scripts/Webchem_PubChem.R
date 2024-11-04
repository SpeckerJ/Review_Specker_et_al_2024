# --------------------------------- README ----------------------------------- #

# This script provides a minimal working examples to illustrate the workflow 
# used to retrieve data from PubChem:
# Workflow follows the webchem vignette which can be accessed under:
# https://cran.r-project.org/web/packages/webchem/vignettes/webchem.html

# ---------------------------------------------------------------------------- #

# Install relevant packages if not installed
# install.packages("tidyverse")
# install.packages("webchem")
# install.packages("xlsx")


# Load relevant packages
library(tidyverse)
library(webchem)
library(xlsx)


# Vector with CAS numbers
example_df <- data.frame(
  Chemical = c(
    "Ibuprofen",
    "Carbamazepine",
    "MCPA",
    "Perfluorooctanesulfonic acid"
  ),
  CAS = c(
    "15687-27-1",
    "298-46-4",
    "94-74-6",
    "1763-23-2" # Wrong CAS for Perfluorooctanesulfonic acid. Correct one is: 1763-23-1 
  )
)

# Assign example_df to a working df
df <- example_df

# ---------------------------------------------------------------------------- #
# Note: At the time of writing, the Chemical Translation Service (CTS)
# (http://cts.fiehnlab.ucdavis.edu/) was down (Error 500). Therefore,
# cts_convert() does not work until the issue has been resolved. Use the  
# manually generated df instead. Once CTS works again, the present R script
# will be updated accordingly.

# Retrieve inchikey
df$inchikey <- unlist(cts_convert(df$CAS, from = "CAS", to = "InChIKey", 
                                  match = "first", verbose = F))
# Manually retrieve inchikey
df$inchikey <- c("HEFNNWSXXWATRW-UHFFFAOYSA-N", # Ibuprofen
                 "FFGPTBGBLSHEPO-UHFFFAOYSA-N", # Carbamazepine
                 "WHKUVVPPKQRRBV-UHFFFAOYSA-N", # MCPA ((4-Chloro-2-methylphenoxy)acetic acid)
                 "NA") # NA result

# -----------------------------------------------------------------------------#

# Retrieve Pubchem CID
x <- get_cid(df$inchikey, from = "inchikey", match = "first", verbose = F)

# Join df and x
df_intermediate <- full_join(df, x, by = c("inchikey" = "query"))

# Retrieve PubChem data
# A full list of properties to retrieve can be seen at: 
# https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables
desired_properties <- c(
  "CanonicalSMILES",
  "MolecularFormula",
  "MolecularWeight",
  "MonoisotopicMass",
  "XLogP"
  )
y <- pc_prop(df_intermediate$cid, properties = desired_properties)

# pc_prop changes CID to integers in object y. CID in df is of type character
str(y)
str(df_intermediate)

# Change to CID to character to join dfs
y$CID <- as.character(y$CID)

# Join dfs
df_final <- full_join(df_intermediate, y, by = c("cid" = "CID"))

# Write df_final to a .xlsx file if desired
# write.xlsx(as.data.frame(df_final), paste0("Output/YOUR_FILENAME_", Sys.Date(), ".xlsx"), row.names = F)

