# Header -----------------------------------------------------------------------
# 
# Compare results from Damiano and Wilberg 2019 with these results
# 
# 
# Load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
save_files <- T # True if want to create new files, False if not.

# Load data --------------------------------------------------------------------
# Instantanous M for choptank river complex from Damiano and Wilberg 2019
M_Chop_DW <- read.csv("./Data/M_Damiano_and_Wilberg_2019.csv")
# Data from the Bayesian model, median by year and NOAA code. anualized rates.
Bayes_M <-  read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")
# Add regions for plotting
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Clean DW M ----------------------------------------------------------------
# Get in same form as Bayes_M.
# make tidy
M_Chop_DW_tidy <- tidyr::gather(M_Chop_DW, "NOAA_name", "M_inst", 2:ncol(M_Chop_DW))
# Modify names
name_lookup <- data.frame(NOAA_name = colnames(M_Chop_DW)[-1],
                          NOAA_code = c("537", "137", "237", "337", "637", 
                                        "53o","53c", "437o", "437c")
                          )
name_lookup # print to make sure it matches, as the key just created was hard coded.

# create a NOAA code column using name_lookup in M_Chop_DW_tidy
M_Chop_DW_tidy <- left_join(M_Chop_DW_tidy, name_lookup)

# remove NAS and open/closed areas - hard coded, may be able to use reg exp.
M_DW_comp <- M_Chop_DW_tidy %>% 
               filter(NOAA_code %in% c("537", "137", "237", "337", "637")) %>% 
               filter(!is.na(M_inst)) %>% 
               mutate(NOAA_code = as.integer(NOAA_code))
# Merge datasets ---------------------------------------------------------------

# Compare ----------------------------------------------------------------------
# plot
# Correlate by year

