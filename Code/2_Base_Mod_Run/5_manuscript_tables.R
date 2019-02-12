# Header -----------------------------------------------------------------------

# Create tables for manuscipt (paper and supplemental)
# Clear global env--------------------------------------------------------------
rm(list = ls())
# load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# load data --------------------------------------------------------------------
# To make the table of bars by region:
bar_reg_key <- read.csv("./Derived_Data/2_Base_Mod_Run/1_run_mod/bar_reg_key.csv")
# to get M summary: 
M_summary <- read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")
regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")
# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/5_manuscript_tables")
# make the folders
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)

# Table of number of bars by region  -------------------------------------------
nbars <- bar_reg_key %>% 
           count(NOAACode) %>%  # get number of bars per region
           rename(NOAA_code = NOAACode) %>% 
           rename(nbars = n) # rename cols
# add region information
nbars_df <- left_join(regions[,c("Region_name", "NOAA_code", "NOAA_Name_Short")], nbars, by = "NOAA_code")
nbars_df$NOAA_code <- as.character(nbars_df$NOAA_code) # make character

# M table ----------------------------------------------------------------------
# Supplemental: Median M estimates by year and region. Median estimates of M.

M_avg <-  M_summary %>% # get avg m of the medians over the time series
             select(NOAA_code, Year, X50.) %>%
             group_by(NOAA_code) %>% 
             summarize(Mean = mean(X50.))

# make into wide format (years in cols instead of rows.)
M_table <- M_summary %>% 
             select(Region_name, NOAA_code, Year, X50.) %>% 
             spread(Year, X50.) %>% # put years into cols 
             left_join(M_avg, by = "NOAA_code") # add avgs

# reorder the NOAA codes
M_table$NOAA_code_fac <- factor(M_table$NOAA_code, levels = regions$NOAA_code)
M_table <- arrange(M_table, NOAA_code_fac) %>% 
             select(-NOAA_code_fac) # delete the factor 


# Save tables ------------------------------------------------------------------
write.csv(nbars_df, paste0(der_dat_spec_path, "/Nbars_table.csv"), row.names = F)
write.csv(M_table, paste0(der_dat_spec_path, "/M_median_table.csv"), row.names = F)
