# Header -----------------------------------------------------------------------

# Clean Fall Survey Data 
# 
# This script takes the spreadsheets from the relational fall survey database
# and combines them to get counts of live oysters and boxes in individual dredge
# tows all standardized to half bushel of cultch (count/ half bu).

# This will be used as input for the Bayesian natural mortality model.

# Created 9 Jan 2019 by Kathryn Doering based on code by Marvin Mace III
# Clear global env--------------------------------------------------------------
rm(list = ls())
# Load Packages and set options ------------------------------------------------

library(tidyverse)
options(stringsAsFactors = FALSE) 

# Read in data -----------------------------------------------------------------

#.csv files that are sepearte tables in the relational Access database:
reps <- read.csv('./Data/DNR_Fall_survey_May_8_2018/Reps.csv')
data <- read.csv('./Data/DNR_Fall_survey_May_8_2018/Data.csv')
bar_info <- read.csv('./Data/DNR_Fall_survey_May_8_2018/BarInfo.csv')

# Name and create output folders -----------------------------------------------
#name folders
der_dat_gen_path <- "./Derived_Data/1_Clean_Data" #folder for all clean data
der_dat_spec_path <- paste0(der_dat_gen_path, "/1_Clean_Fall_Survey") #folder for this script

#create folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)

# Manipulate data --------------------------------------------------------------
# 
# Bind tables together, so that intividaul dredge tows have sampling and bar 
# information attached.
tows <- left_join(reps, data, by = "SampleEvent") %>% # add sample event info
          left_join(bar_info, by = "ID") # add bar information


# remove tows were a sample was not obtained, or where there is no reporting unit value.
tows <- tows %>% 
          filter(!is.na(ReportingUnitValue)) %>% # remove samples with no reporting unit value
          filter(SampleNotObtained == FALSE) # remove tows where no samples were obtained


# Modify counts where the reporting values is 0.5 only
# Make sure the only reporting values are 1 and 0.5
if(any(!(tows$ReportingUnitValue %in% c(1, 0.5) ) )){
  stop("There are other reporting values besides 1 and 0.5, so this code will 
    not work")
}

# Select columns for counts
num_col_names <- tows %>% 
                   select(starts_with("Num")) %>% 
                   colnames()

# loop through the columns to iteratively change all data to be in half bushels
tows_half_bu <- tows
for (n in num_col_names){ # loop through to change
  # note that n is the current column name (we are overwriting it)
  tows_half_bu <- mutate(tows_half_bu, 
                         !! n := ifelse(ReportingUnitValue == 1, 
                                             (!! rlang::sym(n))/2, # divide 1 MD bushel values by 2
                                    ifelse(ReportingUnitValue == 0.5, # keep value if half bushel
                                             (!! rlang::sym(n)), NA))  #Any other inputs result in NA
                         )
}

# Add a new reporting unit value column, and rename the ReportingUnit and ReportingUnitValue Columns
tows_half_bu <- tows_half_bu %>% 
                  rename(OldReportingUnit = ReportingUnit) %>% 
                  rename(OldReportingUnitValue = ReportingUnitValue) %>% 
                  mutate(CurrentReportingUnitValue = 0.5) #everything is in half bushels

# write new data ---------------------------------------------------------------
write.csv(tows_half_bu, paste0(der_dat_spec_path,"/fall_survey_half_bu.csv"), row.names = FALSE)