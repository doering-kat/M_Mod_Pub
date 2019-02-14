# Header -----------------------------------------------------------------------
# 
# Calculate the percent of boxes by bar and year that are residual (esp. in years
# after large M events, like 2003)
#
# This is used in the discussion section
# 
# Written by Kathryn Doering 
# Load packages, set options ---------------------------------------------------
library(rstan)
library(tidyverse)
options(stringsAsFactors = F)

# Load data --------------------------------------------------------------------

# model results
mod <- readRDS("./Derived_Data/2_Base_Mod_Run/1_run_mod/model.rda")

# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/7_calc_resid_boxes")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/7_calc_resid_boxes")
# make the folders
#uncomment *gen_path lines if need to create the general path as well.
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)
dir.create(fig_gen_path) # should already exist.
dir.create(fig_spec_path)

# Calculate residual boxes -----------------------------------------------------
year_0 <- 1990 # year 0 for the model
year_last  <-  2017 # last year of the model
nyears <- year_last - year_0 # nyears, not including year 0

# get summaries of necessary parameters
sum_d <- summary(mod, pars = "d" )$summary # box disarticulation rate
sum_beta_0_i <- summary(mod, pars = "beta_0_i")$summary # mean boxes in year 0
sum_beta_i <- summary(mod, pars = "beta_i")$summary # mean boxes for all other years

# calculate mean number of residual boxes per half bu for year 1 (use year 0)

per_resid_df <- data.frame() #df to store results
# loop through bars and years
for (i in 1:nrow(sum_beta_0_i)){
  # calculate residual boxes in the first year
  tmp_resid <- sum_beta_0_i[i,6]*exp(-sum_d[1,6])
  tmp_str <- paste0("beta_i[",as.character(1), ",",i,"]" )
  tmp_tot_bx <- sum_beta_i[tmp_str,6]
  tmp_per_resid <- 100*tmp_resid/tmp_tot_bx
  #add to a data frame
  tmp_df <- data.frame(year = 1, bar = i, per_resid_box = tmp_per_resid)
  per_resid_df <- bind_rows(per_resid_df, tmp_df)
  
  for (y in 2:nyears){ # now calculate for years 2:27
    # row name for previous year
    tmp_str_prev <- paste0("beta_i[", as.character(y-1) ,",", i,"]")
    # get number of residual boxes in year y
    tmp_resid <- sum_beta_i[tmp_str_prev,6]*exp(-sum_d[1,6])
    # row name for current year 
    tmp_str <- paste0("beta_i[",as.character(y), ",",i,"]" )
    # total number of boxes for the current year
    tmp_tot_bx <- sum_beta_i[tmp_str,6]
    # percent of residual boxes of the total boxes in current year y
    tmp_per_resid <- 100*tmp_resid/tmp_tot_bx
    #add to a data frame
    tmp_df <- data.frame(year = y, bar = i, per_resid_box = tmp_per_resid)
    per_resid_df <- bind_rows(per_resid_df, tmp_df)
  }
}
# Summarize data across bars and years -----------------------------------------
# look at avgs across years

avg_yr <- per_resid_df %>% 
  group_by(year) %>% 
  summarize(avg = mean(per_resid_box)) %>% 
  mutate(year2 = year+1990)


# plot and save
png(paste0(fig_spec_path, "/Percent_resid_boxes.png"), height = 4, width = 6, res = 300, units = "in")
plot(x = avg_yr$year2, y = avg_yr$avg, type = "o", 
  ylab = "Percent Residual Boxes", xlab = "Year")
dev.off()

# Save output ------------------------------------------------------------------ 

write.csv(per_resid_df, paste0(der_dat_spec_path, "/percent_resid_box.csv"), row.names = F)
write.csv(avg_yr, paste0(der_dat_spec_path, "/percent_resid_box_by_yr.csv"), row.names = F)
