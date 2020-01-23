# Header -----------------------------------------------------------------------
# Run the necessary sensitivity models.

# Scenarios: 
# Modify the prior on d
# Try 5%, 95%, final mode estimate, extreme value. Change mean value onl
# Means: 
# 0.17, 0.82, 1.11, 2.00
# Additional notes on these values were kept in the MLE_d_R_priors.xlsx workbook.
# 
# Written by KD
# Load Packages and set options ------------------------------------------------
# packages
library(tidyverse)
library(rstan)
#R stan options
options(mc.cores = parallel::detectCores())
my_seed <- 123
set.seed(my_seed) #seed set in R
write_files <- T # Make false if do not want to write any data to files.
# other options
options(stringsAsFactors = F)
#custom R functions.
source("./Code/Funs/M_Mod_1_output_funs.R", echo = F)

# Specify and Create output paths ----------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/6_Extra_Model_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/1_run_mods")
fig_gen_path <- "./Figs/6_Extra_Model_Runs"
# do not need a fig folder for this script.
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)


# Load Data ---------------------------------------------------------------------

# Fall dredge survey individual tows, all measured in half bushel cultch
dat <-  read.csv("./Derived_Data/1_Clean_Data/1_Clean_Fall_Survey/fall_survey_half_bu.csv")
# the base model run (to call the model code, so that it does not need to be 
# recompiled)
base_mod <- readRDS("./Derived_Data/2_Base_Mod_Run/1_run_mod/model.rda")

# Specify input values ---------------------------------------------------------

#CHANGE NOAA codes here- want all possible NOAA codes
NOAA_list_init <- sort(unique(dat$NOAACode)) #make all the NOAA codes the initial
#list. (some will be deleted if the don't have sufficient data.)

# Stick with these years to run the model due to the data available
yr_0 <- 1990
yr_last <- 2017

# min number of bars with complete time series a bar needs to be included in the model
n_ts <- as.integer(2)

# changes to model inputs (for sensitivity runs.)
d_p_mean <-  c(0.17, 0.82, 1.11, 2.00)

# Modify data ------------------------------------------------------------------
# same for all model versions: 
# Select the complete time series of data to use. 
all_dat_ts <- SelectData(
                         dat = dat, 
                         NOAA_vec = NOAA_list_init, 
                         yr_0 = yr_0, 
                         yr_last = yr_last, 
                         remove_0 = FALSE,
                         n_ts = n_ts #min number of bars with complete time series required. 
                        )

#Get data sets to run the model as well as use for plotting later
# differs among mod runs. 

# loop through the sensitivity runs.
# The only changes will be for R_eff and frac_d_prev.
for (i in seq_along(d_p_mean)) {
  # make the model data
  output <- CreateModDataList(all_dat_ts = all_dat_ts, 
                              d_p = c(d_p_mean[i], 0.04))
    mod_dat <- output$mod_dat
  # Run model-------------------------------------------------------------------
  # WARNING: Run may take several hours with the whole fall survey database.
  mod <- stan(
          #file = "./Code/Stan_Mods/M_Mod_1.stan",
          # use same model as the base_mod to avoid recompiling; can use the file
          #  argument instead of the fit one to recompile model and run. 
          fit = base_mod,
          iter = 4000, #Started with 4000 iterations based on info from previous runs
          data = mod_dat,
          seed = my_seed,
          #init = inits, # uncomment if want to provide initial values for the model; otherwise, used defaults
          #control = list(adapt_delta = 0.99), # Uncomment if model has divergent transitions.
          chains = 3
  )
  
  # Save Model results and data ------------------------------------------------
  if(exists("mod")) {
  saveRDS(mod, paste0(der_dat_spec_path,"/d_p_mean_", round(d_p_mean[i], 2),
    "_model.rda"))
  saveRDS(mod_dat, paste0(der_dat_spec_path, "/d_p_mean_", round(d_p_mean[i], 2),      
    "_model_dat.rda"))
  
  # delete model (to reduce obj in workspace) ----------------------------------
  rm(mod)
  print(paste0("Finished model run ", i))
  } else {
    message("Model run", i, "was not saved so had issues running.")
  }
}

# Save files that do not differ across models ----------------------------------
# all below should be the same as base run and all runs, but just save once (just in case).
write.csv(sens_runs_pars, paste0(der_dat_spec_path, "/sense_runs_pars.csv"))
write.csv(output$bar_key, paste0(der_dat_spec_path,"/bar_reg_key.csv"), row.names = F)
write.csv(output$raw_mod_dat, paste0(der_dat_spec_path,"/raw_dat.csv"),row.names = F)
write.csv(output$NOAA_vec, file = paste0(der_dat_spec_path,"/NOAA_vec.csv"), row.names = F )