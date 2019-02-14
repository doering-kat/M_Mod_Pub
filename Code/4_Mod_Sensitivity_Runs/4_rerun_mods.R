# Header -----------------------------------------------------------------------
# Rerun sensitivity models that had problems in the first run.
# Written by Kathryn Doering
# 
# Two models had problems running, so they need to be rerun.
# Try to diagnose sampling problems before with a subset of data first
# Both do similar things, so problem shoot by running only 1 of the models.

# Load Packages and set options ------------------------------------------------
# packages
library(tidyverse)
library(rstan)
#R stan options
rstan_options(auto_write = T)
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
der_dat_gen_path <- "./Derived_Data/4_Mod_Sensitivity_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/4_rerun_mods")
# do not make any figures.
# make the folders
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)

# Load Data ---------------------------------------------------------------------

# Fall dredge survey individual tows, all measured in half bushel cultch
dat <-  read.csv("./Derived_Data/1_Clean_Data/1_Clean_Fall_Survey/fall_survey_half_bu.csv")
# the base model run (to call the model code, so that it does not need to be 
# recompiled)
base_mod <- readRDS("./Derived_Data/2_Base_Mod_Run/1_run_mod/model.rda")
# broader regions and their associated NOAA codes
regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")
# seee the models to rerun (although they are hard coded in the script)
rerun_mods <- read.csv("./Derived_Data/4_Mod_Sensitivity_Runs/3_check_convergence/mods_to_rerun.csv")

# Specify input values ---------------------------------------------------------

#CHANGE NOAA codes here- want all possible NOAA codes
NOAA_list_init <- sort(unique(dat$NOAACode)) #make all the NOAA codes the initial
#NOAA_list_init <- regions[regions$R_region == "Chester_River","NOAA_code"]
#list. (some will be deleted if the don't have sufficient data.)

# Stick with these years to run the model due to the data available
yr_0 <- 1990
yr_last <- 2017

# min number of bars with complete time series a bar needs to be included in the model
n_ts <- as.integer(2)

# changes to model inputs (for sensitivity runs.)
sens_runs_pars <- data.frame(R_eff = c(1.79, 5.09), 
  frac_d_prev = c(0.3, 0.2)
)

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

# redo model run R_eff_5.09_frac_d_prev_0.2 ------------------------------------


  # get the R_eff value and frac value for the urn
  tmp_R_eff <- 5.09
  tmp_frac_d_prev <- 0.2

  # make the model data
  output <- CreateModDataList(all_dat_ts = all_dat_ts, R_eff = tmp_R_eff, frac_d_prev = tmp_frac_d_prev)
  mod_dat <- output$mod_dat
  # Run model-------------------------------------------------------------------
  # WARNING: Run may take >8 hours with the whole fall survey database. (with
  # max_treedepth = 15)

  mod <- stan(
    #file = "./Code/Stan_Mods/M_Mod_1.stan",
    # use same model as the base_mod to avoid recompiling; can use the file
    #  argument instead of the fit one to recompile model and run.
    fit = base_mod,
    iter = 4000, #Started with 4000 iterations based on info from previous runs
    data = mod_dat,
    seed = my_seed,
    #init = inits, # uncomment if want to provide initial values for the model; otherwise, used defaults
    control = list(max_treedepth = 15), # uncomment if want to adjust max_tree_depth up from 10.
    #control = list(adapt_delta = 0.99), # Uncomment if model has divergent transitions.
    chains = 3
  )

  # Check convergence ----------------------------------------------------------
  mod_sum <- summary(mod)$summary
  any(mod_sum[ ,"n_eff"] < 1000)
    min(mod_sum[ ,"n_eff"])
  any(mod_sum[ ,"Rhat"] > 1.1)
  max(mod_sum[ ,"Rhat"])


  # Save Model results and data ------------------------------------------------
  # save model, model data, and the model summary
  saveRDS(mod, paste0(der_dat_spec_path,"/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_",
    round(tmp_frac_d_prev, 1), "_model.rda"))
  saveRDS(mod_dat, paste0(der_dat_spec_path, "/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_",
    round(tmp_frac_d_prev, 1), "_model_dat.rda"))
  write.csv(mod_sum, paste0(der_dat_spec_path, "/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_",
    round(tmp_frac_d_prev, 1), "_mod_summary.csv"))

# remove model objects
rm(list = c("mod", "mod_dat","mod_sum"))

# Redo model run R_eff_1.79_frac_d_prev_0.3 ------------------------------------
# run the other model that had problems. 
# Run from here! 
# get the R_eff value and frac value for the urn
tmp_R_eff <- 1.79
tmp_frac_d_prev <- 0.3

# make the model data
output <- CreateModDataList(all_dat_ts = all_dat_ts, R_eff = tmp_R_eff, frac_d_prev = tmp_frac_d_prev)
mod_dat <- output$mod_dat
# Run model 2 ------------------------------------------------------------------
# WARNING: Run may take >8 hours with the whole fall survey database. (with 
# max_treedepth = 12) - there were still problems with tree depth, but this does
# not affect the validity of the posterior, only the efficiency of the sampling.
# changed max_tree_depth to 12 to see  if that improves speed from the previous run

mod <- stan(
            #file = "./Code/Stan_Mods/M_Mod_1.stan",
            # use same model as the base_mod to avoid recompiling; can use the file
            #  argument instead of the fit one to recompile model and run. 
            fit = base_mod,
            iter = 4000, #Started with 4000 iterations based on info from previous runs
            data = mod_dat,
            seed = my_seed,
            #init = inits, # uncomment if want to provide initial values for the model; otherwise, used defaults
            control = list(max_treedepth = 12), # uncomment if want to adjust max_tree_depth up from 10.
            #control = list(adapt_delta = 0.99), # Uncomment if model has divergent transitions.
            chains = 3
)

# Check convergence for mod 2 --------------------------------------------------

mod_sum <- summary(mod)$summary
any(mod_sum[ ,"n_eff"] < 1000)
min(mod_sum[ ,"n_eff"])
any(mod_sum[ ,"Rhat"] > 1.1)
max(mod_sum[ ,"Rhat"])


# Save Model results and data ------------------------------------------------
# save model, model data, and the model summary
saveRDS(mod, paste0(der_dat_spec_path,"/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_", 
  round(tmp_frac_d_prev, 1), "_model.rda"))
saveRDS(mod_dat, paste0(der_dat_spec_path, "/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_", 
  round(tmp_frac_d_prev, 1), "_model_dat.rda"))
write.csv(mod_sum, paste0(der_dat_spec_path, "/R_eff_", round(tmp_R_eff, 2), "_frac_d_prev_", 
  round(tmp_frac_d_prev, 1), "_mod_summary.csv"))