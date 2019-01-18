# Header -----------------------------------------------------------------------
# 
# Check model convergence and make diagnostic plots
# Use Rhat, and divergent transitions to determine if there are any indications
# that the model did not converge.
# 
# Written 11 Jan 2019 by Kathryn Doering
# 
# Clear global env--------------------------------------------------------------
rm(list = ls())

# Load Packages and set options ------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
#Load model and data associated with it:
run_mod_path <- "./Derived_Data/2_Base_Mod_Run/1_run_mod"
# bar_reg_key <- read.csv(paste0(run_mod_path, "/bar_reg_key.csv"))
# raw_mod_dat <- read.csv(paste0("./models_and_derived_data/", folder_name, "/raw_dat.csv"))

mod <- readRDS(file = paste0(run_mod_path, "/model.rda"))
mod_dat <- readRDS(file = paste0(run_mod_path, "/model_dat.rda"))
#NoAA_vec is in the order the model was run
NOAA_vec <- read.csv(paste0(run_mod_path,"/NOAA_vec.csv"))
NOAA_vec <- NOAA_vec$x

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/2_check_convergence")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/2_check_convergence")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
dir.create(fig_spec_path) 

# Elapsed time -----------------------------------------------------------------
# #see how long sampling took
print(get_elapsed_time(mod))

# Check for divergent samples --------------------------------------------------
# Get divergent samples
sampler_params <- get_sampler_params(mod, inc_warmup = FALSE)
div_samples_by_chain <- sapply(sampler_params, function(x) sum(x[, "divergent__"]))
print(div_samples_by_chain) # if all 0, then there were no divergent samples.
if(sum(div_samples_by_chain)>0) warning("There are divergent samples")

# Check for sufficient r hat and effective sample size -------------------------
# Want all parameters to have effective sample sizes (neff) >1000

# get param summaries, includes neff and rhat
all_par_summary <- summary(mod)$summary 

# Rhat
# put all rhat values in a vector and check
all_par_rhat <- all_par_summary[,"Rhat"]
# warn if there are r hat values above 1.1
max(all_par_rhat)
if(max(all_par_rhat)>1.1) warning("There are Rhat values > 1.1")

# neff
# put all n_effective values in a vector and check
all_par_neff <- all_par_summary[,"n_eff"]
# see if neff is adequate.
min(all_par_neff) # Hopefully, all above 1,000. (or close to 1,000)
if(any(all_par_neff <1000)) warning("There were parameters with neff < 1000")

# write summary to a file ------------------------------------------------------
write.csv(all_par_summary, paste0(der_dat_spec_path, "/mod_summary_all_chains.csv"),row.names = T)
