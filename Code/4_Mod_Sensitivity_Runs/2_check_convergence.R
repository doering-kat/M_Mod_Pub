# Header -----------------------------------------------------------------------
# 
# Check model convergence and make model summaries (which can be used in later 
#   scripts
# Use Rhat, and divergent transitions to determine if there are any indications
# that the model did not converge.
# 
# Written 24 Jan 2019 by Kathryn Doering
# 
# Clear global env--------------------------------------------------------------
rm(list = ls())
# Load Packages and set options ------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
#Load model and data associated with it: for all runs.
run_mod_path <- "./Derived_Data/4_Mod_Sensitivity_Runs/1_run_mod"
# bar_reg_key <- read.csv(paste0(run_mod_path, "/bar_reg_key.csv"))
# raw_mod_dat <- read.csv(paste0("./models_and_derived_data/", folder_name, "/raw_dat.csv"))

#NoAA_vec is in the order the model was run
NOAA_vec <- read.csv(paste0(run_mod_path,"/NOAA_vec.csv"))
NOAA_vec <- NOAA_vec$x

sens_runs_pars <- read.csv(paste0(run_mod_path, "/sense_runs_pars.csv"))

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/2_check_convergence")
# fig_gen_path <- "./Figs/2_Base_Mod_Run"
# fig_spec_path <- paste0(fig_gen_path, "/2_check_convergence")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
# dir.create(fig_spec_path) 

# loop through models to check convergence.-------------------------------------
file_names <- paste0(run_mod_path,"/R_eff_", round(sens_runs_pars$R_eff, 2), "_frac_d_prev_", 
                round(sens_runs_pars$frac_d_prev, 1))

# Load models (note: model files are really big, so may need to load one at a time instead!)
mods <- purrr::map(file_names, ~readRDS(paste0(.x, "_model.rda"))) 
mod_dat <- purrr::map(file_names, ~readRDS(file = paste0(.x, "_model_dat.rda")))
base_mod <- readRDS("./Derived_Data/2_Base_Mod_Run/1_run_mod/model.rda")

# Elapsed time -----------------------------------------------------------------
# #see how long sampling took
purrr::map(mods, ~print(get_elapsed_time(.x)))

# Check for divergent samples --------------------------------------------------
# See if any divergent samples
for(i in 1:length(mods)){
  tmp_mod <- mods[[i]]
  sampler_params <- get_sampler_params(tmp_mod, inc_warmup = FALSE)
  div_samples_by_chain <- sapply(sampler_params, function(x) sum(x[, "divergent__"]))
  print(div_samples_by_chain) # if all 0, then there were no divergent samples.
  if(sum(div_samples_by_chain)>0) warning(paste0("There are divergent samples in model ", i ))
}

# Check for sufficient r hat and effective sample size -------------------------
# Want all parameters to have effective sample sizes (neff) >1000
# TODO: Edit below here to run for all models.
# get param summaries, includes neff and rhat
all_par_summary <-purrr::map(mods, ~summary(.x)$summary)
base_mod_summary <- summary(base_mod)$summary

# Rhat
# put all rhat values in a vector and check
all_par_rhat <- purrr::map(all_par_summary, "Rhat")
# warn if there are r hat values above 1.1
max_rhat <- map_dbl(all_par_rhat, max)

if( any(max_rhat > 1.1)) warning("There are Rhat values > 1.1")

# neff
# put all n_effective values in a vector and check
all_par_neff <- purrr::map(all_par_summary, "n_eff")

# see if neff is adequate.
min_neff <- map_dbl(all_par_n_eff, min) # Hopefully, all above 1,000. (or close to 1,000)

if(any(min_n_eff < 1000)) warning("There were parameters with neff < 1000")

# write summary to a file ------------------------------------------------------
# add new col to distinguish models apart and combine into 1 dataframe.
all_par_summary_df <- map2_dfr(all_par_summary, 
                               paste0("R_eff", round(sens_runs_pars$R_eff, 2), 
                                      "frac_d_prev_", round(sens_runs_pars$frac_d_prev, 1)
                                      ), 
                                ~mutate(.x, Model = .y)# add new col
                                )
base_mod_summary$Model <- "Base"
# add base model summary
all_par_summary_df <- bind_rows(all_par_summary_df, base_mod_summary)

write.csv(all_par_summary_df, paste0(der_dat_spec_path, "/all_mod_summary.csv"),row.names = T)
