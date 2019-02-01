# Make model summaries 
# 
# Header -----------------------------------------------------------------------
# 
# Check model convergence and make model summaries (which can be used in later 
#   scripts
# Use Rhat, and divergent transitions to determine if there are any indications
# that the model did not converge.
# 
# TODO: make model summary just for M as well. get the model region and year, use
# the bar reg key to change to NOAA codes and years, and write to a .csv.
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
run_mod_path <- "./Derived_Data/4_Mod_Sensitivity_Runs/1_run_mods"
sens_runs_pars <- read.csv(paste0(run_mod_path, "/sense_runs_pars.csv"))
# Load models (note: model files are really big, so may need to load one at a time instead!)
file_names <- paste0(run_mod_path,"/R_eff_", round(sens_runs_pars$R_eff, 2), "_frac_d_prev_", 
  round(sens_runs_pars$frac_d_prev, 1))
mods <- purrr::map(file_names, ~readRDS(paste0(.x, "_model.rda"))) 
mod_dat <- purrr::map(file_names, ~readRDS(file = paste0(.x, "_model_dat.rda")))
base_mod <- readRDS("./Derived_Data/2_Base_Mod_Run/1_run_mod/model.rda")
# Set output paths and make folders --------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/4_Mod_Sensitivity_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/2_get_mod_summaries")

dir.create(der_dat_spec_path)
# dir.create(fig_spec_path) 

# Get summaries and write to file ----------------------------------------------

#get the summaries
all_par_summary <-purrr::map(mods, summary)
all_par_summary <- purrr::map(all_par_summary, "summary")
base_mod_summary <- summary(base_mod)$summary

#remove models from global environment (they are large)
rm(mods)
rm(base_mod)

# add new col to distinguish models apart and combine into 1 dataframe.
tmp_rownames  <- map(all_par_summary, row.names)  #save the parameter names
tmp_rownames <- map(tmp_rownames, ~data.frame(param = .x)) # make into a dataframe
all_par_summary_df <- purrr::map(all_par_summary, data.frame) # convert to df
# add rownames as a col
all_par_summary_df  <- purrr::map2(all_par_summary_df, tmp_rownames, ~bind_cols(.x, .y) )
# add column with the model run
all_par_summary_df <- map2_dfr(all_par_summary_df, 
  paste0("R_eff_", round(sens_runs_pars$R_eff, 2), 
    "_frac_d_prev_", round(sens_runs_pars$frac_d_prev, 1)
  ),
  ~mutate(.x, Model = .y)# add new col
)

# repeat steps for base
rownames_base <- row.names(base_mod_summary)
rownames_base <- data.frame(param = rownames_base)

base_mod_summary_df <- data.frame(base_mod_summary) %>% 
  bind_cols(rownames_base)

base_mod_summary_df$Model <- "Base"

# combine base and sensitivity model summaries
all_par_summary_df <- bind_rows(all_par_summary_df, base_mod_summary_df)

write.csv(all_par_summary_df, paste0(der_dat_spec_path, "/all_mod_summary.csv"),row.names = T)

