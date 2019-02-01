# Header -----------------------------------------------------------------------
# Check convergence of the models that were run again.
# 
# Note that this is hard coded, because it was created specifically to deal with
# sampling issues for the particular dataset and models used
# 
# Written 1 Feb 2019 by Kathryn Doering

# Clear global env--------------------------------------------------------------
rm(list = ls())

# Load Packages and set options ------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------
mod_path <- "./Derived_Data/4_Mod_Sensitivity_Runs/4_rerun_mods"
# names of the models that were rerun in a vector
mod_names <- read.csv("./Derived_Data/4_Mod_Sensitivity_Runs/3_check_convergence/mods_to_rerun.csv")
mod_names <- mod_names$x

# create lists to store the models and their summaries
mods <- vector("list", length(mod_names))
mods_sum <- vector("list", length(mod_names))

# load the mods and summary into the lists
for (i in seq_along(mod_names)){
  mods[[i]] <- readRDS(paste0(mod_path, "/", mod_names[i], "_model.rda"))
  mods_sum[[i]] <- read.csv(paste0(mod_path, "/", mod_names[i], "_mod_summary.csv"))
}
#names the list components
names(mods) <- mod_names
names(mods_sum) <- mod_names
# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/4_Mod_Sensitivity_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/5_check_conv_rerun_mods")
# no need to make a figures folder, but can uncomment if desired.
# fig_gen_path <- "./Figs/4_Mod_Sensitivity_Runs"
# fig_spec_path <- paste0(fig_gen_path, "/5_check_conv_rerun_mods")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
# dir.create(fig_spec_path) 

# Check for divergent samples --------------------------------------------------
# Use models
sampler_params <- purrr::map(mods, ~get_sampler_params(.x, inc_warmup = F))

div_samples_by_chain <- purrr::map(sampler_params, ~sapply(.x, function(y) sum(y[, "divergent__"])))
# put into a dfr
div_samp_df <- purrr::map2_dfr(div_samples_by_chain, names(div_samples_by_chain), ~data.frame(div_samples_chn = .x, Model = .y))
# write to a file
write.csv(div_samp_df, paste0(der_dat_spec_path, "/n_divergent_samples_by_chain.csv"))

# Check for effective sample size, rhat ----------------------------------------
# 
# Check effective sampel size
# Want all parameters to have effective sample sizes (neff) >1000
less_1000 <- purrr::map(mods_sum, ~.x[.x$n_eff < 1000, ])  # mod_summary[mod_summary$n_eff <1000, ]
less_1000_df <- purrr::map2_dfr(less_1000, names(less_1000), ~mutate(.x, Model = .y)) 
# save
write.csv(less_1000_df, paste0(der_dat_spec_path, "/less_1000_neff.csv"))

#remove the models (no longer need)
rm(mods)
# Check Rhat
# put all rhat values in a vector and check (fix this)
lg_rhat <- purrr::map(mods_sum, ~.x[.x$Rhat>1.1, ])  # mod_summary[mod_summary$n_eff <1000, ]
lg_rhat_df <- purrr::map2_dfr(lg_rhat, names(lg_rhat), ~mutate(.x, Model = .y)) 
# save
write.csv(lg_rhat_df, paste0(der_dat_spec_path, "/lg_rhat.csv"))
