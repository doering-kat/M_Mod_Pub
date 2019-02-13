# Header -----------------------------------------------------------------------
# 
# Check model convergence by looking at rhat values and if there were any divergent
# transitions. Also, check the effective sample sizes of the parameters.
# 
# Written 24 Jan 2019 by Kathryn Doering

# Clear global env--------------------------------------------------------------
rm(list = ls())

# Load Packages and set options ------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------
# Load model summaries (in a single .csv file)
mod_summary <- read.csv("./Derived_Data/4_Mod_Sensitivity_Runs/2_get_mod_summaries/all_mod_summary.csv")
# NOTE: models are loaded later in the script (in check divergent samples. 
# this is because they are large and therefore will be removed from the workspace immediately after using.
run_mod_path <- "./Derived_Data/4_Mod_Sensitivity_Runs/1_run_mods"
sens_runs_pars <- read.csv(paste0(run_mod_path, "/sense_runs_pars.csv"))

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/4_Mod_Sensitivity_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/3_check_convergence")
# no need to make a figures folder
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)


# Check for sufficient r hat and effective sample size -------------------------
# Check effective sampel size
# Want all parameters to have effective sample sizes (neff) >1000
less_1000 <- mod_summary[mod_summary$n_eff <1000, ]
to_rerun <- unique(less_1000$Model)
if(nrow(less_1000)>0) warning("There are parameters with neff <1000")

# Check Rhat
# put all rhat values in a vector and check (fix this)
lg_rhat <- mod_summary[mod_summary$Rhat>1.1, ]
to_rerun <- unique(c(to_rerun, lg_rhat$Model))
if(nrow(lg_rhat) > 0) warning("There are Rhat values >1.1")

# write problem model names, and problem parameters to files.
write.csv(less_1000, paste0(der_dat_spec_path, "/less_1000_neff.csv"))
write.csv(lg_rhat, paste0(der_dat_spec_path, "/lg_rhat.csv"))
write.csv(to_rerun, paste0(der_dat_spec_path, "/mods_to_rerun.csv"))

# Check for divergent samples --------------------------------------------------
# Need to load models to do this, which are LARGE! So, load one at a time, check
# then remove from environment. 

#partial names of all the models
file_names <- paste0(run_mod_path,"/R_eff_", round(sens_runs_pars$R_eff, 2), "_frac_d_prev_", 
  round(sens_runs_pars$frac_d_prev, 1), "_model.rda") 
# loop through the names to see if there are any divergent transitions
div_samples_by_chain <- NULL
for(i in seq_along(file_names)){
  tmp_mod <- readRDS(file_names[i])
  tmp_sampler_params <- get_sampler_params(tmp_mod, inc_warmup = FALSE)
  tmp_div_samples_by_chain <- sapply(tmp_sampler_params, function(x) sum(x[, "divergent__"]))
  div_samples_by_chain <- c(div_samples_by_chain, tmp_div_samples_by_chain)
  print(i)
  print(tmp_div_samples_by_chain) # if all 0, then there were no divergent samples.
  if(sum(tmp_div_samples_by_chain)>0) warning(paste0("There are divergent samples in model ", i ))
}
div_samples_by_chain_df<- data.frame(model = rep(seq_along(file_names), each = 3), 
                                     n_div = div_samples_by_chain)
#save output.
write.table(div_samples_by_chain_df, paste0(der_dat_spec_path, "/n_divergent_samples_by_chain.txt"))
