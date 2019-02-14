# Header -----------------------------------------------------------------------
# Select the best DFA model using AICc (and qualitative checks.)
# 
# Use AICc and SS resid/ SS obs as well as qualitative model fits to select 
# the most feasible model. 
# 
# Script based on https://nwfsc-timeseries.github.io/atsa-labs/sec-dfa-fitting-dfa-models-with-marss.html
# SS resid/SSobs is as in Zuur et al. 2003 and Peterson et al. 2017.
# 
# TODO: Get dat_wide from model output itself rather than a 
# separate file 
# 
# Written by Kathryn Doering
# 
# Load packages, set options ---------------------------------------------------

library(tidyverse)
library(MARSS)
options(stringsAsFactors = F) #change the default

# Get data ---------------------------------------------------------------------
# models in a list
mod_list <- readRDS("./Derived_Data/3_DFA_Base_Mod/1_run_DFA/dfa_mod_list.rda")
# dfa model input (in same form as used for the DFAs)
dat_wide <- readRDS("./Derived_Data/3_DFA_Base_Mod/1_run_DFA/med_M_inst_matrix_zscored.rda")
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# name and create folders to store results -------------------------------------

# name folders
der_dat_gen_path <- "./Derived_Data/3_DFA_Base_Mod"
der_dat_spec_path <- paste0(der_dat_gen_path, "/2_select_mod")

#create folders
dir.create(der_dat_gen_path) #should exist already
dir.create(der_dat_spec_path)

# Print model summaries --------------------------------------------------------

# print all model result here
# Print_Mod_Summary: print summaries of the modesl with labels.
# formals are:
# mod, a MARSS model that has been estimated.
# ntrends, the number of trends estimated for the model.
Print_Mod_Summary <- function(mod, ntrends){
  print(paste0("model with n trends = ", ntrends, " and R is diagonal and equal)")) #label
  print(summary(mod))
}
# Save the model summaries
sink(paste0(der_dat_spec_path,"/dfa_results_M_inst_summmaries.txt"))
purrr::map2(mod_list, 1:4, Print_Mod_Summary)
sink() # stop writing to a file. 

# Get AICc, convergence ---------------------------------------------------------

# Make_Mod_Row function.
# Formals are:
# mod, a MARSS model.
# m, the number of trends for that model.
# output is a dataframe with 1 row.
# TODO: perhaps get m from the name of the model itself?
Make_Mod_Row <- function(mod, m){
  df <- data.frame(nTrends = m,
                   logLik = mod$logLik,
                   nParams = mod$num.params,
                   AICc = mod$AICc,
                   Converged = mod$convergence #0 means converged, 10 means did not
                   ) # don't reduce dimensions.
  return(df)
}
# apply to each element of lits, then make into a single dataframe.
model_comp <- purrr::map2_dfr(mod_list, n_trends <- 1:4, Make_Mod_Row) 

#add delta AICc to the model data file.
model_comp$dAICc <- model_comp$AICc- min(model_comp$AICc)

# Compare the mean model fit ---------------------------------------------------
# Get Mean Fit function: calculate the sum of squared residual/sum of squared 
#   measured and take the mean.
# Formals are:
# dat_mat
# dfa_mod, a MARSS model.
# what: default is mean, but if is "all", will give fit by time series.
# Each row is a different time series, each column a different year
# what can take values of "mean" or "all", which is the statistic for each time series
# Note: would need to be modified to run with a 1 trend model.

Get_Mean_Fit <- function( dfa_mod, dat_mat = dat_wide, what = "mean"){
  Z_est <- coef(dfa_mod, type = "matrix")$Z #factor loadings
  # get the inverse of the rotation matrix
  if(dim(Z_est)[2] == 1){ # don't attempt to rotate for 1 trend model.
    # factor loadings
    ZZ <- Z_est
    # the trends
    trends <- dfa_mod$states
  } else { # use the varimax rotation
    H_inv <- varimax(Z_est)$rotmat
    # rotate factor loadings
    ZZ <-  Z_est %*% H_inv
    # rotate the trends (states, or processes)
    trends <-  solve(H_inv) %*% dfa_mod$states #solve(H_inv) gives the inverse, aka just H.
  }
  #Get expected residuals
  ex <- ZZ %*% trends #expected values from the loadings and states
  #use model expectation (fitted values) to calculate the observed
  resid <- dat_mat - ex #difference between the data and the fitted values
  #calculate the sum of squared residuals for each time series
  resid_sq <- resid^2
  SS_resid <- apply(resid_sq,1,sum) #apply for each time series
  #calculate the sum of squared data for each time series
  dat_mat_sq <- dat_mat^2
  SS_dat <- apply(dat_mat_sq,1,sum)
  fit_ss <- SS_resid/SS_dat #fit for each time series
  meanfit <- mean(SS_resid/SS_dat) #average over all time series
  if (what == "mean"){
    return(meanfit)
  }else {
    if(what == "all"){
      return(fit_ss)
    } else{
      stop("what must be 'mean' or 'all'. Cannot take other values.")
    }
  }
}

# add mean fit to the modle comparison dataframe.
model_comp$meanfit <-  map_dbl(mod_list, Get_Mean_Fit)

# Select model and write comparison data and mod to file -----------------------

# write the model data.
write.csv(model_comp, paste0(der_dat_spec_path,"/DFA_mod_comp_M_inst_1.csv"))

# store selected models (one with lowest, and close-to lowest AICc.)
selected_mod <- which(model_comp$dAICc == 0) # selected model based on AICc
# note: similar models may also be worth investigating 
mod_to_check <- which(model_comp$dAICc <=5)

if (any(model_comp[mod_to_check,"meanfit"] > 0.6)){
  warning("Some potential models to select have poor fit as assessed by Mean Fit")
}

# store selected model by itself
saveRDS(mod_list[[selected_mod]], paste0(der_dat_spec_path, "/selected_dfa_mod.rda") )
