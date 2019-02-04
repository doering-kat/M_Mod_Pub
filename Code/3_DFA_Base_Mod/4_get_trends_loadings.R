# Header -----------------------------------------------------------------------
# Get the varimax rotation trends and loadings from DFA and save.
# 
# Written 29 Jan 2019 by Kathryn Doering

# clear global env -------------------------------------------------------------
rm(list = ls())

# Load packages, set options ---------------------------------------------------
library(MARSS)
library(tidyverse)
options(stringsAsFactors = F)

# Read in data -----------------------------------------------------------------
dfa_mod <- readRDS("./Derived_Data/3_DFA_Base_Mod/2_select_mod/selected_dfa_mod.rda")

# Specify and create output paths ----------------------------------------------
# No figures created, so only make one for derived data.
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/3_DFA_Base_Mod"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/4_get_trends_loadings")
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)

# Get the trends and loadings --------------------------------------------------
Z_est <- coef(dfa_mod, type = "matrix")$Z #factor loadings, estimated from the model.
# find the rotation necessary
H_inv <- varimax(Z_est)$rotmat
# rotate factor loadings
loadings <-  Z_est %*% H_inv
# rotate the trends
trends <-  solve(H_inv) %*% dfa_mod$states #solve(H_inv) gives the inverse, aka just H.

# add labels -------------------------------------------------------------------
# Get the labels from the data.
dfa_dat <- dfa_mod$model$data
# add for trends
colnames(trends) <- colnames(dfa_dat)
rownames(trends) <- c("Trend1", "Trend2")
# add for loadings
colnames(loadings) <- c("Trend1", "Trend2")
rownames(loadings) <- rownames(dfa_dat)


# Write to files----------------------------------------------------------------
write.csv(trends, paste0(der_dat_spec_path, "/Sel_Mod_Trends.csv"))
write.csv(loadings, paste0(der_dat_spec_path, "/Sel_Mod_Loadings.csv"))
