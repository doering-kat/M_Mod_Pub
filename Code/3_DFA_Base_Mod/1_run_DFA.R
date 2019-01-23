# Header -----------------------------------------------------------------------
# Dyanamic Factor Analysis of Natural mortality rates in Chesapeake Bay 
# 
# This wil be done by converting the median values for M to instantaneous rates
# and using these values to plug into DFA and compare models w/ 1-4 trends and a 
# diagonal and equal covariance matrix for the error term.
# 
# Next, Use AICc and SS resid/ SS obs as well as qualitative model fits to select 
# the most feasible model. 
# Use the MARS package with all of the time series of oyster natural mortality.
# 
# Script based on https://nwfsc-timeseries.github.io/atsa-labs/sec-dfa-fitting-dfa-models-with-marss.html
# 
# Written by Kathryn Doering on 17 Jan 2019
# 
# Clear global env -------------------------------------------------------------

rm(list = ls())

# Load packages, set options ---------------------------------------------------

library(tidyverse)
library(MARSS)
options(stringsAsFactors = F) #change the default

# Get data ---------------------------------------------------------------------
#get the data (model output)
mod_results_path <- "./Derived_Data/2_Base_Mod_Run"
# summary of the model results for M
mod_sum <- read.csv(paste0(mod_results_path, "/3_plot_M/M_dat_mod_boxcount.csv"))
# NOAA codes and their associated broader regions.
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Name and create folders to store results -------------------------------------

# name folders
der_dat_gen_path <- "./Derived_Data/3_DFA_Base_Mod"
der_dat_spec_path <- paste0(der_dat_gen_path, "/1_run_DFA")
fig_gen_path <- "./Figs/3_DFA_Base_Mod"
fig_spec_path <- paste0(fig_gen_path, "/1_run_DFA")
#create folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# Set years, number of trends to test in the models  ---------------------------

# Years
yr_0 <- 1990
yr_1   <- yr_0+1
yr_max <- 2017
nyears <- yr_max - yr_0 #not including year 0

n_trends <- 1:4 # number of trends to consider in models.

# Get DFA input  ----------------------------------------------------------------
#instantaneous rates of median estimate of M, then zscored.
Med_M <- mod_sum %>% 
          mutate(M_Med_Inst = -log(1-X50.)) %>% # convert to instantaneous
          dplyr::select(Year, NOAA_code, M_Med_Inst) %>% # keep only necessary cols
          rename(year = Year) # rename yr col to be consistent with the script.
          
#make another NOAA code factor column and then use this and years to reorder in terms of regions.
Med_M$NOAA_fac <- factor(Med_M$NOAA_code, levels = regions$NOAA_code)
Med_M <- arrange(Med_M, NOAA_fac, year)

#make Med_M into a matrix where rows are the years and columns are each NOAA code.
Med_M_mat <- matrix(Med_M$M_Med_Inst, nrow = nyears, ncol = length(unique(Med_M$NOAA_code)))

rownames(Med_M_mat) <- yr_1:yr_max
colnames(Med_M_mat) <- unique(Med_M$NOAA_code)

dat_wide_no_Z <- t(Med_M_mat) #transpose because we want each row to be a different NOAA code and each year in a different column. 

NOAA <- rownames(dat_wide_no_Z) # Save the ordered NOAA codes.

# Calculate the z scores for the data, which will make the model easier to run.
mean <- apply(dat_wide_no_Z, 1, mean, na.rm = T) #means for each row
sd <-   apply(dat_wide_no_Z, 1, sd, na.rm = T) #standard dev for each row

# Get the z score by subtracting the mean from each value and then divided by the sd
dat_wide <- (dat_wide_no_Z - mean)/sd

# Get number of time series
N_ts <- dim(dat_wide)[1]

# Plot DFA input ---------------------------------------------------------------
# plot both zscored (used in the analysis) and non-zscored data
# 
# Plot the data (zscored and demeaned). separtely each on its own plot, but the
# same device.
# Plot_Data_Sep function: formals are:
# dat_mat: numeric data to plot in a matrix where rows are NOAACOdes and cols are years
# yr_1, the first year of the model runs
# NOAA_code: the NOAA code name, which should be a rowname of dat_mat.
# returns: NA, but creates a plot
Plot_Data_Sep <- function(dat_mat, yr_1){
  function(NOAA_code){
    plot(dat_mat[NOAA_code, ], xlab = "", ylab = "M index (Z-transformed)", bty = "L",
      xaxt = "n", pch = 16,
      type = "b")
    axis(1, 0:dim(dat_mat)[2] + 1, yr_1 + 0:dim(dat_mat)[2])
    title(NOAA_code)
    return()
  }
}
# Make a closure from Plot_Data_Sep function to use with lapply
Plot_Data_Sep_Clos <- Plot_Data_Sep(dat_mat = dat_wide, yr_1 = yr_1)

png(paste0(fig_spec_path,"/inst_ts_zscored.png"), res = 300, width = 10, height =60, units = "in")
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,
  0, 0, 0))
lapply(NOAA, Plot_Data_Sep_Clos) # use the Plot_Data_Sep_2 closure to plot.
dev.off()

# plot all the code together.
# Alpha is used to add transparency; darker portions are where lines overlay each other,
# Where as single lines are shown in gray. 
# must use png, bmp, or jpeg for trancparency to show up
# 
# Plot_All function: plot all the data in dat_mat on the same plot, overlapping.
# formals are:
# dat_mat: numeric data to plot in a matrix where rows are NOAACOdes and cols are years
# file_name: a string with .png extension specifying the name and file path where
#   the plot wil be saved.
# ylab: text for the y axis of the plot (a character single value.)
# returns: NA
Plot_All <- function(dat_mat, file_name, ylab){
    NOAA <- row.names(dat_mat) # get 
    png(file_name, res = 300, width = 8, height = 4, units = "in")
    par(xaxs="i", yaxs="i")
    plot(dat_mat[1, ], xlab = "Year", ylab = ylab, bty = "L",
      xaxt = "n", ylim = c(min(dat_mat)-0.1, max(dat_mat)+0.1), col = alpha("black", 0.5),
      type = "l", lwd = 2)
    axis(1, 1:nyears, yr_1:yr_max)
    # plot the lines
    lapply(seq_along(NOAA[-1]), function(N) lines(dat_mat[N, ], col = alpha("black", 0.5), lwd = 2))
    dev.off()
    return()
}
# Plot the z scored data
Plot_All(dat_mat = dat_wide, file_name = paste0(fig_spec_path,"/inst_ts_zscored_together.png"), ylab = "M index (Z-transformed)")
# Plot the non-z-scored data.
Plot_All(dat_mat = dat_wide_no_Z, file_name = paste0(fig_spec_path,"/inst_ts_not_zscored_together.png"), ylab = "M (instantaneous)")

# Estimate and save  DFA models ------------------------------------------------
# Code to compare multiple models. use the MARSS package to estimate.

# fit models & store results
# NOTE: this will take a long time to run
# Run_DFA function: create and runs DFA models using MARSS.
# formals are:
# R, the covariance matrix (same as in MARSS package)
# dat_mat, the numeric matrix of data where NOAA codes are rows and years are cols
# m, the number of trends, which will vary by model.
# returns: a dfa_model fitted to dat_mat and for the specified covariance matrix
# and number of trends.
Run_DFA <- function(R = "diagonal and equal", dat_mat){
  function(m){
    # set the control params (min and max iterations, and the test to determine convergence)
    con_list <-  list(minit=200, maxit=50000, conv.test.slope.tol = 0.1)
    dfa_model <-  list(m=m, R=R) #put the R and m into the list of dfa model options.
    # run the model
    mod_run <-  MARSS(dat_mat, model=dfa_model, control=con_list, form="dfa", 
                       z.score=FALSE)
    return(mod_run)
  }
}
#create a closure with the data set (use default for covariance matrix)
# use the same data set for all models.
Run_DFA_Clos <- Run_DFA(dat_mat = dat_wide)
# estimate a model for each of the trends using the closure.
mod_list <- lapply(n_trends, Run_DFA_Clos)
# add names to the list of models - number of trends and the cov matrix used.
names(mod_list) <- paste0(n_trends,"_trends_diagonal_and_equal") 
#save the list of models
saveRDS(mod_list, paste0(der_dat_spec_path, "/dfa_mod_list.rda"))


# Save intermediate data files -------------------------------------------------

# The median M values used as input, ordered in a dataframe
write.csv(Med_M, paste0(der_dat_spec_path, "/med_M_inst_df_ordered.csv"), row.names = F)
#save non-zscored med M inst values in a matrix.
write.csv(dat_wide_no_Z, paste0(der_dat_spec_path, "/med_M_inst_matrix_not_zscored.csv"))
# save model input matrix (zscored)
write.csv(dat_wide, paste0(der_dat_spec_path, "/med_M_inst_matrix_zscored.csv"))
# also save as an r data object
saveRDS(dat_wide, paste0(der_dat_spec_path, "/med_M_inst_matrix_zscored.rda"))
