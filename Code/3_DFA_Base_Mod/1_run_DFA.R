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
# clear global env -------------------------------------------------------------

rm(list = ls())

# Load packages, set options ---------------------------------------------------

library(tidyverse)
library(MARSS)
options(stringsAsFactors = F) #change the default

# Get data ---------------------------------------------------------------------
#get the data (model output)
mod_results_path <- "./Derived_Data/2_Base_Mod_Run"
#all_dat <- read.csv(paste0("./models_and_derived_data/", folder_name, "/median_M_all.csv"))
# #Load model and data associated with it:
# bar_reg_key <- read.csv(paste0(mod_results_path, "/1_run_mod/bar_reg_key.csv"))
# raw_mod_dat <- read.csv(paste0(mod_results_path, "/1_run_mod/raw_dat.csv"))

mod_sum <- read.csv(paste0(mod_results_path, "/3_plot_M/M_dat_mod_boxcount.csv"))
# mod <- readRDS(file = paste0(mod_results_path, "/1_run_mod/model.rda"))
# mod_dat <- readRDS(file = paste0(mod_results_path, "/1_run_mod/model_dat.rda"))

# NOAA_vec <- read.csv(paste0(mod_results_path, "/1_run_mod/NOAA_list.csv"))
# NOAA_vec <- NOAA_vec$x

regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# name and create folders to store results -------------------------------------

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

# Set years --------------------------------------------------------------------

# Years
yr_0 <- 1990
yr_1   <- yr_0+1
yr_max <- 2017
nyears <- yr_max - yr_0 #not including year 0

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
Plot_Data_Sep <- function(dat_wide, yr_1){
  function(NOAA_code){
    plot(dat_wide[NOAA_code, ], xlab = "", ylab = "M index (Z-transformed)", bty = "L",
      xaxt = "n", pch = 16,
      type = "b")
    axis(1, 0:dim(dat_wide)[2] + 1, yr_1 + 0:dim(dat_wide)[2])
    title(NOAA_code)
  }
}
# Make a closure from Plot_Data_Sep function to use with lapply
Plot_Data_Sep_Clos <- Plot_Data_Sep(dat_wide = dat_wide, yr_1 = yr_1)

png(paste0(fig_spec_path,"/inst_ts_zscored.png"), res = 300, width = 10, height =60, units = "in")
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,
  0, 0, 0))
lapply(NOAA, Plot_Data_Sep_Clos) # use the Plot_Data_Sep_2 closure to plot.
dev.off()

# plot all the code together.
# Alpha is used to add transparency; darker portions are where lines overlay each other,
# Where as single lines are shown in gray. 
# must use png, bmp, or jpeg for trancparency to show up
# formals are:
# dat_mat: numeric data to plot in a matrix where rows are NOAACOdes and cols are years
# file_name: a string with .png extension specifying the name and file path where
#   the plot wil be saved.
# ylab: text for the y axis of the plot (a character single value.)
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
}
# Plot the z scored data
Plot_All(NOAA= NOAA, dat_mat = dat_wide, file_name = paste0(fig_spec_path,"/inst_ts_zscored_together.png"), ylab = "M index (Z-transformed)")
# Plot the non-z-scored data.
Plot_All(NOAA= NOAA, dat_mat = dat_wide_no_Z, file_name = paste0(fig_spec_path,"/inst_ts_not_zscored_together.png"), ylab = "M (instantaneous)")

# plot all the code together.
# Alpha is used to add transparency; darker portions are where lines overlay each other,
# Where as single lines are shown in gray. 
# must use png, bmp, or jpeg for trancparency to show up
png(paste0(fig_spec_path,"/inst_ts_zscored_together.png"), res = 300, width = 8, height = 4, units = "in")
#par(mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,0, 0, 0))
par(xaxs="i", yaxs="i")
plot(dat_wide[1, ], xlab = "Year", ylab = "M index (Z-transformed)", bty = "L",
  xaxt = "n", ylim = c(min(dat_wide)-0.1, max(dat_wide)+0.1), col = alpha("black", 0.5),
  type = "l", lwd = 2)
axis(1, 1:nyears, yr_1:yr_max)
# plot the lines
lapply(seq_along(NOAA[-1]), function(N) lines(dat_wide[N, ], col = alpha("black", 0.5), lwd = 2))
dev.off()

#Also plot non-zscored data
png(paste0(fig_spec_path,"/inst_ts_together.png"), res = 300, width = 8, height = 4, units = "in")
#par(mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,0, 0, 0))
par(xaxs="i", yaxs="i")
plot(dat_wide_no_Z[1, ], xlab = "Year", ylab = "M index (Z-transformed)", bty = "L",
  xaxt = "n", ylim = c(min(dat_wide_no_Z)-0.1, max(dat_wide_no_Z)+0.1), col = alpha("black", 0.5),
  type = "l", lwd = 2)
axis(1, 1:nyears, yr_1:yr_max)
for (i in 2:length(NOAA)) {
  lines(dat_wide_no_Z[i, ], col = alpha("black", 0.5), lwd = 2)
}
dev.off()

# Estimate and save  DFA models ------------------------------------------------
# Code to compare multiple models. use the MARSS package to estimate.

# set new control params
con_list <-  list(minit=200, maxit=50000, conv.test.slope.tol = 0.1) #specify the max iterations
# set up forms of R matrices
R <- "diagonal and equal" #only use a diagonal and equal covariance structure
max_m <- 4 #the maximum number of trends (test trends from 1 to max_m)

# fit lots of models & store results
# NOTE: this will take a long time to run!
mod_list <- list()

  for(m in 1:max_m) {
    dfa_model <-  list(m=m, R=R) #put the R and m into the data.
    tmp_mod <-  MARSS(dat_wide, model=dfa_model, control=con_list,
      form="dfa", z.score=FALSE)
    #add model to the list
    mod_list[[m]] <- tmp_mod
    #assign(paste("mod", m,"diagonal_and_equal", sep="_"), tmp_mod) #assign a variable name to the model
  }

# add names to the list of models
names(mod_list) <- paste0("trends_",m, "_diagonal_and_equal") # name wiht the number
#of trends and covariance matrix.
#save the list of models
saveRDS(mod_list, paste0(der_dat_spec_path, "/dfa_mods.rda"))


# Save intermediate data files -------------------------------------------------
# The median M values used as input, ordered in a dataframe
write.csv(Med_M, paste0(der_dat_spec_path, "/med_M_inst_df_ordered.csv"), row.names = F)
#save non-zscored med M inst values in a matrix.
write.csv(dat_wide_no_Z, paste0(der_dat_spec_path, "/med_M_inst_matrix_not_zscored.csv"))
# save model input matrix (zscored)
write.csv(dat_wide, paste0(der_dat_spec_path, "/med_M_inst_matrix_zscored.csv"))
# also save as an r data object
saveRDS(dat_wide, paste0(der_dat_spec_path, "/med_M_inst_matrix_zscored.rda"))
