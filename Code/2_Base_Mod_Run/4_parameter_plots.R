# Header -----------------------------------------------------------------------
# TODO: think about which plots are important to include here.

# Maybe if there are any residual plots, or plots of variables against one
# another that I did?
# 
# Some may also be included in 5_manuscript_figures.R
# 
# Written 17 Jan 2019 by Kathryn Doering

# Load packages -----------------------------------------------------------------
library(dplyr)
library(rstan)
library(shinystan)
#To add maps:
library(rgdal)
library(maptools)
library(ggplot2)
#library(plyr)
library(maps) #later on in the script.
#Another option: using the maps package.
options(stringsAsFactors = F) #Change options.

#Functions to use: 
source('./Code/Extract_and_plot_functions.R')

# Load data ---------------------------------------------------------------------
read_path <- "./Derived_Data/2_Base_Mod_Run/1_run_mod"
#Load model and data associated with it:
bar_reg_key <- read.csv(paste0(read_path, "/bar_reg_key.csv"))
raw_mod_dat <- read.csv(paste0(read_path, "/raw_dat.csv"))

mod <- readRDS(file = paste0(read_path, "/model.rda"))
mod_dat <- readRDS(file = paste0(read_path, "/model_dat.rda"))

NOAA_vec <- read.csv(paste0(read_path, "/NOAA_list.csv"))
NOAA_vec <- NOAA_vec$x

#NOAA code regions
regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/3_diagnostic_plots")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/3_diagonstic_plots")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
dir.create(fig_spec_path) 

# Define variables--------------------------------------------------------------
# years used in the models
yr_0 <- 1990
yr_last <- 2017 #Assume 2017 data have been entered
nyears <-  yr_last-yr_0  #nyears not including year 0.

# Plot M -----------------------------------------------------------------------
# M with Box count estimator
# Prior M
# M without box count estimator
# Think of other ways to plot mortality (any way to synthesize in the paper?)

# Plot d with prior ------------------------------------------------------------
# 
# Some other possible plots below ----------------------------------------------
# Plot Regional Lambda  and beta with prior ------------------------------------
# Plot sigma -------------------------------------------------------------------
# Plot beta 0 and priors -------------------------------------------------------

