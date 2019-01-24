# Header -----------------------------------------------------------------------
# Plot difference between the base model and the sensitivity models.
# Look at how M changes among models and d
# Written 25 Jan 2019 by Kathryn Doering 
# Clear global env -------------------------------------------------------------
rm(list = ls())

# Load packages, set options ---------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
# Base model
# sensitivity models
# barregkey, NOAA codes (should be the same for all )
# 
# Get model summaries ----------------------------------------------------------
# so all models do not need to be left in the global env
# Get the necessary mod summaries/samples, then remove the model objects. 
# 
# Calc diff for d --------------------------------------------------------------
# Start by plotting the d distributions on the same plot.
# 
# Find difference from base model (median? )- unless there is a better way to do this
# Calc diff for M --------------------------------------------------------------
# There is 1 M/yr/NOAA code - so a lot of values!  
# Calculate difference in the median values among the models for the same yr and NOAA code
# 
# Summarize by taking yr and NOAA code avgs, overall avg for each model.
# Perhaps create hists for the 3 models to better visualize outliers.
# Other visualization options? -------------------------------------------------
# TODO: think if there are other interesting, creative ways to display this
# output (however, if we want it to be a fairly small part of the analysis, may want 
# to just make a table).
#