# Header -----------------------------------------------------------------------
# Correlations and averages
# This will include values in the manuscript for estimates. Used to compare regions
# throughout the results section.
# 
# Written by Kathryn Doering, 28 Jan 2019
# 
# Clear global env--------------------------------------------------------------

rm(list = ls())

# load packages, set options ---------------------------------------------------
library(tidyverse)
library(corrplot)
options(stringsAsFactors = F)
# load data --------------------------------------------------------------------
# to get M summary: 
M_summary <- read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")

regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")
# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/6_M_corrs_avgs")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/6_M_corrs_avgs")
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# get correlations -------------------------------------------------------------
# Get the correlations among time series of median natural mortality by NOAA code.
# THis will be useful in describing the results. Do every crosscombo.# Compare Bayesian model results among NOAA codes ------------------------------

#add a regional factor/NOAA code factor for ordering.
M_summary$region_fac <- factor(M_summary$Region_name, levels = unique(regions$Region_name))
M_summary$NOAA_fac <- factor(M_summary$NOAA_code, levels = unique(regions$NOAA_code))
M_summary <- arrange(M_summary,NOAA_fac)



# Put medians from each NOAA code in their own column, 
M_summary_corr_dat <- M_summary %>% 
                        select(Year, NOAA_fac, X50.) %>% 
                        spread(NOAA_fac, X50.)

M_summary_corr_dat_mat <- as.matrix(M_summary_corr_dat)

# get correlations (remove year column)
corr_mat <- cor(M_summary_corr_dat_mat[,-1])

# # reorder columns geographically
# col_order <- colnames(M_summary_corr_dat_mat)[-1]
# corr_mat_geo_order <- corr_mat[,order(col_order)]
# colnames(corr_mat)

# save the correlation matrix---------------------------------------------------

  write.csv(corr_mat, paste0(der_dat_spec_path, "/M_corr_matrix.csv"))


#corrplot(corr_mat,method = "circle")
   
  # Plot correlation  ----------------------------------------------------
  
  png(paste0(fig_spec_path, "/M_corr_median_region_order.png"), width = 7, height = 7, units = "in", res = 300)
  corrplot(corr_mat, order = "original", method = "color", type = "upper")
  dev.off()
  
  # save full (square) plot, ecause may be easier to interpret.
  png(paste0(fig_spec_path, "/M_corr_median_region_order_full.png"), width = 7, height = 7, units = "in", res = 300)
  corrplot(corr_mat, order = "original", method = "color")
  dev.off()
  
  # hierarchical cluster order
  png(paste0(fig_spec_path, "/M_corr_M_median_hclust_3_order.png"), width = 7, height = 7, units = "in", res = 300)
  corrplot(corr_mat, order = "hclust", addrect = 3, method = "color")
  dev.off()
  
  # Calculate avgs by region ---------------------------------------------------
avgs <- vector("list", 4)
avgs_names <- c("M_Median_Avgs_NOAA_Code", "M_Median_Avgs_Region", "M_Median_Avgs_NOAA_Code_Time", "M_Median_Avgs_Region_Time")

  # calculate avgs of median natural mortality within regions to use in the results
  # description of the paper.
  avgs[[1]] <- M_summary %>% 
                  group_by(NOAA_code) %>% 
                  summarize(Mean = mean(X50.))
                  
  avgs[[2]] <- M_summary %>% 
                        group_by(Region_name) %>% 
                        summarize(Mean = mean(X50.))
  
  # calculate before and after 2002 avgs. (noaa code and region)
  avgs[[3]] <- M_summary %>% 
                    mutate(TS_break = ifelse(Year <= 2002, "before2003", "since2003" )) %>% 
                    group_by(NOAA_code, TS_break) %>% 
                    summarize(Mean = mean(X50.))
  
  avgs[[4]] <- M_summary %>% 
                       mutate(TS_break = ifelse(Year <= 2002, "before2003", "since2003" )) %>% 
                       group_by(Region_name, TS_break) %>% 
                       summarize(Mean = mean(X50.))
  # save the summaries
  purrr::map2(avgs, avgs_names, ~write.csv(.x, paste0(der_dat_spec_path,"/", .y, ".csv")))
  