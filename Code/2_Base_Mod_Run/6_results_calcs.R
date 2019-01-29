# Header -----------------------------------------------------------------------
# Calculate values for use in the results section.
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
# to get summary of all model parameters
mod_summary <- read.csv("./Derived_Data/2_Base_Mod_Run/2_check_convergence/mod_summary_all_chains.csv")
# to get M summary: 
M_summary <- read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")

regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")
# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/6_results_calcs")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/6_results_calcs")
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# Describe model convergence ---------------------------------------------------
# Model output to describe the model convergence results.

sink(paste0(der_dat_spec_path, "/describe_mod_convergence.txt"))
print("Check max rhat and min n_eff")
# check all r hat <1.1 and all n_eff >1000.
print("max rhat:")
max(mod_summary$Rhat)
print("min n_eff:")
min(mod_summary$n_eff)
# check percent params with max n_eff
max_n_eff <- max(mod_summary$n_eff)
print(paste0("Percent of model parameters with n_eff = ", max_n_eff, ":"))
100*length(which(mod_summary$n_eff == max_n_eff))/length(mod_summary$n_eff)
sink()
# describe sds from model params  ----------------------------------------------
# (to characterize uncertainty)
# get min, max, avg sd for M.

sink(paste0(der_dat_spec_path, "/describe_M_uncertainty.txt"))
print("Summary of Model M uncertainty (sd):")
summary(M_summary$sd)
# look at avg uncertainty by year.
avg_sd_by_yr <- M_summary %>% 
                group_by(Year) %>% 
                summarize(Mean_SD = mean(sd))
print("Average uncertainty in model M (sd) by year:")
print(avg_sd_by_yr, n = Inf)
print("Min avg uncertainty in model M (sd) by year:")
min(avg_sd_by_yr$Mean_SD)
print("Max avg uncertainty in model M (sd) by year:")
max(avg_sd_by_yr$Mean_SD)
print("Mean of avg uncertinty by year in model M (sd:)")
mean(avg_sd_by_yr$Mean_SD)
sink()

# plot M median estimate v sd:.
png(paste0(fig_spec_path, "/M_sd_vs_M_median.png"), res = 300, width = 6, height = 4, units = "in")
plot(M_summary$sd~M_summary$X50., xlab = "Median", ylab = "SD")
dev.off()

# avg median M -----------------------------------------------------------------
avg_med_M <- M_summary %>% 
              mutate(Ins_M = -log(1-X50.)) %>% # get instantaneous M
              group_by(NOAA_code) %>% 
              summarize(mean_Ins_M = mean(Ins_M)) %>% 
              mutate(annualized_mean_Ins_M = 1 - exp(-mean_Ins_M))
sink(paste0(der_dat_spec_path, "/median_M_avgs.txt"))
print("Median M converted to instantaneous, then averaged by NOAA code. last col is that number but converted back to annualized values.")
print(avg_med_M, n = Inf)
print("summary of previous values (instantaneous, then annual):")
summary(avg_med_M$mean_Ins_M)
summary(avg_med_M$annualized_mean_Ins_M)
sink()

# model and box count comparison -----------------------------------------------

sink(paste0(der_dat_spec_path, "/model_box_cnt_compare.txt"))
# overall average
print("The model results were on average this much higher (annual M) than the\nbox count method during 1991-2017 (neg values signify the model results were lower than the box count):")
M_summary %>% 
  mutate(Diff = X50. - BoxCount) %>% 
  summarize(Avg_diff = mean(Diff))
# Calculate average diff between model and box count 2006 and after:
print("The model results were on average this much higher (annual M) than the\nbox count method during 2006 to 2017 (neg values signify the model results were lower than the box count):")
M_summary %>% 
  mutate(Diff = X50. - BoxCount) %>% 
  filter(Year >=2006) %>% 
  summarize(Avg_diff = mean(Diff))
# Calculate average diff during 2003-2005
print("The model results were on avg this much higher (annual M) than the box\n count method by year (neg values signify the model results were lower than the box count) ")
avg_diff_yr <- M_summary %>% 
          mutate(Diff = X50. - BoxCount) %>% 
          group_by(Year) %>% 
          summarize(Avg_diff = mean(Diff))
print(avg_diff_yr, n = Inf)
sink()

# Averages by region -----------------------------------------------------------

# these are average calculations over specific periods. 

# Western Shore
print("Western Shore, avg (annual) M during 1991-1999, 2003-2017: ")
M_summary %>% 
  filter(R_region == "Western_Shore") %>%   filter(Year %in% c(1991:1999,2003:2017)) %>% 
  summarize(mean = mean(X50.))
print("Western Shore, avg (annual) M during 2000-2002: ")
M_summary %>% 
  filter(R_region == "Western_Shore") %>%   filter(Year %in% c(2000:2002)) %>% 
  summarize(mean = mean(X50.))

# Chester River
print("Chester River, NOAA code 131, 231 avg (annual) M during 2000-2002: ")
M_summary %>% 
   filter(NOAA_code %in% c(131,231)) %>% 
   filter(Year %in% 2000:2002) %>% 
  summarize(mean = mean(X50.))
print("Chester River, NOAA code 131 avg (annual) M during 1992,1996, 2006: ")
M_summary %>% 
  filter(NOAA_code == 131) %>% 
  filter(Year %in% c(1992,1996,2006)) %>%
  summarize(mean = mean(X50.))
print("Chester River, NOAA code 231 avg (annual) M during 1996, 2005: ")
M_summary %>% 
  filter(NOAA_code == 231) %>% 
  filter(Year %in% c(1996,2005)) %>%
  summarize(mean = mean(X50.))
print("Chester River, NOAA code 231 avg (annual) M during 2003: ")
M_summary %>% 
  filter(NOAA_code == 231) %>% 
  filter(Year == 2003) %>%
  select(X50.)
# Eastern Bay
# TODO: fill in here!!!
print("Eastern Bay, avg during 2001-2002, 2007:  ")
M_summary %>% 
  filter(R_region == "Eastern_Bay") %>% 
  filter(Year %in% c(2001,2002,2007)) %>%
  summarize(mean = mean(X50.))
print("Eastern Bay, NOAA codes 99 and 60 during 1991-1992:  ")
M_summary %>% 
  filter(NOAA_code %in% c(99,60)) %>% 
  filter(Year %in% c(1991,1992)) %>%
  summarize(mean = mean(X50.))


# Choptank
print("Choptank, avg during 1991-2002:  ")
M_summary %>% 
  filter(R_region == "Choptank") %>%
  filter(Year %in% 1991:2002) %>%
  summarize(mean = mean(X50.))
print("Choptank, avg during 2003-2017:  ")
M_summary %>% 
  filter(R_region == "Choptank") %>%
  filter(Year %in% 2003:2017) %>%
  summarize(mean = mean(X50.))

# Pax
print("Patuxent, avg during 1991-1992:  ")
M_summary %>% 
  filter(R_region == "Pat") %>%
  filter(Year %in% 1991:1992) %>%
  summarize(mean = mean(X50.))
print("Patuxent, avg during 1999-2000:  ")
M_summary %>% 
  filter(R_region == "Pat") %>%
  filter(Year %in% 1999:2000) %>%
  summarize(mean = mean(X50.))
print("Patuxent, avg during 2003-2017:")
M_summary %>% 
  filter(R_region == "Pat") %>%
  filter(Year %in% 2003:2017) %>%
  summarize(mean = mean(X50.))

# Pot
print("Potomac, avg during 1999-2000:")
M_summary %>% 
  filter(R_region == "Pot") %>%
  filter(Year %in% 1999:2000) %>%
  summarize(mean = mean(X50.))
# Tangier Sound
print("Potomac, avg during 1992 and 1999:")
M_summary %>% 
  filter(R_region == "Pot") %>%
  filter(Year %in% c(1992,1999)) %>%
  summarize(mean = mean(X50.))

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
# Box disarticulation rate -----------------------------------------------------
  # get median rate, convert to percent of boxes disarticulating.
d <- mod_summary %>% 
        mutate(param_start = substr(X, start = 1, stop = 1)) %>% 
        filter(param_start == "d") %>% 
        mutate(percent_disart_median = 1 - exp(-X50.))
  sink(paste0(der_dat_spec_path, "/box_decay_rate.txt"))
  print(d)
  sink()


#  Calculate balance of boxes decaying before/after survey ---------------------
#TODO:
  
  