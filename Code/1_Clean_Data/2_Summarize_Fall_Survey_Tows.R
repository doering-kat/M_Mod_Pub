# Header -----------------------------------------------------------------------
# 
# Check number of replicate tows per site an dyear in the cleaned fall survey
# data, and samples that were different vols than half bu cultch..
# 
# In particular, I interested in how many tows per site are included in the cleaned
# dataset. 
# 
# Also, we know the Disease Bars and Key Bars are sampled with 2 replicate
# tows, whereas most other sites are sampled with 1 tow, so want to count the 
# number of bars in these fixed site subsets.
#
# This information was included in description of the dataset.

# Written  by Kathryn Doering
#

# load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# Load data --------------------------------------------------------------------
# Cleaned fall survey data
dat <- read.csv("./Derived_Data/1_Clean_Data/1_Clean_Fall_Survey/fall_survey_half_bu.csv")
# Fall survey bar information
bar_dat <- read.csv("./Data/DNR_Fall_survey_May_8_2018/BarInfo.csv") 
# Save output ------------------------------------------------------------------
save_der_path <- "./Derived_Data/1_Clean_Data/2_Summarize_Fall_Survey_Tows"
dir.create(save_der_path)

# Summarize # tows/site/year ---------------------------------------------------
# 
# Calculate number of tows by site and year.
tows_yr_bar <- dat %>% 
                select(SampleYr, ID) %>% 
                group_by(SampleYr, ID) %>% 
                count() %>% 
                ungroup()
hist(tows_yr_bar$n) #make histogram

#count number of site/yr combos with each number of tows
sum_t_y_b <- tows_yr_bar %>% 
                group_by(n) %>% 
                count() %>% 
                arrange(n)
#convert to a percent and cumulative percent
total_tows <- sum(sum_t_y_b$nn)
sum_t_y_b <- sum_t_y_b %>% 
               mutate(perc_nn = 100*nn/total_tows)

# get the cumulative  percent
cum_sum_nn <- cumsum(sum_t_y_b$perc_nn)
sum_t_y_b <- bind_cols(sum_t_y_b, data.frame(cumsum = cum_sum_nn)) %>% 
                rename(n_tows = n, n_ID_yr = nn, n_ID_yr_per = perc_nn, n_ID_yr_cum_per = cumsum)


# count # fixed survey sites ---------------------------------------------------

# some DzBars and Key Bars Overlap ("Key Bars" are listed as "SpatKeyBar in this
# data set, while "Disease Bars" are listed as DzKeyBar)
Bars_2_rep <- filter(bar_dat, DzKeyBar == "Yes" | SpatKeyBar == "Yes")

# count number of bars that are fixed sample survey sites
Bars_2_rep %>% 
  select(ID) %>% 
  unique() %>% 
  count()

# count how often > half bu subsamples occured ---------------------------------
# We normalized the data to half bu subsample, but rarely tows are conducted 
# where a half bu of cultch was not collected. These are noted in the dataset
# 
total_n <- nrow(dat) #total samples
subsamples <- unique(dat$SubSample) # the different subsample options
# remove half bu
subsamples <- subsamples[!(subsamples == "1/2 MD Bushel")]
# get all subsamples different than a half.
less_half_bu_n <- dat %>% 
  filter(SubSample %in% subsamples) %>% 
  count()
# find percent of samples that had a subsample vol different than 0.5 bu cultch.
less_half_bu_per <- 100*less_half_bu_n/total_n
less_half_bu_per

# save derived data ------------------------------------------------------------

# save summary data for number of replicate tows
write.csv(sum_t_y_b, paste0(save_der_path, "/Fall_Survey_Num_Rep_Tows.csv"), row.names = F)
# save summary data for Bars that are disease or Key Bars (the fixed sites that are
# sampled with 2 replicate tows)
write.csv(Bars_2_rep, paste0(save_der_path, "/Fall_Survey_Fixed_Site_BarInfo.csv"), row.names = F)

