# Header -----------------------------------------------------------------------
# Run Base Model for the Bayesian M model.
# 
# This model calculates natural mortality on the NOAA code level for the Maryland
# Portion of Chesapeake Bay. 
# (NOTE: NOAA codes are statistical catch areas in the MD portion of Chesapeake Bay)
# 
# This version of the model includes corrections for boxes persisting for longer
# than 1 year, boxes that disarticulate before the box survey (contained in Rq) and
# for the difference in efficiency between boxes and live oysters (also contained
# in Rq)
# 
# Written 9 Jan 2019 by Kathryn Doering
# 
# Load Packages and set options ------------------------------------------------

library(tidyverse)
library(rstan)
#R stan options
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
my_seed <- 123
set.seed(my_seed) #seed set in R
write_files <- T # Make false if do not want to write any data to files.
# other options
options(stringsAsFactors = F)
#custom R functions.
source("./Code/Funs/mod_d_3_3_output_funs.R", echo = F)

# Modify params here -----------------------------------------------------------
# TODO: Add code here, or delete section

# Specify and Create output paths ----------------------------------------------
# 
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/1_run_model")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/1_run_mod")
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# Load Data ---------------------------------------------------------------------

# Fall dredge survey individual tows, all measured in half bushel cultch
dat <-  read.csv("./Derived_Data/1_Clean_Data/1_Clean_Fall_Survey/fall_survey_half_bu.csv")

# Specify input values ---------------------------------------------------------

#CHANGE NOAA codes here- want all possible NOAA codes
NOAA_list_init <- sort(unique(dat$NOAACode)) #make all the NOAA codes the initial
#list.

# Stick with these years to run the model due to the data available
yr_0 <- 1990
yr_last <- 2017

# min number of bars with complete time series a bar needs to be included in the model
n_ts <- as.integer(2)

# Modify data ------------------------------------------------------------------

# Select the complete timeseries of data to use. 
all_dat_ts <- SelectData(
    dat = dat, 
    NOAA_vec = NOAA_list_init, 
    yr_0 = yr_0, 
    yr_last = yr_last, 
    remove_0 = FALSE,
    n_ts = n_ts #min number of bars with complete time series required. 
)


NOAA_list <- sort(unique(all_dat_ts$NOAACode)) #some noaa codes may be eliminated.

# Make diagnostic plots (note: takes ~ 30 s to run)
PlotData(all_dat_ts = all_dat_ts, file_path = fig_spec_path, file_name = "Data_Plots.pdf")

#Get data sets to run the model as well as use for plotting later
output <- CreateModDataList(all_dat_ts = all_dat_ts)
mod_dat <- output$mod_dat

#Check the model data before proceeding to make sure R has been changed. 
#-------------------------------------------------------------------------------

#run model

mod_1 <- stan(
    file = './Code/model_d_3_3.stan', #will this fversion still work?
    iter = 4000, #Started with 4000 iterations based on runs in TS and CHoptank
    data = mod_dat,
    seed = 123, #or set at the top? Only can set 1 X per R session, I think.
    #init = inits,
    #control = list(adapt_delta = 0.99), #added because of divergent transitions.
    chains = 3
)

#see how long sampling took # TODO Move to next script. 
print(get_elapsed_time(mod_1)) #(Currently runs in ~3 min. for 3 NOAA codes.)

# Check convergence ------------------------------------------------------------
# TODO: Move to next script. 
summary <- summary(mod_1)$summary
# Make plots (M, lambda, beta_0 (regional), lambda, beta, beta_0 (bar))
# Check that Rhat values are below 1.1 for all variables
for (i in 1:nrow(summary)){
    if(summary[i,'Rhat']>=1.1){
        print(summary[i,'Rhat'])
    }
}

# Values of Rhat fine for all variables, which suggests convergence.

# # Check convergence
# summary <- summary(mod_2)$summary
# # Make plots (M, lambda, beta_0 (regional), lambda, beta, beta_0 (bar))
# # Check that Rhat values are below 1.1 for all variables
# for (i in 1:nrow(summary)){
#     if(summary[i,'Rhat']>=1.1){
#         print(summary[i,'Rhat'])
#     }
# }
# 
# Save data, model -------------------------------------------------------------
#save model and associated data 
saveRDS(mod_1, paste0(der_dat_spec_path,"/model.rda"))
saveRDS(mod_dat, paste0(der_dat_spec_path,"/model_dat.rda"))
write.csv(output$bar_key, paste0(der_dat_spec_path,"/bar_reg_key.csv"), row.names = F)
write.csv(output$raw_mod_dat, paste0(der_dat_spec_path,"/raw_dat.csv"),row.names = F)
write.csv(NOAA_list, file = paste0(der_dat_spec_path,"/NOAA_list.csv"), row.names = F )

