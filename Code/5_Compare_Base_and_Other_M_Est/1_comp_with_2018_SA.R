# Header -----------------------------------------------------------------------
# 
# Compare M from Bayesian model with the SA model results.
# Also get info about "R_eff" from SA model results.
# 
# Written by Kathryn Doering
# Load packages and set options ------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
save_files <- T # True if want to create new files, False if not.
# Read in data -----------------------------------------------------------------
# Data from the Bayesian model, median by year and NOAA code. anualized rates.
Bayes_M <-  read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")
# Data for the stock assessment models - contains M.
#csv containting SA estimates
SA_mod_dat<- read.csv("./Data/TMBresults.csv")
# Add regions for plotting
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/5_Compare_Base_and_Other_M_Est"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/1_comp_with_2018_SA")
fig_gen_path <- "./Figs/5_Compare_Base_and_Other_M_Est"
fig_spec_path <- paste0(fig_gen_path, "/1_comp_with_2018_SA")
# make the folders
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# Get SA M ---------------------------------------------------------------------

NOAA <- unique(SA_mod_dat$NOAACode) #get the complete list of NOAA codes used in the model
M_SA_dat <- data.frame()
fyear <-  1999
lyear <- 2017
for(n in NOAA){
  tmp_M_SA_dat <- SA_mod_dat %>% 
    filter(NOAACode == n) %>% #results for the NOAA code
    filter(Name == "log_M")%>%                 # results for the specific parameter
    mutate(Year = (fyear+1):lyear)  %>%                # add a column for year
    mutate(LogInM = Estimate) %>%                 # rename Estimate to LogPred by creating a new col
    select(-Estimate) %>%                          # remove Estimate column
    mutate(LogInMSE = StError) %>%                # rename StError to LogPredSE by creating a new col
    select(-StError) %>%                           # Remove StError column
    mutate(MaxLogInM = LogInM + 2*LogInMSE) %>% # get top approx. 95% CI
    mutate(MinLogInM = LogInM - 2*LogInMSE) %>% # get bottom approx 95% CI
    mutate(InM = exp(LogInM)) %>%                # Back transform to get Instantaneous M
    mutate(MaxInM = exp(MaxLogInM)) %>%          # back transform top approx 95% CI Instan. M
    mutate(MinInM = exp(MinLogInM)) %>%          # back transform bottom approx 95% CI INstan.M
    mutate(M_SA = 1-exp(-InM) ) %>%               #convert to annual M
    mutate(MaxM_SA= 1-exp(-MaxInM)) %>% 
    mutate(MinM_SA = 1-exp(-MinInM)) %>% 
    #mutate(Var = "M") %>%                       # insert a better name for the variable
    #mutate(SA_Year = Year) %>% 
    select(NOAACode, Year, M_SA, MinM_SA, MaxM_SA) # remove the old nam use when plotting:
  
  M_SA_dat <- bind_rows(M_SA_dat,tmp_M_SA_dat) #bind with all the data.
}

# rename NOAA code
M_SA_dat <- rename(M_SA_dat, NOAA_code = NOAACode)
# save as .csv
if(save_files == T){
  write.csv(M_SA_dat, paste0(der_dat_spec_path, "/SA_Model_Annual_M.csv"), row.names = F)
}


# Get Baysian model M ----------------------------------------------------------
Bayes_M <- Bayes_M %>% 
             select(NOAA_code, Year, X50., X2.5., X97.5.) %>% # only select necessary cols
             rename(Bayes_M_Med = X50.) %>% # rename the cols
             rename(MinM_Bayes = X2.5.) %>% 
             rename(MaxM_Bayes = X97.5.)
# Compare SA model M and Bayesian model -----------------------------------------
# put into 1 dataframe
# Bind dataframes
Bayes_SA_M <- left_join(Bayes_M, M_SA_dat, by = c("NOAA_code", "Year")) %>% #joining by "NOAA_code" and "Year"
  na.omit()
#add regions
Bayes_SA_M <- left_join(Bayes_SA_M, regions, by = "NOAA_code")

# make NOAAcode into character instead of numeric
Bayes_SA_M$NOAA_code <- as.character(Bayes_SA_M$NOAA_code)

# #Create a scatterplot of point estimates
# with(Bayes_SA_M, plot(M_SA~Bayes_M_Med))
# abline(a = 0, b = 1, col = "red")

summary(lm(Bayes_SA_M$M_SA~Bayes_SA_M$Bayes_M_Med))
#write correlation to file
#
if(save_files==T){
  sink(paste0(der_dat_spec_path, "/Correlation_Bayes_and_SA_Models.txt"))
  cat("The correlation between the Stock Assessment Model (2018) and the Bayesian
    Model (2018) for common regions and years was: ", cor(Bayes_SA_M$M_SA,Bayes_SA_M$Bayes_M_Med))
  sink()
}

# view correlation.
cor(Bayes_SA_M$M_SA,Bayes_SA_M$Bayes_M_Med) #correlation = 0.6745

ggplot(data = Bayes_SA_M, aes(x = Bayes_M_Med, y = M_SA)) +
  geom_point(aes(color = Region_name))+
  geom_smooth(se = F)+#method = lm)+
  geom_abline(slope = 1, intercept = 0)+
  ylab("Stock assessment natural mortality")+
  xlab("Bayesian model natural mortality")+
  theme_classic()
if(save_files == T){
  ggsave(paste0(fig_spec_path, "/SA_Bayes_M_comparison_overall_loess.png"),device = "png", 
    units = "in", width = 12, height = 8)
}

ggplot(data = Bayes_SA_M, aes(x = Bayes_M_Med, y = M_SA)) +
  geom_point(aes(color = Region_name))+
  geom_smooth(aes(color = Region_name), se = F)+#method = lm)+
  geom_abline(slope = 1, intercept = 0)+
  ylab("Stock assessment natural mortality")+
  xlab("Bayesian model natural mortality")+
  theme_classic()
if(save_files == T){
  ggsave(paste0(fig_spec_path, "/SA_Bayes_M_comparison_regional_loess.png"),device = "png", 
    units = "in", width = 12, height = 8)
}

# compute average difference between the SA and Bayesian model results. 
avg_diff <- Bayes_SA_M %>% 
              mutate(Diff = (M_SA - Bayes_M_Med)) %>% #  different
              summarise(avg = mean(Diff))

if(save_files==T){
  sink(paste0(der_dat_spec_path, "/Avg_M_Bayes_and_SA_Models.txt"))
  cat("The average difference between the Stock Assessment Model (2018) and the Bayesian Model (2019)\nfor common regions and years was (neg means Bayesian model was higher): ", avg_diff$avg)
  cat("\nAverage median M in the SA model: ",mean(Bayes_SA_M$M_SA) )
  cat("\nAverage median M in the Baysian model: ",mean(Bayes_SA_M$Bayes_M_Med) )
  sink()
}
# save the comparison-----------------------------------------------------------
if(save_files == T){
  write.csv(Bayes_SA_M, paste0(der_dat_spec_path, "/Bayes_SA_M_comparison.csv"))
}

# compare estimates of efficiency ratio ----------------------------------------
# get from SA_mod_dat
log_q_est <- filter(SA_mod_dat, Name == "log_q")
log_q_est$stage <- c("Spat", "Adult_Live", "Adult_Box") # verified order in model

R_eff_SA <- log_q_est %>%
              filter(stage != "Spat") %>% 
              mutate(Nominal_Estimate = exp(Estimate)) %>%  # convert from log scale
              select(Name, NOAACode, stage, Nominal_Estimate) %>%
              spread(stage, Nominal_Estimate) %>%
  #is the ratio of the efficiency of live oysters to the efficiency of boxes 
              mutate(R_eff = Adult_Live/Adult_Box)
# plot to examine
hist(R_eff_SA$R_eff)
plot(R_eff_SA$R_eff ~ R_eff_SA$NOAACode)
R_eff_sum <- summary(R_eff_SA$R_eff) # more helpful to compare to Bayesian model

if(save_files == T) {
  write.csv(R_eff_SA, file.path(der_dat_spec_path, "SA_R_eff_values.csv"))
  sink(paste0(der_dat_spec_path, "/SA_R_eff_summary.txt"))
  cat("summary of the R effective equivalents calculated from the stock assessment ",names(R_eff_sum), "\n", R_eff_sum)
  sink()
}
