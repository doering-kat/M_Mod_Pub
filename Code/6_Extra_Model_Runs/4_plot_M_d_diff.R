# Header -----------------------------------------------------------------------
# Plot difference between the base model and the sensitivity models.
# include the model reruns for the models for which it was necessary.

# Look at how M changes among models and d
# Written by Kathryn Doering

# Load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------
# sensitivity models summary for M 
mod_sum <- read.csv("./Derived_Data/6_Extra_Model_Runs/2_get_mod_summaries/all_mod_summary.csv") # originals

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/6_Extra_Model_Runs"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/4_plot_M_d_diff")
# for figures
fig_gen_path <- "./Figs/6_Extra_Model_Runs"
fig_spec_path <- paste0(fig_gen_path, "/4_plot_M_d_diff")
# make the folders 
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)
dir.create(fig_gen_path)
dir.create(fig_spec_path)

# Manipulate model summaries ---------------------------------------------------

# Replace intial model runs with the reruns.
# delete intial model runs
final_mod_sum <- mod_sum
# change column order to match and delete X col to match col names with mod_rerun_sum_df cols.
final_mod_sum <- final_mod_sum %>% 
                   select(-X) %>% 
                   select(param, mean:Rhat, Model)

#This is the final dataframe to use in plotting.

# Make into a nested df --------------------------------------------------------
sum_nested <- final_mod_sum %>% 
                group_by(Model) %>% 
                nest()

# Calc diff for d --------------------------------------------------------------
# Find how different d is from the base  model. 

# Start by plotting the d = on the same plot.
d <- final_mod_sum %>% 
      mutate(param_1 = substr(param, 1, 1)) %>% 
      filter(param_1 == "d")

base_median <- d[d$Model == "Base", "X50."]
# calculate differences from other models 
d %>% 
  filter(Model != "Base") %>% 
  mutate(diff = X50.- base_median) %>% 
  select(Model, diff)

ggplot(d, aes(x = Model, y = X50.))+
  geom_errorbar(aes(ymin = X2.5., ymax=X97.5.))+
  geom_crossbar(aes(ymax = X25., ymin = X75.),fill = "gray")+
  xlab("Model")+
  ylab("d")+
  theme_classic(base_size = 7)
# save plot
ggsave(paste0(fig_spec_path, "/d_dist_mods.png"), device = "png", width = 6, height = 4, units = "in")
# pretty similar
# 
# Find difference from base model (median? )- unless there is a better way to do this
d_base <- filter(d, Model == "Base")
d <-  d %>% 
        mutate(d_diff = X50. - d_base$X50.) %>%
        mutate(percent_d_diff = 100*d_diff/d_base$X50.) #Calculate percent difference.
# look at the fractions disarticulating implied from the models. 
d <- d %>% 
  mutate(frac_disart = 1-exp(-X50.)) # not that differnet (for now)
# save the dataframe.
write.csv(d, paste0(der_dat_spec_path, "/d_diff.csv"))

# Calc diff for M --------------------------------------------------------------

Split_to_Yr_Reg <- function(expr, var = "yr") {
  # check input
  if(!var %in% c("yr","reg")) stop("var must 'yr' or 'reg'")
  #split the extpression
  # remove first part
  tmp_str <- strsplit(expr, split = "\\[") %>% 
    purrr::map(~.x[2]) %>% 
    purrr::flatten_chr()
  # separate year and region numbers (save year)
  if(var == "yr"){
    tmp_str <- strsplit(tmp_str, split = ",") %>% 
      purrr::map(~.x[1]) %>%  # get first elements of the string
      purrr::flatten_chr()
  } else { # process for region.
    tmp_str <- strsplit(tmp_str, split = ",") %>% 
      purrr::map(~.x[2]) %>%  # get second elements of the string
      purrr::flatten_chr()
    #remove the last character (])
    tmp_str <- substr(tmp_str, start = 1, stop = nchar(tmp_str)-1)
    
  }
  return(tmp_str)
}

sum_M <- final_mod_sum %>% 
            filter(str_detect(param, "M")) %>%
            mutate(Year = Split_to_Yr_Reg(param, var = "yr")) %>% 
            mutate(Region = Split_to_Yr_Reg(param, var = "reg"))
            
sum_M_base <- filter(sum_M, Model == "Base") %>% 
                rename(base_med = X50.) %>% 
                select(Year, Region, base_med )
sum_M_sens <- filter(sum_M, Model != "Base") %>% 
                 rename(sens_med = X50.) %>% 
                 select(Model, Year, Region, sens_med)

Med_diff <- left_join(sum_M_sens, sum_M_base, by = c("Year", "Region")) %>% 
                mutate(diff = sens_med - base_med)

# likely too much to look at by year and region
Mean_diff_NOAA <- Med_diff %>% 
                    group_by(Model, Region) %>% 
                    summarize(mean_diff = mean(diff))
# look at means by region
ggplot(Mean_diff_NOAA, aes(x = Region, y = mean_diff))+
  geom_col(aes(fill = Model), position = "dodge")+
  theme_classic()

# save plot and dataframe
ggsave(paste0(fig_spec_path, "/Mean_diff_M_NOAA.png"),device = "png", width = 6, height = 4, units = "in")
write.csv(Mean_diff_NOAA, paste0(der_dat_spec_path, "/Mean_diff_M_NOAA.csv"))

# look at means by year
Mean_diff_Year <- Med_diff %>% 
                    group_by(Model, Year) %>% 
                    summarize(mean_diff = mean(diff))

ggplot(Mean_diff_Year, aes(x = Year, y = mean_diff))+
  geom_col(aes(fill = Model), position = "dodge")+
  theme_classic()
# save plot 
ggsave(paste0(fig_spec_path, "/Mean_diff_M_Yr.png"),device = "png", width = 6, height = 4, units = "in")
# save the data
write.csv(Mean_diff_Year, paste0(der_dat_spec_path, "/Mean_diff_Year.csv"))

# look at average difference and save output. 
avg_M_diff <- Med_diff %>% 
                  group_by(Model) %>% 
                  summarize(mean_of_meds = mean(diff))
# save
write.csv(avg_M_diff, paste0(der_dat_spec_path, "/avg_M_med_diff.csv"))

# Make histograms --------------------------------------------------------------
# histograms of differences between the base model and sensitivity mod run's median
# M for each M estimate. 
# Perhaps create hists for the 3 models to better visualize outliers.
hist(Med_diff$diff) # this is for all of the models - maybe also look at models
# individually?
model_names <- unique(Med_diff$Model)
png(paste0(fig_spec_path, "/hist_diff_med_M.png"), res = 300, height = 4, width = 6, units = "in")
par(mfrow = c(2,2))
for(i in model_names){
  hist(Med_diff[Med_diff$Model == i, "diff"], main = i, xlab = "Diff from base in Median M")
}
dev.off()

