# Header -----------------------------------------------------------------------
# 
# Compare results from Damiano and Wilberg 2019 with these results
# Load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
save_files <- T # True if want to create new files, False if not.

# Load data --------------------------------------------------------------------
# Instantanous M for choptank river complex from Damiano and Wilberg 2019
M_Chop_DW <- read.csv("./Data/M_Damiano_and_Wilberg_2019.csv")
# Data from the Bayesian model, median by year and NOAA code. anualized rates.
Bayes_M <-  read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")
# Add regions for plotting
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Specify and Create output paths ----------------------------------------------
# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/5_Compare_Base_and_Other_M_Est"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/2_comp_with_damiano_wilberg_2019")
fig_gen_path <- "./Figs/5_Compare_Base_and_Other_M_Est"
fig_spec_path <- paste0(fig_gen_path, "/2_comp_with_damiano_wilberg_2019")
# make the folders
dir.create(der_dat_gen_path) # should already exist
dir.create(der_dat_spec_path)
dir.create(fig_gen_path) # should already exist
dir.create(fig_spec_path)

# Clean DW M ----------------------------------------------------------------
# Get in same form as Bayes_M.
# make tidy
M_Chop_DW_tidy <- tidyr::gather(M_Chop_DW, "NOAA_name", "M_inst", 2:ncol(M_Chop_DW))
# Modify names
name_lookup <- data.frame(NOAA_name = colnames(M_Chop_DW)[-1],
                          NOAA_code = c("537", "137", "237", "337", "637", 
                                        "53o","53c", "437o", "437c")
                          )
name_lookup # print to make sure it matches, as the key just created was hard coded.

# create a NOAA code column using name_lookup in M_Chop_DW_tidy
M_Chop_DW_tidy <- left_join(M_Chop_DW_tidy, name_lookup)

# remove NAS and open/closed areas - hard coded, may be able to use reg exp.
M_DW_comp <- M_Chop_DW_tidy %>% 
               filter(NOAA_code %in% c("537", "137", "237", "337", "637")) %>% 
               filter(!is.na(M_inst)) %>% 
               mutate(NOAA_code = as.integer(NOAA_code)) %>% 
               select(Year, NOAA_code, M_inst) %>% 
               mutate(M_A_DW = 1 - exp(-M_inst)) %>% 
               rename(M_inst_DW = M_inst)
# clean up Bayesian ------------------------------------------------------------
# Compare median values. Convert median to the instantaneous scale to compare.
Bayes_df <- Bayes_M %>% 
              mutate(M_inst_Bayes = -log(1-X50.)) %>% 
              rename(M_A_Bayes = X50.) %>% 
              select(Year, NOAA_code, M_inst_Bayes, M_A_Bayes)
              
# Merge datasets ---------------------------------------------------------------
comp_df <- left_join(M_DW_comp, Bayes_df) %>%  # join by Year and NOAA_code
             na.omit()
# Compare ----------------------------------------------------------------------
# plot
ggplot(comp_df, aes(x = Year, y = M_inst_DW))+
  geom_line() +
  geom_line(aes(x = Year, y = M_inst_Bayes), color = "blue")+
  facet_wrap(~NOAA_code, scales = "free_x") +
  theme_classic()
ggsave(paste0(fig_spec_path, "/M_comp_DW_blk_Bayes_blue_inst.png"), width = 6, heigh = 4)
# Annual
ggplot(comp_df, aes(x = Year, y = M_A_DW))+
  geom_line() +
  geom_line(aes(x = Year, y = M_A_Bayes), color = "blue")+
  facet_wrap(~NOAA_code, scales = "free_x") +
  theme_classic()
ggsave(paste0(fig_spec_path, "/M_comp_DW_blk_Bayes_blue_inst.png"),width = 6, height = 4)

# Correlate by year, calculate avg diff
sink(paste0(der_dat_spec_path, "/DW_Bayes_comp_cor.txt"))
print("Instantatneous correlation in Choptank")
cor(comp_df$M_inst_DW, comp_df$M_inst_Bayes)
print("Annualized M correlation in Choptank region")
cor(comp_df$M_A_DW, comp_df$M_A_Bayes)
print("Average diff annualized M: Damiano - Bayesian")
# compute average difference - damiano - bayesian.
avg_diff <- comp_df %>% 
  mutate(Diff = (M_A_DW - M_A_Bayes)) %>%
  summarise(avg = mean(Diff))
avg_diff$avg
sink()


# cor = 0.758. Pretty high. 
# Will need to double check this analysis, and run finally. Decide how to include
# in paper ( look at script 1 and manuscript )

