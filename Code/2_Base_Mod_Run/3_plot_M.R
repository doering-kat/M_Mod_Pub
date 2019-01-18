# Header -----------------------------------------------------------------------
# Plot natural mortality (M) from the model in various ways. Plot also to 
# compare with the box count method. 
# 
# Written 17 Jan 2019 by Kathryn Doering
# 
# TODO: 
# create median plots by year on map, 
# (find way to make into gif within R), 
# create plots of the mean/sd of median on map,
# Think about other ways to visualize these results? (EEE last section for some thoughts.)

# Clear global env--------------------------------------------------------------
rm(list = ls())

# Load packages -----------------------------------------------------------------
library(tidyverse)
library(rstan)
library(shinystan)
#To add maps:
library(rgdal)
library(maptools)
library(ggplot2)
#library(plyr)
library(maps) #later on in the script.
library(cowplot)
#Another option: using the maps package.
options(stringsAsFactors = F) #Change options.

#Functions to use:  (NEED?)
#source('./Code/Extract_and_plot_functions.R')

# Load data ---------------------------------------------------------------------
read_path <- "./Derived_Data/2_Base_Mod_Run/1_run_mod"
#Load model and data associated with it:
bar_reg_key <- read.csv(paste0(read_path, "/bar_reg_key.csv"))
raw_mod_dat <- read.csv(paste0(read_path, "/raw_dat.csv"))

mod_sum <- read.csv("./Derived_Data/2_Base_Mod_Run/2_check_convergence/mod_summary_all_chains.csv")
mod_dat <- readRDS(file = paste0(read_path, "/model_dat.rda"))

NOAA_vec <- read.csv(paste0(read_path, "/NOAA_vec.csv"))
NOAA_vec <- NOAA_vec$x

#NOAA code regions
regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/3_plot_M")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/3_plot_M")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
dir.create(fig_spec_path) 

# Define variables--------------------------------------------------------------
# years used in the model
yr_0 <- 1990
yr_last <- 2017 #Assume 2017 data have been entered
nyears <-  yr_last-yr_0  #nyears not including year 0.

# Get M data-----------------------------------------------------------------------


# functions to make region and yr splits. Currently works for any variables that
# are matrices in the model, where year are the row var and regions (NOAA codes)
# are the column Variables. The notatation, though are the model index notation
# Inputs are:
# expr, the variable with indices string to pull info from.
# var, which can be "yr" or "reg", depending on which index is desired.
# returns: a vector (or single value, depending on input) that is a string
# containing the yr or region number, as specified in the model.

split_to_yr_reg_2 <- function(expr, var = "yr"){
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

 M_mod <- mod_sum %>%
              rename(par = X) %>% # rename the col containing parameter and index
              filter(str_detect(par, "M")) %>% # get only the M estimates
              mutate(ModYr = split_to_yr_reg_2(par, var = "yr"))%>%  # put yr in separate col
              mutate(ModYr = as.integer(ModYr)) %>% 
              mutate(ModReg = split_to_yr_reg_2(par, var = "reg")) %>% # put reg in separate col
              mutate(ModReg = as.integer(ModReg))

# Get Box count estimates
  #calculate the box count method (DNR way)
  M_box <- raw_mod_dat %>% 
                  #filter(NOAACode == NOAA_vec[i]) %>%  # for region 1/ NOAA code 192
                  filter(ModYr > 0) %>%  # exclude year 0 data
                  mutate(SampleBoxCount = B/(L+B)) %>%  # do it the DNR way by calculating for a sample first
                  filter(!is.na(SampleBoxCount)) %>%  #remove any NANs.
                  group_by(ModYr, NOAACode) %>% # group data by year
                  summarize(BoxCount = mean(SampleBoxCount)) %>%  # take the avg of box count for each year. 
                  mutate(Year = ModYr + yr_0) # get actual year.

  # Add real year and NOAA code to the M data, and bind model M and box count M.
ModReg_NOAA <- bar_reg_key %>% 
                  select(ModReg, NOAACode) %>% 
                  unique()
  
M_mod <- left_join(M_mod, ModReg_NOAA, by = "ModReg")
# join box count and model M estimates
M_dat<- full_join(M_mod, M_box, by = c("NOAACode", "ModYr")) %>% 
          rename(NOAA_code = NOAACode)
# add region information
M_dat <- left_join(M_dat, regions, by = "NOAA_code")

#add factors for plotting
M_dat$NOAA_code_fac <- factor(M_dat$NOAA_code, 
                              levels = regions$NOAA_code, 
                              labels = paste0("NOAA Code ", regions$NOAA_code, ":\n", regions$NOAA_Name_Short))
 
# #export as .csv
# write.csv(M_dat, paste0("./models_and_derived_data/", folder_name, "/M_dat_mod_boxcount.csv"), row.names = F)

# Plot model and box count M by NOAA code --------------------------------------

# #Plot M
#Make axis labels
lab <- as.character(1991:2017)
lab[c(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27)] <- "" #only label every 5. 

plots <-  M_dat %>%
  group_by(NOAA_code_fac, R_region) %>%
  nest() %>%
  mutate(plot = map2(data, NOAA_code_fac, ~ggplot(data = .x) +
      geom_line(aes(x = Year, y = X50.),color = "#9ad0f3")+
      geom_linerange(aes(x = Year, ymin = X2.5., ymax = X97.5.), color = "black")+ #whiskers for mort. model results
      geom_crossbar(aes(x = Year, y = X50., ymin = X25., ymax = X75.), color = "black", fill = "#9ad0f3", width =0.5)+ #box for mort. model results
      geom_point(aes(x = Year, y = BoxCount), shape = 21, fill = "red", color = "black", size =2)+ # box count mortality.
      #Need to modify below here to be specific for this plot:
      scale_y_continuous(expand = c(0.01,0), limits = c(0,1))+
      scale_x_continuous(expand = c(0.01,0), #to get rid of the x axis space before the first year.
        breaks = 1991:2017,#c(2000,2005,2010,2015), #specify where labels go
        labels =  lab#c("2000-\n2001", "2005-\n2006", "2010-\n2011", "2015-\n2016")#labely by fishing season.
      )+
      theme_classic(base_size = 12)+
      theme(
        aspect.ratio = 4/6,
        plot.title = element_text(hjust = 0.5, size = 10), #center
        axis.title.x = element_blank(),         #No x axis lab
        axis.title.y = element_blank())+        #no y axis lab
      ggtitle(.y)))%>%
  dplyr::arrange(NOAA_code_fac)

#add names to the ggplots by NOAA code
tmpnames <- arrange(M_dat, NOAA_code_fac) #order the NOAA code by factor
tmpnames <- unique(tmpnames$NOAA_code) #this should be the same order as the ggplots
names(plots$plot) <- tmpnames #assign these names to the plot

fig_names <- names(plots$plot)
# Save plots individually
file_names <- paste0(fig_spec_path, "/NOAACode_", fig_names, "_M.png")
map2(file_names, plots$plot,  ggplot2::ggsave, device = "png",  height = 4.58, width = 6.06)

# Layout M by NOAA code for each region using Cowplot --------------------------
#make plots using cowplot as done with the reference point plots. use the same dimensions.


Plot_By_Region <- function(all_plots_df, R_region_name, plot_title_text ="", file_path){
  require(dplyr)
  reg_plots <- dplyr::filter(all_plots_df, R_region == R_region_name)
  reg_plot_list <- reg_plots$plot # get all plots in a list
  labs <- LETTERS[1:length(reg_plot_list)] # add leter labesl on the plots
  #make the cowplot
  tmp_plot_1 <- plot_grid(plotlist = reg_plot_list,labels = labs, ncol = 3)
  # add A years annoatation
  # specify sizes depending on nrows (size of the ouput graphic and relative 
  # heights for plots
  n_row <- ceiling(length(reg_plot_list)/3) #nrows in the final plot. 
  if (n_row == 1){
    rel_heights <-  c(0.15, 1, 0.09)
    plot_height <- 2.679
  } else if( n_row == 2){
    rel_heights <-  c(0.075, 1, 0.045)
    plot_height <- 4.839
  } else if (n_row == 3){
    rel_heights <-  c(0.05, 1, 0.03)
    plot_height <- 7
  } else {
    stop("This code is only set to handle up to 9 plots.")
  }
  title <- ggdraw() + draw_label(plot_title_text, x = 0.16, y = 0.5, fontface = "italic", size = 14)
  xaxislab <- ggdraw() + draw_label("Year", size = 12)
  yaxislab <- ggdraw() + draw_label("Annual M", size = 12, angle = 90)
  tmp_plot_2 <- plot_grid(title, tmp_plot_1, xaxislab, rel_heights = rel_heights, ncol = 1)
  final_plot <- plot_grid(yaxislab, tmp_plot_2, ncol = 2, rel_widths = c(0.05, 1))
  
  #save the plot
  png(paste0(file_path, "/", R_region_name, "_M.png"), res = 300, width = 6.5*1.5, height = plot_height*1.5, units = "in")
  print(final_plot)
  dev.off()
}

#create the plots by broad regions
region_names <- unique(regions$R_region) #get region names
#make the plots and save.
purrr::map(region_names, ~Plot_By_Region(R_region_name = .x, all_plots_df = plots, file_path = fig_spec_path ))

# Plot Mean and sd of median M on CB map ---------------------------------------

# Plot median by year on CB map ------------------------------------------------

# Think of other ways to plot mortality (any way to synthesize in the paper?)---
# Individual plots take up a lot of space! Perhaps there is a better way to show
# them? (or: maybe just show a few key M comparison plots (include the rest in 
# an online supplement, and just show results of the dfa?))

#OR, maybe make a plot like the dfa input, but separate by region? This would 
#reduce the number of plots, but it may be difficult to see for other regions.
#OR, perhaps if the main take home is the DFA, maybe it doesn't matter to show
#the absolute plots, and just showing the standardized M plot and the 
#Mean/sd over years by region will do..... think more about this!! 

