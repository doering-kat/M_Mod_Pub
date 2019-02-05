# Header -----------------------------------------------------------------------
# Plot natural mortality (M) from the model by year. Plot also to 
# compare with the box count method. 
# 
# Written 17 Jan 2019 by Kathryn Doering

# Clear global env--------------------------------------------------------------
# 
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(rstan)
library(cowplot)
library(sp)
library(RColorBrewer)
library(grid)
library(latticeExtra)
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

# to create maps of M data
NOAA <- readRDS("./Data/CB_spatial_objects/NOAA_CB.rds")
NOAAlab <- readRDS("./Data/CB_spatial_objects/NOAAlabels_CB.rds")

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
yr_last <- 2017
nyears <-  yr_last-yr_0  #number of years not including year 0.

# Get M data-----------------------------------------------------------------------

# Split_to_Yr_Reg:
# Function to get te region or year indices. 
# Currently works for any variables that are matrices in the model, where year 
#   are the row var (index starts at 1) and regions (NOAA codes, but with model 
#   notation, so indices starte at 1) are the column variables.
# Inputs are:
# expr: a character vector, of the form parameter_name[yr,reg], where 
#   parameter_name is the name of the parameter from the stan model and  yr and 
#   reg are the yr and reg indices for the parameter.
# var:  which can be "yr" or "reg", depending on which index is desired.
# This function returns: a vector (or single value, depending on input) that is 
# a string containing the yr or region number, as specified in the model.

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

# get estimates of natural mortality from the model
 M_mod <- mod_sum %>%
              rename(par = X) %>% # rename the col containing parameter and index
              filter(str_detect(par, "M")) %>% # get only the estimates for natural mortality
              mutate(ModYr = Split_to_Yr_Reg(par, var = "yr"))%>%  # put yr in separate col
              mutate(ModYr = as.integer(ModYr)) %>% 
              mutate(ModReg = Split_to_Yr_Reg(par, var = "reg")) %>% # put model region in separate col
              mutate(ModReg = as.integer(ModReg))

# Get Box count M estimates
  #calculate the box count method (DNR way)
  M_box <- raw_mod_dat %>% 
                  #filter(NOAACode == NOAA_vec[i]) %>%  # for region 1/ NOAA code 192
                  filter(ModYr > 0) %>%  # exclude year 0 data
                  mutate(SampleBoxCount = B/(L+B)) %>%  # do it the DNR way by calculating for a sample first
                  filter(!is.na(SampleBoxCount)) %>%  #remove any NANs.
                  group_by(ModYr, NOAACode) %>% # group data by year
                  summarize(BoxCount = mean(SampleBoxCount)) %>%  # take the avg of box count for each year. 
                  mutate(Year = ModYr + yr_0) # get actual year.
  
# Add real year (starting at yr_0) and NOAA code to the M data
ModReg_NOAA <- bar_reg_key %>% 
                  select(ModReg, NOAACode) %>% 
                  unique()
M_mod <- left_join(M_mod, ModReg_NOAA, by = "ModReg")
# join box count and model M estimates
M_dat<- full_join(M_mod, M_box, by = c("NOAACode", "ModYr")) %>% 
          rename(NOAA_code = NOAACode)
# add region information (broader regions, not the model regions that are at the
# NOAA code level)
M_dat <- left_join(M_dat, regions, by = "NOAA_code")

#add a NOAA code factors for plotting (useful for ordering plots)
M_dat$NOAA_code_fac <- factor(M_dat$NOAA_code, 
                              levels = regions$NOAA_code, 
                              labels = paste0("NOAA Code ", regions$NOAA_code, ":\n", regions$NOAA_Name_Short))
 
#export as .csv

write.csv(M_dat, paste0(der_dat_spec_path, "/M_dat_mod_boxcount.csv"), row.names = F)

# Plot model and box count M by NOAA code (boxplots) ---------------------------

#Make axis labels
lab <- as.character(1991:2017)
lab[c(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27)] <- "" #only label every 5. 

# make dataframe nested, and then use map function to create ggplots by NOAA
# code. Also added R_regions as a grouping variable, which is useful for making
# the regional panel plots.
plots <-  M_dat %>%
  group_by(NOAA_code_fac, R_region) %>%
  nest() %>%
  mutate(plot = map2(data, NOAA_code_fac, ~ggplot(data = .x) +
      # line connecting median model results
      geom_line(aes(x = Year, y = X50.),color = "#9ad0f3")+
      #whiskers for mort. model results
      geom_linerange(aes(x = Year, ymin = X2.5., ymax = X97.5.), color = "black")+ 
      # box for mortality model results.
      geom_crossbar(aes(x = Year, y = X50., ymin = X25., ymax = X75.), color = "black", fill = "#9ad0f3", width =0.5)+
      # points of box count mortality
      geom_point(aes(x = Year, y = BoxCount), shape = 21, fill = "red", color = "black", size =2)+
      #Need to modify below here to be specific for this plot:
      scale_y_continuous(expand = c(0.01,0), limits = c(0,1))+ # defined y axis limits
      scale_x_continuous(expand = c(0.01,0), #to get rid of the x axis space before the first year.
        breaks = 1991:2017,#c(2000,2005,2010,2015), #specify where labels go
        labels =  lab
      ) +
      theme_classic(base_size = 12)+
      theme(
            aspect.ratio = 4/6,
            plot.title = element_text(hjust = 0.5, size = 10), #center
            axis.title.x = element_blank(),         #No x axis lab
            axis.title.y = element_blank())+        #no y axis lab
      ggtitle(.y)))%>% # Add a title for each noaa code
  dplyr::arrange(NOAA_code_fac) # order the rows by the NOAA code factor

# add names to the ggplots by NOAA code
tmpnames <- arrange(M_dat, NOAA_code_fac) #order the NOAA code by factor
tmpnames <- unique(tmpnames$NOAA_code) #this should be the same order as the ggplots
names(plots$plot) <- tmpnames #assign these names to the plot

# save the plots individually as pdf -------------------------------------------
fig_names <- names(plots$plot)
file_names <- paste0(fig_spec_path, "/NOAACode_", fig_names, "_M_boxplot.pdf")
purrr::map2(file_names, plots$plot,  ggplot2::ggsave, device = "pdf",  height = 4.58, width = 6.06)

# Layout M by NOAA code for each region using Cowplot --------------------------
#make plots using cowplot as done with the reference point plots. use the same dimensions.

# Plot_By_Region function
# For a specified region, make all the ggplots in the same pdf.
# arguments are:
# all_plots_df: a nested dataframe which includes a column of ggplots called plot
# R_region_name: a character single value, the region to be plotted, which will 
#   be matched with values with the R_region column of all_plots_df.
# plot_title_text: A character single value, a title for the entire plot.
# file_path: a character single value. where to save the plot, does not include
#  the file name

Plot_By_Region <- function(all_plots_df, 
                           R_region_name,
                           plot_title_text ="", 
                           file_path, filetext = "boxplot") {
  # specify necessary packages
  require(dplyr)
  require(cowplot)
  reg_plots <- dplyr::filter(all_plots_df, R_region == R_region_name)
  reg_plot_list <- reg_plots$plot # get all plots in a list
  labs <- LETTERS[1:length(reg_plot_list)] # add letter labels on the plots
  #make the cowplot (no title or labels yet)
  tmp_plot_1 <- cowplot::plot_grid(plotlist = reg_plot_list,labels = labs, ncol = 3)
  # add years annoatation
  # specify sizes depending on nrows (size of the ouput graphic and relative 
  # heights for plots
  n_row <- ceiling(length(reg_plot_list)/3) # nrows in the final plot.
  # specify the relative heights of each plot_grid object, as well as the 
  # overall device height. This varies based upon 1-3 rows, assuming 3 plots
  # per row. This code will not work for more than 9 plots.
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
  # add title, and x and y axis labels to the cowplot.
  title      <- cowplot::ggdraw() + cowplot::draw_label(plot_title_text, x = 0.16, y = 0.5, fontface = "italic", size = 14)
  xaxislab   <- cowplot::ggdraw() + cowplot::draw_label("Year", size = 12)
  yaxislab   <- cowplot::ggdraw() + cowplot::draw_label("Annual M", size = 12, angle = 90)
  tmp_plot_2 <- cowplot::plot_grid(title,    tmp_plot_1, xaxislab, rel_heights = rel_heights, ncol = 1)
  # put together the final plot.
  final_plot <- cowplot::plot_grid(yaxislab, tmp_plot_2, ncol = 2, rel_widths = c(0.05, 1))
  
  #save the plot
  pdf(paste0(file_path, "/", R_region_name,"_",filetext, "_M.pdf"), width = 6.5*1.5, height = plot_height*1.5)
  print(final_plot)
  dev.off()
}

#create the plots by broad regions( make 1 per region)
region_names <- unique(regions$R_region) #get region names
# use Plot_By_Region function make the plots and save.
purrr::map(region_names, ~Plot_By_Region(R_region_name = .x, all_plots_df = plots, file_path = fig_spec_path ))
# M by NOAA lines ---------------------------------------------------------------
#Make axis labels
lab <- as.character(1991:2017)
lab[c(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27)] <- "" #only label every 5. 

# make dataframe nested, and then use map function to create ggplots by NOAA
# code. Also added R_regions as a grouping variable, which is useful for making
# the regional panel plots.
plots_line <-  M_dat %>%
  group_by(NOAA_code_fac, R_region) %>%
  nest() %>%
  mutate(plot = map2(data, NOAA_code_fac, ~ggplot(data = .x) +
      # 95% CIS
      geom_ribbon(aes(x = Year, ymin = X2.5., ymax = X97.5.), fill = "gray80")+
      # the 25t and 75th percentiles
      geom_ribbon(aes(x = Year, ymin = X25., ymax = X75.), fill = "gray60")+
      # line connecting median model results
      geom_line(aes(x = Year, y = X50.),color = "gray20")+
      # points of box count mortality
      geom_point(aes(x = Year, y = BoxCount), shape = 21, fill = "black", color = "black", size =2)+
      #Need to modify below here to be specific for this plot:
      scale_y_continuous(expand = c(0.01,0), limits = c(0,1))+ # defined y axis limits
      scale_x_continuous(expand = c(0.01,0), #to get rid of the x axis space before the first year.
        breaks = 1991:2017,#c(2000,2005,2010,2015), #specify where labels go
        labels =  lab
      ) +
      theme_classic(base_size = 12)+
      theme(
        aspect.ratio = 4/6,
        plot.title = element_text(hjust = 0.5, size = 10), #center
        axis.title.x = element_blank(),         #No x axis lab
        axis.title.y = element_blank())+        #no y axis lab
      ggtitle(.y)))%>% # Add a title for each noaa code
  dplyr::arrange(NOAA_code_fac) # order the rows by the NOAA code factor

# add names to the ggplots by NOAA code
tmpnames_line <- arrange(M_dat, NOAA_code_fac) #order the NOAA code by factor
tmpnames_line <- unique(tmpnames_line$NOAA_code) #this should be the same order as the ggplots
names(plots_line$plot) <- tmpnames_line #assign these names to the plot

# save the line plots individually as pdf --------------------------------------
fig_names_line <- names(plots_line$plot)
file_names_line <- paste0(fig_spec_path, "/NOAACode_", fig_names_line, "_M_lineplot.pdf")
purrr::map2(file_names_line, plots_line$plot,  ggplot2::ggsave, device = "pdf",  height = 4.58, width = 6.06)

# Layout M by NOAA code line plots for each region using Cowplot ---------------
#make plots using cowplot as done with the reference point plots. use the same dimensions.

#create the plots by broad regions( make 1 per region)
region_names <- unique(regions$R_region) #get region names
# use Plot_By_Region function make the plots and save.
purrr::map(region_names, ~Plot_By_Region(R_region_name = .x, all_plots_df = plots_line, file_path = fig_spec_path, filetext = "lineplot"))
