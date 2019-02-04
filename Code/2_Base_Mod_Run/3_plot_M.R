# Header -----------------------------------------------------------------------
# Plot natural mortality (M) from the model in various ways. Plot also to 
# compare with the box count method. 
# 
# Written 17 Jan 2019 by Kathryn Doering
# 
# TODO: 
# make plots into a gif from R
# convert objects into lat/lon proj so that x/y axes are not in northing and easting.
# Think about other ways to visualize these results? (see last section for some thoughts.)

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

# save the plots individually as png -------------------------------------------
fig_names <- names(plots$plot)
file_names <- paste0(fig_spec_path, "/NOAACode_", fig_names, "_M.png")
purrr::map2(file_names, plots$plot,  ggplot2::ggsave, device = "png",  height = 4.58, width = 6.06)

# Layout M by NOAA code for each region using Cowplot --------------------------
#make plots using cowplot as done with the reference point plots. use the same dimensions.

# Plot_By_Region function
# For a specified region, make all the ggplots in the same png.
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
  png(paste0(file_path, "/", R_region_name,"_",filetext, "_M.png"), res = 300, width = 6.5*1.5, height = plot_height*1.5, units = "in")
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

# save the line plots individually as png --------------------------------------
fig_names_line <- names(plots_line$plot)
file_names_line <- paste0(fig_spec_path, "/NOAACode_", fig_names_line, "_lineplot_M.png")
purrr::map2(file_names_line, plots_line$plot,  ggplot2::ggsave, device = "png",  height = 4.58, width = 6.06)

# Layout M by NOAA code line plots for each region using Cowplot ---------------
#make plots using cowplot as done with the reference point plots. use the same dimensions.

#create the plots by broad regions( make 1 per region)
region_names <- unique(regions$R_region) #get region names
# use Plot_By_Region function make the plots and save.
purrr::map(region_names, ~Plot_By_Region(R_region_name = .x, all_plots_df = plots_line, file_path = fig_spec_path, filetext = "lineplot"))
# Plot median by year on CB map ------------------------------------------------
# Get annualized median by year and NOAA code values
# Save in object M_df
# Use these to plot. Below is older code, need to modify: 

# Manipulate Data
# rename columns and make sure NOAA code is a character in same form as spatial 
#   objects.
M_map_df <- M_dat %>%  #rename
              select(Year, NOAA_code, `X50.`) %>% # select only necessary cols.
              rename(NOAACODE = NOAA_code, M_annual = `X50.`) %>%  # rename cols
              mutate(NOAACODE = as.character(NOAACODE)) %>% #convert to character
              # change NOAA codes so it matches with spatial objects
              mutate(NOAACODE = ifelse(nchar(NOAACODE) == 3, NOAACODE, 
                ifelse(nchar(NOAACODE) == 2, paste0("0", NOAACODE),
                  ifelse(nchar(NOAACODE) == 1, paste0("00", NOAACODE), 
                    NOAACODE)))) 

# Plot_M_Map: a function to plot natural mortality values on a spatial object for
  # a given year. Function inputs are:
# M, a dataframe with columns Year, M_annual, and NOAACODE
# Yr, a single integer value with the year of M_annual to plot.
# NOAA, an sp spatial polygon data.frame with the NOAACODE polygons.
# lab, a single logical value. True if NOAA code labels are desired on the plot,
#   or False if no labels are wanteed.
# NOAAlab, an sp spatial points data.frame including the labels and where they should be placed.
# axes: a signle logical value, true if want axes in northing and easting, false if not.
# axes_lab: a single logical value, true if want x and y axis labels, false if not.
# title: a single logical value, true if want the year as the title on the plot.
# filepath: specify where the plots should be made. Does not include the filename.
# filename: an optional name for  the file. should have extension ".png"
# palette: a character vector of color names. length should be 1 less than palette breaks.
# palette_breaks: a numeric vector specifying breaks in the scalebar. This can be 
# specified or left as optional. If not specified, function will calculate by scaling
# equally using the number of categories in the palette between 0 and 1.
# returns: an sp plot object (also creates and saves plot in a graphic device.)

Plot_M_Map <- function(M, colname = "M_Annual" , NOAA, lab = F, NOAAlab = NULL, 
                       axes = F, axes_lab = F, title = T, title_text = "Year here", 
                       filepath = ".", filename = NA, 
                       palette = rev(RColorBrewer::brewer.pal(n = 10, name = "RdYlBu")), 
                       palette_breaks = NULL) {
  # List required packages -----------------------------------------------------
  require(RColorBrewer)
  require(sp)
  require(grid)
  require(tidyverse)
  # Make spatialpolygondataframe -----------------------------------------------
  
  #add natural mortality values to the dataframe.
  NOAA@data <- dplyr::left_join(NOAA@data, M, by = "NOAACODE")
  
  # Make the map using spplot --------------------------------------------------
  # The color palette 
  if(is.null(palette_breaks)){
    palette_breaks <- seq(from = 0, to = 1, length.out = (length(palette)+1))
  }
  # check inputs
  if (length(palette_breaks) != length(palette)+1){
    stop("palette_breaks must have length() 1 more than palette_n value")
  }
  
  # The NOAA code labels (if desired)
  if(lab == T){
    sl_labels <- list("sp.text", coordinates(NOAAlab), NOAAlab@data$Labels, cex = 0.7)
  }
  # make the list for axes or not
  if (axes == T) {
    scales_list <- list(draw = T)
    par_settings_list <- list(fontsize = list(text = 20), axis.line = list(col = "black"))
  } else {
    scales_list <- list(draw = F)
    par_settings_list <- list(fontsize = list(text = 20), axis.line = list(col = "transparent"))
  }
  # open the graphics device.
  if(is.na(filename)){
    png(filename = paste0(filepath, "/M_map.png"), width = 9, height = 11, units = "in", res = 300)
  } else{
    png(filename = paste0(filepath,"/", filename), width = 9, height = 11, units = "in", res = 300)
  }
  # create the pplot
  plot <- sp::spplot(NOAA, #spatial object to plot
                   zcol =  colname, #column name in the dataframe to fill.
                   # year in title, if desired
                   main = ifelse(title == T, list(label = title_text, cex = 1.2), list(NULL)),
                   scales = scales_list,
                   # plot limits:
                   ylim = c(NOAA@bbox[2,1] - 3000, NOAA@bbox[2,2] + 3000), #+51000),
                   xlim = c(NOAA@bbox[1,1] - 5000, NOAA@bbox[1,2] + 10000),
                   # axis labels 
                   ylab = ifelse(axes_lab == T, "Northing (m)", list(NULL)),
                   xlab = ifelse(axes_lab == T, "Easting (m)", list(NULL)),
                   col.regions = palette, #specify the colors
                   at = palette_breaks, #specify the breaks fro the scale bar.
                   col = "gray30", #specify the noaa code outline color
                   sp.layout = ifelse(lab == T, list(sl_labels), list(NULL)),
                   par.settings = par_settings_list
                   )
  plot <- plot + latticeExtra::layer_(sp.polygons(NOAA, fill='gray70')) # fill in other NOAA codes with gray.
  print(plot) # to make visible in device
  dev.off()
  return(plot)
}

# Run plot function to get maps of M by year.
for (y in 1991:2017) {
  tmp_M <- dplyr::filter(M_map_df, Year == y) # just M for the year.
  # define the color brewer palette.name))
  Plot_M_Map(M = tmp_M, NOAA = NOAA, colname = "M_annual", 
            filepath = fig_spec_path, filename = paste0("M_", y, ".png"), 
            title_text = as.character(y))
}

# TODO: make into a gif from R!

# Plot Mean and sd of median M on CB map ---------------------------------------
# Use the Plot_M_Map function,
# Calculate mean and sd of median values by NOAA code
Avg_SD_M_df <- M_map_df %>% 
                group_by(NOAACODE) %>% 
                summarize(Mean = mean(M_annual), SD = sd(M_annual))

# plot mean
max_plot_avg <- ceiling(10*max(Avg_SD_M_df$Mean))/10 # the maximum value to plot
n_plot_avg <- as.integer(max_plot_avg*10*2)
avg_pal <- RColorBrewer::brewer.pal(n = n_plot_avg, name = "Oranges")
avg_pal_breaks <- seq(from = 0, to = max_plot_avg, length.out = length(avg_pal)+1)
Plot_M_Map(M = Avg_SD_M_df, NOAA = NOAA, colname = "Mean", filepath = fig_spec_path, 
           filename = "M_Med_Mean_Map.png", title_text = "Mean", palette = avg_pal, palette_breaks = avg_pal_breaks )

# plot SD
max_plot_SD <- ceiling(10*max(Avg_SD_M_df$SD))/10 # the maximum value to plot
n_plot_SD <- as.integer(max_plot_SD*10*2)
SD_pal <- RColorBrewer::brewer.pal(n = n_plot_SD, name = "Greens")
SD_pal_breaks <- seq(from = 0, to = max_plot_SD, length.out = length(SD_pal)+1)
Plot_M_Map(M = Avg_SD_M_df, NOAA = NOAA, colname = "SD", filepath = fig_spec_path, 
  filename = "M_Med_SD_Map.png", title_text = "SD", palette = SD_pal, palette_breaks = SD_pal_breaks )

# TODO: may need to add key, and axes; may need to switch to converting objects to lat/long (change function.)

# other plots ---------------------------------------------------------------

# Think of other ways to plot mortality (any way to synthesize in the paper?)---
# Individual plots take up a lot of space! Perhaps there is a better way to show
# them? (or: maybe just show a few key M comparison plots (include the rest in 
# an online supplement, and just show results of the dfa?))

#OR, maybe make a plot like the dfa input, but separate by region? This would 
#reduce the number of plots, but it may be difficult to see for other regions.
#OR, perhaps if the main take home is the DFA, maybe it doesn't matter to show
#the absolute plots, and just showing the standardized M plot and the 
#Mean/sd over years by region will do..... think more about this!! 

