# Header -----------------------------------------------------------------------
# Plot M on a map, using lat and longitude
# Make these into pdf objects that could be used in a paper.
# Written by Kathryn Doering
# Load packages ----------------------------------------------------------------
library(tidyverse)
library(rstan)
library(sp)
library(RColorBrewer)
options(stringsAsFactors = F) #Change options.

# Load data ---------------------------------------------------------------------
# Natural mortality model and box count estimates
M_dat <- read.csv("./Derived_Data/2_Base_Mod_Run/3_plot_M/M_dat_mod_boxcount.csv")

# to create maps of M data
NOAA <- readRDS("./Data/CB_spatial_objects/NOAA_CB.rds")
NOAAlab <- readRDS("./Data/CB_spatial_objects/NOAAlabels_CB.rds")

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/8_plot_M_map_lat_long")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/8_plot_M_map_lat_long")
# make the folders (gen folders should already exist)
dir.create(der_dat_gen_path)
dir.create(der_dat_spec_path)
dir.create(der_dat_gen_path)
dir.create(fig_spec_path) 

# Manipulate M_dat -------------------------------------------------------------
M_map_df <- M_dat %>%  #rename
  select(Year, NOAA_code, `X50.`) %>% # select only necessary cols.
  rename(NOAACODE = NOAA_code, M_annual = `X50.`) %>%  # rename cols
  mutate(NOAACODE = as.character(NOAACODE)) %>% #convert to character
  # change NOAA codes so it matches with spatial objects
  mutate(NOAACODE = ifelse(nchar(NOAACODE) == 3, NOAACODE, 
    ifelse(nchar(NOAACODE) == 2, paste0("0", NOAACODE),
      ifelse(nchar(NOAACODE) == 1, paste0("00", NOAACODE), 
        NOAACODE)))) 

# Make NOAA in lat/lon ---------------------------------------------------------
NOAA_latlon <- sp::spTransform(NOAA, sp::CRS('+proj=longlat'))
NOAAlab <- sp::spTransform(NOAAlab, sp::CRS('+proj=longlat'))


# Plotting function
# Plot_M_Map_latlon: a function to plot natural mortality values on a spatial object for
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
# filename: an optional name for  the file. should have extension ".pdf"
# palette: a character vector of color names. length should be 1 less than palette breaks.
# palette_breaks: a numeric vector specifying breaks in the scalebar. This can be 
# specified or left as optional. If not specified, function will calculate by scaling
# equally using the number of categories in the palette between 0 and 1.
# returns: an sp plot object (also creates and saves plot in a graphic device.)
Plot_M_Map_latlon <- function(M, colname = "M_Annual" , NOAA, lab = F, NOAAlab = NULL, 
  axes = F, axes_lab = F, title = T, title_text = "Year here", 
  filepath = ".", filename = NA, 
  palette = rev(RColorBrewer::brewer.pal(n = 10, name = "RdYlBu")), 
  palette_breaks = NULL) {
  # List required packages -----------------------------------------------------
  require(RColorBrewer)
  require(sp)
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
    pdf(paste0(filepath, "/M_map_latlon.pdf"), width = 9, height = 11)
  } else{
    pdf(paste0(filepath,"/", filename), width = 9, height = 11)
  }
  # create the pplot
  plot <- sp::spplot(NOAA, #spatial object to plot
           zcol =  colname, #column name in the dataframe to fill.
           # year in title, if desired
           main = ifelse(title == T, list(label = title_text, cex = 1.2), list(NULL)),
           scales = scales_list,
           # plot limits:
           ylim = c(NOAA@bbox[2,1] - .1, NOAA@bbox[2,2] + .1),
           xlim = c(NOAA@bbox[1,1] - .1, NOAA@bbox[1,2] + .1),
           # axis labels 
           ylab = ifelse(axes_lab == T, "Latitude", list(NULL)),
           xlab = ifelse(axes_lab == T, "Longitude", list(NULL)),
           col.regions = palette, #specify the colors
           at  = palette_breaks, #specify the breaks fro the scale bar.
           col = "gray30", #specify the noaa code outline color
           sp.layout = ifelse(lab == T, list(sl_labels), list(NULL)),
           par.settings = par_settings_list
           )

  print(plot) # to make visible in device
  dev.off()
  return(plot)
}

# Make the by year plots -------------------------------------------------------
# Run plot function to get maps of M by year.
maps <- vector("list", length(1991:2017))
cnt <- 1
for (y in 1991:2017) {
  tmp_M <- dplyr::filter(M_map_df, Year == y) # just M for the year.
  # define the color brewer palette.name))
maps[[cnt]] <-   Plot_M_Map_latlon(M = tmp_M, NOAA = NOAA_latlon, colname = "M_annual", 
    filepath = fig_spec_path,axes = T,axes_lab = T, filename = paste0("M_", y, ".pdf"), 
    title_text = as.character(y))
cnt <- cnt + 1
# to change the color of the unmodeled plots, will need to use this and resave the 
#plot.
#saved_plot <- saved_plot + latticeExtra::layer_(sp.polygons(NOAA_latlon,fill='gray70')) # fill in other NOAA codes with gray..
# Now, export saved_plot as a pdf to save with other fill.
}
# TODO: use maps object to make plots all together.
# 
# Make the avg and sd plots ----------------------------------------------------
# Calculate mean and sd of median values by NOAA code
Avg_SD_M_df <- M_map_df %>% 
  group_by(NOAACODE) %>% 
  summarize(Mean = mean(M_annual), SD = sd(M_annual))

# plot mean
max_plot_avg <- ceiling(10*max(Avg_SD_M_df$Mean))/10 # the maximum value to plot
n_plot_avg <- as.integer(max_plot_avg*10*2)
avg_pal <- RColorBrewer::brewer.pal(n = n_plot_avg+1, name = "Greys")[-1] #get rid of the white
avg_pal_breaks <- seq(from = 0, to = max_plot_avg, length.out = length(avg_pal)+1)
Plot_M_Map_latlon(M = Avg_SD_M_df, NOAA = NOAA_latlon, colname = "Mean", filepath = fig_spec_path, axes = T,axes_lab = T,
  filename = "M_Med_Mean_Map.pdf", title_text = "A. Mean", palette = avg_pal, palette_breaks = avg_pal_breaks )

# plot SD
max_plot_SD <- ceiling(10*max(Avg_SD_M_df$SD))/10 # the maximum value to plot
n_plot_SD <- as.integer(max_plot_SD*10*2)
SD_pal <- RColorBrewer::brewer.pal(n = n_plot_SD+1, name = "Greys")[-1] #get rid of the white
SD_pal_breaks <- seq(from = 0, to = max_plot_SD, length.out = length(SD_pal)+1)
Plot_M_Map_latlon(M = Avg_SD_M_df, NOAA = NOAA_latlon, colname = "SD", filepath = fig_spec_path, axes = T,axes_lab = T,
  filename = "M_Med_SD_Map.pdf", title_text = "B. SD", palette = SD_pal, palette_breaks = SD_pal_breaks )


# Save the data ----------------------------------------------------------------
write.csv(M_map_df, paste0(der_dat_spec_path, "/M_annnual_map_df.csv"))
write.csv(Avg_SD_M_df, paste0(der_dat_spec_path, "/Avg_SD_M_annual_df.csv"))
