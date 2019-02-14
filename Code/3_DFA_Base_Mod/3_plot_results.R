# Header -----------------------------------------------------------------------
# Make plots using the selected model for DFA.
# Make fit plots, and plot trends and loadings
# 
# Written by Kathryn Doering
# 
# TODO: Add scatter plot for 2 trend model, but with average salinity?
# 
# Load packages, set options ---------------------------------------------------

library(tidyverse)
library(MARSS)
options(stringsAsFactors = F) #change the default

# Get data ---------------------------------------------------------------------
# all models in a list
mod_list <- readRDS("./Derived_Data/3_DFA_Base_Mod/1_run_DFA/dfa_mod_list.rda")
# the selected model only 
sel_dfa_mod <- readRDS("./Derived_Data/3_DFA_Base_Mod/2_select_mod/selected_dfa_mod.rda")
regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# name and create folders to store results -------------------------------------

# name folders
fig_gen_path <- "./Figs/3_DFA_Base_Mod"
fig_spec_path <- paste0(fig_gen_path, "/3_plot_results")
#create folders
dir.create(fig_gen_path) # should already exist
dir.create(fig_spec_path)

# # Get information from models --------------------------------------------------
# Get number of trends from all models, and put in a vector.
n_trends <- purrr::map(mod_list, "states") %>% 
              purrr::map_dbl(nrow)

# # get number of trends from the model
# n_trends_sel <- dfa_mod$states
# # get the input data for the model (no need to read it in!)
# dat_wide <- dfa_mod$model$data

# Plot trends and loadings -----------------------------------------------------
# Plot the trends and loadings from a DFA model.
# formals are:
# dfa_mod, an estimated MARSS dfa model.
# file name, file path and name (ending in .pdf) where the file should be stored
# minZ, the minimum magnitued of a factor loading to plot 
# lab_names, a character vector of names for the loadings
# varimax, a single logical value. True if the trends and factors should be
  # rotated using the varimax rotation,false if it should be ploted as estimated 
  # in the model.

Plot_Trends_and_Loadings <- function(dfa_mod, file_name = "DFA_Trends_Loadings_Varimax.pdf",
  minZ = 0.2, lab_names = NULL, varimax = T){
  require(MARSS)
  # Get the DFA data.
  dfa_dat <- dfa_mod$model$data
  # Get the trends and loadings.
  Z_est <- coef(dfa_mod, type = "matrix")$Z #factor loadings, estimated from the model.
  if(varimax == T & dim(Z_est)[2] != 1){ #rotate the factor loadings/ trends before plotting. (no need to rotate for a 1 trend model!)
  # get the inverse of the rotation matrix
  print("used varimax")
  H_inv <- varimax(Z_est)$rotmat
    # rotate factor loadings
    ZZ <-  Z_est %*% H_inv
    # rotate the trends
    trends <-  solve(H_inv) %*% dfa_mod$states #solve(H_inv) gives the inverse, aka just H.
  } else { # don't rotate.
  print("used estimated")
  ZZ <- Z_est
  trends <- dfa_mod$states
  }
  if(dim(Z_est)[2] > 3){
    stop("The plotting parameters need to be modified to plot more than 3 trends.")
  }
  # get necessary information to make the plot
  w_ts <- seq(dim(dfa_dat)[2]) # time series dimensions
  N_ts <- dim(dfa_dat)[1] # number of time series.
  n_trends <- ncol(ZZ) #get the number of trends
  
  yr_min <- as.numeric(colnames(dfa_dat)[1]) 
  
  ylbl <- as.character(rownames(dfa_dat)) #labels for the different time series
  if(is.null(lab_names)){ #labels for the years
    yr_max <- colnames(dfa_dat)[dim(dfa_dat)[2]]
    xlbl_trends <- yr_min:yr_max
    lab_names <- paste0(lab_names)
  } else {
    xlbl_trends <- lab_names
  }
  pdf(file_name, width = 12, height = 8)
  layout(matrix(c(1, 2, 3, 4, 5, 6), n_trends, 2), widths = c(1.2, 1))
  ## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
  par(mai = c(0.7, 0.8, 0.5, 0.1), oma = c(2, 2, 0, 0))
  ## plot the processes
  for (i in 1:n_trends) {
    ylm <- c(-1, 1) * max(abs(trends))
    ## set up plot area
    plot(w_ts, trends[i, ], type = "n", bty = "L", ylim = ylm, 
      xlab = "", ylab = NA, xaxt = "n", cex.axis = 1.5, las = 1)
    ## draw zero-line
    abline(h = 0, col = "gray")
    ## plot trend line
    lines(w_ts, trends[i, ], lwd = 2)
    #lines(w_ts, trends[i, ], lwd = 2)
    ## add panel labels
    mtext(paste("Trend", i), side = 3, line = 0.5, adj = 0, cex = 1.5)
    axis(1, 1:dim(dfa_dat)[2], xlbl_trends, cex.axis = 1.2, padj = 0.5)
  }
  mtext("Relative M (instantaneous)", side = 2, outer = T, line = -1, cex = 1.5)
  mtext("Year", side = 1, line = 0.5, outer = T, cex = 1.5, at = 0.3) 
  #mtext("Loadings", sid = 2, outer = T, line = -40, cex = 1.5)
  # plot the loadings
  ylm <- c(-1, 1) * max(abs(ZZ))
  for (i in 1:n_trends) {
    plot(c(1:N_ts)[abs(ZZ[, i]) > minZ], as.vector(ZZ[abs(ZZ[, 
      i]) > minZ, i]), 
      type = "h", lwd = 1, bty = "n",xlab = "", ylab = NA,  cex.axis = 1.5,las = 1,
      xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col = "black")
    for (j in 1:N_ts) {
      if (ZZ[j, i] >= minZ) {
        text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex =1.1, 
          col = "black")
      }
      if (ZZ[j, i] < -minZ) {
        text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.1, 
          col = "black")
      }
      abline(h = 0, lwd = 1.5, col = "gray")
    }
    mtext(paste("Loadings on trend", i), side = 3, line = 0.5, adj = 0, cex = 1.5)
  }
  dev.off()
}

# apply to the selected model (note: will not work if the selected model is over 3 trends)
Plot_Trends_and_Loadings(dfa_mod = sel_dfa_mod, 
                         file_name = paste0(fig_spec_path, "/selected_mod_trends_loadings.pdf"), 
                         minZ = 0, 
                        varimax = T)
# # apply to all of the models (does not work for over 3 trends)
filenames_tl <- paste0(fig_spec_path,"/ntrends_", n_trends, "_diag_equal_mod_trends_loadings.pdf")
purrr::map2(mod_list[-4], filenames_tl[-4], Plot_Trends_and_Loadings, minZ = 0, varimax = T)

# Get diagnostic plots ---------------------------------------------------------

Get_Diag_Plot <- function(dfa_mod, file_name) {
  require(MARSS)
  plot(dfa_mod, plot.type = "observations") + theme_classic()
  ggsave(file_name, device = "pdf", width = 10, height = 7.5)
}
#apply to all of the models
filenames <- paste0(fig_spec_path,"/ntrends_", n_trends, "_diag_equal_mod_fits.pdf")
purrr::map2(mod_list, filenames, Get_Diag_Plot)

# Plot loadings against one another for the 2 trend model ----------------------
# # Make a scatterplot (use region information) to more clearly see how loadings
# on trends 1 and 2 differ


#Make Loadings scatterplot------------------------------------------------------

#Get loadings
Z_est <- coef(sel_dfa_mod, type ="matrix")$Z #factor lodings
if(dim(Z_est)[2]==2){ # only run this code if the selected model has 2 loadings.
  Z_est_df <- data.frame(NOAA_code = row.names(sel_dfa_mod$model$data), 
                         load_1 = Z_est[,1], 
                         load_2 = Z_est[,2])
  regions$NOAA_code <- as.character(regions$NOAA_code)
  # add regions with loadings
  Z_est_df <- left_join(Z_est_df, regions, by = "NOAA_code")
  # make regionname into a factor to set plotting order
  Z_est_df$Region_name_fac <- factor(Z_est_df$Region_name, 
    levels = unique(regions$Region_name))
  # plot loadings
  with(Z_est_df, plot(load_1, load_2))
  
  # assign the color palette
  pal <- RColorBrewer::brewer.pal(n = length(unique(Z_est_df$Region_name_fac))+2, name = "Greys")
  pal <- pal[c(-1,-2)] # don't use the first 2 colors, too light.
  
  ggplot(data = Z_est_df, aes(x = load_1, y = load_2))+
    geom_hline(yintercept =0, color = "gray70")+
    geom_vline(xintercept = 0, color = "gray70")+
    geom_text(aes(label = NOAA_code, color = Region_name_fac), size = 5)+
    scale_color_manual(values = pal, name = "Region")+
    xlab("Loadings on Trend 1")+
    ylab("Loadings on Trend 2")+
    labs(fill="Region") +
    theme_classic(base_size = 18)+
    coord_equal()
  ggsave(paste0(fig_spec_path, "/loadings_2_trend_scatter.pdf"),device = "pdf" , width = 9, height = 6.5)
  
  pal_qual <- RColorBrewer::brewer.pal(n = 7, name = "Dark2")
  ggplot(data = Z_est_df, aes(x = load_1, y = load_2))+
    geom_hline(yintercept =0, color = "gray70")+
    geom_vline(xintercept = 0, color = "gray70")+
    geom_text(aes(label = NOAA_code, color = Region_name_fac))+
    scale_color_manual(values = pal_qual, name = "Region")+
    xlab("Loadings on Trend 1")+
    ylab("Loadings on Trend 2")+
    labs(fill="Region") +
    theme_classic(base_size = 18)+
    coord_equal()
  ggsave(paste0(fig_spec_path, "/loadings_2_trend_scatter_qualitative_col.pdf"),device = "pdf" , width = 9, height = 6.5)
}

