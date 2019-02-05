# Header -----------------------------------------------------------------------
# Plot box disarticulation rate as estimated in the model.
#
# Plot relative to prior.
# 
# Written 18 Jan 2019 by Kathryn Doering
# TODO:
# Check code (including function and formatting.)
# Clear global env--------------------------------------------------------------

rm(list = ls())

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(rstan)
options(stringsAsFactors = F) #Change options.
# Load data ---------------------------------------------------------------------
read_path <- "./Derived_Data/2_Base_Mod_Run/1_run_mod"
# Load model and data associated with it:
mod_sum <- read.csv("./Derived_Data/2_Base_Mod_Run/2_check_convergence/mod_summary_all_chains.csv")
mod <- readRDS(file = paste0(read_path, "/model.rda"))
mod_dat <- readRDS(file = paste0(read_path, "/model_dat.rda"))

# Set output paths and create folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/4_plot_d")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/4_plot_d")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
dir.create(fig_spec_path) 

# #get data: d (box decay rate)-------------------------------------------------

summary_d <- filter(mod_sum, X == "d") # get a summary of the model dist.
#Extract all samples from the model (to create a histogram with d samples)
mod_sim <- rstan::extract(mod) #Extract post-warmup samples
#get prior for d
d_dist <- mod_dat$d_p #mean and sd used.
theta <- seq(0, 1, length  = 1000)
f_theta <- dnorm(theta, mean = d_dist[1], sd = d_dist[2])

# Plot d -----------------------------------------------------------------------

# Model estimate and prior.

pdf(paste0(fig_spec_path,"/d_prior_post.pdf"), 
  width = 6, height = 4) # 8.5, height = 11, units = "in")
par(yaxs="i", bty = "n")
hist(mod_sim$d,
  freq = FALSE,
  cex.axis = 1,
  cex.lab = 1,
  col = 'gray',
  #ylim = c(0, 14),
  xlim = c(0,(0.1+summary_d[,8])),
  xlab = 'Instantaneous Box Decay Rate',
  main = ''
) # plot the distribution for 1 parameter
lines(f_theta~theta)
abline(h = 0)
legend("topleft", 
  legend = c("Prior", "Posterior"), 
  fill = c(NA,"gray"), 
  lty = c(1, NA), 
  border = c(NA, "black")
)
dev.off()

# Save data --------------------------------------------------------------------

# save mod_sim$d 
write.csv(mod_sim$d, paste0(der_dat_spec_path, "/mod_samples_d_post_warmup.csv"))
# save summary_d
write.csv(summary_d, paste0(der_dat_spec_path, "/mod_summary_d.csv"), row.names = F )
