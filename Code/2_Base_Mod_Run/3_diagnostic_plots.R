# TODO: think about what is important to include here.
# Right now, this script contains a lot of old functions from previous model 
# versions. Does not run, most likely.

# Maybe if there are any residual plots, or plots of variables against one
# another that I did?
# 
# Written 17 Jan 2019 by Kathryn Doering

#Load Packages -----------------------------------------------------------------
library(dplyr)
library(rstan)
library(shinystan)
#To add maps:
library(rgdal)
library(maptools)
library(ggplot2)
#library(plyr)
library(maps) #later on in the script.
#Another option: using the maps package.
options(stringsAsFactors = F) #Change options.

#Functions to use: 
source('./Code/Extract_and_plot_functions.R')

# Load Data ---------------------------------------------------------------------
read_path <- "./Derived_Data/2_Base_Mod_Run/1_run_mod"
#Load model and data associated with it:
bar_reg_key <- read.csv(paste0(read_path, "/bar_reg_key.csv"))
raw_mod_dat <- read.csv(paste0(read_path, "/raw_dat.csv"))

mod <- readRDS(file = paste0(read_path, "/model.rda"))
mod_dat <- readRDS(file = paste0(read_path, "/model_dat.rda"))

NOAA_vec <- read.csv(paste0(read_path, "/NOAA_list.csv"))
NOAA_vec <- NOAA_vec$x

#NOAA code regions
regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Set output paths and make folders --------------------------------------------

# general derived data for all base model run scripts:
der_dat_gen_path <- "./Derived_Data/2_Base_Mod_Run"
# subfolder for this script:
der_dat_spec_path <- paste0(der_dat_gen_path, "/3_diagnostic_plots")
fig_gen_path <- "./Figs/2_Base_Mod_Run"
fig_spec_path <- paste0(fig_gen_path, "/3_diagonstic_plots")
# make the folders (gen folders should already exist)
dir.create(der_dat_spec_path)
dir.create(fig_spec_path) 

# Define variables--------------------------------------------------------------
# years used in the models
yr_0 <- 1990
yr_last <- 2017 #Assume 2017 data have been entered
nyears <-  yr_last-yr_0  #nyears not including year 0.

#DELETE?Add a color column to bar_reg_key
tmp_df <- data.frame(NOAACode = NOAA_vec, NOAAColor = NOAA_col_vec)
bar_reg_key <- left_join(bar_reg_key, tmp_df, by = "NOAACode")

# Model fits: regional level lambdas with obs ----------------------------------
# Data for Plots for boxes in the first year - regional-------------------------
# # Data for plots of lives on the bar level in all years. ---------------------
# # Data for Plots for boxes in the first year - bar level.---------------------
# 

# #-----------------------------------------------------------------------------
#Model fits: plot regional level lambdas with observations, 
#lambda_r[year,region]
summary_lambda_r <- list()
l_obs <- list()
max_y_axis <- rep(NA, length.out = length(NOAA_vec))
for(i in 1:length(NOAA_vec)){
  # Get model summary of regional live values
  summary_lambda_r[[i]] <- Summarize_Par_Reg(mod = mod, par = "lambda_r",nyears = nyears, reg = i)
  # Get summary of lives
  #Extract live observations for each region by year from the raw data.
  tmp_l_obs<- raw_mod_dat %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    mutate(Y_ind = SampleYr - yr_0) %>% 
    filter(Y_ind > 0) %>% 
    select(Y_ind, L)
  l_obs[[i]] <- tmp_l_obs # save lives for that NOAA code
  #For plotting: determine max value of L by adding 5 to either the 
  #maximum of the observed or the CI for the distribution.
  max_y_axis[i] <- as.integer(max(c(max(tmp_l_obs$L),max(summary_lambda_r[[i]][,'97.5%'])))+5)
}

#----------------------------------------
# Data for Plots for boxes in the first year - regional.

#Model fits: plot regional level lambdas with observations, 
#lambda_r[year,region]
summary_beta_0_r <- summary(mod, par = "beta_0_r")$summary #for all regions, ordered by NOAA code list.
#Extract live observations for each region by year from the raw data.
beta_0_r_obs <- raw_mod_dat %>% 
  mutate(Y_ind = SampleYr- yr_0) %>% 
  filter(Y_ind == 0) %>% # include year 0 only
  select(NOAACode, B)
tmp_reg_NOAACode <- bar_reg_key %>% 
  select( ModReg, NOAACode) %>% 
  distinct()
#Join so that ModReg can be used for plotting (instead of NOAACode)
beta_0_r_obs <- left_join(beta_0_r_obs, tmp_reg_NOAACode, by = "NOAACode")

#For plotting: determine max value of L by adding 5 to either the 
#maximum of the observed or the CI for the distribution.
max_y_axis_box_0_r <- as.integer(max(c(max(beta_0_r_obs$B),max(summary_beta_0_r[,'97.5%'])))+5)

# ------------------------------------------------------------------------------
# Data for plots of lives on the bar level in all years. 

# Model fits: plot bar level lambdas and betas with observations.
summary_lambda_i <- list() #an empty list
#add each bar summary as a component of the list
n_bars <- length(unique(bar_reg_key$ModBar))
for (i in 1:n_bars){ # the key has unique row for each bar
  summary_lambda_i[[i]] <- Summarize_Par_Bar(
    mod = mod, 
    par = "lambda_i",
    nyears = nyears,  
    bar = i)
}
#Add names to the list. 
names(summary_lambda_i) <- seq(from = 1, to = n_bars)

# plot bar level betas 
summary_beta_i <- list() #an empty list
#add each bar summary as a component of the list
#n_bars <- length(unique(bar_reg_key$ModBar))
for (i in 1:n_bars){ # the key has unique row for each bar
  summary_beta_i[[i]] <- Summarize_Par_Bar(
    mod = mod, 
    par = "beta_i",
    nyears = nyears,  
    bar = i)
}
#Add names to the list. 
names(summary_beta_i) <- seq(from = 1, to = n_bars)

#Extract live and box observations for each region by year from the raw data.
#Use a for loop to loop through years:
live_obs_i <- list() #to store live obs data
box_obs_i <-  list() #to store box obs data
for (i in 1:n_bars){
  live_obs_i[[i]] <- raw_mod_dat %>% 
    filter(ModBar == as.character(i)) %>% #data only for that bar
    mutate(Y_ind = SampleYr- yr_0) %>%  #create Y index
    filter(Y_ind >0) %>% #remove year obs
    select(Y_ind, L)     #select live obs
  box_obs_i[[i]] <- raw_mod_dat %>% 
    filter(ModBar == as.character(i)) %>% #data only for that bar
    mutate(Y_ind = SampleYr- yr_0) %>%  #create Y index
    filter(Y_ind >0) %>% #remove year obs
    select(Y_ind, B)     #select live obs
}
names(live_obs_i) <- seq(from = 1, to = n_bars)
names(box_obs_i) <- seq(from = 1, to = n_bars)

#For plotting: determine max value of L by adding 5 to either the 
#maximum of the observed or the CI for the distribution.
max_y_axis_live <- rep(NA, length.out = length(n_bars))
max_y_axis_box  <- rep(NA, length.out = length(n_bars))
for (i in 1:n_bars){
  max_y_axis_live[i] <- as.integer(max(c(
    max(live_obs_i[[i]]$L),
    max(summary_lambda_i[[i]][,'97.5%'])))+5)
  max_y_axis_box[i]  <- as.integer(max(c(
    max(box_obs_i[[i]]$B),
    max(summary_beta_i[[i]][,'97.5%'])))+5)
}

#-------------------------

# Data for Plots for boxes in the first year - bar level.

summary_beta_0_i <- summary(mod, par = "beta_0_i")$summary #for regions 1 and 2.
#Extract live observations for each region by year from the raw data.
beta_0_i_obs <- raw_mod_dat %>% 
  mutate(Y_ind = SampleYr- yr_0) %>% 
  filter(Y_ind == 0) %>% # include year 0 only
  select(ModBar, B) #Model index and Bar are the only necessary columns.
#For plotting: determine max value of L by adding 5 to either the 
#maximum of the observed or the CI for the distribution.
max_y_axis_box_0_i <- as.integer(max(c(max(beta_0_i_obs$B),max(summary_beta_0_i[,'97.5%'])))+5)
#Add a color column for noaa code to teh bar_reg_key(will be used for plotting)
# bar_reg_key <- mutate(bar_reg_key, NOAAcol = ifelse(NOAACode == 192, 'gray', 
#                                                     ifelse(NOAACode == 292, 'lightskyblue', 'orange')
#                                                     )
#                       )


#--------------------------
#--------------------------






# -----------------------------------------------------------------------
# Diagnostic plots

pdf(paste0("./figures/", folder_name, "/Diagnostic_Plots.pdf"), 
  width = 7, height = 4)
par(mar=c(4.1,5.1,2.0,0.2), bty = "n")

#-------------------regional lambda plots. 
for (i in 1:length(NOAA_vec)){
  bxp(list(stats = t(summary_lambda_r[[i]][,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[i], #make the color the number NOAA code happens to be (modify this to be more general at some point.)
    ylim = c(0, max_y_axis[i]),
    ylab = 'Lives (/ 0.5 bu)',
    xlab = 'Year',
    cex.lab = 1.6,
    main = paste("NOAA Code", NOAA_vec[i]),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  points(l_obs[[i]]$L ~ jitter(l_obs[[i]]$Y_ind, 1), pch = 20, col = 'black')
}
#-------------------------------------------------------------------------------
#Plot regional level boxes in the first year.
bxp(list(stats = t(summary_beta_0_r[,4:8]), names = as.character(NOAA_vec)), 
  boxfill = NOAA_col_vec, 
  ylim = c(0, max_y_axis_box_0_r),
  ylab = '# boxes (/0.5bu)',
  xlab = 'NOAA Code',
  cex.lab = 1.6,
  main = "Regional Boxes in Year 0",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)
points(beta_0_r_obs$B ~ jitter(beta_0_r_obs$ModReg, 1), pch = 20, col = 'black')
#-------------------------------------------------------------------------------
#Live plots by bar.
#par(mfrow = c(3,4) ) #based on making 11 plots; may plot each on a separate page if easier to see. 
for (i in 1:n_bars){
  tmp_summary <- summary_lambda_i[[i]] #model summary for the bar
  tmp_obs <- live_obs_i[[i]]
  tmp_mod_reg <- bar_reg_key[bar_reg_key$ModBar == i,"ModReg"]
  tmp_ID <- bar_reg_key[bar_reg_key$ModBar == i,"ID"]
  tmp_NOAACode <- bar_reg_key[bar_reg_key$ModBar == i,"NOAACode"]
  
  bxp(list(stats = t(tmp_summary[,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[tmp_mod_reg], 
    ylim = c(0, max_y_axis_live[i]),
    ylab = "# lives (/0.5 bu)",
    xlab = "Year",
    cex.lab = 1.6,
    main = paste0("Bar: ", tmp_ID, ", NOAA Code: ", tmp_NOAACode),
    whisklty = "solid",
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  #Live observations
  points(tmp_obs$L ~ jitter(tmp_obs$Y_ind, 1), pch = 20, col = 'black')
}
#-------------------------------------------------------------------------------
#Box plots by bar.
#year 0
bxp(list(stats = t(summary_beta_0_i[,4:8]), names = as.character(bar_reg_key$ID)), 
  boxfill = bar_reg_key$NOAAColor, 
  ylim = c(0, max_y_axis_box_0_i),
  ylab = '# boxes (/0.5bu)',
  xlab = 'NOAA Code',
  cex.lab = 1.4,
  main = "Bar level Boxes in Year 0",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)
points(beta_0_i_obs$B ~ jitter(beta_0_i_obs$ModBar, 1), pch = 20, col = 'black')
#-------------------------------------------------------------------------------
#Box plots by bar.
#All other years
#par(mfrow = c(3,4) ) #based on making 11 plots; may plot each on a separate page if easier to see. 
for (i in 1:n_bars){
  tmp_summary <- summary_beta_i[[i]] #model summary for the bar
  tmp_obs <- box_obs_i[[i]]
  tmp_mod_reg <- bar_reg_key[bar_reg_key$ModBar == i,"ModReg"]
  tmp_ID <- bar_reg_key[bar_reg_key$ModBar == i,"ID"]
  tmp_NOAACode <- bar_reg_key[bar_reg_key$ModBar == i,"NOAACode"]
  bxp(list(stats = t(tmp_summary[,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[tmp_mod_reg], 
    ylim = c(0, max_y_axis_box[i]),
    ylab = '# boxes (/0.5bu)',
    xlab = 'Year',
    cex.lab = 1.6,
    main = paste0("Bar: ", tmp_ID, ", NOAA Code: ", tmp_NOAACode),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  #Live observations
  points(tmp_obs$B ~ jitter(tmp_obs$Y_ind, 1), pch = 20, col = 'black')
}
dev.off()

#-------------------------------------------------------------------------------
#*******************************************************************************
#-------------------------------------------------------------------------------
#parameter plots (no data to compare it to)--------------------------------------

#Start here to run!!!!
#-------------------------------------------------------------------------------
#Get data: M.
summary_M <- list()
M_box     <- list()
for(i in 1:length(NOAA_vec)){
  #Get model summary of M.
  summary_M[[i]] <- Summarize_Par_Reg(mod = mod, par = "M", nyears = nyears, reg = i)
  #calculate the box count method (DNR way)
  M_box[[i]] <- raw_mod_dat %>% 
    filter(NOAACode == NOAA_vec[i]) %>%  # for region 1/ NOAA code 192
    filter(ModYr > 0) %>%  # exclude year 0 data
    mutate(SampleBoxCount = B/(L+B)) %>%  # do it the DNR way by calculating for a sample first
    filter(!is.na(SampleBoxCount)) %>%  #remove any NANs.
    group_by(ModYr) %>% # group data by year
    summarize(BoxCount = mean(SampleBoxCount)) # take the avg of box count for each year. 
}

#-------------------------------------------------------------------------------
#get data: d (box decay rate)
summary_d <- summary(mod, pars = "d")$summary
#Extract all samples from the model (create a histogram)
mod_sim <- rstan::extract(mod) #Extract post-warmup samples
#get prior for d
d_dist <- mod_dat$d_p #mean and sd used.
theta <- seq(0, 1, length  = 1000)
f_theta <- dnorm(theta, mean = d_dist[1], sd = d_dist[2])

#-------------------------------------------------------------------------------
#Plot regional lambda, beta values with the prior distribution used. 
#
log_beta_0_r_prior   <- mod_dat$log_beta_0_r_p

#Create a matrix to store samples for the beta 0  prior in the 2 regions.
prior_log_beta_0_r <- matrix(NA, nrow = 1000, ncol = mod_dat$REG)
dens_prior_log_beta_0_r <- matrix(NA, nrow = nrow(prior_log_beta_0_r), ncol = mod_dat$REG)
for (r in 1:mod_dat$REG){
  tmp_min <-  log_beta_0_r_prior[r,1] - 2.5*log_beta_0_r_prior[r,2]
  tmp_max <-  log_beta_0_r_prior[r,1] + 2.5*log_beta_0_r_prior[r,2]
  #get samples from prior
  prior_log_beta_0_r[ ,r] <- seq(tmp_min, tmp_max, length  = 1000)#get 1000 samples
  dens_prior_log_beta_0_r[ ,r] <- dnorm(prior_log_beta_0_r[ ,r], 
    mean = log_beta_0_r_prior[r,1], 
    sd   = log_beta_0_r_prior[r,2]
  )
}

#Extract model estimates 
mod_sim_log_beta_0_r <- mod_sim$log_beta_0_r #[iter,region]


#-------------------------------
#Plot regional lambda estimates with prior.
#for now [region, mean and sd], same dist for each year.
log_lambda_r_prior <- mod_dat$log_lambda_r_p #prior used in the model


#Create a matrix to store samples for the beta 0  prior in the 2 regions.
prior_log_lambda_r <- matrix(NA, nrow = 1000, ncol = mod_dat$REG)
dens_prior_log_lambda_r <- matrix(NA, nrow = nrow(prior_log_lambda_r), ncol = mod_dat$REG)
for (r in 1:mod_dat$REG){
  tmp_min <-  log_lambda_r_prior[r,1] - 2.5*log_lambda_r_prior[r,2]
  tmp_max <-  log_lambda_r_prior[r,1] + 2.5*log_lambda_r_prior[r,2]
  #get samples from prior
  prior_log_lambda_r[ ,r] <- seq(tmp_min, tmp_max, length  = 1000)#get 100 samples
  dens_prior_log_lambda_r[ ,r] <- dnorm(prior_log_lambda_r[ ,r], 
    mean = log_lambda_r_prior[r,1], 
    sd   = log_lambda_r_prior[r,2]
  )
}


#-------------------------------------------------------------------------------
# get data: sigma
summary_sigma <- summary(mod, pars = "sigma")$summary

#---------------------------------------------------------------------------------
#Make Parameter plot

pdf(paste0("./figures/", folder_name, "/Parameter_Plots.pdf"), 
  width = 7, height = 4)
par(mar=c(4.1,5.1,2.0,0.2), bty = "n")
for(i in 1:length(NOAA_vec)){
  bxp(list(stats = t(summary_M[[i]][,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[i], 
    ylim = c(0, 1),
    ylab = 'Annual M',
    xlab = 'Year',
    cex.lab = 1.6,
    main = paste("NOAA Code", NOAA_vec[i]),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  points(M_box[[i]]$BoxCount ~ M_box[[i]]$ModYr, type = "o", pch = 20, col = 'black')
}

#Plot d (needed to add as.matrix to avoid converting the summary to a vector)

hist(mod_sim$d,
  freq = FALSE,
  cex.axis = 1.4,
  cex.lab = 1.4,
  col = 'green',
  #ylim = c(0, 14),
  xlim = c(0.4,(0.1+summary_d[,8])),
  xlab = 'Instantaneous Box Decay Rate',
  main = ''
) # plot the distribution for 1 parameter
lines(f_theta~theta)

#Plot d as box plot, without prior.
bxp(list(stats = as.matrix(summary_d[,4:8]), names = c('All')), 
  boxfill = 'green', 
  #ylim = c(0, 1),
  ylab = 'annual rate of decay',
  xlab = 'All years, regions',
  cex.lab = 1.6,
  main = "Instantaneous box decay rate",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)


#Plot histograms of model estimates of beta 0 and their priors. 
for(r in 1:mod_dat$REG){
  #find x axes limits based onprior values an d
  tmp_min_x <- min(c(min(mod_sim_log_beta_0_r[,r])-0.5,
    log_beta_0_r_prior[r,1] - 2.5*log_beta_0_r_prior[r,2]
  )
  )  #start x axis
  tmp_max_x <- max(c(max(mod_sim_log_beta_0_r[,r])+0.5, 
    log_beta_0_r_prior[r,1] + 2.5*log_beta_0_r_prior[r,2]
  )
  )
  hist(mod_sim_log_beta_0_r[,r],
    freq = FALSE,
    cex.axis = 1.4,
    cex.lab = 1.4,
    col = NOAA_col_vec[r],
    #ylim = c(0, 14),
    xlim = c(tmp_min_x, tmp_max_x),
    #xlim = c(-10, 10),
    xlab = paste("log_beta_0_r: NOAA_code", NOAA_vec[r]),
    main = ''
  ) # plot the distribution for 1 parameter
  lines(dens_prior_log_beta_0_r[ ,r] ~ prior_log_beta_0_r[ ,r])
}

#Plot histograms of model estimates of lambda 0 and their priors. 
for(r in 1:mod_dat$REG){
  for(y in 1:mod_dat$Y){
    #Extract model estimates specific for the year and region
    tmp_mod_sim_log_lambda_r <- mod_sim$log_lambda_r[,y,r]
    #find x axes limits based onprior values an d
    tmp_min_x <- min(c(min(tmp_mod_sim_log_lambda_r)-0.5,
      log_lambda_r_prior[r,1] - 2.5*log_lambda_r_prior[r,2]
    )
    )  #start x axis
    tmp_max_x <- max(c(max(tmp_mod_sim_log_lambda_r)+0.5, 
      log_lambda_r_prior[r,1] + 2.5*log_lambda_r_prior[r,2]
    )
    )
    hist(tmp_mod_sim_log_lambda_r,
      freq = FALSE,
      cex.axis = 1.4,
      cex.lab = 1.4,
      col = ifelse(r == 1, 'gray', ifelse(r == 2, 'skyblue', 'green')),
      #ylim = c(0, 14),
      xlim = c(tmp_min_x, tmp_max_x),
      #xlim = c(-10, 10),
      xlab = paste0("log_lambda_r: NOAA CODE ", NOAA_vec[r] , ", Year: ", y),
      main = ""
    ) # plot the distribution for 1 parameter (dist is the same for all years)
    lines(dens_prior_log_lambda_r[ ,r] ~ prior_log_lambda_r[ ,r])
  }
}


#Plot sigma (needed to add as.matrix to avoid converting the summary to a vector)
bxp(list(stats = as.matrix(summary_sigma[,4:8]), names = c('All')), 
  boxfill = 'green', 
  #ylim = c(0, 1),
  ylab = 'log scale sd',
  xlab = 'All years, regions, lives and boxes',
  cex.lab = 1.6,
  main = "log scale sigma",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)

dev.off()

#-------------------------------------------------------------------------------
#*******************************************************************************
#-------------------------------------------------------------------------------

# Diagnostic plots, but plot box and lives next to each other

pdf(paste0("./figures/", folder_name, "/Diagnostic_Plots_Paired_Live_Box.pdf"), 
  width = 6, height = 4)
#par(mar=c(4.1,5.1,2.0,0.2))

#-------------------regional lambda plots. 
for (i in 1:length(NOAA_vec)){
  bxp(list(stats = t(summary_lambda_r[[i]][,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[i], #make the color the number NOAA code happens to be (modify this to be more general at some point.)
    ylim = c(0, max_y_axis[i]),
    ylab = 'Lives (/ 0.5 bu)',
    xlab = 'Year',
    #cex.lab = 1.6,
    main = paste("NOAA Code", NOAA_vec[i]),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  points(l_obs[[i]]$L ~ jitter(l_obs[[i]]$Y_ind, 1), pch = 20, col = 'black')
}
#-------------------------------------------------------------------------------
#Plot regional level boxes in the first year.
bxp(list(stats = t(summary_beta_0_r[,4:8]), names = as.character(NOAA_vec)), 
  boxfill = NOAA_col_vec, 
  ylim = c(0, max_y_axis_box_0_r),
  ylab = '# boxes (/0.5bu)',
  xlab = 'NOAA Code',
  #cex.lab = 1.6,
  main = "Regional Boxes in Year 0",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)
points(beta_0_r_obs$B ~ jitter(beta_0_r_obs$ModReg, 1), pch = 20, col = 'black')
#-------------------------------------------------------------------------------
#Live plots by bar.
#par(mfrow = c(3,4) ) #based on making 11 plots; may plot each on a separate page if easier to see. 
par_old_mar <- par("mar")
par(mfrow = c(2,1), mar = c(2,4,1,1), bty = "n")
for (i in 1:n_bars){
  tmp_summary <- summary_lambda_i[[i]] #model summary for the bar
  tmp_obs <- live_obs_i[[i]]
  tmp_mod_reg <- bar_reg_key[bar_reg_key$ModBar == i,"ModReg"]
  tmp_ID <- bar_reg_key[bar_reg_key$ModBar == i,"ID"]
  tmp_NOAACode <- bar_reg_key[bar_reg_key$ModBar == i,"NOAACode"]
  
  bxp(list(stats = t(tmp_summary[,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[tmp_mod_reg], 
    ylim = c(0, max_y_axis_live[i]),
    ylab = "# lives (/0.5 bu)",
    xlab = "Year",
    #cex.lab = 1.6,
    main = paste0("Bar: ", tmp_ID, ", NOAA Code: ", tmp_NOAACode),
    whisklty = "solid",
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  #Live observations
  points(tmp_obs$L ~ jitter(tmp_obs$Y_ind, 1), pch = 1, col = 'black')
  #Box plots by bar.
  #All other years
  #par(mfrow = c(3,4) ) #based on making 11 plots; may plot each on a separate page if easier to see. 
  tmp_summary <- summary_beta_i[[i]] #model summary for the bar
  tmp_obs <- box_obs_i[[i]]
  tmp_mod_reg <- bar_reg_key[bar_reg_key$ModBar == i,"ModReg"]
  tmp_ID <- bar_reg_key[bar_reg_key$ModBar == i,"ID"]
  tmp_NOAACode <- bar_reg_key[bar_reg_key$ModBar == i,"NOAACode"]
  
  bxp(list(stats = t(tmp_summary[,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[tmp_mod_reg], 
    ylim = c(0, max_y_axis_box[i]),
    ylab = '# boxes (/0.5bu)',
    xlab = 'Year',
    #cex.lab = 1.2,
    
    #main = paste0("Bar: ", tmp_ID, ", ", tmp_NOAACode),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  #Live observations
  points(tmp_obs$B ~ jitter(tmp_obs$Y_ind, 1), pch = 1, col = 'black')
}
#-------------------------------------------------------------------------------
#Box plots by bar.
#Need to redo these to make them easier to read. 
#year 0
par(mfrow = c(1,1), mar = par_old_mar, bty = "n")
# for(i in 1:length(NOAA_vec)){
#     
# tmp_bars <- filter(bar_reg_key, ModReg == i)
# tmp_names <- tmp_bars$ID
# tmp_bars <- tmp_bars$ModBar
# tmp_NOAACode <- unique(bar_reg_key[bar_reg_key$ModReg == i, "NOAACode"])
# tmp_col <- unique(bar_reg_key[bar_reg_key$ModReg == i, "NOAAColor"])
# tmp_beta_0_i_obs <- 
#Box plots by bar.
#year 0
bxp(list(stats = t(summary_beta_0_i[,4:8]), names = as.character(bar_reg_key$ID)), 
  boxfill = bar_reg_key$NOAAColor, 
  ylim = c(0, max_y_axis_box_0_i),
  ylab = '# boxes (/0.5bu)',
  xlab = 'NOAA Code',
  cex.lab = 1.4,
  main = "Bar level Boxes in Year 0",
  whisklty = 'solid',
  medlwd = .8,
  boxlwd  = .8,
  axes = T
)
abline(h = 0, col = "black")
points(beta_0_i_obs$B ~ jitter(beta_0_i_obs$ModBar, 1), pch = 20, col = 'black')
#}
dev.off()

#--------------------------------------------------------------------------
names(summary_M) <- NOAA_vec
#save median values only (will use these to analyze trends over time

tmp_M_med <- summary_M[[1]][,"50%"]
tmp_yr <- (yr_0+1):yr_last
tmp_NOAACode <- rep(names(summary_M)[1], length.out = length(tmp_yr))
M_med <- data.frame(NOAACode = tmp_NOAACode, Year = tmp_yr, Median_M = tmp_M_med)

for (i in 2:length(NOAA_vec)){
  tmp_M_med <- summary_M[[i]][,"50%"]
  tmp_yr <- (yr_0+1):yr_last
  tmp_NOAACode <- rep(names(summary_M)[i], length.out = length(tmp_yr))
  tmp_M_med_df <- data.frame(NOAACode = tmp_NOAACode, Year = tmp_yr, Median_M = tmp_M_med)
  M_med <- bind_rows(M_med,tmp_M_med_df)
}

write.csv(M_med, paste0("./models_and_derived_data/",folder_name, "/median_M_all.csv"), row.names = F)

#-------------------------------------------------------------------------------
#Added 10 May 2018

#add a map to the corner of M plots
#load NOAA code polygons as a spatial object
BaySeg <-  readOGR(dsn="./ArcGIS", layer="NOAACodes_ShellfishAll_ForBuyTicketBook")
BaySeg@data$id <-  rownames(BaySeg@data)
# BaySeg_points <-  fortify(BaySeg, region="id")
##fortify may be deprecated in the future. recommended waY;
##library(broom)
##BaySeg_points <- tidy(BaySeg)
# colnames(BaySeg_points)[6] <- "id"

#Transform the coordinate system to lon/lat.
#Transforming a CRS (coordinate reference system) #From http://rspatial.org/spatial/rst/6-crs.html#transforming-vector-data
ll <- CRS("+proj=longlat +datum=WGS84")
BaySegll <- spTransform(BaySeg, ll) #bay segment in lat and lon instead. 

BaySegll@data$id <-  rownames(BaySegll@data)
# BaySegll_points <-  fortify(BaySegll, region="id")
# colnames(BaySegll_points)[6] <- "id"
#map("state")

#---------------
pdf(paste0("./figures/",folder_name,"/M_with_map.pdf"), width = 12, height = 8)
#add an inset to M 
par(mar=c(4.1,6.1,2.0,0.2),yaxs="i", bty = "n") #bty = "n" ; get rid of box around plot
for (i in 1:length(NOAA_vec)){
  bxp(list(stats = t(summary_M[[i]][,4:8]), names = as.character(seq((yr_0+1), yr_last, by = 1))), 
    boxfill = NOAA_col_vec[i], 
    ylim = c(0, 1),
    ylab = 'Annual M',
    xlab = 'Year',
    cex.lab = 1.6,
    main = paste("NOAA Code", NOAA_vec[i]),
    whisklty = 'solid',
    medlwd = .8,
    boxlwd  = .8,
    axes = T
  )
  points(M_box[[i]]$BoxCount ~ M_box[[i]]$ModYr, type = "o", pch = 20, col = 'black')
  abline(h=0)
  u <- par("usr")
  v <- c(
    grconvertX(u[1:2], "user", "ndc"),
    grconvertY(u[3:4], "user", "ndc")
  )
  
  # v <- c( (v[1]+v[2])/2, v[2], (v[3]+v[4])/2, v[4] )
  orig_par <- par(no.readonly=T)
  par( fig=c(v[2]-0.20, v[2], v[4]-0.30, v[4]), new=TRUE, mar=c(0,0,0,0) ) # fig = c(xmin, xmax, ymin, ymax)
  #par( usr=c(-0.2, 10, 0, 5), new=TRUE, mar=c(0,0,0,0) )
  #plot(M_box[[i]]$BoxCount ~ M_box[[i]]$ModYr, axes=FALSE, xlab="", ylab="")
  
  #get the NOAA code position in the BaySeg layer.
  tmp_NOAA <- ifelse(nchar(NOAA_vec[i]) ==2,paste0("0",as.character(NOAA_vec[i])), as.character(NOAA_vec[i]))   #position of the NOAA code
  pos <- which(BaySeg$NOAACODE == tmp_NOAA)
  # #Only highlight the color of interest
  # map_col <- c(rep("white", length.out = pos - 1), "red", rep("white", length.out = length(BaySeg$NOAACODE)-pos))
  
  #plot(BaySeg, col = map_col)
  ##Use this one for color:
  #plot(BaySegll, col = map_col, axes = F)  #doesn't make a difference to use the lat lon version, unles we want to add axis labels. 
  plot(BaySegll, axes = F) #do not fill in the polygons.
  #may be hard to see the filled in NOAA codes for some of the smaller ones; 
  # figure out a way to draw a box around the NOAA code instead (or in addition to)
  #box() #to use, need to add bly = "o" into par().
  
  
  # Instead of coloring in the region, want to draw a box around the coordinates
  tmp_poly <- which(BaySegll$NOAACODE == tmp_NOAA)
  coord_test <- BaySegll@polygons[[tmp_poly]]@Polygons[[1]]@coords
  min_lon <- min(coord_test[,1])
  max_lon <- max(coord_test[,1])
  min_lat <- min(coord_test[,2])
  max_lat <- max(coord_test[,2])
  # box in these coordinates (may want to extend the area by some small amount, 
  # say 5-10%? depends on how the plots look.)
  rect(min_lon-0.05, min_lat-0.05, max_lon+0.05, max_lat+0.05, border = NOAA_col_vec[i], lwd = 3)
  #Add text with the number of time series.
  tmp_n_bar <- bar_reg_key %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    count()
  tmp_n_bar <- as.character(tmp_n_bar$n)
  #add number of bars as text. 
  text(x = -77,y = 39.5 , labels = tmp_n_bar, col = NOAA_col_vec[i], cex = 2)
  par(orig_par) #restore original plotting
}

dev.off()


#---------------------
# still need to create plots to look at values of M v. the number 
# of lives (mean), number of boxes (mean), etc. 
# can look at other parameters in this way as well, possibly? 

# 1. find the mean (or median) of l_obs by year.
pdf(paste0("./figures/",folder_name,"/Mod_obs_reg_live.pdf"), width = 12, height = 8)
for (i in 1: length(NOAA_vec)) { #plot for each NOAA code
  # tmp_l_obs <- l_obs[[i]] # extract the live obs of interest.
  tmp_l_obs<- raw_mod_dat %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    mutate(Y_ind = SampleYr - yr_0) %>% 
    filter(Y_ind > 0) %>%  #don't want for year 0
    select(Y_ind, L)
  #Get the median and sd for each year: 
  tmp_sum_l_obs <- tmp_l_obs %>% 
    group_by(Y_ind) %>% 
    summarise(obs_med = median(L), obs_sd = sd(L))
  #get the regional live model data:
  tmp_lambda_r <- data.frame(summary_lambda_r[[i]][,c(4,6,8)], Y_ind = 1:nyears)
  #Join model and observed results.
  tmp_df <- left_join(tmp_lambda_r, tmp_sum_l_obs, by = "Y_ind" ) 
  #rename colomuns for more intuitive plotitng
  colnames(tmp_df) <- c("mod_min", "mod_med", "mod_max", "Year", "obs_med", "obs_sd")
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = obs_med, y= mod_med )) +
    geom_abline(aes(intercept = 0, slope = 1), linetype = "dotted") + #1:1 line for reference.
    geom_pointrange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(y = 0, x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(expand = c(0,0), name = "Regional Lambda, Model")+ # to remove the space below 0.
    scale_x_continuous(expand = c(0,0), name = "Median observed live density/half bu")+#to get rid of the x axis space before the first year.
    ggtitle(paste("NOAA Code", NOAA_vec[i]) )+
    theme_classic()
  print(tmp_gg)
}
dev.off()
#Can also plot this by bar.

#plot live obs v model mortality rate
pdf(paste0("./figures/",folder_name,"/Mod_M_obs_live.pdf"), width = 12, height = 8)
for (i in 1:length(NOAA_vec)) { #plot for each NOAA code
  #tmp_l_obs <- l_obs[[i]] # extract the live obs of interest.
  tmp_l_obs<- raw_mod_dat %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    mutate(Y_ind = SampleYr - yr_0) %>% 
    filter(Y_ind > 0) %>%  #don't want for year 0
    select(Y_ind, L)
  #Get the median and sd for each year: 
  tmp_sum_l_obs <- tmp_l_obs %>% 
    group_by(Y_ind) %>% 
    summarise(obs_med = median(L), obs_sd = sd(L))
  #get the regional live model data:
  tmp_M <- data.frame(summary_M[[i]][,c(4,6,8)], Y_ind = 1:nyears)
  #Join model and observed results.
  tmp_df <- left_join(tmp_M, tmp_sum_l_obs, by = "Y_ind" ) 
  #rename colomuns for more intuitive plotitng
  colnames(tmp_df) <- c("mod_min", "mod_med", "mod_max", "Year", "obs_med", "obs_sd")
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = obs_med, y= mod_med )) +
    geom_hline(aes(yintercept=0))+
    geom_pointrange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(#y = 0, 
      x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(#expand = c(0,0), 
      name = "Model annual M", limits = c(0,1))+ # to remove the space below 0.
    scale_x_continuous(#expand = c(0,0), 
      name = "Median observed live density/half bu")+#to get rid of the x axis space before the first year.
    ggtitle(paste("NOAA Code", NOAA_vec[i], "M v Lives"))+
    theme_classic()
  print(tmp_gg)
}
dev.off()


#plot box obs v model mortality rate
pdf(paste0("./figures/",folder_name,"/Mod_M_obs_box.pdf"), width = 12, height = 8)
for (i in 1:length(NOAA_vec)) { #plot for each NOAA code
  # extract the box obs of interest.
  tmp_obs<- raw_mod_dat %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    mutate(Y_ind = SampleYr - yr_0) %>% 
    filter(Y_ind > 0) %>%  #don't want for year 0
    select(Y_ind, B)
  
  #Get the median and sd for each year: 
  tmp_sum_obs <- tmp_obs %>% 
    group_by(Y_ind) %>% 
    summarise(obs_med = median(B), obs_sd = sd(B))
  #get the M model data:
  tmp_M <- data.frame(summary_M[[i]][,c(4,6,8)], Y_ind = 1:nyears)
  #Join model and observed results.
  tmp_df <- left_join(tmp_M, tmp_sum_obs, by = "Y_ind" ) 
  #rename colomuns for more intuitive plotitng
  colnames(tmp_df) <- c("mod_min", "mod_med", "mod_max", "Year", "obs_med", "obs_sd")
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = obs_med, y= mod_med )) +
    geom_hline(aes(yintercept = 0))+
    geom_pointrange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(# y = 0, 
      x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(# expand = c(0,0), 
      name = "Model annual M", limits = c(0,1))+ # to remove the space below 0.
    scale_x_continuous(# expand = c(0,0), 
      name = "Median observed box density/half bu")+#to get rid of the x axis space before the first year.
    ggtitle(paste("NOAA Code", NOAA_vec[i], "M v. Boxes") )+
    theme_classic()
  print(tmp_gg)
}
dev.off()


#plot model live v model mortality rate
pdf(paste0("./figures/",folder_name,"/Mod_M_v_Mod_live.pdf"), width = 12, height = 8)
for (i in 1:length(NOAA_vec)) { #plot for each NOAA code
  #get the box model data:
  #find which bars go with the NOAA code
  tmp_bars_n_ID <- bar_reg_key %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    select(ModBar, ID)
  tmp_bars <- tmp_bars_n_ID$ModBar
  lambda_by_bar <- NULL
  for (n in 1:length(tmp_bars)){
    if(n == 1){
      lambda_by_bar <- data.frame(summary_lambda_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(lambda_by_bar) <- c("L_min", "L_med", "L_max") #95%CI's
      #create a new row with the BarID
      lambda_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],"ID"], length.out = nrow(lambda_by_bar))
      #create a new row with the Year Index (should be in order)
      lambda_by_bar$Y_ind <- 1:nyears
    }
    else{
      tmp_lambda_by_bar <- data.frame(summary_lambda_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(tmp_lambda_by_bar) <- c("L_min", "L_med", "L_max") #95%CI's
      #create a new row with the Bar ID
      tmp_lambda_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],'ID'], length.out = nrow(tmp_lambda_by_bar))
      tmp_lambda_by_bar$Y_ind <- 1:nyears
      lambda_by_bar <- bind_rows(lambda_by_bar, tmp_lambda_by_bar)
    }
  }
  #get the M model data:
  tmp_M <- data.frame(summary_M[[i]][,c(4,6,8)], Y_ind = 1:nyears)
  #rename colomuns for more intuitive plotitng
  colnames(tmp_M) <- c("M_min", "M_med", "M_max", "Y_ind")
  #Join model and observed results. Not M will be the same for all bars.
  tmp_df <- left_join(lambda_by_bar, tmp_M, by = "Y_ind" ) 
  
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = L_med, y= M_med )) +
    geom_hline(aes(yintercept = 0))+
    geom_pointrange(aes(ymin = M_min, ymax = M_max))+ #color = NOAA_col_vec[i] #can add color
    geom_errorbarh(aes(xmin = L_min, xmax = L_max))+
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(# y = 0, 
      x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(# expand = c(0,0), 
      name = "Model annual M", limits = c(0,1))+ # to remove the space below 0.
    scale_x_continuous(# expand = c(0,0), 
      name = "Model Median adult live density/half bu")+#to get rid of the x axis space before the first year.
    facet_wrap(~ID, scales = "free_x")+
    ggtitle(paste("NOAA Code", NOAA_vec[i], "M v. Lives") )+
    theme_classic()
  print(tmp_gg)
}
dev.off()


#plot model boxes v model mortality rate. 
pdf(paste0("./figures/",folder_name,"/Mod_M_v_Mod_box.pdf"), width = 12, height = 8)
for (i in 1:length(NOAA_vec)) { #plot for each NOAA code
  #get the box model data:
  #find which bars go with the NOAA code
  tmp_bars_n_ID <- bar_reg_key %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    select(ModBar, ID)
  tmp_bars <- tmp_bars_n_ID$ModBar
  beta_by_bar <- NULL
  for (n in 1:length(tmp_bars)){
    if(n == 1){
      beta_by_bar <- data.frame(summary_beta_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(beta_by_bar) <- c("B_min", "B_med", "B_max") #95%CI's
      #create a new row with the BarID
      beta_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],"ID"], length.out = nrow(beta_by_bar))
      #create a new row with the Year Index (should be in order)
      beta_by_bar$Y_ind <- 1:nyears
    }
    else{
      tmp_beta_by_bar <- data.frame(summary_beta_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(tmp_beta_by_bar) <- c("B_min", "B_med", "B_max") #95%CI's
      #create a new row with the Bar ID
      tmp_beta_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],'ID'], length.out = nrow(tmp_beta_by_bar))
      tmp_beta_by_bar$Y_ind <- 1:nyears
      beta_by_bar <- bind_rows(beta_by_bar, tmp_beta_by_bar)
    }
  }
  #get the M model data:
  tmp_M <- data.frame(summary_M[[i]][,c(4,6,8)], Y_ind = 1:nyears)
  #rename colomuns for more intuitive plotitng
  colnames(tmp_M) <- c("M_min", "M_med", "M_max", "Y_ind")
  #Join model and observed results. Not M will be the same for all bars.
  tmp_df <- left_join(beta_by_bar, tmp_M, by = "Y_ind" ) 
  
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = B_med, y= M_med )) +
    geom_hline(aes(yintercept = 0))+
    geom_pointrange(aes(ymin = M_min, ymax = M_max))+ #color = NOAA_col_vec[i] #can add color
    geom_errorbarh(aes(xmin = B_min, xmax = B_max))+
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(# y = 0, 
      x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(# expand = c(0,0), 
      name = "Model annual M", limits = c(0,1))+ # to remove the space below 0.
    scale_x_continuous(# expand = c(0,0), 
      name = "Model Median box density/half bu")+#to get rid of the x axis space before the first year.
    facet_wrap(~ID, scales = "free_x")+
    ggtitle(paste("NOAA Code", NOAA_vec[i], "M v. Boxes") )+
    theme_classic()
  print(tmp_gg)
}
dev.off()

#plot lagged model boxes v model mortality rate. 
#M in y versus boxes in y+1 (I think?)
pdf(paste0("./figures/",folder_name,"/Mod_M_y_v_Mod_box_y_plus_1.pdf"), width = 12, height = 8)
for (i in 1:length(NOAA_vec)) { #plot for each NOAA code
  #get the box model data:
  #find which bars go with the NOAA code
  tmp_bars_n_ID <- bar_reg_key %>% 
    filter(NOAACode == NOAA_vec[i]) %>% 
    select(ModBar, ID)
  tmp_bars <- tmp_bars_n_ID$ModBar
  beta_by_bar <- NULL
  for (n in 1:length(tmp_bars)){
    if(n == 1){
      beta_by_bar <- data.frame(summary_beta_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(beta_by_bar) <- c("B_min", "B_med", "B_max") #95%CI's
      #create a new row with the BarID
      beta_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],"ID"], length.out = nrow(beta_by_bar))
      #create a new row with the Year Index (should be in order)
      beta_by_bar$Y_ind <- 0:(nyears-1) #Y_ind-1, actually. to plot M versus beta in the next year.
    }
    else{
      tmp_beta_by_bar <- data.frame(summary_beta_i[[tmp_bars[n]]][,c(4,6,8)])
      colnames(tmp_beta_by_bar) <- c("B_min", "B_med", "B_max") #95%CI's
      #create a new row with the Bar ID
      tmp_beta_by_bar$ID <- rep(tmp_bars_n_ID[tmp_bars_n_ID$ModBar==tmp_bars[n],'ID'], length.out = nrow(tmp_beta_by_bar))
      tmp_beta_by_bar$Y_ind <- 0:(nyears-1) #Y_ind-1, actually. to plot M versus beta in the next year.
      beta_by_bar <- bind_rows(beta_by_bar, tmp_beta_by_bar)
    }
  }
  #get the M model data:
  tmp_M <- data.frame(summary_M[[i]][,c(4,6,8)], Y_ind = 1:nyears) #leave normal y index.
  #rename colomuns for more intuitive plotitng
  colnames(tmp_M) <- c("M_min", "M_med", "M_max", "Y_ind")
  #Join model and observed results. Not M will be the same for all bars.
  tmp_df <- left_join(beta_by_bar, tmp_M, by = "Y_ind" ) 
  
  #plot.
  tmp_gg <- ggplot(tmp_df, aes(x = B_med, y= M_med )) +
    geom_hline(aes(yintercept = 0))+
    geom_pointrange(aes(ymin = M_min, ymax = M_max))+ #color = NOAA_col_vec[i] #can add color
    geom_errorbarh(aes(xmin = B_min, xmax = B_max))+
    #geom_text(aes(label = Y_ind), color = 'red')+
    #geom_linerange(aes(ymin = mod_min, ymax = mod_max))+ #color = NOAA_col_vec[i] #can add color
    #geom_text(aes(label = Year), color = "blue")+ #use this and line to label by year 
    expand_limits(# y = 0, 
      x = 0)+ #to force all the graphs to start at 0.
    scale_y_continuous(# expand = c(0,0), 
      name = "Model annual M", limits = c(0,1))+ # to remove the space below 0.
    scale_x_continuous(# expand = c(0,0), 
      name = "Model Median box density/half bu in yr y+1")+#to get rid of the x axis space before the first year.
    facet_wrap(~ID)+
    ggtitle(paste("NOAA Code", NOAA_vec[i], "M v. Boxes in following yr") )+
    theme_classic()
  print(tmp_gg)
} #warnings are because of "year 0" values which can not be shown. 
dev.off()


#Maybe make other similar plots? Or perhaps it isn't necessary?


