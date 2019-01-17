# Header -----------------------------------------------------------------------
# Dyanamic Factor Analysis of Natural mortality rates in Chesapeake Bay 
# 
# This wil be done by converting the median values for M to instantaneous rates
# and Using these values to plug into DFA and compare models w/ 1-4 trends and a 
# diagonal and equal covariance matrix for the error term.
# 
# Next, Use AICc and SS resid/ SS obs as well as qualitative model fits to select 
# the most feasible model. 
# Use the MARS package with all of the time series of oyster natural mortality.
# 
# Script based on https://nwfsc-timeseries.github.io/atsa-labs/sec-dfa-fitting-dfa-models-with-marss.html
# 
# Written by Kathryn Doering on 17 Jan 2019
#
#TODO: split this script into several: 1 to run the models, 1 to select the model,
# and 1 to plot the results of the best model(s).
# Also, perhaps add an additional script with high quality plots for a manuscript
# (could consider doing 1 script for all? Or by folder? Not sure right now...)
# Load packages, set options ---------------------------------------------------

library(rstan)
library(MARSS)
library(dplyr)
library(tidyr)
options(stringsAsFactors = F) #change the default

# Functions to use: 
source('./Code/Extract_and_plot_functions.R')

# Get data ---------------------------------------------------------------------
#get the data (model output)
folder_name <- "All_run_4"
#all_dat <- read.csv(paste0("./models_and_derived_data/", folder_name, "/median_M_all.csv"))
#Load model and data associated with it:
bar_reg_key <- read.csv(paste0("./models_and_derived_data/", folder_name, "/bar_reg_key.csv"))
raw_mod_dat <- read.csv(paste0("./models_and_derived_data/", folder_name, "/raw_dat.csv"))

mod <- readRDS(file = paste0("./models_and_derived_data/", folder_name, "/model.rda"))
mod_dat <- readRDS(file = paste0("./models_and_derived_data/", folder_name, "/model_dat.rda"))

NOAA_vec <- read.csv(paste0("./models_and_derived_data/",folder_name,"/NOAA_list.csv"))
NOAA_vec <- NOAA_vec$x

regions <-  read.csv("./Data/Doering_Thesis_Regions_3.csv")

# Set years --------------------------------------------------------------------
# Years
# dat <- filter(all_dat, NOAACode %in% NOAA_Codes)
#dat <- all_dat #when all NOAA codes included
yr_min <- 1990
yr_1   <- 1991
yr_max <- 2017
nyears <- yr_max - yr_min #not including year 0

# Get DFA input ----------------------------------------------------------------

#Get data: M. Put in a data frame instead of a list. 
Med_M <- data.frame()
for(i in 1:length(NOAA_vec)){
  #Get model summary of M.
  tmp <- Summarize_Par_Reg(mod = mod, par = "M", nyears = nyears, reg = i)
  tmp_med <- tmp[,"50%"] #get median values only
  tmp_med_ins <- -log(1-tmp_med) #convert to instantaneous
  tmp_df <- data.frame(year = yr_1:yr_max, NOAA_code= NOAA_vec[i], M_Med_Inst = tmp_med_ins)
  Med_M <- bind_rows(Med_M, tmp_df)
}
#make another NOAA code factor column and then use this and years to reorder in terms of regions.
# Dont add the region labels for now.
Med_M$NOAA_fac <- factor(Med_M$NOAA_code, levels = regions$NOAA_code, 
  #labels = paste0("NOAA Code ", Report_reg$NOAA_code, ":\n", Report_reg$NOAA_Name_Short),
  ordered = T)
Med_M <- Med_M %>% 
  arrange(NOAA_fac, year) %>% 
  select(year, NOAA_code, M_Med_Inst)

#write.csv(Med_M, paste0("./models_and_derived_data/", folder_name, "/median_M_inst_df_ordered.csv"), quote = T)

#make Med_M into a matrix where rows are the years and columns are each NOAA code.
Med_M_mat <- matrix(Med_M$M_Med_Inst, nrow = nyears, ncol = length(unique(Med_M$NOAA_code)))

rownames(Med_M_mat) <- yr_1:yr_max
colnames(Med_M_mat) <- unique(Med_M$NOAA_code)

dat_wide <- t(Med_M_mat) #transpose because we want each row to be a different nooa code and each year in a different column. 

NOAA <- rownames(dat_wide)
#Calculate the z scores for the data, which will make the model easier to run.
mean <- apply(dat_wide, 1, mean, na.rm = T) #means for each row
sd <-   apply(dat_wide, 1, sd, na.rm = T) #standard dev for each row
#get the z score by subtracting the mean from each value and then divided by the sd
# (check with mike to make sure this is correct. )
#this is also what the Zurr et al. 2003 paper describes as "standardized"

dat_wide <- (dat_wide - mean)/sd

# dat_wide is in the correct format with the right values to be modeled.
## get number of time series
N_ts <- dim(dat_wide)[1]
## get length of time series
TT <- dim(dat_wide)[2]

# Plot DFA input ---------------------------------------------------------------
#Plot the data (zscored and demeaned).
#NOAA <- rownames(dat_wide)
#clr <- c("brown", "blue", "darkgreen", "darkred", "purple")
cnt <- 1
png(paste0("./figures/",folder_name,"/dfa_plot_inst_ts.png"), res = 300, width = 10, height =60, units = "in")
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,
  0, 0, 0))
for (i in NOAA) {
  plot(dat_wide[i, ], xlab = "", ylab = "M index (Z-transformed)", bty = "L",
    xaxt = "n", pch = 16,
    #col = clr[cnt],
    type = "b")
  axis(1, 0:dim(dat_wide)[2] + 1, yr_1 + 0:dim(dat_wide)[2])
  title(i)
  cnt <- cnt + 1
}
dev.off()

#plot all the code together.
#Alpha is used to add transparency; darker portions are where lines overlay each other,
#Where as single lines are shown in gray. 
#must use png, bmp, or jpeg for this to show up
png(paste0("./figures/",folder_name,"/dfa_plot_inst_ts_together.png"), res = 300, width = 8, height = 4, units = "in")
#par(mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0,0, 0, 0))
par(xaxs="i", yaxs="i")
plot(dat_wide[1, ], xlab = "Year", ylab = "M index (Z-transformed)", bty = "L",
  xaxt = "n", ylim = c(min(dat_wide)-0.1, max(dat_wide)+0.1), col = alpha("black", 0.5),
  #col = clr[cnt],
  type = "l", lwd = 2)
axis(1, 1:nyears, yr_1:yr_max)
for (i in 2:length(NOAA)) {
  lines(dat_wide[i, ], col = alpha("black", 0.5), lwd = 2)
}
dev.off()

#Plot the means and standard deviations of the data.
NOAACODE <- names(mean)
mean_sd_sum <- data.frame(NOAACODE = NOAACODE, Mean = mean, SD = sd)
#Add back zero in front of 2 character NOAA codes.
mean_sd_sum$NOAACODE <- ifelse(nchar(mean_sd_sum$NOAACODE)==2, paste0("0",mean_sd_sum$NOAACODE), mean_sd_sum$NOAACODE) #add back zero
#write to .csv.
write.csv(mean_sd_sum, paste0("./models_and_derived_data/", folder_name, "/mean_sd_M_inst_by_NOAA_summary.csv"),
  row.names = F, 
  quote = T
)
#Will use these data to plot means and sds by NOAA code in ArcGIS. 
#
# Estimate DFA models ----------------------------------------------------------
# Code to compare multiple models

# set new control params
con_list <-  list(minit=200, maxit=50000, conv.test.slope.tol = 0.1) #specify the max iterations
# set up forms of R matrices
# levels_R <-  c("diagonal and equal",
#              #"diagonal and unequal",
#              #"equalvarcov")#,
#              #"unconstrained")
levels_R <- "diagonal and equal" #only use a diagonal and equal covariance structure
max_m <- 4 #the maximum number of trends
model_data <- data.frame()
# fit lots of models & store results
# NOTE: this will take a long time to run!
#This will test all correlation structures, as well as all
for(R in levels_R) {
  for(m in 1:max_m) {
    dfa_model <-  list(m=m, R=R) #put the R and m into the data.
    tmp_mod <-  MARSS(dat_wide, model=dfa_model, control=con_list,
      form="dfa", z.score=FALSE)
    model_data <-  rbind(model_data,
      data.frame(R=R,
        m=m,
        logLik=tmp_mod$logLik,
        K=tmp_mod$num.params,
        AICc=tmp_mod$AICc,
        Converged = tmp_mod$convergence, #0 means converged, 10 means did not
        stringsAsFactors=FALSE))
    assign(paste("tmp_mod", m,"diagonal_and_equal", sep="_"), tmp_mod) #assign a variable name to the model
  } # end m loop
} # end R loop
#print all model result to the screen
sink(paste0("./models_and_derived_data/",folder_name,"/dfa_results_M_inst_summmaries.txt"))
for(R in levels_R) {
  for(m in 1:max_m) {
    print(paste("model with m = ", m, "and R is ", R)) #label
    print(summary(get(paste("tmp_mod", m,"diagonal_and_equal", sep="_")))) #print the summary
  }
}
sink()
sink(paste0("./models_and_derived_data/",folder_name,"/dfa_results_summmaries_M_inst_quick.txt"))
for(R in levels_R) {
  for(m in 1:max_m) {
    print(paste("model with m = ", m, "and R is ", R)) #label
    print(get(paste("tmp_mod", m,"diagonal_and_equal", sep="_"))) #print the summary
  }
}
sink()
model_data$AICc- min(model_data$AICc)
write.csv(model_data, paste0("./models_and_derived_data/",folder_name,"/DFA_results_M_inst_1.csv"))

# Model selection --------------------------------------------------------------

#TODO: decide if this should be moved to a separate script?

#Compare AICc values

#compare mean fit diagnostic among the models

dfa_2 <- get(paste("tmp_mod", 2,"diagonal_and_equal", sep="_"))
dfa_3 <- get(paste("tmp_mod", 3,"diagonal_and_equal", sep="_"))

#save just in case
saveRDS(dfa_2, paste0("./models_and_derived_data/",folder_name,"/DFA_results_M_inst_2_trends.rda"))
saveRDS(dfa_3, paste0("./models_and_derived_data/",folder_name,"/DFA_results_M_inst_3_trends.rda"))


# To do:- calculate mean fit diagnostic, expected values, and plots for 2 and 3 trend
# models


# Plot trends and loadings -----------------------------------------------------
# TODO: move to a separate script, and do Varimax rotation before making plots
# of trends and loadings.

#Make plots for 2 or 3 trends based on AIC:


#Plot location code.
#paste0("./figures/", folder_name,"/dfa_",m_min,"_trends_diag_equal_all_ts_trends_loading.png"

#a function to plot the trends and loadings.
#dfa_dat is the model data input, MLEobj is the dfa model, plot location is where
#the png plot will be made, minZ is the cutoff loading value to plot (can be 0. )


#make labels that correspond to the modle years (the second year is when natural
# mortality actualy occurs)
years <- yr_1:yr_max
years_prev <- (yr_1-1):(yr_max-1)
lab_names <- paste0(years)

#Use the function above to plot the trends.
plot_trends_and_loadings_no_varimax(dfa_dat = dat_wide, MLEobj = dfa_2, 
  file_name = paste0("./figures/", folder_name,"/dfa_","2","_trends_diag_equal_trends_loading_inst_ordered.png"),
  minZ = 0, lab_names = lab_names)

plot_trends_and_loadings_no_varimax(dfa_dat = dat_wide, MLEobj = dfa_3, 
  file_name = paste0("./figures/", folder_name,"/dfa_","3","_trends_diag_equal_trends_loading_inst_ordered.png"),
  minZ = 0, lab_names = lab_names)
#The trends are still understandable (maybe even more so) WITHOUT the varimax rotation. 


# Compare the model fits. -------------------------------------------------------

mean_fit <- NULL
#calculate Mean fit for all plots
#for(R in levels_R) {
for(m in 1:max_m) {
  tmp_mod <- get(paste("tmp_mod", m,"diagonal_and_equal", sep="_"))
  tmp_mean_fit <- get_mean_fit_no_varimax(dat_wide, tmp_mod) #get the tmp mean fit
  #sarve only mean fit since that is all we care about for now
  mean_fit <- c(mean_fit, tmp_mean_fit)
}
#}
#mean fit is acceptable for all of the models. gets better 

model_data$mean_fit <- mean_fit
model_data$deltaAICc <- model_data$AICc - min(model_data$AICc)
#save again wit these new columns
write.csv(model_data, paste0("./models_and_derived_data/",folder_name,"/DFA_results_M_inst_1_mean fits.csv"))

# get model fits & CI's---------------------------------------------------------

mod_fit_2 <- get_DFA_fits_no_varimax(dfa_2) #fit for 2 trends
mod_fit_3 <- get_DFA_fits_no_varimax(dfa_3) #fit for 3 trends
#check ss calcs. 

resid_sq <- (dat_wide - mod_fit_2$ex)^2
SS_resid <- apply(resid_sq,1,sum)
# #calculate the sum of squared data for each time series as a check on the mean_fit_no_varimax function.
# DFA_dat_sq <- dat_wide^2
# SS_dat <- apply(DFA_dat_sq,1,sum)
# fit_ss <- SS_resid/SS_dat #fit for each time series
# meanfit <- mean(SS_resid/SS_dat) #average over all time series

# Plot the fits------------------------------------------------------------------

#2 trends'
mod_fit <- mod_fit_2
w_ts <- seq(dim(dat_wide)[2]) 
ylbl <- as.character(rownames(dat_wide))
pdf(paste0("./figures/", folder_name,"/dfa_2_trends_diag_equal_all_ts_plot_fits.pdf"), width = 10, height =60)
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0, 
  0, 0, 0))
for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", ylab = ylbl[i], xaxt = "n", type = "n", 
    cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1, 0:dim(dat_wide)[2] + 1, yr_min + 0:dim(dat_wide)[2])
  points(w_ts, dat_wide[i, ], pch = 16, col = "red")
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}
dev.off()

#3 trends
mod_fit <- mod_fit_3
w_ts <- seq(dim(dat_wide)[2]) 
ylbl <- as.character(rownames(dat_wide))
pdf(paste0("./figures/", folder_name,"/dfa_3_trends_diag_equal_all_ts_plot_fits.pdf"), width = 10, height =60)
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0, 
  0, 0, 0))
for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", ylab = ylbl[i], xaxt = "n", type = "n", 
    cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1, 0:dim(dat_wide)[2] + 1, yr_min + 0:dim(dat_wide)[2])
  points(w_ts, dat_wide[i, ], pch = 16, col = "red")
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}
dev.off()

