# Functions to manipulate data from the fall dredge survey into a form that can
# be run by the model, and make some diagnostic plots. 
#-------------------------------------------------------------------------------
# Function 1: Data Selection. Select only datas in NOAA codes specified that have
# bars with a complete time series

#inputs are dat, the fall survey data, NOAA_list, a vector containing the NOAA 
#codes of interest (some may not have sufficient data, though), the year 0 for 
# the model, the last year of the model, if observations of boxes and live oysters
# where both are 0 should be removed, the minimum number of bars the time series
# must have to be included. 
DataSelection <- function(dat, NOAA_list, yr_0, yr_last, remove_0 = FALSE, n_ts = 2){
    require(dplyr)
    n_yr_tot <- yr_last-yr_0+1 # Select only bars that have a COMPLETE time series.
    # Data Selection
    
    # #vector containting the number of bars by NOAA code to fill in 
    # n_bars <- rep(0, length.out = length(NOAA_list))
    # #matix containing the number of observations by year NOAA code to fill in 
    # n_obs_by_yr <- matrix(0, nrow = (yr_last - yr_0 + 1), ncol = length(NOAA_list)) #to fill in
    
    # Select to observations to set up the data frame. These are flagged and will be
    # deleted after selecting data. 
    all_dat_ts <- dat[1:2,] %>% 
        select(SampleEvent, RepEvent, ID, SampleYr, NOAACode, NumMrktLive, 
               NumSmllLive, NumMrktAllBox, NumSmllAllBox) #will delete these two obs later
    all_dat_ts$ID <- c("to delete", "to delete") #to flag to delete later
    
    # Select data and fill in n_bars, n_obs_by_yr, and all_dat_ts by looping over 
    # NOAA codes.
    for (i in 1:length(NOAA_list)){
        #find which bars have at least n_yr_tot years of data. These bars (in vector
        # called bars) will be included in the analysis
        bars <- dat %>% 
            filter(NOAACode == NOAA_list[i]) %>% 
            filter(SampleYr >= yr_0, SampleYr <= yr_last) %>% 
            select(ID, SampleYr, NOAACode, NumMrktLive, NumSmllLive, NumMrktAllBox, NumSmllAllBox) %>% 
            na.omit() %>%  #remove any samples that have NA's in any of the necessary columns selected above
            select(ID, SampleYr) %>% #only want these to count number of years for each ID
            distinct() %>%  #remove duplicates
            count(ID) %>%  #number of years for each ID
            filter(n == n_yr_tot) %>%  #select only bars with a complete time series
            select(ID)
        
        # if (nrow(bars) == 0) {
        #     n_bars[i] <- 0
        #     next #go to next iteration of for loop if nothing else to calculate
        # }
        if (nrow(bars) < n_ts){
            next #go to next iteration of for loop if nothing else to calculate
        }
        #Extract bars and data columns needed (all numbers are standardized per 1/2 bu)
        dat_ts <- dat %>% 
            filter(SampleYr >= yr_0, SampleYr <= yr_last) %>%
            filter(ID %in% bars$ID) %>% #subset only the bars n_yr_tot of data.
            select(SampleEvent, RepEvent, ID, SampleYr, NOAACode, NumMrktLive, 
                   NumSmllLive, NumMrktAllBox, NumSmllAllBox) %>% 
            na.omit()
        
        #save the data set for each bar in 1 dataframe.
        all_dat_ts <- bind_rows(all_dat_ts, dat_ts)
        
    #     #Calculate number of samples by year for the NOAA code
    #     tmp_n_obs <- dat_ts %>% 
    #         group_by(SampleYr) %>% 
    #         count() %>% 
    #         arrange(SampleYr)
    #     #Save number of bars with complete series
    #     n_bars[i] <- length(bars$ID)
    #     #save n_obs_by_yr (noaa code on top of data frame, each row represents a yr)
    #     if(nrow(tmp_n_obs)==0){
    #         n_obs_by_yr[,i] <- 0
    #         next
    #     }
    #     n_obs_by_yr[,i] <- tmp_n_obs$n
    }

    #delete 2 rows used to intialize dataframe
    all_dat_ts <-  all_dat_ts %>% 
        filter(ID != "to delete")
    
    # calculate for all the data the total number of adult lives and total number of
    # adult-sized boxes. 
    all_dat_ts <- all_dat_ts %>% mutate(L = NumMrktLive + NumSmllLive) %>% 
        mutate(B = NumMrktAllBox + NumSmllAllBox)
    
    if(remove_0 == TRUE){
        all_dat_ts <- all_dat_ts %>% filter(L>0|B>0)
    }
    return(all_dat_ts)
}


#-------------------------------------------------------------------------------
# Explore data set to make sure it subset correctly.
# inputs required are a time series of complete bars (output from DataSelection
# function, folder name (must exist), and file name of the pdf that is being 
# output.)
MakeDataPlots <- function(all_dat_ts, folder_name, file_name) {
    require(dplyr)
    require(ggplot2)
    dir.create(paste0("./figures/", folder_name))
    pdf(paste0('./figures/', folder_name, '/', file_name, '.pdf'), width = 6, height = 4)
    length(unique(all_dat_ts$ID)) 
    #For all the data:
    print(ggplot(all_dat_ts, aes(SampleYr)) +
        geom_bar()+ #by year, number os samles, fairly even.
        ggtitle("Number of samples by year, all NOAA Codes"))
    # ggplot(all_dat_ts, aes(ID,SampleYr))+
    #     geom_count() #sampling by year and ID>
    #By NOAA code
    for (i in 1:length(NOAA_list)){ #Loop over NOAA codes.
        print(ggplot(all_dat_ts[all_dat_ts$NOAACode == NOAA_list[i],], aes(ID)) +
            geom_bar()+#Total number of samples by bar. (May need to add title with NOAA code?)
            ggtitle(paste("Number of samples by bar, NOAA Code", NOAA_list[i])))
        print(ggplot(all_dat_ts[all_dat_ts$NOAACode == NOAA_list[i],], aes(x =ID,y =SampleYr))+
            geom_count()+# Number of samples by year and ID>
            ggtitle(paste("Number of samples by year and bar, NOAA Code", NOAA_list[i])))
        
        tmp_count <- all_dat_ts %>% filter(NOAACode == NOAA_list[i]) %>%
            group_by(ID, SampleYr) %>%
            count() #count the number of samples in each year and bar
        #plot the number of samples in each year and bar
        print(ggplot(tmp_count, aes(x=SampleYr, y=n))+
            geom_col()+
            facet_wrap(~ID)+
            ggtitle(paste("Number of samples by bar, NOAA Code", NOAA_list[i])))
    }
    dev.off()
    #not there are no variables returned from making these plots. 
}

#-------------------------------------------------------------------------------
#Next section: modified from M_Probability model/code/stan/mod_d/d_1/mod_d_1_6_real_dat_1.R.
#modified for the data structure of model_d_3_1.stan.
# R_q: value for ratio of catchability
# calc_log_r_p: T if log_lambda_r_p and log_beta_0_r_p should be calculated from the
# data, false if they should be set to values in the function
# log_r_p: set values for log_lambda_r_p and log_beta_0_r_p
CreateModDataList <- function(all_dat_ts, R_q = 1.68, calc_log_r_p = T, 
                              log_lambda_r_p = 0, log_beta_0_r_p = 0){
    require(dplyr)
    #put data for each NOAA code into its own component in a list. 
    dat_list <- list() #create a blank list to explore the
    for (i in 1:length(NOAA_list)){
        dat_list[[i]] <- all_dat_ts %>% filter(NOAACode == NOAA_list[i]) %>% arrange(SampleYr) %>% arrange(ID)
    }
    
    #Data for Years
    yr_0  <- all_dat_ts %>% select(SampleYr) %>% min()
    yr_last <- all_dat_ts %>% select(SampleYr) %>% max()
    yr_vec <- seq(yr_0, yr_last, by = 1)
    
    #Number of bars in each region. Call 192 - region 1 and 292 region 2.
    #I <- c(length(unique(dat_192$ID)), length(unique(dat_292$ID))) #8 bars in 192, 3 in 292.
    
    #year key (use for both regions)
    yr_key <- data.frame(ModYr = seq(0, by = 1, length.out = length(yr_vec)), 
                         SampleYr = yr_vec)
    Y <- length(yr_vec)-1 # subtract 1 for year 0.
    
    #-------------------
    #For region i
    
    #bar key is not numbering correctly. need to keep looking into.
    #make a list of bar keys where each bar key is for a different NOAA Code
    bar_key_each <- list() 
    I_each       <- list() #make a list of number of bars for each region. 
    obs_each     <- list()
    n_obs_each_tot <- list()
    obs_each_0   <- list()
    n_obs_each_0 <- list()
    tmp_ModBar_max <- 0
    for (i in 1:length(NOAA_list)){
        #bar key
        tmp_bar_list <- unique(dat_list[[i]]$ID) # Not sure if this will subset correctly?
        I_each[[i]] <- length(tmp_bar_list) #number of bars
        bar_key_each[[i]] <- data.frame(ModBar = seq((tmp_ModBar_max+1), by = 1, length.out = length(tmp_bar_list)), 
                                ID = tmp_bar_list, stringsAsFactors = F)
        tmp_ModBar_max <- max(bar_key_each[[i]]$ModBar) #get max to use for next iteration.
        #add ModYr and ModBar columns to the data frame
        dat_list[[i]] <- dat_list[[i]] %>% 
            left_join(yr_key, by = "SampleYr") %>%  # yr key should be the same for all regions.
            left_join(bar_key_each[[i]], by = "ID")      # bar key depends on region.
        
        obs_each[[i]] <- dat_list[[i]] %>%
            select(ModBar, ModYr, L, B) %>%
            filter(ModYr > 0)
        obs_each[[i]]$ModBar <- as.integer(obs_each[[i]]$ModBar)
        obs_each[[i]]$ModYr <- as.integer(obs_each[[i]]$ModYr)
        obs_each[[i]]$L <-  as.integer(obs_each[[i]]$L)
        obs_each[[i]]$B <- as.integer(obs_each[[i]]$B)
        obs_each[[i]] <- as.matrix(obs_each[[i]])
        n_obs_each_tot[[i]] <- nrow(obs_each[[i]]) #total number of observations
        obs_each_0[[i]] <- dat_list[[i]] %>% 
                            filter(ModYr == 0) %>% 
                            select(ModBar, B) #select only year 0 box obs. Don't need lives for year 0. 
        obs_each_0[[i]]$ModBar <- as.integer(obs_each_0[[i]]$ModBar)
        obs_each_0[[i]]$B <- as.integer(obs_each_0[[i]]$B)
        obs_each_0[[i]] <- as.matrix(obs_each_0[[i]]) #not sure if this will work?
        n_obs_each_0[[i]] <- nrow(obs_each_0[[i]])
    }
    
    #-------------------------------------------------------------------------------
    #Now, put data together for all bars:
    REG <- length(NOAA_list) #number of regions
    #First iteration
    I <- I_each[[1]]
    obs <- obs_each[[1]]
    obs_0 <- obs_each_0[[1]]
    for (i in 2:length(NOAA_list)){
        I <- c(I,I_each[[i]]) #number of bars in each region.
        obs <- rbind(obs,obs_each[[i]])
        obs_0 <- rbind(obs_0, obs_each_0[[i]])
    }
    I_tot <- sum(I)
    n_obs_tot <- nrow(obs)
    n_obs_0 <- nrow(obs_0)
    
    I_REG <- rep(1, times = I[1])#first iteration
    for (i in 2:length(NOAA_list)){
        I_REG <- c(I_REG, rep(i, times = I[i]))
    }
    
    #Make sure to keep track of bars and the region they are from
    for (i in 1:length(NOAA_list)){
    bar_key_each[[i]]$ModReg <- rep(i, length.out = nrow(bar_key_each[[i]]))
    bar_key_each[[i]]$NOAACode <- rep(NOAA_list[i], length.out = nrow(bar_key_each[[i]]))
    }
    
    #Bind rows
    bar_key <- bar_key_each[[1]]
    for (i in 2:length(NOAA_list)){
        bar_key <- bind_rows(bar_key,bar_key_each[[i]])
    }
    
    # Data for constants and priors (may need to modify these at some point)
    #R_q <- 1.68 # Mean value, ratio of catchability # added to function input instead.
    d_p <- c(0.51, 0.04) #Normal distribution for the mean.
    M_p <- c(1, 1) # Gives more support for M ~ 0.2
    Z_sigma <- 3.0
    
    logmeanL <- mean(log(obs[,'L']+0.01)) # rough mean param for prior.
    logmeanB <- mean(log(obs[,'B']+0.01)) #rough mean for prior on Beta.
    #rough priors (could change them to be region specific, but not meant to be that informative.)
    if (calc_log_r_p == T){ # change if want to calculate them from data. 
        log_lambda_r_p <- matrix(rep(c(logmeanL,5), times = length(NOAA_list)), nrow = length(NOAA_list), ncol = 2, byrow = T)
        log_beta_0_r_p <- matrix(rep(c(logmeanB,5), times = length(NOAA_list)), nrow = length(NOAA_list), ncol = 2, byrow = T)
    } else{
        log_lambda_r_p <- matrix(rep(c(log_lambda_r_p,5), times = length(NOAA_list)), nrow = length(NOAA_list), ncol = 2, byrow = T)
        log_beta_0_r_p <- matrix(rep(c(log_beta_0_r_p,5), times = length(NOAA_list)), nrow = length(NOAA_list), ncol = 2, byrow = T)
    }
    
    
    
    #-------------------------------------------------------------------------------
    # Put in a list to read into model:
    mod_dat <- list(
        Y = as.integer(Y),
        REG = as.integer(REG),
        I = I,
        I_tot = I_tot, 
        n_obs_tot=n_obs_tot,
        n_obs_0 = n_obs_0,
        Y_ind = as.vector(obs[,"ModYr"]),
        I_ind = as.vector(obs[,"ModBar"]),
        L     = as.vector(obs[,"L"]),
        B     = as.vector(obs[,"B"]),
        I_ind_0 = as.vector(obs_0[,"ModBar"]),
        B_0     = as.vector(obs_0[,"B"]),
        I_REG   = I_REG,
        R_q = R_q, #KD modified 12/12/2018 (but should be back compatible)
        d_p = d_p,
        M_p = M_p,
        Z_sigma = Z_sigma,
        log_lambda_r_p=log_lambda_r_p,
        log_beta_0_r_p=log_beta_0_r_p
    )
    
    # create a list of the "raw_mod_dat", essentially the model in flat form that
    # may be easier to manipulate for later plotting.
    raw_mod_dat <- dat_list[[1]]
    for (i in 2:length(NOAA_list)){
        raw_mod_dat <- bind_rows(raw_mod_dat, dat_list[[i]])
    }
    #-------------------------
    # Make return data list: things of interest
    return_list <- list(
                        mod_dat = mod_dat, 
                        bar_key = bar_key,
                        raw_mod_dat = raw_mod_dat
                        )
    return(return_list)
}


