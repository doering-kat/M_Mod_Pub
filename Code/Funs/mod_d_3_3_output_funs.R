# Header ----------------------------------------------------------------------

# Functions to manipulate data from the fall dredge survey into a form that can
# be run by the model, and make some diagnostic plots. 
# Contains 3 functions
# Written by KD

# Function 1: Select Data ---------------------------------------------------

# Select only datas in NOAA codes specified that have bars with a complete time 
# series

# inputs:
# dat, the fall survey data, (dataframe) 
# NOAA_vec, a vector containing the NOAA  codes of interest (some may not have 
  # sufficient data, though),
# yr_0, the year 0 for the model, an integer
# yr_last, the last year of the model, an integer
# remove_0, true if observations of boxes and live oysters  where both are 0 
  # should be removed, logical
# n_ts, the minimum number of bars the time series must have to be included, an integer

# output:
#  all_dat_ts, a dataframe containing data for bars wit a complete time series

SelectData <- function(dat, NOAA_vec, yr_0, yr_last, remove_0 = FALSE, n_ts = 2) {
  
    require(dplyr)
    n_yr_tot <- yr_last-yr_0+1 # calc total number of years, including yr_0
    
    #Check Inputs --------------------------------------------------------------
    
    # Only some basic checks, does not make function completely foolproof.
    #make sure the number of years is at least 2
    if(n_yr_tot < 2) stop(paste0("Number of years is ", n_yr_tot,", which is too few. Check yr_last and yr_0."))
    # check dat is a dataframe
    if(!is.data.frame(dat)) stop("dat must be a dataframe.")
    # check NOAA_vec is a vector of integers.
    if((!is.vector(NOAA_vec))|(!is.integer(NOAA_vec))) stop("NOAA_vec must be a vector of integers.")
    # check that n_ts is an integer
    if(!is.integer(n_ts)) stop("n_ts must be an integer.")
    
    # Data Selection -----------------------------------------------------------
    # 
    # remove observations where both live and box counts are zero, if specified.
    if(remove_0 == TRUE){ 
      dat <- dplyr::filter(dat, 
        ((NumMrktLive+NumSmllLive)>0|(NumMrktAllBox+NumSmllAllBox)>0))
    }
    
    # Select data by looping through NOAA codes
    all_dat_ts <- data.frame() # data.frame to fill in.
    for (i in 1:length(NOAA_vec)){ 
      
        # For the NOAA code, find which bars have at least n_yr_tot years of data. 
        # These bars (in vector called bars) will be included in the analysis
        bars <- dat %>% 
            dplyr::filter(NOAACode == NOAA_vec[i]) %>% 
            dplyr::filter(SampleYr >= yr_0, SampleYr <= yr_last) %>% 
            dplyr::select(ID, SampleYr, NOAACode, NumMrktLive, NumSmllLive, NumMrktAllBox, NumSmllAllBox) %>% 
            na.omit() %>%  #remove any samples that have NA's in any of the necessary columns selected above
            dplyr::select(ID, SampleYr) %>% #only want these to count number of years for each ID
            dplyr::distinct() %>%  #remove duplicates
            dplyr::count(ID) %>%  #number of years for each ID
            dplyr::filter(n == n_yr_tot) %>%  #select only bars with a complete time series
            dplyr::select(ID)
        
        if (nrow(bars) < n_ts){
            next #go to next iteration of for loop if nothing else to calculate
        }
        
        #Extract bars and data columns needed (all numbers are standardized per 1/2 bu)
        tmp_dat_ts <- dat %>% 
            dplyr::filter(SampleYr >= yr_0, SampleYr <= yr_last) %>%
            dplyr::filter(ID %in% bars$ID) %>% #subset only the bars n_yr_tot of data.
            # select only relavent columns for the model
            dplyr::select(SampleEvent, RepEvent, ID, SampleYr, NOAACode, NumMrktLive, 
                   NumSmllLive, NumMrktAllBox, NumSmllAllBox) %>% 
            na.omit() # remove any na's in the dataframe
        
        #save the data set for each bar in 1 dataframe.
        all_dat_ts <- dplyr::bind_rows(all_dat_ts, tmp_dat_ts)
    }
    
    # calculate for all the data the total number of adult lives and total 
    # number of adult-sized boxes. 
    all_dat_ts <- all_dat_ts %>% 
                    dplyr::mutate(L = NumMrktLive + NumSmllLive) %>% 
                    dplyr::mutate(B = NumMrktAllBox + NumSmllAllBox)
    
  # Check subsetting -----------------------------------------------------------
  # Test 1 : there are no NOAACodes with < n_ts
    test <- all_dat_ts %>%
      dplyr::select(NOAACode, ID) %>%
      dplyr::distinct() %>%
      dplyr::group_by(NOAACode) %>%
      dplyr::count()
    test <- any(test$n < n_ts) #Should return FALSE.
    if(test == TRUE){
      stop("DataSelection did not subset correctly; dataframe includes NOAAcodes with too few bars to be included.")
    }
    
    #  Test 2: check that there are no bars without complete time series.
    test_2 <- all_dat_ts %>% 
      dplyr::select(ID,SampleYr) %>% 
      dplyr::distinct() %>%  # to get bar/year combos included
      dplyr::group_by(ID) %>% 
      dplyr::count()
    # 
    test_2 <- any(!(test_2$n == (yr_last - yr_0 + 1))) # expect to return FALSE.
    
    if(test == TRUE){
      stop("Dataselection did not subset correctly: wrong number of years in some NOAA codes")
    }
    
    # Return dataframe ---------------------------------------------------------
    return(all_dat_ts)
}

# Function 2: Plot Data --------------------------------------------------------

# Plot data that was the output from SelectData function.
# inputs:
# all_dat_ts, a dataframe that is output from SelectData
# file path, which is the complete file path to the folder that the plots will
  # be placed in.
# file_name, which is the name and extension (must be.pdf) of the file created.

# Output: 
# a pdf containing plots of all_dat_ts.

PlotData <- function(all_dat_ts, file_path = "." ,file_name = "Plots.pdf") {
  
    require(dplyr) # required packages
    require(ggplot2)
  
    # Create file paths
    dir.create(file_path)
    full_file_name <- paste0(file_path, "/", file_name)
    
    # Define NOAA codes to loop over based on the data
    NOAA_vec <- unique(all_dat_ts$NOAACode)
    
    pdf(full_file_name, width = 6, height = 4) # Open pdf device
    #For all the data:
    print(ggplot2::ggplot(all_dat_ts, aes(SampleYr)) +
            ggplot2::geom_bar()+ #by year, number os samles, fairly even.
            ggplot2::theme_classic()+
            ggplot2::ggtitle("Number of samples by year, all NOAA Codes"))
    
    #By NOAA code
    for(i in 1:length(NOAA_vec)) { #Loop over NOAA codes.
        # For each NOAA code, plot samples by bar
        print(ggplot2::ggplot(all_dat_ts[all_dat_ts$NOAACode == NOAA_vec[i],], aes(ID)) +
              ggplot2::geom_bar()+
              ggplot2::ggtitle(paste("Number of samples by bar, NOAA Code", NOAA_vec[i])))
      
        # For each NOAA code, plot number of samples by year and bar.
        print(ggplot2::ggplot(all_dat_ts[all_dat_ts$NOAACode == NOAA_vec[i],], aes(x =ID,y =SampleYr))+
              ggplot2::geom_count()+# Number of samples by year and ID>
              ggplot2::ggtitle(paste("Number of samples by year and bar, NOAA Code", NOAA_vec[i])))
        
        # Calculate number of samples by bar and year to plot to make a faceted by 
        # ID bar plots of number of samples by year.
        tmp_count <- all_dat_ts %>% 
                       dplyr::filter(NOAACode == NOAA_vec[i]) %>%
                       dplyr::group_by(ID, SampleYr) %>%
                       dplyr::count() #count the number of samples in each year and bar
        # Plot the number of samples in each year and bar
        print(ggplot2::ggplot(tmp_count, aes(x=SampleYr, y=n))+
                ggplot2::geom_col()+
                ggplot2::facet_wrap(~ID)+
                ggplot2::ggtitle(paste("Number of samples by bar, NOAA Code", NOAA_vec[i])))
    }
    dev.off()# Close pdf
    return() # No variables are returned.
}

# Function 3: Create list of data for model ------------------------------------

# Designed for the data structure of model_d_3_3.stan.

# Inputs:
# all_dat_ts, output from the SelectData function, a dataframe.
# e_diff, the ratio of efficiency of lives/ efficiency of boxes. default is 
  # the value for the base model. Should be numeric.
  # frac_disart, proportion of oysters disarticuling before the survey. Default is
  # the value for the base model. Should be numeric between 0 and 1.
# d_p, prior distribution for the disarticulation rate. a numeric vector of 
  # length 2, where the first component is the mean and second the standard 
  # deviation. Default is the base model values
# calc_log_r_p, T if log_lambda_r_p and log_beta_0_r_p should be calculated from 
  # the data, F if they should be set to values log_lambda_r_p and 
  # log_beta_0_r_p. logical.
# log_r_p: set values for log_lambda_r_p and log_beta_0_r_p. numeric. Are only 
  # used if calc_log_r_p == F.

# Outputs: 
# A list with 3 components:
# mod_dat, the data in the correct form that can be read into mod_d_3_3.stan.
# bar_key, Matches the bar number in mod_dat with its ID and NOAA code
  # (necessary for relating model results back to the Fall Survey Data)
# raw_mod_dat, model data in flat form (but not including priors and constants, 
  # just the samples)

CreateModDataList <- function(all_dat_ts, e_diff = 1.79, frac_disart = .2 , 
                              d_p = c(0.51, 0.04), calc_log_r_p = F, 
                              log_lambda_r_p = 0, log_beta_0_r_p = 0) {
  require(dplyr)
  require(purrr)
  
  # Calculate necessary values -----------------------------------------------
  # Necessary for data manipulation or model input.
  R_q <- e_diff*(1/(1-frac_disart)) # Calculate R_q, model input.
  #Calculate Years
  yr_0  <- all_dat_ts %>% 
             dplyr::select(SampleYr) %>% 
             min() 
  yr_last <- all_dat_ts %>% 
               dplyr::select(SampleYr) %>% 
               max()
  yr_vec <- seq(yr_0, yr_last, by = 1)
  
  #Create the year key (use for both regions)
  yr_key <- data.frame(ModYr = seq(0, by = 1, length.out = length(yr_vec)), 
    SampleYr = yr_vec)
  Y <- length(yr_vec)-1 # subtract 1 for year 0.
  
  # sort all_dat_ts by Bar ID, then by NOAA code, then use to make an ordered
  # vector of NOAA codes.
  arr_dat <- all_dat_ts %>% 
    dplyr::arrange(ID) %>% 
    dplyr::arrange(NOAACode)
  
  NOAA_vec <- unique(arr_dat$NOAACode)
  
  # Manipulate all_dat_ts-----------------------------------------------------
  
  # Put data for each NOAA code into its own component in a list. 
  dat_list <- list()
  for (i in 1:length(NOAA_vec)){
      dat_list[[i]] <- all_dat_ts %>% 
                         dplyr::filter(NOAACode == NOAA_vec[i]) %>%
                         dplyr::arrange(SampleYr) %>% 
                         dplyr::arrange(ID)
  }
  
  # Put NOAA code level data as a list components ----------------------------
  
  # for both the bar key and for the model data inputs.
  bar_key_each <- list() #make a list of bar keys where each bar key is for a different NOAA Code
  I_each       <- list() #make a list of number of bars for each NOAACode/region
  obs_each     <- list() # list of observations for each NOAACode/region
  n_obs_each_tot <- list() # number of observations for each NOAA Code/region
  obs_each_0   <- list() # list of observations fore each NOAA code in year 0
  n_obs_each_0 <- list() # number of observations of each NOAA code/region yr 0
  tmp_ModBar_max <- 0 # initialize the bar index to 0
  
  for (i in 1:length(NOAA_vec)){ # Loop through NOAA codes
    # make bar keys
    tmp_bar_list <- unique(dat_list[[i]]$ID) # unique bars for the NOAA code
    I_each[[i]] <- length(tmp_bar_list) #number of bars for the NOAA code
    # put together the bar key
    bar_key_each[[i]] <- data.frame(ModBar = seq(from=(tmp_ModBar_max+1), by = 1, length.out = length(tmp_bar_list)), 
                                        ID = tmp_bar_list, 
                                    stringsAsFactors = F)
    #get max to use for next iteration of making the bar key.
    tmp_ModBar_max <- max(bar_key_each[[i]]$ModBar) 
    #add ModYr and ModBar columns to the dat_list
    dat_list[[i]] <- dat_list[[i]] %>% 
                       # yr key should be the same for all regions.
                       dplyr::left_join(yr_key, by = "SampleYr") %>%
                       # bar key depends on region.
                       dplyr::left_join(bar_key_each[[i]], b = "ID")     
    # Get the obsrvations for the NOAA code.
    obs_each[[i]] <- dat_list[[i]] %>%
                       dplyr::select(ModBar, ModYr, L, B) %>%
                       dplyr::filter(ModYr > 0)
    # Convert the observations to the correct type.
    obs_each[[i]]$ModBar <- as.integer(obs_each[[i]]$ModBar)
    obs_each[[i]]$ModYr <- as.integer(obs_each[[i]]$ModYr)
    obs_each[[i]]$L <-  as.integer(obs_each[[i]]$L)
    obs_each[[i]]$B <- as.integer(obs_each[[i]]$B)
    obs_each[[i]] <- as.matrix(obs_each[[i]])
    # Get the total number of observations for the NOAA code
    n_obs_each_tot[[i]] <- nrow(obs_each[[i]])
    # Get the observations for year 0
    obs_each_0[[i]] <- dat_list[[i]] %>% 
                        filter(ModYr == 0) %>% 
                        select(ModBar, B) #select only year 0 box obs. Don't need lives for year 0. 
    # Convert the year 0 obs to the correct data type
    obs_each_0[[i]]$ModBar <- as.integer(obs_each_0[[i]]$ModBar)
    obs_each_0[[i]]$B <- as.integer(obs_each_0[[i]]$B)
    obs_each_0[[i]] <- as.matrix(obs_each_0[[i]])
    # Get the total number of observations for the NOAA code in year.
    n_obs_each_0[[i]] <- nrow(obs_each_0[[i]])
  }
  
  #Combine lists -------------------------------------------------------------
  #Now, put data together for all NOAA codes,to create the model data
  
  REG <- length(NOAA_vec) #number of regions
  # #First iteration
  I <- I_each[[1]]
  obs <- obs_each[[1]]
  obs_0 <- obs_each_0[[1]]
  for (i in 2:length(NOAA_vec)){
    I <- c(I,I_each[[i]]) #number of bars in each region.
    obs <- rbind(obs,obs_each[[i]])
    obs_0 <- rbind(obs_0, obs_each_0[[i]])
  }
  # To change (get rid of top part)
  # I <- purrr::flatten_int(I_each)
  # obs <- purrr:flatten_dfr(obs_each)
  # obs_0 <- purrr:flatten_dfr(obs_0)
  
  I_tot <- sum(I)
  n_obs_tot <- nrow(obs)
  n_obs_0 <- nrow(obs_0)
  
  I_REG <- rep(1, times = I[1])#first iteration
  for (i in 2:length(NOAA_vec)){
    I_REG <- c(I_REG, rep(i, times = I[i]))
  }
  
  # Create bar key -----------------------------------------------------------
  #Make sure to keep track of bars and the region they are from
  for (i in 1:length(NOAA_vec)){
    bar_key_each[[i]]$ModReg <- rep(i, length.out = nrow(bar_key_each[[i]]))
    bar_key_each[[i]]$NOAACode <- rep(NOAA_vec[i], length.out = nrow(bar_key_each[[i]]))
  }
  
  #Bind rows to create the bar key.
  bar_key <- bar_key_each[[1]]
  for (i in 2:length(NOAA_vec)){
    bar_key <- bind_rows(bar_key,bar_key_each[[i]])
  }
  
  # Create Data for model priors ---------------------------------------------
  # Only ones that are not already created.
  
  #R_q <- 1.68 # Mean value, ratio of catchability # added to function input instead.
  #d_p <- c(0.51, 0.04) #Normal distribution for the mean. # added to function input instead.
  M_p <- c(1, 1) # pars are alpha and beta for a beta distribution.
  Z_sigma <- 3.0
  
  if (calc_log_r_p == T){ # change if want to calculate them from data. 
    #calculate from the data.
    logmeanL <- mean(log(obs[,'L']+0.01)) # rough mean param for prior.
    logmeanB <- mean(log(obs[,'B']+0.01)) #rough mean for prior on Beta.
    #Set the priors from data
    log_lambda_r_p <- matrix(rep(c(logmeanL,5), times = length(NOAA_vec)), nrow = length(NOAA_vec), ncol = 2, byrow = T)
    log_beta_0_r_p <- matrix(rep(c(logmeanB,5), times = length(NOAA_vec)), nrow = length(NOAA_vec), ncol = 2, byrow = T)
  } else {
    # set the priors from function inputs.
    log_lambda_r_p <- matrix(rep(c(log_lambda_r_p,5), times = length(NOAA_vec)), nrow = length(NOAA_vec), ncol = 2, byrow = T)
    log_beta_0_r_p <- matrix(rep(c(log_beta_0_r_p,5), times = length(NOAA_vec)), nrow = length(NOAA_vec), ncol = 2, byrow = T)
  }
  
  #Create model data list ----------------------------------------------------
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
  for (i in 2:length(NOAA_vec)){
      raw_mod_dat <- bind_rows(raw_mod_dat, dat_list[[i]])
  }
  # to change: use purrr instead
  # raw_mod_dat <- purrr::flatten_dfr(dat_list)
  # Make return data list and return -----------------------------------------
  return_list <- list(
                      mod_dat = mod_dat, 
                      bar_key = bar_key,
                      raw_mod_dat = raw_mod_dat
                      )
  return(return_list)
}


