/*
* Model for M,

*-----------------------------------------------------------------------------
* This model calculates M for for all NOAA codes included by estimating a common
* rate of disarticulation among the NOAA codes,and also includes bar-by_year
* random effects. 
*
* similar to mod_d_3_3 in the Oyster_Stock_Assessment project
* only difference is R_q was modified so that separate inputs for box disarticulation
* before the survey and difference in catchabilities could be separate inputs
* 
* Last modified By Kathryn Doering
*-------------------------------------------------------------------------------
*/
data {
    // Indexing for data and data.
    int<lower=0> Y; // Number of year M will be estimated for
    int<lower=0> REG; // Number of regions
    int<lower=0> I[REG]; // Number of bars in each region.

    int<lower=0> I_tot; // total number of bars for the whole model.
    int<lower=0> n_obs_tot; // Total number of observations for all regions, years, bars.
    int<lower=0> n_obs_0; // Total number of observations in year 0.
    
    // Live and boxes observations (except for year 0) with their associated
    // indices, all in individual vectors:
    int<lower=1> Y_ind[n_obs_tot]; // Year index for observations.
    int<lower=1> I_ind[n_obs_tot]; // Bar index for observations.
    int<lower=0> L[n_obs_tot]; // Live observations (match up with Y_Ind, REG_ind, I_ind)
    int<lower=0> B[n_obs_tot]; // Box observations (match up with Y_Ind, REG_ind, I_ind)
    
    // Box observations for year 0 (do not need live observations:)
    int<lower=1> I_ind_0[n_obs_0]; // Bar index for year 0 observations.
    int<lower=0> B_0[n_obs_0]; // Box observations for year 0 (match up with I_ind_0)
    
    // Indices for parameters: (easier to manipulate in R and read in than do in stan)
    int<lower=0> I_REG[I_tot]; // vector with region of each par (index is the bar, e.g., I_REG[1] is the region index that corresponds with bar 1. )
    
    // constants and prior inputs (Many options can be changed):
    real<lower=0> R_q; // Ratio of catchability: lives over boxes (constant value)
    real<lower=0, upper=1> frac_d; // fraction oysters disarticulating before the survey (constant value)
    // Prior parameter values. 1st parameter in 1, 2nd in 2.
    real<lower=0> d_p[2]; // For disarticulation rate (assume 1 prior for all regions)
    real<lower=0> M_p[2]; // For  mortality. (Use the same for each region.)
    real<lower=0> Z_sigma; // For hyperprior on sigma.
    
    // regional level priors (may change these to be region specific):
    real log_lambda_r_p[REG,2]; // Each for a different region
    real log_beta_0_r_p[REG,2]; // Each for a different region
} 
transformed data {
//can add print statements to test input reading here.
}
parameters {
    // Level I parameters - bar level
    real log_lambda_i[Y,I_tot]; // Mean for lives for each bar and year
    real<lower=-4> log_beta_0_i[I_tot]; // beta for each bar in year 0. Bounded parameter to address problems with 0's in the first year.
    // Level II parameters - regional level
    real log_lambda_r[Y,REG]; // Log scale mean for lives in the region by year
    real log_beta_0_r[REG];    // Log scale beta for the region in year 0. 
    real<lower=0> sigma; //standard deviation for boxes and lives in the region.
    // Level III parameters - regional level M estimation.
    real<lower=0> d; // disarticulation rate
    real<lower=0, upper=1> M[Y,REG]; // Natural mortality for each year and region
}
transformed parameters {
    //Nominal scale bar-level and regional level variables.
    real<lower=0> lambda_i[Y,I_tot]; // Nominal bar mean Mean for lives for each bar and year
    real<lower=0> beta_0_i[I_tot]; // Nominal regional beta for each bar in year 0.
    real<lower=0> beta_i[Y,I_tot]; // Nominal bar level mean for boxes by year.
    
    // Backtransform log scale bar-level and regional level variables
    lambda_i = exp(log_lambda_i);
    beta_0_i = exp(log_beta_0_i);

    // Define relationships between the bar level parameters.
    //Link together parameters to estimate beta for the next year.
    //Start by linking year 0 beta to year 1 beta,M,and lambda
    for(i in 1:I_tot){ //loop through each bar.
        //for the first year
        beta_i[1,i] = beta_0_i[i]*exp(-d) + 
            lambda_i[1,i]*((1.0/(1.0-M[1,I_REG[i]]))-1.0)/R_q*(1/(1-frac_d));
        for(y in 2:Y){ //for the other years
            beta_i[y,i] = beta_i[y-1,i]*exp(-d) + 
                lambda_i[y,i]*((1.0/(1.0-M[y,I_REG[i]]))-1.0)/R_q*(1/(1-fac_d));
        }
    }
}
model {
    // Declare the probability model here: (priors, hyperpriors & likelihood)
    
    //Local indices (try using the same names as in the transformed parameter section,
    // but if that does not work, declare new indices.)
    //int tmp_Y_ind = 0;  // start at year 0.
    //int tmp_I_ind = 0;  // start at 0 (will change in the loop)

    //Hyperpriors for sig_lambda_r, sig_beta_r.
    //Z_lambda ~ normal(Z_lambda_p[1],Z_lambda_p[2]); //May need to change these dists. (or set as constant?)
    //Z_beta ~ normal(Z_beta_p[1], Z_beta_p[2]);//May need to change these dists.
    //sig_lambda_r ~ uniform(0,Z_lambda);
    //sig_beta_r   ~ uniform(0,Z_beta);
    sigma ~ uniform(0,Z_sigma); // for now assume 1 sd for everything (can change this.)

    //Priors
    d ~ normal(d_p[1], d_p[2]);
    
    for (r in 1:REG){
        for(y in 1:Y){
            M[y,r] ~ beta(M_p[1], M_p[2]); // use the same prior for each year of mortality.
        }
    }

    //phi ~ normal(phi_p[1], phi_p[2]); // May need a prior on the overdispersion parameter.
    
    //regional priors for log_lambda_r and log_beta_0_r. 
    for (r in 1:REG){
        for (y in 1:Y){
            log_lambda_r[y,r] ~ normal(log_lambda_r_p[r,1], log_lambda_r_p[r,2]);
        }
    }
    for (r in 1:REG){
        log_beta_0_r[r] ~ normal(log_beta_0_r_p[r,1], log_beta_0_r_p[r,2]);
    }

    // Level II: Regional level values by year. (map regional values to bar level values.)
    for (i in 1:I_tot){ // Regional values for lives:
        for (y in 1:Y){
            log_lambda_i[y,i] ~ normal(log_lambda_r[y,I_REG[i]], sigma);
            // should this be done for boxes also?
            //log_beta_i[y,i] ~ normal(log_beta_r[y,I_REG[i]], sigma);
        }
    }
    for (i in 1:I_tot){ //boxes for year 0.
        log_beta_0_i[i] ~ normal(log_beta_0_r[I_REG[i]], sigma);
    }
    
    //Level I: Bar level means by year.
    //Likelihood for L and B obs on the bar level.
    // Map observations to the parameters (could change this to neg bin.)
    for(n in 1:n_obs_tot){
        //tmp_Y_ind = Y_ind[n]; //Get year associated with observation.
        //tmp_I_ind = I_ind[n]; //Get bar associated with each observation.
        L[n] ~ poisson(lambda_i[Y_ind[n], I_ind[n]]);
        B[n] ~ poisson(beta_i[Y_ind[n], I_ind[n]]);
    }
    // Likelihood for B obs in year 0 on the bar level.
    for (n in 1:n_obs_0){
        //tmp_I_ind = I_ind_0[n]; // Get bar associated wth each observation.
        B_0[n] ~ poisson(beta_0_i[I_ind_0[n]]);
    }
}
generated quantities{
    //backtransformed regional values. (maybe add beta?)
    real<lower=0> lambda_r[Y,REG]; // Nominal regional median of live oysters.
    real<lower=0> beta_0_r[REG]; // Nominal regional median of first yr boxes.
    lambda_r = exp(log_lambda_r);
    beta_0_r = exp(log_beta_0_r);
}


