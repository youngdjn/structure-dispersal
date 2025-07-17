## Model Experiments log 


#### Fecundity model tests ####

# Start using the prepped Delta fire data set 

# Compare multiplier fecundity model to 2-parameter model 

m_exppow_multiplier = fit_stan_model_fecund(
  dataset_name = "delta-PIPJ", # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
  disp_mod = "exppow", # 2Dt or exppow
  err_mod = "pois", # pois only currently
  fecund_mod = "multiplier",
  n_warmup = 500, # stan warmup iter
  n_iter = 1500, # stan iter, includes warmup
  n_chains = 3, # stan n chains
  n_cores = 3 # stan n cores
)

m_exppow_multiplier_exponent = fit_stan_model_fecund(
  dataset_name = "delta-PIPJ", # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
  disp_mod = "exppow", # 2Dt or exppow
  err_mod = "pois", # pois only currently
  fecund_mod = "multiplier_exponent",
  n_warmup = 500, # stan warmup iter
  n_iter = 1500, # stan iter, includes warmup
  n_chains = 3, # stan n chains
  n_cores = 3 # stan n cores
)

loo(m_exppow_multiplier)
loo(m_exppow_multiplier_exponent) # slightly better but by <2 points

#### Prior checking for exppow model ####

# NOTE: for this check, using delta-PIPJ and single-parameter fecundity model

# Starting point: informed priors based on prior testing and biol info 
# k ~ dgamma(30, 30)
# a ~ exp(dnorm(4, 0.5))
# b ~ exp((dnorm(0, 1))
# Result:              mean se_mean     sd   2.5%    25%    50%     75%   97.5% n_eff Rhat
#alpha          6.88    0.01   0.18   6.53   6.76   6.88    7.00    7.26   554 1.01
#kappa          1.53    0.01   0.21   1.15   1.39   1.52    1.66    1.99   563 1.00
#mu_beta        2.56    0.02   0.38   1.82   2.30   2.56    2.80    3.40   499 1.01
#a            990.01    7.93 186.29 687.51 860.05 968.28 1098.78 1426.76   552 1.01
# Wants this dispersal kernel to be VERY heavy tailed for Pines. 

# Now let's check these same priors for Delta FIRS 
# Result: 
#mean se_mean    sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
#alpha           4.74    0.03  0.52    3.73    4.38    4.76    5.12    5.76   363 1.00
#kappa           0.58    0.01  0.11    0.43    0.50    0.56    0.64    0.88   301 1.00
#mu_beta         2.06    0.02  0.40    1.36    1.78    2.02    2.29    2.95   412 1.01
#a             131.35    3.68 71.01   41.82   79.45  116.80  166.69  317.26   372 1.00
# This is not heavy tailed (k < 1) but very long distance. 

#One idea: try loosening prior on b to allow higher fecundity easily. 
# New priors. Same on k and a, looser on b. Running for FIRS. 
# k ~ dgamma(30, 30)
# a ~ exp(dnorm(4, 0.5))
# b ~ exp((dnorm(0, 3))
#mean se_mean    sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
#alpha           4.69    0.03  0.50    3.61    4.36    4.70    5.01    5.61   383 1.01
#kappa           0.52    0.01  0.09    0.39    0.45    0.50    0.57    0.73   224 1.00
#mu_beta         2.55    0.03  0.57    1.61    2.12    2.48    2.94    3.77   398 1.00
#a             122.31    3.18 61.62   37.04   78.00  110.47  150.00  274.47   377 1.00
# Same general result, I think it's better to loosen up on mu_beta 

# Next step: try loosening up prior on k because it seems to want to get even smaller? 
# The alternative would be to be more prescriptive on a to ensure it doesn't get too big. 
# Looking at pairs(m_exppow_multiplier, pars = c(k, alpha, mu_beta)) shows that alpha and k are fairly strongly positively correlated, while k and mu_beta are negatively correlated. I think we have to constrain either k or alpha pretty strongly. 
# But we have to let k be >1 or <1 by a bit, so I think we have to add more constraint to the dispersal distance? 


