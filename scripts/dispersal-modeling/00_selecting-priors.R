# This is exploratory code used for selecting Stan priors

scalar_fun = function(ht_diff, b1) {
  
  scalar = exp(b1 * ht_diff)
  
}


b0 = 0

log_b1 = -5.298317  # log(0.005)

b1 = exp(log_b1)



field_ht_diff = seq(-300, 300, by = 10)

scalars = scalar_fun(field_ht_diff, b1)

plot(field_ht_diff, scalars)


# 0.0001    0.005      0.010

log(0.0001)
log(0.005)
log(0.01)

# -5, 3

# Checking priors on 2Dt model 

calc_kern_2Dt_priors = function(a, k, r) {
  kern = k / (pi*a) * (1 + r^2/a)^(-1-k) # 2Dt
  kern
}

a = 30000 # a shouldn't go below about 1000 or above about 30000
# could set prior on log(a) ~ normal(8.5, 0.5) to try and hold it in that range 
k = 3
r = 0:500
kern = calc_kern_2Dt_priors(a = a, k = k, r = r)
kern_df_1 = data.frame(r = r, kern = kern, a = a, k = k)
# plot the result
ggplot(kern_df_1, aes(x=r, y=kern)) +
  geom_line() + theme_bw() 

kern_df_1[101,]
kern_df_1$kern[10] * (exp(1.94)*20^1.2) * seedling_plot_area



# priors are set on the log of a for some reason 
log(30000) # alpha shouldn't go much above 10 
log(1000) # or much below 7 

# Prior on k is set through inv_log_k = inv_logit(inv_k) where inv_k ~ normal(0, 1) 
library(boot)
inv_k_real = -2
(1 + exp(-inv_k_real))/2
1/ (2 * inv.logit(inv_k_real)) # ok this is the same! 
# To keep k within about 0.5 and 2, inv_k should be between -1 and 2 
# So inv_k ~ normal(0.5, 0.5) should work

# Prior on b is via log(b) or mu_beta 
# Note smallest tree is 10m tall, so b should be set to give between say 5 and 50 seeds per meter of height? 
# Nina's data suggest 1300 (~500-3000) germinants per uninjured median-sized yellow pine 
# For a 20m tree, that's 65 germinants per meter of height
# For median height sugar pine, Nina's data shows more seed production: 3900 (~1000-10,000). 
# For a 20m tree, that's 195 germinants per meter of height
# For white fire, her data show much higher seed production: 13,800 germinants, with little variation by tree size 
# For a 20m tree, that's 690 germinants per meter of height

# Using that info to set priors for pines, we can try and constrain b to be between about 25 and 500 (germinants per meter of tree height)
log(25) # 3.2
log(500) # 6.2
# So mu_beta could be set at normal(4.7, 0.5) to keep b in that range


# That's for seeds. But our model is for seedlings. Marchand et al. used a much lower prior for seedling counts: N(-2, 3). But note they are predicting seedlings per unit basal area of tree. Whereas we are using heights... Which should be related to DBH via roughly a 2/3 exponent. 
f <- function(x) {return(1.47*x^0.68)}
plot(1:40, f(1:40)) # not too nonlinear, so we can roughly think of seedlings per meter of height as about half the number of seedlings per cm of DBH 
# We might want to adjust Marchand et al's prior downward by 50% 
#normal(-2, 3) was a pretty wide prior for seedlings per unit basal area of tree.
# we could aim for something that gives about half as many seedlings and varies a bit less. 
exp(-11)
exp(-2)
exp(7)
# Testing alternative of normal(-2.5, 2.5)
exp(-2.5)
exp(-10)
exp(5)
