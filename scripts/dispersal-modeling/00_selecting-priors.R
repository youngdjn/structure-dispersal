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

calc_kern_2Dt = function(a, k, r) {
  kern = k / (pi*a) * (1 + r^2/a)^(-1-k) # 2Dt
}

a = 30000 # a shouldn't go below about 1000 or above about 30000
# could set prior on log(a) ~ normal(8.5, 0.5) to try and hold it in that range 
k = 0.1
r = 0:500
kern = calc_kern_2Dt(a = a, k = k, r = r)
kern_df_1 = data.frame(r = r, kern = kern, a = a, k = k)
# plot the result
ggplot(kern_df_1, aes(x=r, y=kern)) +
  geom_line() + theme_bw() 

# priors are set on the log of a for some reason 
log(30000) # alpha shouldn't go much above 10 
log(1000) # or much below 7 
