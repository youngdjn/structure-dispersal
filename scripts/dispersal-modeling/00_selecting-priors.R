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