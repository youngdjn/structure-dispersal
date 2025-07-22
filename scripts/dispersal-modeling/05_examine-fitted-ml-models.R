# Visualize model results 

# Fit models 
m1 = fit_model_ml(pars = startpars1, fixed_pars = "a", parscale = c(0.5, 10, 10, 0.5), disp_data = disp_data, settings = settings_to_use)


m = m1 # choose model to plot

# Plot fitted vs observed 
obspred_data <- data.frame(fitted = m$fitted.values, observed = disp_data$seedling_counts)
ggplot(obspred_data, aes(x = log(fitted), y = log(observed+0.5))) + geom_point()

# Check of residuals 
obspred_data$resids = obspred_data$observed-obspred_data$fitted
qqnorm(obspred_data$resids)
plot(resids~fitted, obspred_data)
hist(obspred_data$resids)

# plot dispersal kernel based on fitted parameters
kernel_plot_data <- data.frame(Distance = 1:800, Probability = calculate_dispersal(distance = 1:800, pars = pars, kernel_type = "exppow"))
ggplot(kernel_plot_data, aes(x = Distance, y = Probability)) + geom_line()

# visualize likelihood surface 
kvals = seq(0.2, 0.7, by = 0.05)
avals = seq(5, 120, by = 5)
bvals = 20
#thetavals = 50
parameter_test_set <- expand.grid(kvals, avals, bvals) 
names(parameter_test_set) = par_structure
head(parameter_test_set)
fn_to_apply_negloglik <- function(param_test_vals, disp_data = disp_data, 
                                  settings = settings_to_use) {
  pars = c(param_test_vals[1], param_test_vals[2], 
           param_test_vals[3], param_test_vals[4])
  nll = calculate_negloglik(pars = pars, disp_data = disp_data, 
                            settings = settings)
  return(nll)
}

negloglikvals <- apply(parameter_test_set, 1, fn_to_apply_negloglik, 
                       disp_data = disp_data, settings = settings)
lik_surface_data <- cbind(parameter_test_set, negloglikvals)
head(lik_surface_data)
hist(negloglikvals)

# Where is the maximum? 
lik_surface_data[which.max(lik_surface_data$negloglikvals),]

# plot a 2D likelihood surface using negloglikvals data
ggplot(lik_surface_data, aes(x = a, y = k, z = negloglikvals)) + 
  geom_tile(aes(fill = log(negloglikvals))) + 
  #geom_contour() + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "a", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood"))

ggplot(lik_surface_data, aes(x = b, y = k)) + 
  geom_tile(aes(fill = negloglikvals)) + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "b", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood"))


# NOTE: Convergence is sensitive to starting values -- can converge to "reasonable" or extreme values for most data sets 
# NOTE: Using 500m distance seems slighly more stable (more informative)


## Next steps: 
# Visualize likelihood surface. DONE - shows a ridge as expected based on a / k correlation. 
# The model runs away to very unrealistic and extreme parameter combinations. TRY keeping b fixed. DONE - didn't help 
# Check that model can recover params from simulation. DONE -- yes it can! 

# Since simulations work to recover parameters, see if any of the data sets for the other 3 fires (other than Delta) can converge to reasonable parameter values. 

# Implement negbin likelihood - done 

# Systematically test which data sets x likelihood types converge consistently to something biologically plausible, or at least do so from reasonable starting values. 



