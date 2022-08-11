## Takes a treetop map and seedling plots and fits a dispersal kernel

library(sf)
library(here)
library(tidyverse)
library(units)
library(terra)
library(raster)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))

## load ttops
ttops = st_read(datadir("drone/processed-products/crater/Crater120m_20210122T1219_ttops_vwf196.gpkg"))

## load plots
plots = st_read(datadir("surveys/crater/intermediate/crater_foc.geojson")) %>% st_transform(3310)
plots_foc = plots %>%
  filter(BurnClass != "Low")
         #sapling_density_ha < 150)

observed_regen = plots_foc$sapling_density_ha
mean_observed = mean(observed_regen)

# calc observed seedling count per 0.09 ha plot
observed_count = round(observed_regen * 0.09)


## filter to reproductive trees
hist(ttops$height)
ttops = ttops %>%
  filter(height > 10)


# dists from each plot to each tree
dist_mat = st_distance(plots_foc,ttops) %>% drop_units

# height of each tree (for estimating fecundity)
tree_heights = ttops$height


## for each field plot, calc the input from each tree


# The PDFs
pdf_2Dt <- function(r, a, k) {
  k / (pi*a) * (1 + r^2/a)^(-1-k)
}

pdf_exppow <- function(r, a, k) {
  k / (2*pi*a^2 * gamma(2/k)) * exp(-(r/a)^k)
}


# For a vector of tree distances and a PDF parameterization, get the PDF value for each tree's contribution.
# Then multiply each trees contrib by each tree's fecundity, then sum across all trees.

kernelsum = function(dist,a,k, fecundity) { # fecundity is for each tree, in the same order as dist

  kernelvals = pdf_exppow(r = dist, a = a, k = k)
  
  seed_input = kernelvals * fecundity
  
  #cat("unique kernelvals:",unique(kernelvals),"  sum of fecundity:",sum(fecundity),"\n")
  
  return(sum(seed_input))
}




loss = function(params) { # params to optimize are in order: alpha, inv_k;  height-to-fecundity scalar
  
  # convert the optimization params to the params for the PDF
  
  # for exppow
  # a = exp(params[1] - params[2])
  # k = 1/params[2]
  a = params[1]
  k = params[2]
  
  
  # # for 2Dt
  # inv_k <- 2 / (1 + exp(-params[2]))
  # a <- exp(params[1])
  # k <- 1 / inv_k
  
  #take a vector of the height of each tree and calculate its fecundity
  fecundity = params[3] * tree_heights^2
  
  pred = apply(dist_mat,MARGIN = 1,kernelsum, a = a, k = k, fecundity = fecundity)
  
  # # apply a scalar so the mean predicted density = the mean observed density
  # mean_pred = mean(pred)
  # scalar = mean_observed / mean_pred
  # pred = pred * scalar
  
  # cat("a:",a," k:",k," scalar:",scalar,"\n")
  cat("a:",a," k:",k," fecundity scalar: ",params[3],"SD:",params[4], "\n")
  
  # different options for loss functions to minimize
  mae = abs(pred - observed_regen) %>% mean
  rmse = sqrt(mean((pred - observed_regen)^2))
  
  # negative log likelihood (assuming normal distrib)
  #nll = -sum(dnorm(x = observed_regen, mean = pred, sd = params[4], log = TRUE))
  #nll = -sum(dpois(x = observed_count, lambda = exp(pred), log = TRUE))
  
  return(mae)
  
}



fit = optim(par = c(200,0.5,30), fn = loss, method="BFGS", control=list(maxit=1000))
fit


# get fitted params and scalar val

params = fit$par

# for exppow
# a = exp(params[1] - params[2])
# k = 1/params[2]
a = params[1]
k = params[2]

# # for 2Dt

# inv_k <- 2 / (1 + exp(-params[2]))
# a <- exp(params[1])
# k <- 1 / inv_k

fecundity = params[3] * tree_heights^2

preds = apply(dist_mat,MARGIN = 1,kernelsum, a = a, k = k, fecundity = fecundity)
# scalar = mean_observed / mean(preds)
# preds_scaled = preds * scalar

# plot predicted seedl density with distance from a single tree of average height
dists = seq(from=0,to=500,by=1)
pred_w_dist = pdf_exppow(r = dists, a = a, k = k)
plot(x = dists, y = pred_w_dist, type="l")


# plot predicted vs observed (one point for each plot)
plot(x = observed_regen, y = preds)



















#########
#### Comparison: distance to nearest seed tree ####

# Get the nearest seed source to each plot
nearest_seedsource = apply(dist_mat,MARGIN=1,min)
plots_foc$nearest_seedsource = nearest_seedsource

library(mgcv)

plots_mod = plots_foc

st_geometry(plots_mod) = NULL

plots_mod = plots_mod %>%
  mutate(sapling_density_ha_transf = ifelse(sapling_density_ha == 0,.001,sapling_density_ha))

m_nearest = gam(sapling_density_ha_transf ~ 1 + s(nearest_seedsource), data=plots_mod, family=Gamma)
m_nearest = glm(sapling_density_ha_transf ~ 1 + nearest_seedsource, data=plots_mod, family=Gamma)
m_nearest = lm(sapling_density_ha_transf ~ 1 + nearest_seedsource, data=plots_mod)
summary(m_nearest)
pred_dens = predict(m_nearest, type = "response")

plot(plots_mod$sapling_density_ha_transf,pred_dens)

ggplot(data=plots_mod, aes(x=sapling_density_ha_transf, y=pred_dens)) +
  geom_abline(slope=1,intercept=0, color="blue") +
  geom_point() +
  lims(x=c(0,250), y=c(0,100)) +
  labs(x = "Observed seedlings / ha", y = "Fitted seedlings / ha") +
  theme_bw(20)

ggsave(datadir("temp/fit_obs_dist.png"), width=6,height=5)


mae = mean(abs(pred_dens - plots_mod$sapling_density_ha_transf))
mae



#### Comparison: BA density kernel from poscrpt ####

# get BA of each tree
a = 1.4721
b = 0.6848
dbh = function(h) {
  (h/a)^(1/b)
}

dbh_pred = dbh(ttops$h)
ba_pred = 3.14 * (dbh_pred/2)^2

plot(ttops$h,dbh_pred)
plot(ttops$h,ba_pred)

ba = function(h) {
  3.14 * (   ((h/a)^(1/b))   /2)^2
}



ttops$ba =  (dbh(ttops$height)/2)^2

# make a 30 m grid covering the study area

grid = rast(resolution = 30, ext = extent(ttops) , crs = "EPSG:3310")


# calc BA of each 30 m pixel

ba = terra::rasterize(ttops %>% vect,grid, field = "ba", fun = sum)

writeRaster(ba, datadir("temp/ba_from_drone.tif"))

# convert from sq cm / 900 sq m  to sq m / ha

sqcm_sqm = ba / 900
sqm_sqm = sqcm_sqm / 10000
sqm_ha = sqm_sqm * 10000

# get seed output based on BA
seeds = (0.0107*(  0.1226  ^-0.58)*((113000*( sqm_ha ^0.855))^1.08))

## apply gaussian smoother
fw <- raster::focalWeight(seeds, 45, type = "Gauss")
dispersal <- terra::focal(seeds, fw, na.rm=TRUE)
dispersal[is.na(dispersal)] = 0

smoothed_ba = terra::focal(sqm_ha, fw, na.rm=TRUE)
smoothed_ba[is.na(smoothed_ba)] = 0

writeRaster(smoothed_ba,datadir("temp/smoothed_ba.tif"))

ba[is.na(ba)] = 0
writeRaster(ba,datadir("temp/ba.tif"), overwrite=TRUE)


# extract seedfall at each plot
plot_seeds = terra::extract(dispersal,plots_foc %>% vect, method="bilinear")[,2]
plots_mod$seedrain = plot_seeds


## see how well it works as a predictor
m_seedrain = gam(sapling_density_ha_transf ~ 1 + s(seedrain), data=plots_mod, family=Gamma)
summary(m_seedrain)
pred_dens = predict(m_seedrain, type = "response")

plot(pred_dens,plots_mod$sapling_density_ha_transf)

ggplot(data=plots_mod, aes(x=sapling_density_ha_transf, y=pred_dens)) +
  geom_abline(slope=1,intercept=0, color="blue") +
  geom_point() +
  #lims(x=c(0,250), y=c(0,100)) +
  labs(x = "Observed seedlings / ha", y = "Fitted seedlings / ha") +
  theme_bw(20)

ggsave(datadir("temp/fit_obs_poscrpt.png"), width=6,height=5)




mae = mean(abs(pred_dens - plots_mod$sapling_density_ha_transf))
mae




#### fitted vs observed

#### add fecundity



#### confirm fits, try 2Dt, try mean abs prop error, try diff forms of fecundity function