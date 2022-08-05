library(tidyverse)
library(here)
library(rstan)
library(sf)
library(furrr)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))


res = readRDS(datadir("temp/mod_nb_diam2.rds"))

samples = extract(res)


## function to plot a dispersal kernel from one tree of average size

r = 100
calc_kern = function(samples, r) {
  
  kern = samples$k / (2*pi * samples$a^2 * gamma(2/samples$k)) * exp(-(r / samples$a)^samples$k)

}

r = 0:500

kern_out = sapply(r,calc_kern, samples = samples)

kern_out = t(kern_out)

kern_summary = data.frame (r = 0:500,
                           fit = apply(kern_out,1,mean),
                           lwr = apply(kern_out,1,quantile,probs=c(0.25)),
                           upr = apply(kern_out,1,quantile,probs=c(0.75)))

ggplot(data = kern_summary, aes(x = r, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3) +
  geom_line()



#### Predict seedlings at each plot ####


## need tree data: years as columns, with the values of the column being the size. Also need id, x, and y

d = st_read(datadir("drone/processed-products/crater/Crater120m_20210122T1219_ttops_vwf196.gpkg")) %>% st_transform(32611)
coords = st_coordinates(d)
d$x = coords[,1]
d$y = coords[,2]


a = 1.4721
b = 0.6848
ba_from_height = function(h) {
  3.14 * (   ((h/a)^(1/b))   /2)^2
}

diam_from_height = function(h) {
  ((h/a)^(1/b))
}

d = d %>%
  filter(height > 10) %>%
  mutate(ba = ba_from_height(height),
         diam = diam_from_height(height))


tree_data = d %>%
  select(id = treeID,
         x,
         y,
         `2020` = diam)

st_geometry(tree_data) = NULL

tree_sizes <- as.matrix(select(tree_data, -id, -x, -y))


## need seedling data: a column for "year" with values being n seedlings, X, Y

d = st_read(datadir("/surveys/crater/intermediate/crater_foc.geojson")) %>% st_transform(32611)
coords = st_coordinates(d)
d$x = coords[,1]
d$y = coords[,2]

seed_data = d %>%
  filter(BurnClass != "Low") %>% #,
  # sapling_density_ha < 150)
  mutate(observed_count = sapling_density_ha * 0.09)

seed_data = seed_data %>%
  select(x,y,`2020` = observed_count)

st_geometry(seed_data) = NULL


seed_data$itrap <- 1:nrow(seed_data)

# # Scale by dt to get seedlings by year
nseed <- as.matrix(select(seed_data, -itrap, -x, -y))


# # Round fractions up or down randomly (with p = fraction)
# round_frac <- function(x) {
#   ifelse(runif(length(x)) < (x %% 1), ceiling(x), floor(x))
# }
# nseed[nseed %% 1 > 0] <- round_frac(nseed[nseed %% 1 > 0])
# 
# 


# Calculate tree-trap distance matrix, maximum radius in plot and edge-correction weights
traps <- distinct(seed_data, itrap, x, y)

d2min <- 0.01

dist_sq <- outer(traps$x, tree_data$x, "-")^2 + outer(traps$y, tree_data$y, "-")^2
dist_sq[dist_sq < d2min] <- d2min

r <- sqrt(dist_sq)


### Function to predict seedl dens for a plot


# pred seedl reaching one plot from one tree
predict_seedl_bytree = function(samples,tree_dist,tree_size) {
  
  kern = samples$k / (2*pi * samples$a^2 * gamma(2/samples$k)) * exp(-(tree_dist / samples$a)^samples$k)
  
  b = exp(samples$mu_beta + samples$sd_beta * as.vector(samples$beta_off))
  
  seedl = 900 * b * tree_size * kern # trap area is 900
  
}



predict_seedl_plot = function(iplot) {
  
  tree_dists = r[iplot,]
  
  predicted_seedl_bytree = mapply(FUN = predict_seedl_bytree, tree_dist = tree_dists, tree_size = tree_sizes , MoreArgs = list(samples = samples))
  # in what's returned (pred_seedl), the rows are samples (of the contribution of each tree)
  
  # so we can sum across rows to get samples of the total plot seedl density
  predicted_seedl = rowSums(predicted_seedl_bytree)
  
  # and summarize that across the samples
  predicted_seedl_fit= mean(predicted_seedl)
  predicted_seedl_lwr = quantile(predicted_seedl, 0.025)
  predicted_seedl_upr = quantile(predicted_seedl, 0.975)
  
  plot_df = data.frame(predicted_seedl_fit, predicted_seedl_lwr, predicted_seedl_upr)

}

plan(multisession, workers=8)

plot_seedl_preds = future_map_dfr(1:nrow(seed_data), predict_seedl_plot)
row.names(plot_seedl_preds) = NULL


## combine with observed seedl and compare

seed_data = bind_cols(seed_data,plot_seedl_preds)

plot(`2020`~predicted_seedl_fit,data=seed_data)


ggplot(data=seed_data, aes(x=`2020`, y=predicted_seedl_fit)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  lims(x=c(0,25), y=c(0,10))


## calc the MAE

mae = mean(abs(seed_data$`2020` - seed_data$predicted_seedl_fit))
mae
# 1.39 for height, pois





## make raster of predictions

## change the shape of the fecundity model

## try NB


