library(tidyverse)
library(here)
library(rstan)
library(sf)
library(furrr)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))


res = readRDS(datadir("temp/mod_pois_diam.rds"))

samples = extract(res)


## function to plot a dispersal kernel from one tree of average size

r = 100
calc_kern = function(samples, r) {
  
  kern = samples$k / (2*pi * samples$a^2 * gamma(2/samples$k)) * exp(-(r / samples$a)^samples$k)
  #kern = samples$k / (pi*samples$a) * (1 + r^2/samples$a)^(-1-samples$k)
}

r = 0:500

kern_out = sapply(r,calc_kern, samples = samples)

kern_out = t(kern_out)

kern_summary = data.frame (r = 0:500,
                           fit = apply(kern_out,1,mean),
                           lwr = apply(kern_out,1,quantile,probs=c(0.25)),
                           upr = apply(kern_out,1,quantile,probs=c(0.75)))

#kern_summary_comb = bind_rows(kern_summary_exppow, kern_summary_2Dt)

ggplot(data = kern_summary, aes(x = r, y = fit)) + # color=kernel, fill=kernel
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
  geom_line(size=1) +
  theme_bw(20) +
  #scale_color_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  #scale_fill_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  labs(x="Distance (m)", y = "Kernel density")


ggsave(datadir("temp/kern.png"), width=8, height=5)

#### Predict seedlings at each plot ####


## need tree data: years as columns, with the values of the column being the size. Also need id, x, and y

ttops = st_read(datadir("drone/processed-products/crater/Crater120m_20210122T1219_ttops_vwf196.gpkg")) %>% st_transform(32611)
coords = st_coordinates(ttops)
ttops$x = coords[,1]
ttops$y = coords[,2]


a = 1.4721
b = 0.6848
ba_from_height = function(h) {
  3.14 * (   ((h/a)^(1/b))   /2)^2
}

diam_from_height = function(h) {
  ((h/a)^(1/b))
}

ttops = ttops %>%
  filter(height > 10) %>%
  mutate(ba = ba_from_height(height),
         diam = diam_from_height(height))


tree_data = ttops %>%
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
  #kern = samples$k / (pi*samples$a) * (1 + tree_dist^2/samples$a)^(-1-samples$k)
  
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
seed_data$observed = seed_data$`2020`

plot(`2020`~predicted_seedl_fit,data=seed_data)

#fitted vs observed

seed_data_transf = seed_data %>%
  mutate(observed = observed/0.09,
         predicted_seedl_fit = predicted_seedl_fit/0.09)

ggplot(data=seed_data_transf, aes(x=observed, y=predicted_seedl_fit)) +
  geom_abline(slope=1,intercept=0, color="blue") +
  geom_point() +
  lims(x=c(0,250), y=c(0,100)) +
  labs(x = "Observed seedlings / ha", y = "Fitted seedlings / ha") +
  theme_bw(20)

ggsave(datadir("temp/fit_obs_exppow.png"), width=6,height=5)

## calc the MAE

mae = mean(abs(seed_data$`2020` - seed_data$predicted_seedl_fit))
mae
# 1.39 for height, pois





## make raster of predictions

# for every 30 cell in this landscape, make a "plot", calc mat of distances from trees, predict

library(terra)

# make a "negative buffer" of the focal area
foc = st_read(datadir("temp/plots_buff2.gpkg"))
bbox = st_bbox(foc) %>% st_as_sfc
bbox = st_buffer(bbox,300)
foc_inv = st_difference(bbox,foc)
st_write(foc_inv,datadir("temp/foc_inv.gpkg"), delete_dsn = TRUE)

grid = rast(resolution = 30, ext = extent(foc) , crs = "EPSG:26911")
values(grid) = 1:ncell(grid)

# make the cells into points

pts = as.points(grid) %>% st_as_sf
coords = st_coordinates(pts)
pts$x = coords[,1]
pts$y = coords[,2]

st_geometry(pts) = NULL


# Calculate tree-point distance matrix

d2min <- 0.01

dist_sq <- outer(pts$x, tree_data$x, "-")^2 + outer(pts$y, tree_data$y, "-")^2
dist_sq[dist_sq < d2min] <- d2min

r <- sqrt(dist_sq)

plan(multisession, workers=8)

plot_seedl_preds = future_map_dfr(1:nrow(pts), predict_seedl_plot)
row.names(plot_seedl_preds) = NULL

pts = bind_cols(pts,plot_seedl_preds)

values(grid) = pts$predicted_seedl_fit

writeRaster(grid,datadir("temp/pred_seedl_rast.tif"), overwrite=TRUE)

## just for cartography/viz: buffer of within 10 m of a tree

st_write(ttops %>% filter(height > 10), datadir("temp/ttops_foc.gpkg"), delete_dsn=TRUE)

tree_buff = st_buffer(ttops,30) %>% st_union
tree_buff = nngeo::st_remove_holes(tree_buff)
tree_buff = st_buffer(tree_buff,-10)
st_write(tree_buff,datadir("temp/tree_buff.gpkg"), delete_dsn=TRUE)

## compare BA to GNN layer