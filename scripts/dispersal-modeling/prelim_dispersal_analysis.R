library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


plots = read_csv(datadir("surveys/main/processed/plots.csv"))
species = read_csv(datadir("surveys/main/processed/species.csv"))


# compute an "all species" count summary

sp_summ = species %>%
  group_by(fire, plot_id) %>%
  summarize(all_seedl_count = sum(seedl_analyze))

# pull species data in to plot data
d = full_join(plots,sp_summ) %>%
  mutate(all_seedl_count = ifelse(is.na(all_seedl_count),0,all_seedl_count))


outlier = d %>%
  filter(all_seedl_count > 50 & seed_source_any > 150)




### get the number of seed trees near it from the drone data
# first make it spatial
d_sf = st_as_sf(d,coords=c("lon","lat"), crs=4326) %>%
  filter(fire == "Delta")



# load drone trees
delta_drone_trees = st_read(datadir("drone/processed-products/filtered-trees/ttops_filtered_live.gpkg"))

# make buffer around drone plots
plot_buff = st_buffer(d_sf %>% st_transform(3310),dist=200) %>% st_transform(4326)
a = st_intersects(plot_buff,delta_drone_trees,sparse=FALSE)
n_intersecting = rowSums(a)

d_sf$n_trees_within_200m = n_intersecting


na_to_zero = function(x) {
  ifelse(is.na(x),0,x)
}

# summary by species
sp_summ = species %>%
  #filter(species_coarse %in% c("PIPJ")) %>%
  group_by(fire, plot_id, species_coarse) %>%
  summarize(seedl_count = sum(seedl_analyze),
            cone_count = sum(cones_total),
            seed_dist = min(seed_distance)) %>%
  ungroup() %>%
  mutate(seedl_count = ifelse(is.na(seedl_count),0,seedl_count)) %>%
  #group_by(fire,plot_id) %>%
  complete(species_coarse, nesting(plot_id,fire), fill=list(seedl_count=0, cone_count=0)) %>%
  filter(!is.na(species_coarse))
  # pivot_wider(names_from=species_coarse,values_from=c(seedl_count,seed_dist)) %>%
  # mutate(across(starts_with("seedl_count_"), na_to_zero))
  

sp_summ_wide = sp_summ %>%   
  pivot_wider(names_from=species_coarse,values_from=c(seedl_count,seed_dist, cone_count)) %>%
  mutate(across(starts_with("seedl_count_"), na_to_zero))
  

d2 = left_join(d,sp_summ_wide)

d_sf2= left_join(d_sf,sp_summ)

# write spatial for mapping
st_write(d_sf,datadir("surveys/main/processed/spatial/plots_allseedlsp.gpkg"))


ggplot(d2 %>% filter(fire=="Delta"),aes(x=seed_dist_PIPJ, y=seedl_count_PIPJ,color=fire)) +
  geom_point() +
  theme_bw()

d_plot = d_sf2 %>%
  filter(species_coarse %in% c("ABCO")) %>%
  mutate(seedl_dens = seedl_count/0.02)
p = ggplot(d_plot %>% filter(fire=="Delta"),aes(x=n_trees_within_200m, y=seedl_dens)) +
  geom_point(alpha=.3, size=2, color="cyan4") +
  
  theme_bw(25) +
  lims(x = c(0,500),y = c(0,750)) +
  labs(x="Number of seed trees within 200 m", y = "Yellow pine seedling density (no / ha)") +
  #scale_x_sqrt(limits=rev(c(0,500)),breaks=(c(0,100,200,300,400,500)), expand = c(0,0)) +
  geom_smooth(color="cyan4",method = "lm", formula = formula(y~sqrt(x)))
  


png(datadir("figures/prelim_fac_serot.png"))
p
dev.off()


## Get the R2

m = lm(seedl_dens ~ sqrt(n_trees_within_200m), data = d_plot %>% filter(fire=="Delta"))
summary(m)



## Delta plots > 200 m from seed sources
plots_far_seedsource_delta = d_sf2 %>%
  mutate(seedl_dens = seedl_count/0.02) %>%
  filter(fire=="Delta",
         n_trees_within_200m ==0) %>%
  select(fire,plot_id,species_coarse,shrub_cover,seedl_count,cone_count)

st_geometry(plots_far_seedsource_delta) = NULL

write_csv(plots_far_seedsource_delta,datadir("temp/sp_cone_farSeedSource_forDavid.csv"))

## Test correlation between cones and seedlings, when seed source far

d_plot = plots_far_seedsource_delta %>%
  filter(species_coarse == "PSME")

ggplot(d_plot, aes(x=cone_count, y = seedl_count)) +
         geom_jitter() +
  lims(y=c(0,8))









