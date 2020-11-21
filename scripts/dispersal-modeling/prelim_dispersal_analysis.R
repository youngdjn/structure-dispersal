library(tidyverse)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


plots = read_csv(data("surveys/main/processed/plots.csv"))
species = read_csv(data("surveys/main/processed/species.csv"))


# compute an "all species" count summary

sp_summ = species %>%
  group_by(fire, plot_id) %>%
  summarize(seedl_count = sum(seedl_analyze))

# pull species data in to plot data
d = full_join(plots,sp_summ) %>%
  mutate(seedl_count = ifelse(is.na(seedl_count),0,seedl_count))


outlier = d %>%
  filter(seedl_count > 50 & seed_source_any > 150)


# 
# # try pines only
# sp_summ = species %>%
#   filter(species_coarse %in% c("PIPJ"),
#          seed_distance_beyond == FALSE) %>%
#   group_by(fire, plot_id) %>%
#   summarize(pine_seedl_count = sum(seedl_analyze),
#             pine_seed_dist = min(seed_distance))
# 
# d = full_join(d,sp_summ) %>%
#   mutate(pine_seedl_count = ifelse(is.na(seedl_count),0,pine_seedl_count))


ggplot(d,aes(x=pine_seed_dist, y=pine_seedl_count,color=fire)) +
  geom_point() +
  theme_bw()



