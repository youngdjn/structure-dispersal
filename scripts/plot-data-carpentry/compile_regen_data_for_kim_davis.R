library(tidyverse)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


plots = read_csv(data("surveys/main/processed/plots.csv"))
species = read_csv(data("surveys/main/processed/species.csv"))


## Make species data wide
sp_wide = species %>%
  select(fire, plot_id, species_coarse, seedl_total) %>%
  filter(species_coarse %in% c("ABCO","CADE","PILA","PIPJ","PSME")) %>%
  pivot_wider(id_cols=c(fire,plot_id),
              names_from = species_coarse,
              values_from = seedl_total,
              values_fill = list(seedl_total=0))


## Get/add the plot data we need
plots2 = plots %>%
  mutate(plot_id_concat = paste(fire,plot_id,sep="_")) %>%
  mutate(contributor = "DJNY") %>%
  mutate(fire_year = recode(fire,
                            Delta = 2018,
                            Valley = 2015,
                            Eiler = 2014,
                            Chips = 2012,
                            Lassic = 2015)) %>%
  mutate(sample_year = 2020) %>%
  mutate(time_since_fire = sample_year-fire_year) %>%
  mutate(fire_severity_category = "high") %>%
  # make seed source distance NA if it's "beyond"
  mutate(distance_seed_source = ifelse(seed_source_any_beyond == TRUE,NA,seed_source_any))

## Pull in the seedling data, organize cols
d = left_join(plots2,sp_wide) %>%
  mutate(count_ABGR = 0,
         count_ABMA = 0,
         count_ABLA = 0,
         count_LAOC = 0,
         count_PIAL = 0,
         count_PICO = 0,
         count_PIEN = 0,
         count_PIST = 0)

d = d %>%
  select(plot_id = plot_id_concat,
         contributor = contributor,
         longitude = lon,
         latitude = lat,
         fire_year = fire_year,
         plot_size = plot_area,
         sample_year = sample_year,
         time_since_fire = time_since_fire,
         count_ABCO = ABCO,
         count_ABGR,
         count_ABLA,
         count_ABMA,
         count_CADE = CADE,
         count_LAOC,
         count_PIAL,
         count_PICO,
         count_PIEN,
         count_PILA = PILA,
         count_PIPO_PIJE = PIPJ,
         count_PIST,
         count_PSME = PSME,
         distance_seed_source,
         fire_severity_category,
         shrub_cover)



write_csv(d,"/home/derek/Dropbox/Research projects/KDavis-regen-synthesis/data_2020.csv")

