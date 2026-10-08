# Claude Opus 5.5 built this on top of existing codebase 10/7/26
# Andrew currently testing, verifying, extending -- Andrew 

# Real-geometry identifiability analysis for the inverse dispersal models.
# Functions are in 06_identifiability-sim_functions.R (base R + ggplot2 for figures).
#
# For each site x species this script:
#   1. Builds the geometry from the real tree map and real plot locations (get_dispdata()).
#   2. Computes the profile likelihood surface of the real data over
#      (median dispersal distance, kernel shape), with b, theta (and optionally a background
#      seedling rate) maximized out. -> Is there a ridge? Where does it run?
#   3. Measures how much predicted seedling density varies across the joint 95% region,
#      by distance to nearest source tree. -> Does the ridge matter for management?
#   4. Profiles the size-fecundity exponent zeta (fecundity ~ height^zeta).
#   5. Simulates seedling counts from the SAME geometry under known kernels, with NB noise
#      calibrated to the real data, and refits. Scenarios:
#        realistic  - theta estimated from the real data
#        low_noise  - theta = 20 (what better data would buy)
#        unmapped   - 20% of source trees missing from the map used for fitting
#      -> Could the shape be recovered here even if the model were exactly right?
#
# Outputs: {data_dir}/identifiability-sim/{site}-{species}/ (an .rds plus PNG figures)
#
# Runtime: roughly 3-6 s per simulated fit with the default 40 x 25 grid, so the defaults
#   (9 truths x [20 + 20 + 10] reps) take ~30-45 min per site x species. Set n_reps lower
#   for a first pass.

#### Setup ####

library(here)
library(tidyverse)

source(here("scripts/dispersal-modeling/01_load-data-for-modeling_functions.R"))
source(here("scripts/dispersal-modeling/06_identifiability-sim_functions.R"))

# Set data directory -- detect whether on Jetstream vs Andrew's machine
if (grepl("latimer", here())) {
  data_dir = readLines(here("data_dir_andrew.txt"), n = 1)
} else {
  data_dir = readLines(here("data_dir.txt"), n = 1)
}

#### Settings ####

# Site x species combinations to run (add the new sites as their maps come in)
site_species = tribble(
  ~site,    ~species,
  # "crater", "PILA",
  "delta",  "PIPJ",
  "delta",  "ABCO"
)

tree_distance_cutoff = 300   # m; trees farther than this contribute nothing
seedling_plot_area = 201     # m^2
min_tree_height = 10         # m
kernel = "exppow"            # "exppow" or "2Dt"
zeta = 1                     # fecundity ~ height^zeta for the main analysis (1 = current linear model)
stocking_threshold = 250     # seedlings/ha that counts as "adequately stocked" -- EDIT to the
                             #   threshold you want to use for management classification
n_reps = 20                  # simulated datasets per true parameter set (try 5 for a first pass)

# True parameter sets to simulate: median dispersal distance (m) x shape.
# exppow k: 0.5 = fat-tailed, 1 = exponential, 2 = Gaussian
truths = expand.grid(median = c(10, 25, 50), k = c(0.5, 1, 2))

# Kernel grid used for fitting (log-spaced). Median range should comfortably bracket the
# plausible values; the shape range is the default for the kernel.
grid = make_kernel_grid(kernel, median_range = c(2, 300), n_median = 40, n_shape = 25)

out_root = file.path(data_dir, "identifiability-sim")

#### Run ####

results = list()
for (i in seq_len(nrow(site_species))) {
  site_name = site_species$site[i]
  focal_species = site_species$species[i]
  label = paste0(site_name, "-", focal_species)

  set.seed(1) # get_dispdata() randomly rounds fractional counts
  disp_data = get_dispdata(
    data_dir = data_dir,
    site_name = site_name,
    focal_species = focal_species,
    overstory_tree_filepath = paste0("predicted-treecrowns-w-predicted-species/", site_name, ".geojson"),
    seedling_plot_filepath = paste0("regen-plots-standardized/", site_name, ".gpkg"),
    target_crs = 3310,
    seedling_plot_area = seedling_plot_area,
    min_tree_height = min_tree_height,
    density_raster_resolution = 10,
    tree_distance_cutoff = tree_distance_cutoff
  )

  results[[label]] = run_site_species(
    disp_data, label = label, out_dir = file.path(out_root, label),
    kernel = kernel, zeta = zeta, cutoff = tree_distance_cutoff,
    grid = grid, truths = truths, n_reps = n_reps,
    stocking_threshold = stocking_threshold
  )
}

#### Summaries across site x species ####

# Real data: MLE and 95% profile CIs for shape and median (with / without background term)
real_table = map_dfr(results, ~ mutate(.x$real_summary, label = .x$label, n_plots = .x$n_plots,
                                       n_trees = .x$n_trees, n_plots_no_trees = .x$n_plots_no_trees)) |>
  select(label, model, n_plots, n_trees, n_plots_no_trees, median_hat, median_lo, median_hi,
         k_hat, k_lo, k_hi, k_hits_lower_edge, k_hits_upper_edge, q95_lo, q95_hi,
         theta_hat, lambda0_hat, ll_max)
real_table

# Simulations: how often is the shape bounded, and is the truth covered?
sim_table = map_dfr(results, function(r) {
  map_dfr(names(r$sim_summary), ~ mutate(r$sim_summary[[.x]], scenario = .x, label = r$label))
}) |>
  select(label, scenario, median_true, k_true, frac_shape_bounded, median_k_fold, k_coverage,
         median_median_fold, median_coverage, starts_with("pred_abs_log10err"),
         starts_with("stocking_misclass"))
sim_table

# Ridge prediction spread: orders of magnitude of disagreement among plausible kernels
spread_table = map_dfr(results, ~ mutate(.x$ridge_spread, label = .x$label))
spread_table

# Fecundity exponent profiles
zeta_table = map_dfr(results, ~ mutate(.x$zeta_profile, label = .x$label,
                                       dll = ll_max - max(ll_max)))
ggplot(zeta_table, aes(zeta, dll, colour = label)) + geom_line() +
  geom_hline(yintercept = -qchisq(0.95, 1) / 2, linetype = 2) + theme_minimal() +
  labs(y = "Profile log-likelihood relative to max")

write_csv(real_table, file.path(out_root, "real_data_summary.csv"))
write_csv(sim_table, file.path(out_root, "simulation_summary.csv"))
write_csv(spread_table, file.path(out_root, "ridge_prediction_spread.csv"))
write_csv(zeta_table, file.path(out_root, "zeta_profiles.csv"))

#### Interpreting the output ####
# - {label}_surface.png: if the white 95% region is a long diagonal band reaching the top or
#   bottom of the shape axis, shape is not identified from the real data. The dashed lines
#   (95th-percentile dispersal distance) show whether the band follows a line of constant
#   tail reach -- if so, the data constrain the tail reach even when shape is unconstrained.
# - {label}_sim_shape_ci.png: if shape CIs span the axis even in the "realistic" scenario,
#   the GEOMETRY + NOISE cannot identify shape, so no model choice will fix it. If they are
#   bounded in "low_noise" only, overdispersion is the limiting factor.
# - {label}_ridge_spread.png and ridge_prediction_spread.csv: if the bars are short in the
#   distance bins that matter for replanting decisions (and frac_call_uncertain is low), fix
#   the shape (or use a 1-parameter kernel) without loss for prediction.
# - zeta profiles: a flat profile (span of zeta values within the dashed line) means the
#   size-fecundity relationship is not identified; use a fixed exponent from the literature
#   or equal fecundity.
