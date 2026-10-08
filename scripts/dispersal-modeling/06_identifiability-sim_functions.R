# Claude Opus 5.5 built this on top of existing codebase 10/7/26
# Andrew currently testing, verifying, extending -- Andrew 

# Functions for the real-geometry identifiability analysis of inverse dispersal models.
#
# Question: given the ACTUAL tree map and the ACTUAL seedling plot locations at a site,
#   (1) can the two dispersal kernel parameters (scale and shape) be estimated separately?
#   (2) can the size-fecundity exponent be estimated?
#   (3) when they can't, how much do predictions differ along the likelihood ridge, and where?
#
# Approach (profile likelihood on a grid; no MCMC, no stochastic optimizer):
#   - The kernel is parameterized by MEDIAN dispersal distance and SHAPE k. The median is
#     finite for every kernel/shape combination (the mean is not, for fat-tailed 2Dt).
#   - For every (median, k) grid point, fecundity scale b, NB dispersion theta, and
#     (optionally) a background seedling rate are maximized out ("profiled").
#   - Speed trick: for each plot, trees within the cutoff are binned by distance (0.25 to 0.5 m
#     bins), weighted by fecundity weight (size^zeta), with a first-order within-bin correction.
#     The expected count for ANY kernel is then a single matrix product:
#     mu = b * area * H %*% [K(mids), K'(mids)]. This is computed once per geometry for the
#     whole grid, so every simulated dataset reuses it.
#   - Simulations draw counts from the real geometry under known parameters (with NB noise
#     calibrated to the real data), refit, and record whether the shape is identified, whether
#     the truth is recovered, and how wrong the predictions are by distance-to-nearest-tree.
#
# Base R only. Works on the disp_data object returned by get_dispdata() (01_ functions file).
#
# Kernel parameterizations (normalized 2D densities, per m^2):
#   exppow: K(r) = k / (2*pi*a^2*Gamma(2/k)) * exp(-(r/a)^k)         k > 0
#           (k = 1 exponential, k = 2 Gaussian, k < 1 fat-tailed)
#   2Dt   : K(r) = (k-1) / (pi*a^2) * (1 + (r/a)^2)^(-k)                 k > 1
#           (small k = fat-tailed; large k approaches Gaussian)


#### Kernel functions ####

kernel_density <- function(r, a, k, kernel) {
  switch(kernel,
    "exppow" = exp(log(k) - log(2 * pi) - 2 * log(a) - lgamma(2 / k) - (r / a)^k),
    "2Dt" = (k - 1) / (pi * a^2) * (1 + (r / a)^2)^(-k),
    stop("Unknown kernel: ", kernel)
  )
}

# Quantile q of dispersal DISTANCE (not of the 2D density)
kernel_distance_quantile <- function(q, a, k, kernel) {
  switch(kernel,
    # (r/a)^k ~ Gamma(shape = 2/k, rate = 1)
    "exppow" = a * qgamma(q, shape = 2 / k)^(1 / k),
    # CDF F(r) = 1 - (1 + r^2/a^2)^(1-k)
    "2Dt" = a * sqrt((1 - q)^(1 / (1 - k)) - 1),
    stop("Unknown kernel: ", kernel)
  )
}

# Scale parameter a that gives a specified median dispersal distance
a_from_median <- function(median_dist, k, kernel) {
  median_dist / kernel_distance_quantile(0.5, a = 1, k = k, kernel = kernel)
}

kernel_mean_distance <- function(a, k, kernel) {
  switch(kernel,
    "exppow" = a * exp(lgamma(3 / k) - lgamma(2 / k)),
    "2Dt" = ifelse(k > 1.5, a * sqrt(pi) / 2 * exp(lgamma(k - 1.5) - lgamma(k - 1)), Inf),
    stop("Unknown kernel: ", kernel)
  )
}

# Default shape grid for each kernel
default_shape_values <- function(kernel, n = 25) {
  switch(kernel,
    "exppow" = exp(seq(log(0.2), log(4), length.out = n)),
    "2Dt" = 1 + exp(seq(log(0.05), log(30), length.out = n)),
    stop("Unknown kernel: ", kernel)
  )
}

# Grid of candidate kernels, indexed by median dispersal distance and shape
make_kernel_grid <- function(kernel = "exppow",
                             median_range = c(2, 200), # m
                             n_median = 40,
                             shape_values = NULL,
                             n_shape = 25) {
  if (is.null(shape_values)) shape_values <- default_shape_values(kernel, n_shape)
  medians <- exp(seq(log(median_range[1]), log(median_range[2]), length.out = n_median))
  grid <- expand.grid(median = medians, k = shape_values)
  grid$a <- a_from_median(grid$median, grid$k, kernel)
  grid$mean <- kernel_mean_distance(grid$a, grid$k, kernel)
  grid$q95 <- kernel_distance_quantile(0.95, grid$a, grid$k, kernel) # tail reach
  attr(grid, "kernel") <- kernel
  grid
}


#### Geometry: distance histograms ####

# Distance bins: fine near the source, coarser farther out
make_distance_breaks <- function(cutoff = 300) {
  br <- c(seq(0, min(50, cutoff), by = 0.25))
  if (cutoff > 50) br <- c(br, seq(50.5, cutoff, by = 0.5))
  if (max(br) < cutoff) br <- c(br, cutoff)
  br
}

# For each target point, the fecundity-weighted histogram of distances to trees within cutoff.
# Returns an n_target x (2 * n_bins) matrix [H0 | H1]:
#   H0 = sum of weights in each distance bin
#   H1 = sum of weight * (distance - bin midpoint)
# so that sum_j w_j K(r_j) ~= H0 %*% K(mids) + H1 %*% K'(mids)  (first-order, near-exact)
dist_histogram <- function(target_xy, tree_xy, w, breaks, chunk = 200) {
  target_xy <- as.matrix(target_xy)
  tree_xy <- as.matrix(tree_xy)
  nb <- length(breaks) - 1
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  nt <- nrow(target_xy)
  cutoff <- max(breaks)
  H <- matrix(0, nt, 2 * nb)
  for (s in seq(1, nt, by = chunk)) {
    idx <- s:min(nt, s + chunk - 1)
    nr <- length(idx)
    d <- sqrt(outer(target_xy[idx, 1], tree_xy[, 1], "-")^2 +
              outer(target_xy[idx, 2], tree_xy[, 2], "-")^2)
    keep <- which(d <= cutoff)
    if (length(keep) == 0) next
    row <- (keep - 1) %% nr + 1
    col <- (keep - 1) %/% nr + 1
    dk <- d[keep]
    bin <- findInterval(dk, breaks, rightmost.closed = TRUE, all.inside = TRUE)
    cell <- (bin - 1) * nr + row
    agg <- rowsum(cbind(w[col], w[col] * (dk - mids[bin])), cell, reorder = FALSE)
    cells <- as.integer(rownames(agg))
    H0 <- H1 <- matrix(0, nr, nb)
    H0[cells] <- agg[, 1]
    H1[cells] <- agg[, 2]
    H[idx, ] <- cbind(H0, H1)
  }
  H
}

# Kernel value and derivative at bin midpoints, stacked to match dist_histogram() columns
kernel_bin_vector <- function(mids, a, k, kernel, h = 1e-3) {
  K <- kernel_density(mids, a, k, kernel)
  dK <- (kernel_density(mids + h, a, k, kernel) - kernel_density(pmax(mids - h, 0), a, k, kernel)) /
    (mids + h - pmax(mids - h, 0))
  c(K, dK)
}

# Regular grid of prediction points over the plot extent (buffered), subsampled to n_max
make_prediction_points <- function(plot_xy, buffer = 50, res = 10, n_max = 3000, seed = 1) {
  xr <- range(plot_xy[, 1]) + c(-buffer, buffer)
  yr <- range(plot_xy[, 2]) + c(-buffer, buffer)
  pts <- as.matrix(expand.grid(x = seq(xr[1], xr[2], by = res),
                               y = seq(yr[1], yr[2], by = res)))
  if (nrow(pts) > n_max) {
    set.seed(seed)
    pts <- pts[sort(sample(nrow(pts), n_max)), ]
  }
  pts
}

# Build the geometry object used by everything else.
#   zeta: fecundity exponent, fecundity weight = size^zeta (0 = constant, 1 = linear in size)
#   pred_xy: prediction points (NULL = regular grid over plot extent)
make_geometry <- function(tree_xy, tree_size, plot_xy, seedling_counts, plot_area,
                          zeta = 1, cutoff = 300, pred_xy = NULL, pred_res = 10,
                          n_pred_max = 3000) {
  tree_xy <- as.matrix(tree_xy)
  plot_xy <- as.matrix(plot_xy)
  breaks <- make_distance_breaks(cutoff)
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  w <- tree_size^zeta
  if (is.null(pred_xy)) pred_xy <- make_prediction_points(plot_xy, res = pred_res, n_max = n_pred_max)
  pred_xy <- as.matrix(pred_xy)

  # distance from each plot / prediction point to nearest source tree
  nearest <- function(xy) {
    vapply(seq_len(nrow(xy)), function(i) {
      min(sqrt((tree_xy[, 1] - xy[i, 1])^2 + (tree_xy[, 2] - xy[i, 2])^2))
    }, numeric(1))
  }

  geom <- list(
    tree_xy = tree_xy, tree_size = tree_size, zeta = zeta,
    plot_xy = plot_xy, y = seedling_counts, area = plot_area,
    cutoff = cutoff, breaks = breaks, mids = mids,
    H_plots = dist_histogram(plot_xy, tree_xy, w, breaks),
    pred_xy = pred_xy,
    H_pred = dist_histogram(pred_xy, tree_xy, w, breaks),
    plot_nearest = nearest(plot_xy),
    pred_nearest = nearest(pred_xy)
  )
  n_empty <- sum(rowSums(geom$H_plots) == 0)
  if (n_empty > 0) {
    message(n_empty, " plot(s) have no source trees within ", cutoff, " m; ",
            sum(geom$y[rowSums(geom$H_plots) == 0] > 0), " of them have seedlings. ",
            "Consider background = TRUE.")
  }
  geom
}

# Convenience wrapper: geometry straight from a get_dispdata() object
geometry_from_dispdata <- function(disp_data, zeta = 1, cutoff = 300, ...) {
  tr <- disp_data$overstory_trees
  pl <- disp_data$seedling_plots
  make_geometry(tree_xy = cbind(tr$x, tr$y),
                tree_size = tr$size,
                plot_xy = cbind(pl$x, pl$y),
                seedling_counts = disp_data$seedling_counts,
                plot_area = disp_data$seedling_plot_area,
                zeta = zeta, cutoff = cutoff, ...)
}

# Kernel values (and derivatives) at the bin midpoints for every grid column: (2 n_bins) x n_grid
kernel_bin_matrix <- function(grid, mids) {
  kernel <- attr(grid, "kernel")
  vapply(seq_len(nrow(grid)), function(g) {
    kernel_bin_vector(mids, grid$a[g], grid$k[g], kernel)
  }, numeric(2 * length(mids)))
}

# Seed "shadow" S = area * H %*% K for every grid column (expected count per unit b)
seed_shadow <- function(H, Kbin, area) area * (H %*% Kbin)


#### Likelihood profiling ####

# Maximize the likelihood over b, theta (and background lambda0) for a fixed seed shadow S.
#   S: expected count per unit fecundity b (vector over plots)
#   lik: "negbin" or "pois"
#   background: add a constant per-plot rate lambda0 (unmapped / outside-map sources)
profile_nuisance <- function(y, S, lik = "negbin", background = FALSE, start = NULL) {
  sS <- sum(S)
  b0 <- if (sS > 0) max(sum(y) / sS, 1e-12) else 1e-12
  if (is.null(start)) start <- c(log_b = log(b0), log_theta = 0, log_lambda0 = log(max(mean(y), 1e-3)) - 3)

  mu_fn <- function(p) {
    mu <- exp(p[1]) * S
    if (background) mu <- mu + exp(p[3])
    pmax(mu, 1e-10)
  }

  if (lik == "pois" && !background) {
    mu <- pmax(b0 * S, 1e-10)
    return(list(ll = sum(dpois(y, mu, log = TRUE)), b = b0, theta = Inf, lambda0 = 0))
  }

  nll <- function(p) {
    mu <- mu_fn(p)
    if (lik == "pois") -sum(dpois(y, mu, log = TRUE))
    else -sum(dnbinom(y, size = exp(p[2]), mu = mu, log = TRUE))
  }
  free <- c(TRUE, lik == "negbin", background)
  p0 <- start
  p0[1] <- log(b0) # always restart b from its Poisson MLE for this S
  f <- function(pf) { p <- p0; p[free] <- pf; nll(p) }
  lower <- c(-40, -8, -20)[free]
  upper <- c(40, 10, 10)[free]
  opt <- optim(p0[free], f, method = "L-BFGS-B", lower = lower, upper = upper)
  p <- p0; p[free] <- opt$par
  list(ll = -opt$value, b = exp(p[1]),
       theta = if (lik == "negbin") exp(p[2]) else Inf,
       lambda0 = if (background) exp(p[3]) else 0,
       par = p)
}

# Profile likelihood over the whole (median, shape) grid.
#   S_mat: n_plots x n_grid seed shadows (from seed_shadow); computed once per geometry.
profile_surface <- function(y, S_mat, grid, lik = "negbin", background = FALSE) {
  G <- ncol(S_mat)
  out <- data.frame(grid, ll = NA_real_, b = NA_real_, theta = NA_real_, lambda0 = NA_real_)
  start <- NULL
  for (g in seq_len(G)) {
    pr <- profile_nuisance(y, S_mat[, g], lik = lik, background = background, start = start)
    out$ll[g] <- pr$ll
    out$b[g] <- pr$b
    out$theta[g] <- pr$theta
    out$lambda0[g] <- pr$lambda0
    if (!is.null(pr$par) && is.finite(pr$ll)) start <- pr$par # warm start
  }
  attr(out, "kernel") <- attr(grid, "kernel")
  attr(out, "lik") <- lik
  attr(out, "background") <- background
  out
}

# 1-D profile-likelihood CI from a gridded profile, interpolating the deviance between grid
# points on a transformed scale (log for median and exppow k; log(k - 1) for 2Dt k)
profile_ci <- function(vals, prof, llmax, crit, ftrans = log, finv = exp) {
  dev <- llmax - prof
  n <- length(vals)
  tv <- ftrans(vals)
  inside <- which(dev <= crit)
  lo_i <- min(inside); hi_i <- max(inside)
  cross <- function(i_in, i_out) {
    if (!is.finite(dev[i_out])) return((tv[i_in] + tv[i_out]) / 2)
    f <- (crit - dev[i_in]) / (dev[i_out] - dev[i_in])
    tv[i_in] + f * (tv[i_out] - tv[i_in])
  }
  list(lo = finv(if (lo_i == 1) tv[1] else cross(lo_i, lo_i - 1)),
       hi = finv(if (hi_i == n) tv[n] else cross(hi_i, hi_i + 1)),
       lo_edge = lo_i == 1, hi_edge = hi_i == n)
}

# Summarize a profile surface: MLE, 1-D profile CIs for shape and median, joint region
summarize_surface <- function(surf, level = 0.95) {
  ll <- surf$ll
  ll[!is.finite(ll)] <- -Inf
  best <- which.max(ll)
  llmax <- ll[best]
  crit1 <- qchisq(level, 1) / 2
  crit2 <- qchisq(level, 2) / 2
  kernel <- attr(surf, "kernel")

  kv <- sort(unique(surf$k))
  mv <- sort(unique(surf$median))
  prof_k <- vapply(kv, function(v) max(ll[surf$k == v]), numeric(1))
  prof_m <- vapply(mv, function(v) max(ll[surf$median == v]), numeric(1))
  if (kernel == "2Dt") {
    ci_k <- profile_ci(kv, prof_k, llmax, crit1, function(x) log(x - 1), function(x) 1 + exp(x))
  } else {
    ci_k <- profile_ci(kv, prof_k, llmax, crit1)
  }
  ci_m <- profile_ci(mv, prof_m, llmax, crit1)
  joint <- which(llmax - ll <= crit2)

  data.frame(
    kernel = kernel,
    ll_max = llmax,
    median_hat = surf$median[best], k_hat = surf$k[best],
    mean_hat = surf$mean[best], q95_hat = surf$q95[best],
    b_hat = surf$b[best], theta_hat = surf$theta[best], lambda0_hat = surf$lambda0[best],
    k_lo = ci_k$lo, k_hi = ci_k$hi,
    k_fold = ci_k$hi / ci_k$lo,
    # shape CI runs into an edge of the grid -> shape effectively unidentified in that direction
    k_hits_lower_edge = ci_k$lo_edge,
    k_hits_upper_edge = ci_k$hi_edge,
    median_lo = ci_m$lo, median_hi = ci_m$hi,
    median_fold = ci_m$hi / ci_m$lo,
    median_hits_edge = ci_m$lo_edge | ci_m$hi_edge,
    q95_lo = min(surf$q95[joint]), q95_hi = max(surf$q95[joint]),
    n_joint = length(joint)
  )
}


# Fit = coarse profile surface + (if needed) a local fine grid around the maximum.
# The coarse grid is what shows the ridge; the local grid is only used when the coarse CI for
# shape or median spans fewer than `min_steps` grid steps (i.e., the data are informative
# enough that the coarse grid would misstate the CI).
# Returns list(surf_coarse, surf, Kbin, summary, refined), where surf/Kbin are the surface used
# for the summary and for predictions.
fit_surface <- function(y, H, area, grid, Kbin = NULL, lik = "negbin", background = FALSE,
                        refine = TRUE, min_steps = 4, n_local = 21, span_steps = 3,
                        mids) {
  kernel <- attr(grid, "kernel")
  if (is.null(Kbin)) Kbin <- kernel_bin_matrix(grid, mids)
  surf <- profile_surface(y, seed_shadow(H, Kbin, area), grid, lik = lik, background = background)
  sm <- summarize_surface(surf)
  out <- list(surf_coarse = surf, surf = surf, Kbin = Kbin, summary = sm, refined = FALSE)
  if (!refine) return(out)

  ftrans <- if (kernel == "2Dt") function(x) log(x - 1) else log
  finv <- if (kernel == "2Dt") function(x) 1 + exp(x) else exp
  kv <- sort(unique(grid$k)); mv <- sort(unique(grid$median))
  dk <- mean(diff(ftrans(kv))); dm <- mean(diff(log(mv)))
  narrow_k <- (ftrans(sm$k_hi) - ftrans(sm$k_lo)) < min_steps * dk
  narrow_m <- (log(sm$median_hi) - log(sm$median_lo)) < min_steps * dm
  if (!(narrow_k || narrow_m)) return(out)

  k_loc <- finv(seq(ftrans(sm$k_hat) - span_steps * dk, ftrans(sm$k_hat) + span_steps * dk, length.out = n_local))
  m_rng <- exp(log(sm$median_hat) + c(-1, 1) * span_steps * dm)
  grid_loc <- make_kernel_grid(kernel, median_range = m_rng, n_median = n_local, shape_values = k_loc)
  Kbin_loc <- kernel_bin_matrix(grid_loc, mids)
  surf_loc <- profile_surface(y, seed_shadow(H, Kbin_loc, area), grid_loc, lik = lik, background = background)
  sm_loc <- summarize_surface(surf_loc)
  # Use the local result only if its CIs are contained in the local window
  if (sm_loc$k_hits_lower_edge || sm_loc$k_hits_upper_edge || sm_loc$median_hits_edge) return(out)
  list(surf_coarse = surf, surf = surf_loc, Kbin = Kbin_loc, summary = sm_loc, refined = TRUE)
}


#### Predictions along the ridge ####

# Predicted density (seedlings per ha) at prediction points for a set of grid columns
predict_density <- function(geom, surf, Kbin, cols) {
  P <- geom$H_pred %*% Kbin[, cols, drop = FALSE] # per m^2 per unit b
  P <- sweep(P, 2, surf$b[cols], "*")
  P <- sweep(P, 2, surf$lambda0[cols] / geom$area, "+")
  P * 1e4
}

default_distance_bins <- function() c(0, 25, 50, 100, 200, Inf)

# How much do predictions change across the joint 95% region of the (median, shape) surface?
# Returns a per-distance-bin summary of the spread in predicted density, and (if a stocking
# threshold is given) the fraction of points whose stocked / not-stocked call depends on
# which ridge parameters you pick.
ridge_prediction_spread <- function(geom, surf, Kbin, level = 0.95,
                                    dist_bins = default_distance_bins(),
                                    stocking_threshold = NULL, # seedlings per ha
                                    max_cols = 200) {
  ll <- surf$ll
  ll[!is.finite(ll)] <- -Inf
  best <- which.max(ll)
  joint <- which(max(ll) - ll <= qchisq(level, 2) / 2)
  if (length(joint) > max_cols) joint <- joint[unique(round(seq(1, length(joint), length.out = max_cols)))]
  cols <- unique(c(best, joint))
  P <- predict_density(geom, surf, Kbin, cols)
  p_mle <- P[, 1]
  p_min <- apply(P, 1, min)
  p_max <- apply(P, 1, max)
  bin <- cut(geom$pred_nearest, dist_bins, right = FALSE)

  by_bin <- do.call(rbind, lapply(split(seq_along(bin), bin), function(i) {
    if (length(i) == 0) return(NULL)
    row <- data.frame(
      n_points = length(i),
      mle_median_density = median(p_mle[i]),
      ridge_min_median_density = median(p_min[i]),
      ridge_max_median_density = median(p_max[i]),
      # typical max/min ratio across the ridge at a point (orders of magnitude)
      median_log10_spread = median(log10(pmax(p_max[i], 1e-6) / pmax(p_min[i], 1e-6)))
    )
    if (!is.null(stocking_threshold)) {
      row$frac_call_uncertain <- mean(p_min[i] < stocking_threshold & p_max[i] >= stocking_threshold)
      row$frac_stocked_mle <- mean(p_mle[i] >= stocking_threshold)
    }
    row
  }))
  by_bin <- cbind(distance_bin = rownames(by_bin), by_bin)
  rownames(by_bin) <- NULL
  list(by_bin = by_bin,
       point_summary = data.frame(geom$pred_xy, nearest = geom$pred_nearest,
                                  mle = p_mle, ridge_min = p_min, ridge_max = p_max),
       n_ridge_kernels = length(cols))
}


#### Simulation from the real geometry ####

# Expected counts at plots for a "true" kernel
true_expected_counts <- function(geom, median_dist, k, kernel, b, lambda0 = 0, H = geom$H_plots) {
  a <- a_from_median(median_dist, k, kernel)
  S <- geom$area * as.vector(H %*% kernel_bin_vector(geom$mids, a, k, kernel))
  b * S + lambda0
}

# Main simulation driver.
#   truths: data.frame with columns median, k (in the parameterization of kernel_true)
#   target_mean_count: b is calibrated so the mean expected count per plot equals this
#     (default: the observed mean count at the site)
#   theta: NB dispersion for simulated counts (default: estimate from the real-data fit)
#   frac_unmapped: fraction of source trees present when simulating but missing from the map
#     used for fitting (mimics omission / misclassification / sources outside the map)
#   zeta_true, zeta_fit: fecundity exponents used to simulate and to fit
run_identifiability_sim <- function(geom,
                                    truths,
                                    kernel_true = "exppow",
                                    kernel_fit = kernel_true,
                                    grid_fit = make_kernel_grid(kernel_fit),
                                    n_reps = 20,
                                    theta,
                                    target_mean_count = mean(geom$y),
                                    lik = "negbin",
                                    background = FALSE,
                                    frac_unmapped = 0,
                                    zeta_true = geom$zeta,
                                    zeta_fit = geom$zeta,
                                    dist_bins = default_distance_bins(),
                                    stocking_threshold = NULL, # seedlings/ha, for misclassification rates
                                    density_floor = 1,         # seedlings/ha, floor for log errors
                                    seed = 1,
                                    verbose = TRUE) {
  set.seed(seed)
  Kbin_fit <- kernel_bin_matrix(grid_fit, geom$mids)
  rebuild_H <- function(xy, keep, zeta) {
    dist_histogram(xy, geom$tree_xy[keep, , drop = FALSE], geom$tree_size[keep]^zeta, geom$breaks)
  }
  all_trees <- rep(TRUE, nrow(geom$tree_xy))
  H_true_plots <- if (zeta_true == geom$zeta) geom$H_plots else rebuild_H(geom$plot_xy, all_trees, zeta_true)
  H_true_pred <- if (zeta_true == geom$zeta) geom$H_pred else rebuild_H(geom$pred_xy, all_trees, zeta_true)
  same_fit_geom <- frac_unmapped == 0 && zeta_fit == geom$zeta
  bin_pred <- cut(geom$pred_nearest, dist_bins, right = FALSE)

  results <- list()
  for (t in seq_len(nrow(truths))) {
    tr <- truths[t, ]
    a_true <- a_from_median(tr$median, tr$k, kernel_true)
    S_true <- true_expected_counts(geom, tr$median, tr$k, kernel_true, b = 1, H = H_true_plots)
    b_true <- target_mean_count / mean(S_true)
    mu_true <- b_true * S_true
    dens_true <- 1e4 * b_true * as.vector(H_true_pred %*% kernel_bin_vector(geom$mids, a_true, tr$k, kernel_true))

    for (r in seq_len(n_reps)) {
      y_sim <- rnbinom(length(mu_true), size = theta, mu = mu_true)

      if (same_fit_geom) {
        geom_fit <- geom
      } else {
        keep <- if (frac_unmapped > 0) runif(nrow(geom$tree_xy)) > frac_unmapped else all_trees
        geom_fit <- geom
        geom_fit$H_plots <- rebuild_H(geom$plot_xy, keep, zeta_fit)
        geom_fit$H_pred <- rebuild_H(geom$pred_xy, keep, zeta_fit)
      }

      fit <- fit_surface(y_sim, geom_fit$H_plots, geom$area, grid_fit, Kbin = Kbin_fit,
                         lik = lik, background = background, mids = geom$mids)
      sm <- fit$summary

      # Prediction error of the MLE vs the truth, by distance to nearest (true) source tree
      best <- which.max(replace(fit$surf$ll, !is.finite(fit$surf$ll), -Inf))
      dens_fit <- predict_density(geom_fit, fit$surf, fit$Kbin, best)[, 1]
      # densities below density_floor (seedlings/ha) are treated as equivalent ("none")
      lr <- log10(pmax(dens_fit, density_floor) / pmax(dens_true, density_floor))
      bin_lab <- gsub("[^0-9Inf]+", "_", levels(bin_pred))
      perr <- tapply(abs(lr), bin_pred, median)
      names(perr) <- paste0("pred_abs_log10err", bin_lab)
      if (!is.null(stocking_threshold)) {
        mis <- tapply((dens_fit >= stocking_threshold) != (dens_true >= stocking_threshold), bin_pred, mean)
        names(mis) <- paste0("stocking_misclass", bin_lab)
        perr <- c(perr, mis)
      }

      results[[length(results) + 1]] <- data.frame(
        truth_id = t, rep = r, kernel_true = kernel_true, kernel_fit = kernel_fit,
        median_true = tr$median, k_true = tr$k, a_true = a_true,
        q95_true = kernel_distance_quantile(0.95, a_true, tr$k, kernel_true),
        b_true = b_true, theta_true = theta, frac_unmapped = frac_unmapped,
        zeta_true = zeta_true, zeta_fit = zeta_fit,
        n_nonzero = sum(y_sim > 0), mean_count = mean(y_sim),
        sm, refined = fit$refined,
        # coverage (only meaningful when kernel_fit == kernel_true)
        k_covered = tr$k >= sm$k_lo & tr$k <= sm$k_hi,
        median_covered = tr$median >= sm$median_lo & tr$median <= sm$median_hi,
        as.list(perr)
      )
    }
    if (verbose) message("Truth ", t, "/", nrow(truths), " done (median = ",
                         round(tr$median, 1), " m, k = ", signif(tr$k, 2), ")")
  }
  do.call(rbind, results)
}

# Per-truth summary of a simulation result table
summarize_sim <- function(sim) {
  per <- split(sim, sim$truth_id)
  do.call(rbind, lapply(per, function(d) {
    perr_cols <- grep("^(pred_abs_log10err|stocking_misclass)", names(d), value = TRUE)
    data.frame(
      truth_id = d$truth_id[1], median_true = d$median_true[1], k_true = d$k_true[1],
      q95_true = d$q95_true[1], n_reps = nrow(d),
      # how often the shape CI is bounded on both sides (i.e., shape identified)
      frac_shape_bounded = mean(!d$k_hits_lower_edge & !d$k_hits_upper_edge),
      median_k_fold = median(d$k_fold),
      k_coverage = mean(d$k_covered),
      frac_median_bounded = mean(!d$median_hits_edge),
      median_median_fold = median(d$median_fold),
      median_coverage = mean(d$median_covered),
      median_k_hat = median(d$k_hat),
      median_median_hat = median(d$median_hat),
      as.list(colMeans(d[, perr_cols, drop = FALSE], na.rm = TRUE))
    )
  }))
}


#### Fecundity exponent profile (real data) ####

# Profile likelihood for the size-fecundity exponent zeta (weight = size^zeta).
# For each zeta: rebuild the plot histograms and maximize over the full kernel grid.
profile_zeta <- function(geom, grid, zeta_values = seq(0, 3, by = 0.25),
                         lik = "negbin", background = FALSE) {
  Kbin <- kernel_bin_matrix(grid, geom$mids)
  do.call(rbind, lapply(zeta_values, function(z) {
    H <- dist_histogram(geom$plot_xy, geom$tree_xy, geom$tree_size^z, geom$breaks)
    surf <- profile_surface(geom$y, seed_shadow(H, Kbin, geom$area), grid,
                            lik = lik, background = background)
    sm <- summarize_surface(surf)
    data.frame(zeta = z, ll_max = sm$ll_max, median_hat = sm$median_hat, k_hat = sm$k_hat)
  }))
}


#### Wrapper: full analysis for one site x species ####

# Runs (1) the real-data profile surface (with and without a background term),
# (2) the spread of predictions along the ridge, (3) the fecundity-exponent profile, and
# (4) the simulation scenarios. Saves an .rds and PNG figures to out_dir.
run_site_species <- function(disp_data, label, out_dir,
                             kernel = "exppow",
                             zeta = 1,
                             cutoff = 300,
                             lik = "negbin",
                             grid = make_kernel_grid(kernel, median_range = c(2, 300),
                                                     n_median = 40, n_shape = 25),
                             truths = expand.grid(median = c(10, 25, 50),
                                                  k = if (kernel == "exppow") c(0.5, 1, 2) else c(1.5, 3, 10)),
                             n_reps = 20,
                             theta_lownoise = 20,   # "what if the data were much less noisy"
                             frac_unmapped = 0.2,   # "what if 20% of sources are missing from the map"
                             zeta_values = seq(0, 3, by = 0.25),
                             stocking_threshold = NULL, # seedlings/ha
                             run_sims = TRUE,
                             make_plots = TRUE,
                             seed = 1) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  t0 <- Sys.time()
  message("== ", label, ": building geometry")
  geom <- geometry_from_dispdata(disp_data, zeta = zeta, cutoff = cutoff)
  Kbin <- kernel_bin_matrix(grid, geom$mids)

  message("== ", label, ": real-data profile surfaces")
  fit_real <- fit_surface(geom$y, geom$H_plots, geom$area, grid, Kbin, lik = lik,
                          background = FALSE, mids = geom$mids)
  fit_real_bg <- fit_surface(geom$y, geom$H_plots, geom$area, grid, Kbin, lik = lik,
                             background = TRUE, mids = geom$mids)
  real_summary <- rbind(cbind(model = "no_background", fit_real$summary),
                        cbind(model = "background", fit_real_bg$summary))

  message("== ", label, ": prediction spread along the ridge")
  spread <- ridge_prediction_spread(geom, fit_real$surf_coarse, Kbin,
                                    stocking_threshold = stocking_threshold)

  message("== ", label, ": fecundity exponent profile")
  zprof <- profile_zeta(geom, grid, zeta_values = zeta_values, lik = lik)

  sims <- NULL
  if (run_sims) {
    theta_hat <- fit_real$summary$theta_hat
    message("== ", label, ": simulations (theta from real data = ", signif(theta_hat, 3), ")")
    sims <- list(
      realistic = run_identifiability_sim(geom, truths, kernel_true = kernel, grid_fit = grid,
                                          n_reps = n_reps, theta = theta_hat, lik = lik,
                                          stocking_threshold = stocking_threshold, seed = seed),
      low_noise = run_identifiability_sim(geom, truths, kernel_true = kernel, grid_fit = grid,
                                          n_reps = n_reps, theta = theta_lownoise, lik = lik,
                                          stocking_threshold = stocking_threshold, seed = seed + 1),
      unmapped = run_identifiability_sim(geom, truths, kernel_true = kernel, grid_fit = grid,
                                         n_reps = max(5, n_reps %/% 2), theta = theta_hat, lik = lik,
                                         frac_unmapped = frac_unmapped, background = TRUE,
                                         stocking_threshold = stocking_threshold, seed = seed + 2)
    )
  }

  res <- list(label = label, settings = list(kernel = kernel, zeta = zeta, cutoff = cutoff, lik = lik,
                                             n_reps = n_reps, theta_lownoise = theta_lownoise,
                                             frac_unmapped = frac_unmapped,
                                             stocking_threshold = stocking_threshold),
              n_plots = length(geom$y), n_trees = nrow(geom$tree_xy),
              n_plots_no_trees = sum(rowSums(geom$H_plots) == 0),
              real_summary = real_summary,
              surface = fit_real$surf_coarse, surface_bg = fit_real_bg$surf_coarse,
              ridge_spread = spread$by_bin, ridge_points = spread$point_summary,
              zeta_profile = zprof,
              sims = sims,
              sim_summary = if (!is.null(sims)) lapply(sims, summarize_sim) else NULL)
  saveRDS(res, file.path(out_dir, paste0(label, "_identifiability.rds")))

  if (make_plots) plot_identifiability(res, out_dir)
  message("== ", label, ": done in ", format(round(Sys.time() - t0, 1)))
  invisible(res)
}


#### Plots ####

plot_identifiability <- function(res, out_dir) {
  require(ggplot2)
  lab <- res$label
  save <- function(p, name, w = 7, h = 5) {
    ggsave(file.path(out_dir, paste0(lab, "_", name, ".png")), p, width = w, height = h, dpi = 150)
  }

  # 1. Likelihood surface in (median distance, shape) space
  surf <- res$surface
  surf$dll <- pmax(surf$ll - max(surf$ll[is.finite(surf$ll)]), -20)
  best <- surf[which.max(surf$dll), ]
  p1 <- ggplot(surf, aes(median, k)) +
    geom_tile(aes(fill = dll)) +
    geom_contour(aes(z = dll), breaks = -qchisq(0.95, 2) / 2, colour = "white", linewidth = 0.8) +
    geom_contour(aes(z = q95), colour = "grey85", linetype = 2, breaks = c(25, 50, 100, 200)) +
    geom_point(data = best, colour = "red", size = 3) +
    scale_x_log10() + scale_y_log10() +
    scale_fill_viridis_c(name = "log-lik\nrel. to max") +
    labs(x = "Median dispersal distance (m)", y = paste0("Kernel shape k (", res$settings$kernel, ")"),
         title = paste0(lab, ": profile likelihood"),
         subtitle = "White: joint 95% region. Dashed: 95th-percentile dispersal distance = 25, 50, 100, 200 m") +
    theme_minimal()
  save(p1, "surface")

  # 2. Prediction spread across the joint 95% region, by distance to nearest source tree
  sp <- res$ridge_spread
  sp$distance_bin <- factor(sp$distance_bin, levels = sp$distance_bin)
  p2 <- ggplot(sp, aes(distance_bin)) +
    geom_linerange(aes(ymin = pmax(ridge_min_median_density, 1e-3),
                       ymax = pmax(ridge_max_median_density, 1e-3)), linewidth = 3, colour = "grey70") +
    geom_point(aes(y = pmax(mle_median_density, 1e-3)), size = 3) +
    scale_y_log10() +
    { if (!is.null(res$settings$stocking_threshold))
        geom_hline(yintercept = res$settings$stocking_threshold, linetype = 2, colour = "red") } +
    labs(x = "Distance to nearest mapped source tree (m)", y = "Predicted seedlings / ha (median over points)",
         title = paste0(lab, ": predictions along the likelihood ridge"),
         subtitle = "Point = MLE; bar = range across kernels in the joint 95% region") +
    theme_minimal()
  save(p2, "ridge_spread")

  # 3. Fecundity exponent profile
  zp <- res$zeta_profile
  zp$dll <- zp$ll_max - max(zp$ll_max)
  p3 <- ggplot(zp, aes(zeta, dll)) + geom_line() + geom_point() +
    geom_hline(yintercept = -qchisq(0.95, 1) / 2, linetype = 2) +
    labs(x = "Fecundity exponent zeta (fecundity ~ height^zeta)", y = "Profile log-lik rel. to max",
         title = paste0(lab, ": size-fecundity exponent"),
         subtitle = "Dashed: 95% CI threshold. zeta = 0 is equal fecundity for all trees") +
    theme_minimal()
  save(p3, "zeta_profile", 6, 4)

  # 4. Simulation results: shape CIs by truth and scenario
  if (!is.null(res$sims)) {
    sims <- do.call(rbind, lapply(names(res$sims), function(n) {
      d <- res$sims[[n]]
      d$scenario <- n
      d[, c("scenario", "truth_id", "rep", "median_true", "k_true", "k_hat", "k_lo", "k_hi",
            "median_hat", "median_lo", "median_hi")]
    }))
    sims$truth <- paste0("median ", sims$median_true, " m, k = ", sims$k_true)
    sims$scenario <- factor(sims$scenario, levels = names(res$sims))
    p4 <- ggplot(sims, aes(x = rep, colour = scenario)) +
      geom_linerange(aes(ymin = k_lo, ymax = k_hi), position = position_dodge(width = 0.7)) +
      geom_hline(aes(yintercept = k_true), linetype = 2) +
      scale_y_log10() + facet_wrap(~truth) +
      labs(x = "Replicate", y = "95% profile CI for shape k",
           title = paste0(lab, ": can the kernel shape be recovered from this geometry?"),
           subtitle = "Dashed: true k. A CI spanning the whole axis means the shape is not identified") +
      theme_minimal()
    save(p4, "sim_shape_ci", 10, 7)

    p5 <- ggplot(sims, aes(x = rep, colour = scenario)) +
      geom_linerange(aes(ymin = median_lo, ymax = median_hi), position = position_dodge(width = 0.7)) +
      geom_hline(aes(yintercept = median_true), linetype = 2) +
      scale_y_log10() + facet_wrap(~truth) +
      labs(x = "Replicate", y = "95% profile CI for median dispersal distance (m)",
           title = paste0(lab, ": recovery of median dispersal distance")) +
      theme_minimal()
    save(p5, "sim_median_ci", 10, 7)
  }
  invisible(NULL)
}
