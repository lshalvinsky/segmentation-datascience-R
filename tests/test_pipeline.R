for (file in c("data.R", "features.R", "clustering.R", "models.R")) {
  source(file.path("R", file))
}

customers <- generate_customers(n = 500, seed = 1)
stopifnot(nrow(customers) == 500L)
stopifnot(length(unique(customers$customer_id)) == 500L)
stopifnot(all(customers$true_persona %in% names(PERSONAS)))
stopifnot(all(customers$responded_campaign %in% c(0, 1)))
stopifnot(all(customers$avg_order_value > 0))

rfm <- rfm_scores(customers)
stopifnot(all(vapply(rfm[c("R", "F", "M")], function(x) {
  all(x >= 1 & x <= 5)
}, logical(1))))

cluster_customers <- generate_customers(n = 1500, seed = 2)
x <- cluster_preprocessor(cluster_customers)
cluster_customers$cluster <- fit_kmeans(x, 5)$cluster
profile <- index_vs_average(cluster_customers, "cluster", PROFILE_FEATURES)
mapping <- name_segments(profile)
stopifnot(length(unique(mapping)) == 5L)

gains <- gains_table(c(1, 0, 1, 0), c(0.9, 0.8, 0.2, 0.1), n_bins = 2)
stopifnot(identical(gains$responders, c(1, 1)))
stopifnot(isTRUE(all.equal(gains$cum_pct_responders, c(0.5, 1))))

message("All segmentation pipeline checks passed.")
