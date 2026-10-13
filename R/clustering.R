adjusted_rand <- function(x, y) {
  as.numeric(mclust::adjustedRandIndex(x, y))
}

cluster_indices <- function(x, labels) {
  labels <- as.integer(as.factor(labels))
  groups <- split(seq_len(nrow(x)), labels)
  k <- length(groups)
  n <- nrow(x)
  centers <- lapply(groups, function(i) colMeans(x[i, , drop = FALSE]))
  within <- vapply(seq_along(groups), function(i) {
    sum(rowSums(sweep(x[groups[[i]], , drop = FALSE], 2, centers[[i]])^2))
  }, numeric(1))
  total <- sum(rowSums(sweep(x, 2, colMeans(x))^2))
  ch <- ((total - sum(within)) / (k - 1)) / (sum(within) / (n - k))
  scatter <- vapply(seq_along(groups), function(i) {
    mean(sqrt(rowSums(sweep(x[groups[[i]], , drop = FALSE], 2, centers[[i]])^2)))
  }, numeric(1))
  center_matrix <- do.call(rbind, centers)
  center_dist <- as.matrix(stats::dist(center_matrix))
  diag(center_dist) <- Inf
  db <- mean(vapply(seq_len(k), function(i) {
    max((scatter[[i]] + scatter) / center_dist[i, ])
  }, numeric(1)))
  c(calinski_harabasz = ch, davies_bouldin = db)
}

sampled_silhouette <- function(x, labels, sample_size = 3000L, seed = 42L) {
  n <- nrow(x)
  idx <- if (n > sample_size) {
    set.seed(seed)
    sample.int(n, sample_size)
  } else {
    seq_len(n)
  }
  sil <- cluster::silhouette(as.integer(as.factor(labels[idx])),
                             stats::dist(x[idx, , drop = FALSE]))
  mean(sil[, "sil_width"])
}

evaluate_k <- function(x, k_range = 2:10, sample_size = 3000L) {
  rows <- lapply(k_range, function(k) {
    fit <- stats::kmeans(x, centers = k, nstart = 10, iter.max = 100)
    indices <- cluster_indices(x, fit$cluster)
    data.frame(
      k = k, inertia = fit$tot.withinss,
      silhouette = sampled_silhouette(x, fit$cluster, sample_size),
      calinski_harabasz = indices[["calinski_harabasz"]],
      davies_bouldin = indices[["davies_bouldin"]]
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stability_by_k <- function(x, k_range = 2:10, n_boot = 10L, frac = 0.8) {
  set.seed(42)
  result <- vapply(k_range, function(k) {
    runs <- lapply(seq_len(n_boot), function(b) {
      idx <- sample.int(nrow(x), floor(frac * nrow(x)), replace = FALSE)
      fit <- stats::kmeans(x[idx, , drop = FALSE], centers = k,
                           nstart = 5, iter.max = 100)
      nearest <- apply(x, 1, function(row) {
        which.min(rowSums(sweep(fit$centers, 2, row)^2))
      })
      nearest
    })
    pairs <- utils::combn(seq_len(n_boot), 2)
    mean(apply(pairs, 2, function(pair) adjusted_rand(runs[[pair[[1]]]],
                                                       runs[[pair[[2]]]])))
  }, numeric(1))
  stats::setNames(result, k_range)
}

gmm_bic <- function(x, k_range = 2:10) {
  values <- vapply(k_range, function(k) {
    fit <- mclust::Mclust(x, G = k, modelNames = "VVV", verbose = FALSE)
    -fit$bic
  }, numeric(1))
  stats::setNames(values, k_range)
}

fit_kmeans <- function(x, k) {
  stats::kmeans(x, centers = k, nstart = 20, iter.max = 100)
}

compare_algorithms <- function(x, k, truth = NULL, sample_size = 3000L) {
  km <- fit_kmeans(x, k)
  gmm <- mclust::Mclust(x, G = k, modelNames = "VVV", verbose = FALSE)
  ward <- stats::cutree(stats::hclust(stats::dist(x), method = "ward.D2"), k = k)
  labels <- list(
    "K-Means" = km$cluster,
    "Gaussian Mixture" = gmm$classification,
    "Agglomerative (Ward)" = ward
  )
  rows <- lapply(names(labels), function(name) {
    lab <- labels[[name]]
    idx <- cluster_indices(x, lab)
    row <- data.frame(
      algorithm = name,
      silhouette = sampled_silhouette(x, lab, sample_size),
      calinski_harabasz = idx[["calinski_harabasz"]],
      davies_bouldin = idx[["davies_bouldin"]]
    )
    if (!is.null(truth)) row$ARI_vs_hidden_persona <- adjusted_rand(truth, lab)
    row
  })
  scores <- do.call(rbind, rows)
  rownames(scores) <- scores$algorithm
  list(scores = scores, labels = labels)
}

profile_segments <- function(data, segment_col, features) {
  groups <- split(data, data[[segment_col]])
  result <- lapply(groups, function(group) {
    c(customers = nrow(group), share = nrow(group) / nrow(data),
      vapply(group[features], mean, numeric(1)))
  })
  out <- as.data.frame(do.call(rbind, result), check.names = FALSE)
  out[[segment_col]] <- names(groups)
  rownames(out) <- NULL
  out[c(segment_col, setdiff(names(out), segment_col))]
}

index_vs_average <- function(data, segment_col, features) {
  groups <- split(data[features], data[[segment_col]])
  means <- do.call(rbind, lapply(groups, function(group) {
    vapply(group, mean, numeric(1))
  }))
  overall_mean <- vapply(data[features], mean, numeric(1))
  overall_sd <- vapply(data[features], stats::sd, numeric(1))
  out <- sweep(sweep(means, 2, overall_mean), 2, overall_sd, "/")
  rownames(out) <- names(groups)
  out
}

ARCHETYPES <- list(
  "Loyal High-Value" = c(frequency_12m = 1, monetary_12m = 1,
                         recency_days = -1, email_open_rate = 1, nps_score = 1),
  "Bargain Hunters" = c(discount_usage_rate = 2, avg_order_value = -1,
                        web_sessions_month = 1),
  "Digital Natives" = c(app_sessions_month = 2, social_engagement = 1, age = -1),
  "Occasional Big Spenders" = c(avg_order_value = 2, frequency_12m = -1, income_k = 1),
  "At-Risk Lapsed" = c(recency_days = 2, web_sessions_month = -1,
                       email_open_rate = -1, nps_score = -1)
)

name_segments <- function(z_profile) {
  clusters <- rownames(z_profile)
  archetype_names <- names(ARCHETYPES)
  score <- vapply(ARCHETYPES, function(weights) {
    rowSums(sweep(z_profile[, names(weights), drop = FALSE], 2, weights, "*"))
  }, numeric(nrow(z_profile)))
  assignment <- clue::solve_LSAP(score, maximum = TRUE)
  mapping <- stats::setNames(paste("Segment", clusters), clusters)
  mapping[clusters[seq_along(assignment)]] <- archetype_names[as.integer(assignment)]
  mapping
}
