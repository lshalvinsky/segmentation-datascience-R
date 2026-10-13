CLUSTER_FEATURES <- c(
  "recency_days", "frequency_12m", "avg_order_value", "discount_usage_rate",
  "web_sessions_month", "app_sessions_month", "email_open_rate", "social_engagement"
)

SKEWED_FEATURES <- c(
  "recency_days", "frequency_12m", "avg_order_value",
  "web_sessions_month", "app_sessions_month", "social_engagement"
)

PROFILE_FEATURES <- c(
  CLUSTER_FEATURES, "monetary_12m", "tenure_months", "age", "income_k",
  "nps_score", "email_click_rate", "share_electronics", "share_apparel",
  "share_home", "share_beauty", "responded_campaign", "churned_6m"
)

NUMERIC_FEATURES <- c(
  "age", "income_k", "tenure_months", "recency_days", "frequency_12m",
  "monetary_12m", "avg_order_value", "discount_usage_rate",
  "web_sessions_month", "app_sessions_month", "email_open_rate",
  "email_click_rate", "social_engagement", "support_tickets_12m",
  "nps_score", "share_electronics", "share_apparel", "share_home", "share_beauty"
)

CATEGORICAL_FEATURES <- c("gender", "region", "preferred_channel")

cluster_preprocessor <- function(data) {
  transformed <- data[CLUSTER_FEATURES]
  transformed[SKEWED_FEATURES] <- lapply(transformed[SKEWED_FEATURES], log1p)
  scaled <- scale(transformed)
  if (any(!is.finite(scaled))) {
    stop("Clustering features must have finite, non-constant values.")
  }
  scaled
}

rank_quintile <- function(x) {
  pmin(5L, as.integer(ceiling(rank(x, ties.method = "first") * 5 / length(x))))
}

rfm_label <- function(r, f, m) {
  fm <- (f + m) / 2
  if (r >= 4 && fm >= 4) return("Champions")
  if (r >= 3 && fm >= 3) return("Loyal")
  if (r >= 4) return("Promising")
  if (r <= 2 && fm >= 3) return("At Risk")
  if (r <= 2) return("Hibernating")
  "Needs Attention"
}

rfm_scores <- function(data) {
  r <- 6L - rank_quintile(data$recency_days)
  f <- rank_quintile(data$frequency_12m)
  m <- rank_quintile(data$monetary_12m)
  data.frame(
    R = r, F = f, M = m,
    rfm_segment = mapply(rfm_label, r, f, m, USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}
