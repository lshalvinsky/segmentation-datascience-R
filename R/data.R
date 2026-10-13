REGIONS <- c("Northeast", "Southeast", "Midwest", "Southwest", "West")
CHANNELS <- c("store", "web", "app")
CATEGORIES <- c("electronics", "apparel", "home", "beauty")

PERSONAS <- list(
  "Loyal High-Value" = list(
    share = 0.15, age = c(48, 9), income = 95, tenure = 60, recency = 15,
    freq = 22, aov = 140, discount = c(2, 10), web = 8, app = 6,
    email = c(8, 4), social = 2, nps = 8.5, channel = c(0.30, 0.50, 0.20),
    category = c(3, 2, 3, 2)
  ),
  "Bargain Hunters" = list(
    share = 0.25, age = c(38, 11), income = 52, tenure = 30, recency = 25,
    freq = 18, aov = 35, discount = c(8, 3), web = 12, app = 4,
    email = c(5, 5), social = 3, nps = 6.5, channel = c(0.40, 0.50, 0.10),
    category = c(2, 4, 3, 3)
  ),
  "Digital Natives" = list(
    share = 0.22, age = c(26, 5), income = 48, tenure = 14, recency = 30,
    freq = 10, aov = 55, discount = c(4, 6), web = 6, app = 18,
    email = c(2, 8), social = 12, nps = 7.0, channel = c(0.05, 0.25, 0.70),
    category = c(4, 4, 1, 3)
  ),
  "Occasional Big Spenders" = list(
    share = 0.13, age = c(45, 10), income = 120, tenure = 40, recency = 90,
    freq = 3, aov = 320, discount = c(1, 12), web = 3, app = 1,
    email = c(3, 7), social = 1, nps = 7.0, channel = c(0.60, 0.35, 0.05),
    category = c(5, 1, 4, 1)
  ),
  "At-Risk Lapsed" = list(
    share = 0.25, age = c(41, 13), income = 60, tenure = 36, recency = 220,
    freq = 2, aov = 60, discount = c(3, 6), web = 1, app = 0.5,
    email = c(1, 9), social = 0.5, nps = 4.5, channel = c(0.50, 0.40, 0.10),
    category = c(2, 3, 3, 2)
  )
)

RESPONSE_BASE <- c(
  "Loyal High-Value" = -0.8,
  "Bargain Hunters" = -0.6,
  "Digital Natives" = -1.4,
  "Occasional Big Spenders" = -1.8,
  "At-Risk Lapsed" = -3.0
)

draw_counts <- function(mean, n, shape = 1.5) {
  stats::rpois(n, mean * stats::rgamma(n, shape = shape, scale = 1 / shape))
}

draw_beta <- function(ab, n, concentration = 0.5) {
  stats::rbeta(n, ab[[1L]] * concentration, ab[[2L]] * concentration)
}

persona_block <- function(name, p, n) {
  age <- round(pmax(18, pmin(85, stats::rnorm(n, p$age[[1L]], p$age[[2L]]))))
  income <- round(stats::rlnorm(n, log(p$income), 0.4), 1)
  tenure <- round(pmax(1, pmin(180, stats::rgamma(n, shape = 4, scale = p$tenure / 4))))
  recency <- round(pmax(0, pmin(730, stats::rexp(n, rate = 1 / p$recency))))
  frequency <- 1 + draw_counts(max(p$freq - 1, 0.1), n)
  monetary <- round(frequency * stats::rlnorm(n, log(p$aov), 0.55) *
                      stats::rlnorm(n, 0, 0.1), 2)
  email_open <- draw_beta(p$email, n)
  category_weights <- matrix(
    stats::rgamma(n * length(CATEGORIES), shape = rep(p$category, each = n)),
    nrow = n
  )
  category_shares <- category_weights / rowSums(category_weights)

  out <- data.frame(
    age = as.integer(age),
    gender = sample(c("F", "M", "Other"), n, replace = TRUE, prob = c(0.52, 0.46, 0.02)),
    region = sample(REGIONS, n, replace = TRUE),
    income_k = income,
    tenure_months = as.integer(tenure),
    recency_days = as.integer(recency),
    frequency_12m = frequency,
    monetary_12m = monetary,
    avg_order_value = round(monetary / frequency, 2),
    discount_usage_rate = round(draw_beta(p$discount, n), 3),
    web_sessions_month = draw_counts(p$web, n),
    app_sessions_month = draw_counts(p$app, n),
    email_open_rate = round(email_open, 3),
    email_click_rate = round(email_open * stats::rbeta(n, 2, 6), 3),
    social_engagement = draw_counts(p$social, n),
    support_tickets_12m = stats::rpois(n, 1),
    nps_score = as.integer(round(pmax(0, pmin(10, stats::rnorm(n, p$nps, 1.8))))),
    preferred_channel = sample(CHANNELS, n, replace = TRUE, prob = p$channel),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(CATEGORIES)) {
    out[[paste0("share_", CATEGORIES[[i]])]] <- round(category_shares[, i], 3)
  }
  out$true_persona <- name
  out
}

generate_customers <- function(n = 8000L, seed = 42L) {
  stopifnot(length(n) == 1L, n >= 1L, n == as.integer(n))
  set.seed(seed)
  persona_names <- names(PERSONAS)
  counts <- as.vector(stats::rmultinom(1, n, vapply(PERSONAS, `[[`, numeric(1), "share")))
  active <- counts > 0L
  pieces <- Map(function(name, count) persona_block(name, PERSONAS[[name]], count),
                persona_names[active], counts[active])
  customers <- do.call(rbind, pieces)
  customers <- customers[sample.int(nrow(customers)), , drop = FALSE]
  rownames(customers) <- NULL
  customers$customer_id <- sprintf("C%06d", seq_len(nrow(customers)))
  customers <- customers[c("customer_id", setdiff(names(customers), "customer_id"))]

  logit <- unname(RESPONSE_BASE[customers$true_persona]) +
    2.5 * (customers$email_open_rate - 0.4) +
    1.2 * (customers$discount_usage_rate - 0.4) -
    0.004 * (customers$recency_days - 60) +
    0.15 * (customers$nps_score - 6) + stats::rnorm(nrow(customers), 0, 0.5)
  customers$responded_campaign <- stats::rbinom(nrow(customers), 1, stats::plogis(logit))

  churn_logit <- -2.2 + 0.012 * (customers$recency_days - 60) -
    0.35 * (customers$nps_score - 6) + 0.25 * customers$support_tickets_12m -
    0.02 * customers$tenure_months + stats::rnorm(nrow(customers), 0, 0.5)
  customers$churned_6m <- stats::rbinom(nrow(customers), 1, stats::plogis(churn_logit))
  customers
}

save_customers <- function(customers, path = file.path("data", "raw", "customers.csv")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(customers, path, row.names = FALSE)
  invisible(path)
}

load_customers <- function(path = file.path("data", "raw", "customers.csv")) {
  if (!file.exists(path)) {
    save_customers(generate_customers(), path)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}
