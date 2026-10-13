SEED <- 42L

stratified_split <- function(y, test_size, seed = SEED) {
  set.seed(seed)
  groups <- split(seq_along(y), y)
  test <- unlist(lapply(groups, function(idx) {
    size <- max(1L, floor(length(idx) * test_size))
    sample(idx, size = min(size, length(idx) - 1L))
  }), use.names = FALSE)
  list(train = setdiff(seq_along(y), test), test = test)
}

classification_report <- function(truth, predicted) {
  levels <- sort(unique(c(as.character(truth), as.character(predicted))))
  rows <- lapply(levels, function(label) {
    tp <- sum(truth == label & predicted == label)
    fp <- sum(truth != label & predicted == label)
    fn <- sum(truth == label & predicted != label)
    precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
    recall <- if (tp + fn == 0) 0 else tp / (tp + fn)
    f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
    data.frame(class = label, precision = precision, recall = recall, f1 = f1,
               support = sum(truth == label))
  })
  do.call(rbind, rows)
}

train_segment_classifier <- function(data, segment_col = "segment") {
  x <- data[c(NUMERIC_FEATURES, CATEGORICAL_FEATURES)]
  y <- factor(data[[segment_col]])
  split <- stratified_split(y, 0.25)
  model <- ranger::ranger(
    x = x[split$train, , drop = FALSE], y = y[split$train],
    num.trees = 300, min.node.size = 3, importance = "permutation",
    seed = SEED
  )
  predicted <- predict(model, x[split$test, , drop = FALSE])$predictions
  importance <- sort(model$variable.importance, decreasing = TRUE)
  list(
    model = model,
    report = classification_report(y[split$test], predicted),
    importance = importance,
    test_indices = split$test
  )
}

response_design <- function(data, factor_levels = NULL) {
  features <- data[c(NUMERIC_FEATURES, CATEGORICAL_FEATURES, "segment")]
  if (!is.null(factor_levels)) {
    for (name in names(factor_levels)) {
      features[[name]] <- factor(features[[name]], levels = factor_levels[[name]])
    }
  } else {
    features[CATEGORICAL_FEATURES] <- lapply(features[CATEGORICAL_FEATURES], factor)
    features$segment <- factor(features$segment)
  }
  stats::model.matrix(~ ., data = features)
}

train_response_models <- function(data, target = "responded_campaign",
                                  segment_col = "segment") {
  if (segment_col != "segment") {
    data$segment <- data[[segment_col]]
  }
  factor_levels <- lapply(data[c(CATEGORICAL_FEATURES, "segment")], function(x) {
    sort(unique(as.character(x)))
  })
  design <- response_design(data, factor_levels)
  split <- stratified_split(data[[target]], 0.30)
  x_train <- design[split$train, , drop = FALSE]
  x_test <- design[split$test, , drop = FALSE]
  numeric_columns <- intersect(NUMERIC_FEATURES, colnames(design))
  centers <- colMeans(x_train[, numeric_columns, drop = FALSE])
  scales <- apply(x_train[, numeric_columns, drop = FALSE], 2, stats::sd)
  scales[!is.finite(scales) | scales == 0] <- 1
  x_train[, numeric_columns] <- scale(x_train[, numeric_columns, drop = FALSE],
                                      center = centers, scale = scales)
  x_test[, numeric_columns] <- scale(x_test[, numeric_columns, drop = FALSE],
                                     center = centers, scale = scales)

  y_train <- data[[target]][split$train]
  y_test <- data[[target]][split$test]
  logistic <- stats::glm.fit(x_train, y_train, family = stats::binomial())
  logistic_scores <- stats::plogis(drop(x_test %*% logistic$coefficients))
  boosting <- gbm::gbm.fit(
    x = x_train, y = y_train, distribution = "bernoulli", n.trees = 300,
    interaction.depth = 3, shrinkage = 0.05, n.minobsinnode = 10,
    verbose = FALSE
  )
  boosting_scores <- as.numeric(stats::predict(
    boosting, newdata = x_test, n.trees = 300, type = "response"
  ))
  models <- list(
    "Logistic regression" = list(model = logistic, scores = logistic_scores),
    "Gradient boosting" = list(model = boosting, scores = boosting_scores)
  )
  for (name in names(models)) {
    roc <- pROC::roc(y_test, models[[name]]$scores, quiet = TRUE, direction = "<")
    models[[name]]$auc <- as.numeric(pROC::auc(roc))
    models[[name]]$fpr <- 1 - roc$specificities
    models[[name]]$tpr <- roc$sensitivities
  }
  list(
    results = models, x_test = x_test, y_test = y_test,
    factor_levels = factor_levels, centers = centers, scales = scales,
    numeric_columns = numeric_columns, design_columns = colnames(design),
    target = target
  )
}

predict_response <- function(fit, data) {
  new_data <- data
  if (!"segment" %in% names(new_data)) stop("Input data must include a segment column.")
  design <- response_design(new_data, fit$factor_levels)
  missing <- setdiff(fit$design_columns, colnames(design))
  extra <- setdiff(colnames(design), fit$design_columns)
  if (length(missing) || length(extra)) {
    stop("Prediction data does not match the response model feature columns.")
  }
  design <- design[, fit$design_columns, drop = FALSE]
  design[, fit$numeric_columns] <- scale(
    design[, fit$numeric_columns, drop = FALSE],
    center = fit$centers, scale = fit$scales
  )
  logistic <- stats::plogis(drop(design %*% fit$results[["Logistic regression"]]$model$coefficients))
  boosting <- as.numeric(stats::predict(
    fit$results[["Gradient boosting"]]$model, newdata = design,
    n.trees = 300, type = "response"
  ))
  list("Logistic regression" = logistic, "Gradient boosting" = boosting)
}

gains_table <- function(y_true, scores, n_bins = 10L) {
  if (length(y_true) != length(scores) || length(y_true) == 0L) {
    stop("y_true and scores must have the same non-zero length.")
  }
  order_idx <- order(scores, decreasing = TRUE)
  n <- length(y_true)
  decile <- floor((seq_len(n) - 1) * n_bins / n) + 1L
  ranked <- data.frame(y = y_true[order_idx], decile = decile)
  groups <- split(ranked, ranked$decile)
  out <- do.call(rbind, lapply(groups, function(group) {
    data.frame(customers = nrow(group), responders = sum(group$y))
  }))
  out$decile <- as.integer(rownames(out))
  out$response_rate <- out$responders / out$customers
  out$lift <- out$response_rate / mean(y_true)
  out$cum_pct_customers <- cumsum(out$customers) / n
  total_responders <- sum(y_true)
  out$cum_pct_responders <- if (total_responders == 0) 0 else
    cumsum(out$responders) / total_responders
  rownames(out) <- NULL
  out
}
