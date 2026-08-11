# xG model training and scoring helpers for the Soccer Shot Tracking app.

required_xg_packages <- c("dplyr", "caret", "pROC", "randomForest", "glmnet", "rpart", "nnet", "xgboost")

standardize_xg_features <- function(data) {
  data <- as.data.frame(data)
  data$X <- suppressWarnings(as.numeric(data$X))
  data$Y <- suppressWarnings(as.numeric(data$Y))
  goal_x <- ifelse(is.na(data$X) | data$X <= 60, 0, 120)
  goal_y <- 37.5
  dx <- abs(data$X - goal_x)
  dy <- abs(data$Y - goal_y)
  data$distance_to_goal <- sqrt(dx^2 + dy^2)
  post_offset <- 4
  data$angle_to_goal <- abs(atan2(goal_y + post_offset - data$Y, dx) - atan2(goal_y - post_offset - data$Y, dx))
  data$angle_to_goal[!is.finite(data$angle_to_goal)] <- NA_real_
  data$shotType <- factor(data$shotType)
  data$typeOfAttack <- factor(data$typeOfAttack)
  data$typeOfAssist <- factor(data$typeOfAssist)
  data$sideOfAttack <- factor(data$sideOfAttack)
  if ("domVSnondom" %in% names(data)) {
    data$domVSnondom <- ifelse(data$shotType == "HEADER", NA, data$domVSnondom)
    data$domVSnondom <- factor(data$domVSnondom)
  }
  data$goal <- ifelse(data$Result == "GOAL" | (!is.na(data$goals.x) & data$goals.x == 1), 1, 0)
  data
}

xg_model_terms <- c("distance_to_goal", "angle_to_goal", "shotType", "typeOfAttack", "typeOfAssist", "sideOfAttack", "domVSnondom")

xg_formula <- function(extra_terms = character()) {
  stats::as.formula(paste("goal ~", paste(c(xg_model_terms, extra_terms), collapse = " + ")))
}

prepare_xg_training_data <- function(data, extra_terms = character()) {
  keep_cols <- intersect(c("goal", xg_model_terms, extra_terms), names(data))
  data <- data[stats::complete.cases(data[, keep_cols]), ]
  data$goal <- factor(ifelse(data$goal == 1, "goal", "no_goal"), levels = c("no_goal", "goal"))
  data
}

# Random Forest Grid: 10 distinct options for the 'mtry' parameter (features randomly sampled at each split)
rf_grid <- expand.grid(mtry = seq(1, 10, by = 1))

# XGBoost Grid: 10 combinations of learning rate, max depth, and iterations
xgb_grid <- expand.grid(
  nrounds = c(100, 150),
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.05),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)
# Trim to exactly 10 options if desired, or let caret run through all 12 combinations from the vectors above.
xgb_grid <- xgb_grid[1:10, ]

train_xg_model <- function(data, folds = 5, seed = 42, model_path = "xg_model.rds") {
  missing <- required_xg_packages[!vapply(required_xg_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) stop("Install packages for model training: ", paste(missing, collapse = ", "))
  
  set.seed(seed)
  model_data <- standardize_xg_features(data)
  
  model_data <- prepare_xg_training_data(model_data)
  formula <- xg_formula()
  
  ctrl <- caret::trainControl(method = "cv", number = folds, classProbs = TRUE, summaryFunction = caret::twoClassSummary, savePredictions = "final")
  
  # Try/catch blocks bypass failed models instead of halting execution
  candidates <- list(
    logistic = try(caret::train(formula, data = model_data, method = "glm", family = stats::binomial(), metric = "ROC", trControl = ctrl), silent = TRUE),
    elastic_net = try(caret::train(formula, data = model_data, method = "glmnet", metric = "ROC", trControl = ctrl), silent = TRUE),
    regression_tree = try(caret::train(formula, data = model_data, method = "rpart", metric = "ROC", trControl = ctrl), silent = TRUE),
    random_forest = try(caret::train(formula, data = model_data, method = "rf", metric = "ROC", trControl = ctrl, tuneGrid = rf_grid), silent = TRUE),
    xgboost = try(caret::train(formula, data = model_data, method = "xgbTree", metric = "ROC", trControl = ctrl, verbosity = 0, tuneGrid = xgb_grid), silent = TRUE),
    neural_net = try(caret::train(formula, data = model_data, method = "nnet", metric = "ROC", trControl = ctrl, trace = FALSE), silent = TRUE)
  )
  
  candidates <- candidates[!vapply(candidates, inherits, logical(1), "try-error")]
  if (length(candidates) == 0) return(NULL)
  
  scores <- vapply(candidates, function(model) max(model$results$ROC, na.rm = TRUE), numeric(1))
  best_name <- names(which.max(scores))
  artifact <- list(model = candidates[[best_name]], model_name = best_name, cv_scores = scores, trained_at = Sys.time(), features = all.vars(formula))
  saveRDS(artifact, model_path)
  artifact
}

train_psxg_model <- function(data, folds = 5, seed = 42, model_path = "psxg_model.rds") {
  missing <- required_xg_packages[!vapply(required_xg_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) stop("Install packages for model training: ", paste(missing, collapse = ", "))
  
  set.seed(seed)
  model_data <- standardize_xg_features(data)
  model_data$net_x <- suppressWarnings(as.numeric(model_data$net_x))
  model_data$net_y <- suppressWarnings(as.numeric(model_data$net_y))
  model_data <- model_data[!is.na(model_data$net_x) & !is.na(model_data$net_y), ]
  model_data <- prepare_xg_training_data(model_data, extra_terms = c("net_x", "net_y"))
  
  if (nrow(model_data) < 50) return(NULL)
  
  ctrl <- caret::trainControl(method = "cv", number = folds, classProbs = TRUE, summaryFunction = caret::twoClassSummary, savePredictions = "final")
  formula <- xg_formula(extra_terms = c("net_x", "net_y"))
  
  candidates <- list(
    logistic = try(caret::train(formula, data = model_data, method = "glm", family = stats::binomial(), metric = "ROC", trControl = ctrl), silent = TRUE),
    elastic_net = try(caret::train(formula, data = model_data, method = "glmnet", metric = "ROC", trControl = ctrl), silent = TRUE),
    regression_tree = try(caret::train(formula, data = model_data, method = "rpart", metric = "ROC", trControl = ctrl), silent = TRUE),
    random_forest = try(caret::train(formula, data = model_data, method = "rf", metric = "ROC", trControl = ctrl, tuneGrid = rf_grid), silent = TRUE),
    xgboost = try(caret::train(formula, data = model_data, method = "xgbTree", metric = "ROC", trControl = ctrl, verbosity = 0, tuneGrid = xgb_grid), silent = TRUE),
    neural_net = try(caret::train(formula, data = model_data, method = "nnet", metric = "ROC", trControl = ctrl, trace = FALSE), silent = TRUE)
  )
  
  candidates <- candidates[!vapply(candidates, inherits, logical(1), "try-error")]
  if (length(candidates) == 0) return(NULL)
  
  scores <- vapply(candidates, function(model) max(model$results$ROC, na.rm = TRUE), numeric(1))
  best_name <- names(which.max(scores))
  artifact <- list(model = candidates[[best_name]], model_name = best_name, cv_scores = scores, trained_at = Sys.time(), features = all.vars(formula))
  saveRDS(artifact, model_path)
  artifact
}

predict_xg <- function(data, model_path = "xg_model.rds", fallback = NULL) {
  features <- standardize_xg_features(data)
  if (file.exists(model_path)) {
    artifact <- readRDS(model_path)
    preds <- try(stats::predict(artifact$model, newdata = features, type = "prob")[, "goal"], silent = TRUE)
    if (!inherits(preds, "try-error")) return(pmax(0.01, pmin(0.99, as.numeric(preds))))
  }
  heuristic <- stats::plogis(-1.2 - 0.08 * features$distance_to_goal + 1.2 * features$angle_to_goal)
  if (!is.null(fallback)) heuristic <- ifelse(is.na(heuristic), fallback, heuristic)
  pmax(0.01, pmin(0.99, heuristic))
}

predict_psxg <- function(data, fallback = NULL, model_path = "psxg_model.rds") {
  net_x <- suppressWarnings(as.numeric(data$net_x))
  net_y <- suppressWarnings(as.numeric(data$net_y))
  result <- as.character(data$Result)
  goal_width <- get0("GOAL_WIDTH", ifnotfound = 24)
  goal_height <- get0("GOAL_HEIGHT", ifnotfound = 8)
  in_frame <- !is.na(net_x) & !is.na(net_y) &
    net_x >= -goal_width / 2 & net_x <= goal_width / 2 &
    net_y >= 0 & net_y <= goal_height
  
  center_distance <- sqrt((net_x / (goal_width / 2))^2 + ((net_y - goal_height / 2) / (goal_height / 2))^2)
  
  if (file.exists(model_path)) {
    artifact <- readRDS(model_path)
    features <- standardize_xg_features(data)
    features$net_x <- net_x
    features$net_y <- net_y
    preds <- try(stats::predict(artifact$model, newdata = features, type = "prob")[, "goal"], silent = TRUE)
    if (!inherits(preds, "try-error")) {
      psxg <- as.numeric(preds)
      psxg <- ifelse(in_frame, psxg, 0.01)
      psxg <- ifelse(result == "GOAL" & is.na(net_x), 0.75, psxg)
      psxg <- ifelse(result == "SAVED" & is.na(net_x), 0.25, psxg)
      psxg <- ifelse(result %in% c("MISSED", "BLOCKED") & is.na(net_x), 0.01, psxg)
      if (!is.null(fallback)) psxg <- ifelse(is.na(psxg), fallback, psxg)
      return(pmax(0.01, pmin(0.99, psxg)))
    }
  }
  
  psxg <- stats::plogis(-1.8 + 1.2 * center_distance) 
  psxg <- ifelse(in_frame, psxg, 0.01)
  psxg <- ifelse(result == "GOAL" & is.na(net_x), 0.75, psxg)
  psxg <- ifelse(result == "SAVED" & is.na(net_x), 0.25, psxg)
  psxg <- ifelse(result %in% c("MISSED", "BLOCKED") & is.na(net_x), 0.01, psxg)
  if (!is.null(fallback)) psxg <- ifelse(is.na(psxg), fallback, psxg)
  pmax(0.01, pmin(0.99, psxg))
}

retrain_xg_if_new_shots <- function(data_path = "XGStats.csv", model_path = "xg_model.rds", psxg_model_path = "psxg_model.rds", state_path = "xg_model_state.rds") {
  
  # Ensure the time is between 3:00 AM and 3:59 AM EST
  current_time_est <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  if (current_time_est$hour != 3) {
    return(FALSE)
  }
  
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr to load CSV data.")
  shots <- readr::read_csv(data_path, show_col_types = FALSE)
  current_count <- nrow(shots)
  shots_with_net <- shots[!is.na(suppressWarnings(as.numeric(shots$net_x))) & !is.na(suppressWarnings(as.numeric(shots$net_y))), ]
  current_net_count <- nrow(shots_with_net)
  
  state <- if (file.exists(state_path)) readRDS(state_path) else list(shot_count = 0, net_shot_count = 0)
  previous_count <- if (!is.null(state$shot_count)) state$shot_count else 0
  
  if (current_count >= previous_count + 50 && current_net_count >= 50) {
    
    # Define temporary file paths for atomic saves
    temp_model_path <- paste0("temp_", model_path)
    temp_psxg_path <- paste0("temp_", psxg_model_path)
    
    # Train standard xG model (ensure train_xg_model passes the custom grids and saves to the temp path)
    train_xg_model(shots, model_path = temp_model_path)
    if (file.exists(temp_model_path)) {
      file.rename(temp_model_path, model_path)
    }
    
    # Train PSxG model from shots that have net coordinates.
    train_psxg_model(shots_with_net, model_path = temp_psxg_path)
    if (file.exists(temp_psxg_path)) {
      file.rename(temp_psxg_path, psxg_model_path)
    }
    
    # Update state file atomically
    temp_state_path <- paste0("temp_", state_path)
    saveRDS(list(shot_count = current_count, net_shot_count = current_net_count, trained_at = Sys.time()), temp_state_path)
    if (file.exists(temp_state_path)) {
      file.rename(temp_state_path, state_path)
    }
    
    return(TRUE)
  }
  return(FALSE)
}