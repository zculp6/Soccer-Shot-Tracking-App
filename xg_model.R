# xG model training and scoring helpers for the Soccer Shot Tracking app.

required_xg_packages <- c("dplyr", "caret", "pROC", "randomForest")

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
  data$domVSnondom <- ifelse(data$shotType == "HEADER", NA, data$domVSnondom)
  data$domVSnondom <- factor(data$domVSnondom)
  data$goal <- ifelse(data$Result == "GOAL" | data$goals.x == 1, 1, 0)
  data
}

train_xg_model <- function(data, folds = 5, seed = 42, model_path = "xg_model.rds") {
  missing <- required_xg_packages[!vapply(required_xg_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) stop("Install packages for model training: ", paste(missing, collapse = ", "))
  set.seed(seed)
  model_data <- standardize_xg_features(data)
  model_data <- model_data[stats::complete.cases(model_data[, c("goal", "distance_to_goal", "angle_to_goal")]), ]
  model_data$goal <- factor(ifelse(model_data$goal == 1, "goal", "no_goal"), levels = c("no_goal", "goal"))
  ctrl <- caret::trainControl(method = "cv", number = folds, classProbs = TRUE, summaryFunction = caret::twoClassSummary, savePredictions = "final")
  formula <- goal ~ distance_to_goal + angle_to_goal + shotType + typeOfAttack + typeOfAssist + sideOfAttack + domVSnondom
  candidates <- list(
    logistic = caret::train(formula, data = model_data, method = "glm", family = stats::binomial(), metric = "ROC", trControl = ctrl),
    random_forest = caret::train(formula, data = model_data, method = "rf", metric = "ROC", trControl = ctrl)
  )
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
  heuristic <- stats::plogis(1.1 - 0.09 * features$distance_to_goal + 1.7 * features$angle_to_goal)
  if (!is.null(fallback)) heuristic <- ifelse(is.na(heuristic), fallback, heuristic)
  pmax(0.01, pmin(0.99, heuristic))
}

predict_psxg <- function(data, fallback = NULL) {
  net_x <- suppressWarnings(as.numeric(data$net_x))
  net_y <- suppressWarnings(as.numeric(data$net_y))
  result <- as.character(data$Result)
  goal_width <- get0("GOAL_WIDTH", ifnotfound = 24)
  goal_height <- get0("GOAL_HEIGHT", ifnotfound = 8)
  in_frame <- !is.na(net_x) & !is.na(net_y) &
    net_x >= -goal_width / 2 & net_x <= goal_width / 2 &
    net_y >= 0 & net_y <= goal_height
  center_distance <- sqrt((net_x / (goal_width / 2))^2 + ((net_y - goal_height / 2) / (goal_height / 2))^2)
  psxg <- stats::plogis(1.6 - 1.4 * center_distance)
  psxg <- ifelse(in_frame, psxg, 0.01)
  psxg <- ifelse(result == "GOAL" & is.na(net_x), 0.75, psxg)
  psxg <- ifelse(result == "SAVED" & is.na(net_x), 0.25, psxg)
  psxg <- ifelse(result %in% c("MISSED", "BLOCKED") & is.na(net_x), 0.01, psxg)
  if (!is.null(fallback)) psxg <- ifelse(is.na(psxg), fallback, psxg)
  pmax(0.01, pmin(0.99, psxg))
}

run_advanced_tests <- function(data) {
  data <- standardize_xg_features(data)
  data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
  tests <- list()
  clean_for_test <- function(dataset, column) {
    dataset[!is.na(dataset[[column]]) & dataset[[column]] != "" & dataset[[column]] != "N/A" & !is.na(dataset$goal), , drop = FALSE]
  }
  safe_aov <- function(formula, dataset) try(summary(stats::aov(formula, data = dataset)), silent = TRUE)
  safe_t <- function(formula, dataset) try(stats::t.test(formula, data = dataset), silent = TRUE)
  tests$assist_goal_anova <- safe_aov(goal ~ typeOfAssist, clean_for_test(data, "typeOfAssist"))
  tests$side_middle_goal_anova <- safe_aov(goal ~ side_group, clean_for_test(data, "side_group"))
  tests$left_right_middle_goal_anova <- safe_aov(goal ~ sideOfAttack, clean_for_test(data, "sideOfAttack"))
  attack_data <- clean_for_test(data, "typeOfAttack")
  tests$attack_type_goal_anova <- safe_aov(goal ~ typeOfAttack, attack_data[attack_data$typeOfAttack != "PENALTY", ])
  tests$shot_type_goal_anova <- safe_aov(goal ~ shotType, clean_for_test(data, "shotType"))
  foot_data <- clean_for_test(data, "domVSnondom")
  foot_data <- foot_data[foot_data$domVSnondom %in% c("DOM", "NONDOM"), ]
  tests$dominant_foot_t_test <- safe_t(goal ~ domVSnondom, foot_data)
  tests
}

retrain_xg_if_new_shots <- function(data_path = "XGStats.csv", model_path = "xg_model.rds", state_path = "xg_model_state.rds", hour_et = 3) {
  now_et <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  if (now_et$hour != hour_et) return(FALSE)
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr to load CSV data for scheduled retraining.")
  shots <- readr::read_csv(data_path, show_col_types = FALSE)
  current_count <- nrow(shots)
  previous_count <- if (file.exists(state_path)) readRDS(state_path)$shot_count else 0
  if (current_count <= previous_count) return(FALSE)
  artifact <- train_xg_model(shots, model_path = model_path)
  saveRDS(list(shot_count = current_count, trained_at = artifact$trained_at), state_path)
  TRUE
}