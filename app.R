# Soccer Shot Tracking Shiny App
# Run with: shiny::runApp()

required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "readr", "readxl", "DT", "tidyr", "digest", "scales", "tibble", "magick"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install required packages first: ", paste(missing_packages, collapse = ", "))
}

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(DT)
library(tidyr)

source("xg_model.R")

DATA_PATH <- "XGStats.csv"
USER_DATA_DIR <- "user_data"
FIELD_LENGTH <- 120
FIELD_WIDTH <- 75
GOAL_WIDTH <- 24
GOAL_HEIGHT <- 8
REQUIRED_UPLOAD_COLUMNS <- c(
  "Minute", "Result", "X", "Y", "player", "h_a", "situation", "shotType",
  "domVSnondom", "Opponent", "playerAssist", "typeOfAssist", "sideOfAttack", "PossesionWon",
  "typeOfAttack", "year", "sideOfAttackGrouped", "Team", "GK", "net_x", "net_y", "PSxG"
)
SHOT_RESULTS <- c("GOAL", "SAVED", "MISSED", "BLOCKED")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

clean_text <- function(x) {
  if (is.character(x)) toupper(trimws(x)) else x
}

anonymize_demo_data <- function(data) {
  data <- as.data.frame(data)
  data[] <- lapply(data, clean_text)
  if ("" %in% names(data)) data[[""]] <- NULL
  if ("...1" %in% names(data)) data[["...1"]] <- NULL
  data$Team <- ifelse(toupper(as.character(data$player %||% "")) == "OPP" | toupper(as.character(data$Team %||% "")) %in% c("OPPONENT", "OPP"), "OPPONENT", "TEAM")
  if ("Opponent" %in% names(data)) {
    opponents <- sort(unique(stats::na.omit(data$Opponent)))
    opponent_map <- setNames(paste("OPPONENT", seq_along(opponents)), opponents)
    data$Opponent <- unname(opponent_map[data$Opponent])
  }
  # Single block to handle both player and playerAssist at the same time
  if ("player" %in% names(data)) {
    all_players <- sort(unique(stats::na.omit(c(data$player, data$playerAssist))))
    player_map <- setNames(paste("PLAYER", seq_along(all_players)), all_players)
    data$player <- unname(player_map[data$player])
    if ("playerAssist" %in% names(data)) {
      data$playerAssist <- unname(player_map[data$playerAssist])
    }
  }
  data$Game <- paste(data$year %||% "UNKNOWN", data$Opponent %||% "OPPONENT", data$h_a %||% "", sep = " - ")
  data
}

load_demo_data <- function() {
  readr::read_csv(DATA_PATH, show_col_types = FALSE) |>
    anonymize_demo_data() |>
    standardize_shots()
}

user_profile_file <- function(email) {
  dir.create(USER_DATA_DIR, showWarnings = FALSE)
  file.path(USER_DATA_DIR, paste0(digest::digest(email, algo = "sha256"), "_profile.rds"))
}

default_user_profile <- function() {
  list(
    email = "demo.user@example.com", team_name = "My Team", level = "High School",
    team_code = "DEMO-TEAM", role = "Coach", show_player_data = TRUE, signed_in = FALSE
  )
}

load_user_profile <- function(email) {
  path <- user_profile_file(email)
  if (file.exists(path)) readRDS(path) else modifyList(default_user_profile(), list(email = email, signed_in = TRUE))
}

save_user_profile <- function(profile) {
  saveRDS(profile, user_profile_file(profile$email))
}

user_file <- function(email) {
  dir.create(USER_DATA_DIR, showWarnings = FALSE)
  file.path(USER_DATA_DIR, paste0(digest::digest(email, algo = "sha256"), ".csv"))
}

empty_shots_frame <- function() {
  df <- as.data.frame(matrix(nrow = 0, ncol = length(REQUIRED_UPLOAD_COLUMNS)))
  names(df) <- REQUIRED_UPLOAD_COLUMNS
  df$datetime_added <- character()
  df
}

load_user_data <- function(email) {
  if (email == default_user_profile()$email) {
    return(load_demo_data())
  }
  
  path <- user_file(email)
  if (!file.exists(path)) return(empty_shots_frame())
  readr::read_csv(path, show_col_types = FALSE) |> standardize_shots()
}

save_user_data <- function(data, email, signed_in = TRUE) {
  if (!isTRUE(signed_in)) {
    # If using demo environment, write to XGStats.csv
    readr::write_csv(data, DATA_PATH, na = "") 
  } else {
    # If signed in, write to specific user file
    readr::write_csv(data, user_file(email), na = "")
  }
}

standardize_shots <- function(data) {
  data <- as.data.frame(data)
  for (col in REQUIRED_UPLOAD_COLUMNS) if (!col %in% names(data)) data[[col]] <- NA
  data[] <- lapply(data, clean_text)
  data$Minute <- suppressWarnings(as.numeric(data$Minute))
  data$X <- suppressWarnings(as.numeric(data$X))
  data$Y <- suppressWarnings(as.numeric(data$Y))
  data$net_x <- suppressWarnings(as.numeric(data$net_x))
  data$net_y <- suppressWarnings(as.numeric(data$net_y))
  data$year <- suppressWarnings(as.integer(data$year))
  data$goals.x <- ifelse(data$Result == "GOAL", 1, suppressWarnings(as.numeric(data$goals.x %||% 0)))
  data$sideOfAttackGrouped <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", data$sideOfAttack)
  data$goals.y <- predict_xg(data, fallback = suppressWarnings(as.numeric(data$goals.y)))
  data$PSxG <- predict_psxg(data, fallback = suppressWarnings(as.numeric(data$PSxG)))
  data$Team <- ifelse(toupper(as.character(data$Team %||% "")) %in% c("OPPONENT", "OPP") | toupper(as.character(data$player %||% "")) == "OPP", "OPPONENT", "TEAM")
  data$GK <- ifelse(is.na(data$GK) | data$GK == "", NA_character_, as.character(data$GK))
  if (!"datetime_added" %in% names(data)) data$datetime_added <- NA_character_
  data$datetime_added <- as.character(data$datetime_added)
  data
}

round_shot_display <- function(data) {
  data |> mutate(across(any_of(c("Minute", "X", "Y", "net_x", "net_y", "goals.x", "goals.y", "PSxG", "xG", "xG_Against", "PSxG_Against", "Difference", "Goal_Percentage")), ~ {
    if (is.numeric(.x)) round(.x, 2) else .x
  }))
}

soccer_pitch <- function(data = NULL, aes_extra = aes()) {
  ggplot(data, aes_extra) +
    annotate("rect", xmin = 0, xmax = FIELD_LENGTH, ymin = 0, ymax = FIELD_WIDTH, fill = "#4f9f50", colour = "white") +
    annotate("segment", x = FIELD_LENGTH / 2, xend = FIELD_LENGTH / 2, y = 0, yend = FIELD_WIDTH, colour = "white") +
    annotate("path", x = FIELD_LENGTH / 2 + 10 * cos(seq(0, 2 * pi, length.out = 100)), y = FIELD_WIDTH / 2 + 10 * sin(seq(0, 2 * pi, length.out = 100)), colour = "white") +
    annotate("rect", xmin = 0, xmax = 6, ymin = 26.5, ymax = 48.5, fill = NA, colour = "white") +
    annotate("rect", xmin = 114, xmax = 120, ymin = 26.5, ymax = 48.5, fill = NA, colour = "white") +
    annotate("rect", xmin = 0, xmax = 18, ymin = 15.5, ymax = 59.5, fill = NA, colour = "white") +
    annotate("rect", xmin = 102, xmax = 120, ymin = 15.5, ymax = 59.5, fill = NA, colour = "white") +
    annotate("point", x = c(12, 108, 60), y = c(FIELD_WIDTH / 2, FIELD_WIDTH / 2, FIELD_WIDTH / 2), colour = "white") +
    coord_fixed(xlim = c(0, FIELD_LENGTH), ylim = c(0, FIELD_WIDTH), expand = FALSE) +
    theme_void()
}

filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("game"), "Game", choices = NULL, multiple = TRUE),
    selectInput(ns("year"), "Year", choices = NULL, multiple = TRUE),
    selectInput(ns("player"), "Player", choices = NULL, multiple = TRUE),
    selectInput(ns("shotType"), "Shot type", choices = NULL, multiple = TRUE),
    selectInput(ns("assist"), "Type of assist", choices = NULL, multiple = TRUE),
    selectInput(ns("attack"), "Type of attack", choices = NULL, multiple = TRUE)
  )
}

apply_filters <- function(data, input, id) {
  for (field in c("game", "year", "player", "shotType", "assist", "attack")) {
    value <- input[[paste0(id, "-", field)]]
    if (!is.null(value) && length(value) > 0) {
      column <- c(game = "Game", year = "year", player = "player", shotType = "shotType", assist = "typeOfAssist", attack = "typeOfAttack")[[field]]
      data <- data[data[[column]] %in% value, , drop = FALSE]
    }
  }
  data
}

round_xg_columns <- function(data) {
  data |> mutate(across(where(is.numeric), ~ round(.x, 2)))
}

descriptive_stats <- function(data, column, label) {
  data |>
    mutate(value = ifelse(is.na(.data[[column]]) | .data[[column]] == "" | .data[[column]] == "N/A", "UNKNOWN", .data[[column]])) |>
    count(value, name = "Shots") |>
    mutate(Category = label, Percent = round(100 * Shots / sum(Shots), 2)) |>
    select(Category, Type = value, Shots, Percent)
}

analytics_specs <- tibble::tribble(
  ~id, ~label, ~column, ~test_type,
  "assist_goal_anova", "Assist type vs goal percentage", "typeOfAssist", "ANOVA",
  "shot_type_goal_anova", "Shot type vs goal percentage", "shotType", "ANOVA",
  "attack_type_goal_anova", "Attack type vs goal percentage", "typeOfAttack", "ANOVA",
  "side_middle_goal_anova", "Side vs middle attack goal percentage", "side_group", "ANOVA",
  "left_right_middle_goal_anova", "Left/right/middle attack goal percentage", "sideOfAttack", "ANOVA",
  "dominant_foot_t_test", "Dominant vs non-dominant foot goal percentage", "domVSnondom", "t-test"
)

extract_p_value <- function(test_result) {
  if (inherits(test_result, "try-error") || is.null(test_result)) return(NA_real_)
  if (inherits(test_result, "htest")) return(test_result$p.value)
  if (is.list(test_result) && length(test_result) > 0 && is.data.frame(test_result[[1]])) {
    p_col <- grep("Pr\\(>F\\)", names(test_result[[1]]), value = TRUE)
    if (length(p_col) > 0) return(suppressWarnings(as.numeric(test_result[[1]][[p_col[1]]][1])))
  }
  NA_real_
}

pairwise_comparisons <- function(clean_data, column) {
  empty <- tibble(Comparison = character(), group1 = character(), group2 = character(), p_adj = numeric(), label = character(), y = numeric())
  groups <- unique(as.character(clean_data[[column]]))
  groups <- groups[!is.na(groups) & !groups %in% c("", "N/A", "NA", "UNKNOWN")]
  if (length(groups) < 2) return(empty)
  fit <- try(stats::aov(stats::as.formula(paste("goal ~", column)), data = clean_data), silent = TRUE)
  if (inherits(fit, "try-error")) return(empty)
  tk <- try(stats::TukeyHSD(fit), silent = TRUE)
  if (inherits(tk, "try-error") || length(tk) == 0) return(empty)
  tk_df <- as.data.frame(tk[[1]])
  tk_df$Comparison <- rownames(tk_df)
  group_levels <- as.character(groups)
  parse_pair <- function(comp) {
    for (g1 in group_levels) {
      prefix <- paste0(g1, "-")
      if (startsWith(comp, prefix)) {
        g2 <- substring(comp, nchar(prefix) + 1)
        if (g2 %in% group_levels) return(c(g1, g2))
      }
    }
    # Tukey stores as level2-level1; also try reverse scan
    for (g2 in group_levels) {
      suffix <- paste0("-", g2)
      if (endsWith(comp, suffix)) {
        g1 <- substring(comp, 1, nchar(comp) - nchar(suffix))
        if (g1 %in% group_levels) return(c(g1, g2))
      }
    }
    c(NA_character_, NA_character_)
  }
  parsed <- t(vapply(tk_df$Comparison, parse_pair, character(2)))
  tk_df$group1 <- parsed[, 1]
  tk_df$group2 <- parsed[, 2]
  rates <- clean_data |>
    group_by(Group = as.character(.data[[column]])) |>
    summarise(rate = mean(goal, na.rm = TRUE), .groups = "drop")
  max_rate <- max(rates$rate, 0, na.rm = TRUE)
  tk_df |>
    filter(!is.na(group1), !is.na(group2)) |>
    transmute(
      Comparison,
      group1,
      group2,
      p_adj = `p adj`,
      label = ifelse(p_adj < 0.10, sprintf("p=%.2f*", p_adj), sprintf("p=%.2f", p_adj)),
      y = max_rate + 0.08 + 0.07 * (seq_len(n()) - 1)
    )
}

goal_rate_by_group <- function(data, column) {
  data |>
    filter(!is.na(.data[[column]]), !.data[[column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal)) |>
    group_by(Group = as.character(.data[[column]])) |>
    summarise(Shots = n(), Goals = sum(goal, na.rm = TRUE), Goal_Percentage = round(100 * mean(goal, na.rm = TRUE), 2), .groups = "drop") |>
    arrange(desc(Goal_Percentage))
}

advanced_summary <- function(data, is_defense = FALSE) {
  data <- standardize_xg_features(data)
  data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
  tests <- run_advanced_tests(data)
  bind_rows(lapply(seq_len(nrow(analytics_specs)), function(i) {
    spec <- analytics_specs[i, ]
    p_value <- extract_p_value(tests[[spec$id]])
    
    clean_data <- data |>
      filter(!is.na(.data[[spec$column]]), !.data[[spec$column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal))
    
    group_counts <- table(clean_data[[spec$column]])
    valid_groups <- names(group_counts)[group_counts >= 10]
    clean_data <- clean_data |> filter(.data[[spec$column]] %in% valid_groups)
    
    if (identical(spec$test_type, "t-test") && is.na(p_value) && nrow(clean_data) > 0) {
      if (length(unique(clean_data[[spec$column]])) == 2) {
        tt <- try(stats::t.test(stats::as.formula(paste("goal ~", spec$column)), data = clean_data), silent = TRUE)
        if (!inherits(tt, "try-error")) p_value <- tt$p.value
      } else {
        p_value <- NA_real_
      }
    }
    
    groups <- clean_data |>
      group_by(Group = as.character(.data[[spec$column]])) |>
      summarise(Shots = n(), Goal_Percentage = mean(goal, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(Goal_Percentage))
    
    if (is_defense) {
      best_rate <- if (nrow(groups) > 0) min(groups$Goal_Percentage, na.rm = TRUE) else NA_real_
      worst_rate <- if (nrow(groups) > 0) max(groups$Goal_Percentage, na.rm = TRUE) else NA_real_
    } else {
      best_rate <- if (nrow(groups) > 0) max(groups$Goal_Percentage, na.rm = TRUE) else NA_real_
      worst_rate <- if (nrow(groups) > 0) min(groups$Goal_Percentage, na.rm = TRUE) else NA_real_
    }
    
    best_groups <- if (nrow(groups) > 0) paste(groups$Group[groups$Goal_Percentage == best_rate], collapse = ", ") else "N/A"
    worst_groups <- if (nrow(groups) > 0) paste(groups$Group[groups$Goal_Percentage == worst_rate], collapse = ", ") else "N/A"
    
    significant <- !is.na(p_value) & p_value < 0.10
    takeaway <- if (significant) {
      sprintf("%s shows a statistically significant effect (p = %.2f). Best option(s): %s (%.1f%%). Worst option(s): %s (%.1f%%).",
              spec$label, p_value, best_groups, 100 * best_rate, worst_groups, 100 * worst_rate)
    } else {
      sprintf("No significant difference found for %s (p = %s). Conversion rates remained consistent across groups.",
              tolower(spec$label), ifelse(is.na(p_value), "N/A", sprintf("%.2f", p_value)))
    }
    
    tibble(
      Test = spec$label, Test_ID = spec$id, Type = spec$test_type,
      P_Value = p_value, Significant = significant, Top_Group = best_groups,
      Takeaway = takeaway
    )
  }))
}

coach_takeaway_ui <- function(data, perspective, is_defense = FALSE) {
  summary <- advanced_summary(data, is_defense)
  sig_findings <- summary |> filter(Significant)
  non_sig_findings <- summary |> filter(!Significant)
  
  tagList(
    tags$h5(paste(perspective, "- Significant Findings (p < 0.10)")),
    if (nrow(sig_findings) > 0) {
      tags$ul(lapply(seq_len(nrow(sig_findings)), function(i) tags$li(sig_findings$Takeaway[i])))
    } else {
      tags$p("No significant factors at p < 0.10 for current filters.")
    },
    tags$hr(),
    tags$h5(paste(perspective, "- No Significant Difference Found")),
    if (nrow(non_sig_findings) > 0) {
      tags$ul(lapply(seq_len(nrow(non_sig_findings)), function(i) tags$li(non_sig_findings$Takeaway[i])))
    } else {
      tags$p("All evaluated factors showed statistically significant differences.")
    }
  )
}

render_pie_chart <- function(df, var_col, var_label) {
  if (nrow(df) == 0) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "No data available for current selection") + theme_void())
  }
  chart_data <- df %>%
    mutate(value = ifelse(is.na(.data[[var_col]]) | .data[[var_col]] == "" | .data[[var_col]] == "N/A", "UNKNOWN", .data[[var_col]])) %>%
    count(value) %>%
    arrange(desc(n)) %>%
    mutate(
      pct = n / sum(n),
      legend_label = sprintf("%s: %d (%.1f%%)", value, n, pct * 100)
    )
  
  ggplot(chart_data, aes(x = "", y = n, fill = reorder(legend_label, n))) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    # Add labels directly to the pie slices
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "black", fontface = "bold") +
    theme_void() +
    # Move legend to the bottom to prevent UI overlap
    theme(legend.title = element_blank(), legend.text = element_text(size = 10),
          plot.margin = margin(12, 12, 12, 12), legend.position = "bottom") +
    guides(fill = guide_legend(title = NULL, ncol = 2)) +
    labs(title = paste("Shot Distribution by", var_label))
}

net_plot <- function(data = NULL, point = NULL) {
  v_lines <- seq(-GOAL_WIDTH / 2, GOAL_WIDTH / 2, length.out = 5)
  h_lines <- seq(0, GOAL_HEIGHT, length.out = 3)
  ggplot() +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 12, fill = "#0b4f6c", colour = NA, alpha = .95) +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 0, fill = "#56a832", colour = NA) +
    annotate("rect", xmin = -GOAL_WIDTH / 2, xmax = GOAL_WIDTH / 2, ymin = 0, ymax = GOAL_HEIGHT, fill = "#102a43", colour = NA, alpha = .55) +
    annotate("segment", x = v_lines, xend = v_lines, y = 0, yend = GOAL_HEIGHT, colour = "grey85", alpha = .65) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = h_lines, yend = h_lines, colour = "grey85", alpha = .65) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = -GOAL_WIDTH / 2, y = 0, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("segment", x = GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = 0, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = GOAL_HEIGHT, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("point", x = c(-GOAL_WIDTH / 2, GOAL_WIDTH / 2), y = c(0, 0), colour = "white", size = 5) +
    {if (!is.null(data) && nrow(data) > 1) stat_density_2d(data = data, aes(net_x, net_y, fill = after_stat(level)), geom = "polygon", alpha = .45) else NULL} +
    # Render non-goals as white dots
    {if (!is.null(data) && nrow(data) > 0) geom_point(data = filter(data, Result != "GOAL" & (is.na(goals.x) | goals.x != 1)), aes(net_x, net_y), colour = "white", alpha = .75, size = 2) else NULL} +
    # Render goals as green stars (shape = 8)
    {if (!is.null(data) && nrow(data) > 0) geom_point(data = filter(data, Result == "GOAL" | goals.x == 1), aes(net_x, net_y), colour = "#32CD32", shape = 8, size = 4, stroke = 1.2) else NULL} +
    {if (!is.null(data) && nrow(data) > 0) geom_point(data = data, aes(net_x, net_y), colour = "white", alpha = .75, size = 2) else NULL} +
    {if (!is.null(point) && !is.null(point$x) && !is.null(point$y) && !is.na(point$x) && !is.na(point$y)) {
      list(
        annotate("point", x = point$x, y = point$y, shape = 21, size = 7, fill = "white", colour = "black", stroke = 1.4),
        annotate("point", x = point$x, y = point$y, shape = 16, size = 4.5, colour = "black"),
        annotate("text", x = point$x, y = point$y, label = "⚽", size = 7, vjust = 0.35)
      )
    } else NULL} +
    coord_fixed(xlim = c(-18, 18), ylim = c(-4, 12), expand = FALSE) +
    theme_void() + labs(x = "Net width (yds)", y = "Net height (yds)")
}

net_zone <- function(x, y) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  col <- cut(x, breaks = seq(-GOAL_WIDTH / 2, GOAL_WIDTH / 2, length.out = 5), labels = 1:4, include.lowest = TRUE)
  row <- cut(y, breaks = seq(0, GOAL_HEIGHT, length.out = 3), labels = 1:2, include.lowest = TRUE)
  # Subtract 1 from row integer to correctly calculate zones 1 through 8
  ifelse(is.na(col) | is.na(row), NA_character_, paste("Zone", (as.integer(row) - 1) * 4 + as.integer(col)))
}

zone_summary <- function(data, selected = NULL, is_gk = FALSE) {
  # Ensure goal tracking translates correctly
  data$goal_val <- ifelse(data$Result == "GOAL" | (is.numeric(data$goals.x) & data$goals.x == 1), 1, 0)
  data <- data |> mutate(zone = net_zone(net_x, net_y)) |> filter(!is.na(zone))
  if (!is.null(selected) && length(selected) > 0) data <- data |> filter(zone %in% selected)
  
  zs <- data |> 
    group_by(zone) |> 
    summarise(Shots = n(), Goals = sum(goal_val, na.rm = TRUE), .groups = "drop") |> 
    complete(zone = paste("Zone", 1:8), fill = list(Shots = 0, Goals = 0))
  
  if (is_gk) {
    zs |> mutate(
      Percent = ifelse(Shots == 0, 0, round(100 * (Shots - Goals) / Shots, 0)),
      Label = sprintf("%.0f%%\n(%d GA, %d shots)", Percent, Goals, Shots)
    )
  } else {
    zs |> mutate(
      Percent = ifelse(Shots == 0, 0, round(100 * Goals / Shots, 0)),
      Label = sprintf("%.0f%%\n(%d goals, %d shots)", Percent, Goals, Shots)
    )
  }
}

zone_grid_plot <- function(data, selected = NULL, title = "Net location distribution", is_gk = FALSE) {
  zs <- zone_summary(data, selected, is_gk) |> 
    mutate(
      zone_num = as.integer(gsub("Zone ", "", zone)),
      col = ((zone_num - 1) %% 4) + 1,
      row = ifelse(zone_num <= 4, 1, 2),
      xmin = -12 + (col - 1) * 6,
      xmax = -12 + col * 6,
      ymin = (row - 1) * 4,
      ymax = row * 4,
      x_center = (xmin + xmax) / 2,
      y_center = (ymin + ymax) / 2
    )
  
  fill_label <- if(is_gk) "Save %" else "Goal %"
  
  ggplot(zs) + 
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 12, fill = "#0b4f6c", colour = NA, alpha = 0.95) +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 0, fill = "#56a832", colour = NA) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Percent), colour = "black", linewidth = 0.5) + 
    geom_text(aes(x = x_center, y = y_center, label = Label), colour = "white", fontface = "bold") + 
    geom_segment(aes(x = -12, xend = -12, y = 0, yend = 8), color = "white", linewidth = 2) + 
    geom_segment(aes(x = 12, xend = 12, y = 0, yend = 8), color = "white", linewidth = 2) + 
    geom_segment(aes(x = -12, xend = 12, y = 8, yend = 8), color = "white", linewidth = 2) + 
    # Use a red-to-green gradient bounded from 0 to 100
    scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 50, limits = c(0, 100), na.value = "grey50") + 
    coord_fixed(xlim = c(-18, 18), ylim = c(-4, 12), expand = FALSE) + 
    theme_void() + 
    labs(title = title, fill = fill_label)
}

ui <- page_navbar(
  title = "Soccer Shot Tracking",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel("Sign in / user info",
            h3(textOutput("auth_header")),
            layout_columns(
              card(card_header("Sign in"), textInput("login_email", "Email", value = "demo.user@example.com"), passwordInput("login_password", "Password"), helpText("Password hashing and SQL validation note: user credentials will be stored in a SQL table with SHA-256/bcrypt hashes."), actionButton("sign_in", "Sign in", class = "btn-primary"), actionButton("sign_out", "Sign out", class = "btn-secondary")),
              card(card_header("User info"), textOutput("user_info"), textInput("team_name", "Team name", value = "My Team"), selectInput("team_level", "Level", choices = c("Youth", "High School", "College/University", "Professional")), textInput("team_code", "Team code", value = "DEMO-TEAM"), radioButtons("user_role", "Access", choices = c("Coach", "Player"), inline = TRUE), checkboxInput("show_player_data", "Coach allows players to see player data", TRUE), actionButton("save_profile", "Save user info", class = "btn-success"))
            ),
            p("Players who sign in with the team code can review shared dashboards without seeing coach email. Player access hides add/upload shots and edit shot data; coaches can also hide player-level data.")),
  nav_panel("Basic stats", 
            # Changed title to "Filter"
            layout_sidebar(sidebar = sidebar(filter_ui("basic"), selectInput("basic_opponents", "Filter opponents (optional)", choices = NULL, multiple = TRUE), title = "Filter", open = FALSE),
                           h4(textOutput("signed_in_as")), p("xG stands for expected goals. PSxG stands for post-shot expected goals. Values are rounded to hundredths."),
                           navset_tab(
                             nav_panel("Team stats",
                                       # Added opponent view toggle radio buttons
                                       radioButtons("team_stat_view", "Opponent View:", choices = c("Total Opponent", "Individual Opponent Teams"), inline = TRUE),
                                       DTOutput("team_stats"),
                                       hr(),
                                       fluidRow(
                                         column(4, selectInput("team_pie_var", "Distribution Variable", choices = c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result")))
                                       ),
                                       plotOutput("team_pie", height = "45vh")),
                             nav_panel("Player stats",
                                       DTOutput("player_stats"),
                                       hr(),
                                       fluidRow(
                                         column(4, selectInput("player_pie_var", "Distribution Variable", choices = c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result")))
                                       ),
                                       plotOutput("player_pie", height = "45vh"))
                           ),
                           DTOutput("descriptive_stats"))),
  nav_panel("Advanced analytics", 
            # Changed title to "Filter"
            layout_sidebar(
              sidebar = sidebar(filter_ui("advanced"), title = "Filter", open = FALSE),
              p(em("* Note: Statistical tests and takeaways exclude factors with fewer than 10 shots (e.g., if a throw-in only has 3 shots, it is not evaluated).")),
              navset_tab(
                nav_panel("Offensive",
                          uiOutput("offensive_takeaways"), hr(),
                          selectInput("off_analytics_test", "Analytics test", choices = NULL),
                          fluidRow(
                            column(7, plotOutput("off_analytics_plot", width = "100%", height = "50vh")),
                            column(5, h5("Goal percentages"), DTOutput("off_goal_rates"))
                          )),
                nav_panel("Defensive",
                          uiOutput("defensive_takeaways"), hr(),
                          selectInput("def_analytics_test", "Analytics test", choices = NULL),
                          fluidRow(
                            column(7, plotOutput("def_analytics_plot", width = "100%", height = "50vh")),
                            column(5, h5("Goal percentages"), DTOutput("def_goal_rates"))
                          )),
                nav_panel("Shooting", 
                          actionButton("open_zone_filter", "Filter by Shot Location", icon = icon("crosshairs"), class = "btn-info"),
                          plotOutput("shooting_grid", height = "45vh"), plotOutput("shooting_heat", height = "45vh")),
                nav_panel("GK", 
                          actionButton("open_zone_filter", "Filter by Shot Location", icon = icon("crosshairs"), class = "btn-info"),
                          plotOutput("gk_grid", height = "45vh"), plotOutput("gk_heat", height = "45vh")),
                nav_panel("Penalties", 
                          selectInput("penalty_team_filter", "Select Team", choices = c("My Team", "Opponent")),
                          fluidRow(
                            column(4, h5("Penalty Goal %"), DTOutput("penalty_goal_rates")),
                            column(8, plotOutput("penalty_grid", height = "45vh"))
                          ),
                          hr(),
                          plotOutput("penalty_heat", height = "45vh"))
              )
            )
  ),
  nav_panel("Game stats", layout_sidebar(sidebar = sidebar(selectInput("single_game", "Individual game shot chart", choices = NULL), title = "Game", open = TRUE), plotOutput("shot_chart", click = "shot_chart_click", width = "100%", height = "60vh"), plotOutput("clicked_net", width = "100%", height = "35vh"), DTOutput("clicked_shot"), plotOutput("xg_timeline", width = "100%", height = "45vh"))),
  nav_panel("Add/upload shots", h4("Click shot location or upload data"), actionButton("open_upload", "Upload data", class = "btn-secondary"), plotOutput("field_click", click = "field_click", hover = hoverOpts("field_hover"), width = "100%", height = "70vh"), textOutput("field_point")),
  nav_panel("Edit shot data", p("Select a row to edit and double click the input to change it. Use Delete selected row if a shot should be removed."), actionButton("delete_shot", "Delete selected row", class = "btn-danger"), DTOutput("recent_shots")),
  
  # Contact Tab
  nav_panel("Contact", 
            fluidPage(
              h3("About the Author"),
              p("Hi! I'm Zach Culp. I'm recent graduate with a Master of Science in Applied Statistics from Loyola University Chicago, having previously earned a Bachelor of Science in Statistics from Ohio Northern University."),
              p("With a strong background in competitive soccer and experience as a youth coach, I combine my statistical expertise and passion for the sport to engineer analytics frameworks, predictive algorithms, and tracking dashboards."),
              p("I am available to do consulting work (paid or unpaid) for any team that would like additional analytics."),
              p("Feel free to contact me with any potential issues or concerns with the app."),
              hr(),
              h4("Contact Information"),
              p(strong("Email: "), a(href="mailto:zachculp6@gmail.com", "zachculp6@gmail.com")),
              p(strong("LinkedIn: "), a(href="https://www.linkedin.com/in/zculp6/", "https://www.linkedin.com/in/zculp6/"))
            )
  )
)

server <- function(input, output, session) {
  profile <- reactiveVal(default_user_profile())
  shots <- reactiveVal(load_user_data(default_user_profile()$email))
  
  observe({
    data <- shots()
    for (id in c("basic", "advanced", "game")) {
      updateSelectInput(session, paste0(id, "-game"), choices = unname(sort(unique(data$Game))))
      updateSelectInput(session, paste0(id, "-year"), choices = unname(sort(unique(stats::na.omit(data$year)))))
      updateSelectInput(session, paste0(id, "-player"), choices = unname(sort(unique(data$player))))
      updateSelectInput(session, paste0(id, "-shotType"), choices = unname(sort(unique(data$shotType))))
      updateSelectInput(session, paste0(id, "-assist"), choices = unname(sort(unique(data$typeOfAssist))))
      updateSelectInput(session, paste0(id, "-attack"), choices = unname(sort(unique(data$typeOfAttack))))
    }
    updateSelectInput(session, "single_game", choices = unname(sort(unique(data$Game))))
    updateSelectInput(session, "basic_opponents", choices = unname(sort(unique(stats::na.omit(data$Opponent)))))
    updateSelectInput(session, "player_pie_player", choices = unname(sort(unique(data$player[data$Team == "TEAM"]))))
    
    test_choices <- setNames(analytics_specs$id, analytics_specs$label)
    updateSelectInput(session, "off_analytics_test", choices = test_choices)
    updateSelectInput(session, "def_analytics_test", choices = test_choices)
  })
  
  output$auth_header <- renderText(if (isTRUE(profile()$signed_in)) "Signed in" else "Sign in")
  output$user_info <- renderText({
    pr <- profile(); paste("Email:", pr$email, "| Team:", pr$team_name, "| Level:", pr$level, "| Team code:", pr$team_code, "| Role:", pr$role)
  })
  output$signed_in_as <- renderText({ pr <- profile(); paste("Signed in as", if (isTRUE(pr$signed_in)) pr$email else "demo user", "| Team:", pr$team_name, "| Role:", pr$role) })
  
  observeEvent(input$sign_in, {
    req(input$login_email, input$login_password)
    pr <- load_user_profile(input$login_email); pr$signed_in <- TRUE
    profile(pr); shots(load_user_data(pr$email))
    updateTextInput(session, "team_name", value = pr$team_name); updateSelectInput(session, "team_level", selected = pr$level)
    updateTextInput(session, "team_code", value = pr$team_code); updateRadioButtons(session, "user_role", selected = pr$role)
    updateCheckboxInput(session, "show_player_data", value = isTRUE(pr$show_player_data))
  })
  
  observeEvent(input$sign_out, { profile(default_user_profile()); shots(load_demo_data()) })
  
  observeEvent(input$save_profile, {
    pr <- modifyList(profile(), list(team_name = input$team_name, level = input$team_level, team_code = input$team_code, role = input$user_role, show_player_data = isTRUE(input$show_player_data)))
    profile(pr); if (isTRUE(pr$signed_in)) save_user_profile(pr); showNotification("User info saved", type = "message")
  })
  
  basic_data <- reactive({
    data <- apply_filters(shots(), input, "basic")
    if (length(input$basic_opponents %||% character()) > 0) {
      data <- data |> filter(Team == "TEAM" | Opponent %in% input$basic_opponents)
    }
    data
  })
  
  advanced_data <- reactive({
    # 1. Get the base filtered data from the sidebar
    data <- apply_filters(shots(), input, "advanced")
    
    # 2. Get the currently selected zones from your modal UI
    zones <- selected_zones()
    
    # 3. If any zones are selected, filter the dataset by X/Y coordinates
    if (length(zones) > 0) {
      y_max <- FIELD_WIDTH # Uses the 75 defined at the top of your script
      
      data <- data %>%
        filter(
          (1 %in% zones & X >= 0 & X <= 6 & Y >= 34.5 & Y <= 41.5) |
            (2 %in% zones & X >= 0 & X <= 6 & Y >= 26.5 & Y <= 48.5) |
            (3 %in% zones & X >= 6 & X <= 18 & Y >= 26.5 & Y <= 48.5) |
            (4 %in% zones & X >= 6 & X <= 18 & ((Y >= 15.5 & Y <= 26.5) | (Y >= (y_max - 26.5) & Y <= (y_max - 15.5)))) |
            (5 %in% zones & X >= 0 & X <= 6 & ((Y >= 15.5 & Y <= 26.5) | (Y >= (y_max - 26.5) & Y <= (y_max - 15.5)))) |
            (6 %in% zones & X >= 18 & X <= 27 & Y >= 26.5 & Y <= 59.5) |
            (7 %in% zones & X >= 27 & X <= 50 & Y >= 26.5 & Y <= 59.5) |
            (8 %in% zones & X >= 0 & X <= 18 & ((Y >= 0 & Y <= 15.5) | (Y >= (y_max - 15.5) & Y <= y_max)))
        )
    }
    
    data
  })
  game_data <- reactive(shots())
  
  output$team_stats <- renderDT({
    req(isTRUE(profile()$show_player_data) || profile()$role == "Coach")
    data <- basic_data()
    my_team <- data |> filter(Team == "TEAM") |> summarise(
      Side = "My Team", Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
      xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE)
    )
    
    if (input$team_stat_view == "Individual Opponent Teams") {
      opponent <- data |> filter(Team == "OPPONENT") |> group_by(Side = Opponent) |> summarise(
        Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
        xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE), .groups = "drop"
      )
    } else {
      opponent <- data |> filter(Team == "OPPONENT") |> summarise(
        Side = "Total Opponent", Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
        xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE)
      )
    }
    
    # Added selection = "single" so we can capture the clicked row
    datatable(round_xg_columns(bind_rows(my_team, opponent)), selection = "single", options = list(dom = "t"), rownames = FALSE)
  })
  
  output$player_stats <- renderDT({
    basic_data() |> 
      filter(Team == "TEAM") |> 
      group_by(player) |> 
      summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE), Difference = Goals - xG, .groups = "drop") |> 
      round_xg_columns() |> 
      datatable(selection = "single", options = list(pageLength = 8, lengthChange = FALSE, scrollX = TRUE))
  })
  
  output$team_pie <- renderPlot({
    req(input$team_pie_var)
    selected_row <- input$team_stats_rows_selected
    
    if (is.null(selected_row)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Select a team from the table above") + theme_void())
    }
    
    data <- basic_data()
    # Recalculate the summary order exactly as it appears in the table
    my_team_summ <- data |> filter(Team == "TEAM") |> summarise(Side = "My Team", Shots = n())
    
    if (input$team_stat_view == "Individual Opponent Teams") {
      opp_summ <- data |> filter(Team == "OPPONENT") |> group_by(Side = Opponent) |> summarise(Shots = n(), .groups = "drop")
    } else {
      opp_summ <- data |> filter(Team == "OPPONENT") |> summarise(Side = "Total Opponent", Shots = n())
    }
    summary_data <- bind_rows(my_team_summ, opp_summ)
    req(selected_row <= nrow(summary_data))
    
    team_label <- summary_data$Side[selected_row]
    
    if (team_label == "My Team") {
      p_data <- data |> filter(Team == "TEAM")
    } else if (team_label == "Total Opponent") {
      p_data <- data |> filter(Team == "OPPONENT")
    } else {
      p_data <- data |> filter(Team == "OPPONENT", Opponent == team_label)
    }
    
    var_label <- names(c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result"))[c("shotType", "sideOfAttack", "typeOfAssist", "typeOfAttack", "Result") == input$team_pie_var]
    render_pie_chart(p_data, input$team_pie_var, paste(team_label, "-", var_label %||% input$team_pie_var))
  })
  
  output$player_pie <- renderPlot({
    req(input$player_pie_var)
    selected_row <- input$player_stats_rows_selected
    
    if (is.null(selected_row)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Select a player from the table above") + theme_void())
    }
    
    # Retrieve the summarized data in the exact same order as the table
    summary_data <- basic_data() |> 
      filter(Team == "TEAM") |> 
      group_by(player) |> 
      summarise(Shots = n(), .groups = "drop")
    
    selected_player <- summary_data$player[selected_row]
    var_label <- names(c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result"))[c("shotType", "sideOfAttack", "typeOfAssist", "typeOfAttack", "Result") == input$player_pie_var]
    p_data <- basic_data() |> filter(Team == "TEAM", player == selected_player)
    render_pie_chart(p_data, input$player_pie_var, paste(selected_player, "-", var_label %||% input$player_pie_var))
  })
  
  output$descriptive_stats <- renderDT({
    data <- basic_data()
    bind_rows(
      descriptive_stats(data, "typeOfAssist", "Type of assist"),
      descriptive_stats(data, "typeOfAttack", "Type of attack"),
      descriptive_stats(data, "shotType", "Shot type")
    ) |> datatable(options = list(pageLength = 10))
  })
  
  output$offensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() |> filter(Team == "TEAM"), "Offense (My Team)", is_defense = FALSE))
  output$defensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() |> filter(Team == "OPPONENT"), "Defense (Opponents)", is_defense = TRUE))
  
  analytics_bundle <- function(team_filter, test_input) {
    req(test_input)
    spec <- analytics_specs |> filter(id == test_input) |> slice(1)
    data <- standardize_xg_features(advanced_data() |> filter(Team == team_filter))
    data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
    clean <- data |>
      filter(!is.na(.data[[spec$column]]), !.data[[spec$column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal))
    
    # FILTER: Require >= 10 shots per grouping variable
    group_counts <- table(clean[[spec$column]])
    valid_groups <- names(group_counts)[group_counts >= 10]
    clean <- clean |> filter(.data[[spec$column]] %in% valid_groups)
    
    rates <- goal_rate_by_group(clean, spec$column)
    pairs <- pairwise_comparisons(clean, spec$column)
    if (nrow(pairs) == 0 && identical(spec$test_type, "t-test") && nrow(rates) == 2) {
      tt <- try(stats::t.test(stats::as.formula(paste("goal ~", spec$column)), data = clean), silent = TRUE)
      p_adj <- if (!inherits(tt, "try-error")) tt$p.value else NA_real_
      pairs <- tibble(
        Comparison = paste(rates$Group[1], rates$Group[2], sep = "-"),
        group1 = rates$Group[1],
        group2 = rates$Group[2],
        p_adj = p_adj,
        label = ifelse(!is.na(p_adj) & p_adj < 0.10, sprintf("p=%.2f*", p_adj), sprintf("p=%.2f", p_adj %||% NA_real_)),
        y = max(rates$Goal_Percentage / 100, 0, na.rm = TRUE) + 0.08
      )
    }
    list(spec = spec, clean = clean, rates = rates, pairs = pairs)
  }
  
  render_analytics_plot <- function(bundle) {
    data <- bundle$clean
    spec <- bundle$spec
    if (nrow(data) == 0) return(ggplot() + annotate("text", x = 1, y = 1, label = "No data for this test") + theme_void())
    
    # Calculate Goal Percentage per Game to generate distribution for boxplot
    game_rates <- data |> 
      group_by(Game, Group = as.character(.data[[spec$column]])) |> 
      summarise(Goal_Percentage = 100 * mean(goal, na.rm = TRUE), Shots = n(), .groups = "drop") |>
      filter(Shots > 0)
    
    ggplot(game_rates, aes(x = Group, y = Goal_Percentage, fill = Group)) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
      labs(x = spec$label, y = "Goal Percentage per Game (%)", title = paste("Game-by-Game Goal % Distribution by", spec$label)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "none")
  }
  
  off_bundle <- reactive(analytics_bundle("TEAM", input$off_analytics_test))
  def_bundle <- reactive(analytics_bundle("OPPONENT", input$def_analytics_test))
  
  output$off_analytics_plot <- renderPlot(render_analytics_plot(off_bundle()))
  output$def_analytics_plot <- renderPlot(render_analytics_plot(def_bundle()))
  
  output$off_goal_rates <- renderDT({
    rates <- off_bundle()$rates
    if (nrow(rates) == 0) return(datatable(tibble(Note = "No groups available"), options = list(dom = "t"), rownames = FALSE))
    wide <- rates |>
      select(Group, Goal_Percentage, Shots, Goals) |>
      tidyr::pivot_longer(c(Goal_Percentage, Shots, Goals), names_to = "Metric", values_to = "value") |>
      tidyr::pivot_wider(names_from = Group, values_from = value)
    datatable(wide, options = list(dom = "t", pageLength = 15), rownames = FALSE)
  })
  output$def_goal_rates <- renderDT({
    rates <- def_bundle()$rates
    if (nrow(rates) == 0) return(datatable(tibble(Note = "No groups available"), options = list(dom = "t"), rownames = FALSE))
    wide <- rates |>
      select(Group, Goal_Percentage, Shots, Goals) |>
      tidyr::pivot_longer(c(Goal_Percentage, Shots, Goals), names_to = "Metric", values_to = "value") |>
      tidyr::pivot_wider(names_from = Group, values_from = value)
    datatable(wide, options = list(dom = "t", pageLength = 15), rownames = FALSE)
  })
  output$off_pairwise <- renderDT({
    pairs <- off_bundle()$pairs
    if (nrow(pairs) == 0) {
      return(datatable(tibble(Note = "Pairwise comparisons shown on plot when 2+ groups are available"), options = list(dom = "t"), rownames = FALSE))
    }
    datatable(
      pairs |> transmute(Comparison, P_Value = round(p_adj, 4), Significant = p_adj < 0.10),
      options = list(dom = "t", pageLength = 15), rownames = FALSE
    )
  })
  output$def_pairwise <- renderDT({
    pairs <- def_bundle()$pairs
    if (nrow(pairs) == 0) {
      return(datatable(tibble(Note = "Pairwise comparisons shown on plot when 2+ groups are available"), options = list(dom = "t"), rownames = FALSE))
    }
    datatable(
      pairs |> transmute(Comparison, P_Value = round(p_adj, 4), Significant = p_adj < 0.10),
      options = list(dom = "t", pageLength = 15), rownames = FALSE
    )
  })
  
  # 1. Reactive value to store the user's selected zones
  selected_zones <- reactiveVal(integer(0))
  hovered_zone <- reactiveVal(NA)
  
  # Helper function to map X/Y coordinates to a zone
  get_zone_from_xy <- function(x, y) {
    y_max <- FIELD_WIDTH
    zone <- NA
    if (x >= 0 && x <= 6 && y >= 34.5 && y <= 41.5) {
      zone <- 1
    } else if (x >= 0 && x <= 6 && ((y >= 26.5 && y < 34.5) || (y > 41.5 && y <= 48.5))) {
      zone <- 2
    } else if (x > 6 && x <= 18 && y >= 26.5 && y <= 48.5) {
      zone <- 3
    } else if (x > 6 && x <= 18 && ((y >= 15.5 && y < 26.5) || (y > 48.5 && y <= 59.5))) {
      zone <- 4
    } else if (x >= 0 && x <= 6 && ((y >= 15.5 && y < 26.5) || (y > 48.5 && y <= 59.5))) {
      zone <- 5
    } else if (x > 18 && x <= 27 && y >= 15.5 && y <= 59.5) {
      zone <- 6  
    } else if (x > 27 && x <= 60) {
      zone <- 7  
    } else if (x >= 0 && x <= 27 && ((y >= 0 && y < 15.5) || (y > 59.5 && y <= y_max))) {
      zone <- 8  
    }
    return(zone)
  }
  
  # Track hover events
  # Note: only react to real hover positions (ignoreNULL = TRUE). Because this
  # plot redraws on every hover change, the image element gets swapped out from
  # under the cursor, which makes the browser briefly report the mouse as having
  # left the plot. With nullOutside also disabled below, that spurious "leave"
  # would otherwise reset hovered_zone() to NA even though the mouse never moved,
  # causing the highlight to disappear right after it first appears.
  observeEvent(input$zone_image_hover, {
    hovered_zone(get_zone_from_xy(input$zone_image_hover$x, input$zone_image_hover$y))
  }, ignoreNULL = TRUE)
  
  # 2. Open the modal when the button is clicked
  observeEvent(input$open_zone_filter, {
    hovered_zone(NA) # start fresh; nullOutside = FALSE means hover no longer auto-clears
    showModal(modalDialog(
      title = "Select Shot Zones",
      p("Click on the zones in the image below to toggle your filters. Selected zones will be highlighted."),
      # Set delay = 0 for instant hover feedback and enable clip = FALSE.
      # nullOutside = FALSE keeps the last hovered zone highlighted instead of
      # clearing it when the plot's own redraw (triggered by the hover change
      # itself) makes the browser think the mouse briefly left the image.
      plotOutput("zone_image_plot", 
                 click = "zone_image_click", 
                 hover = hoverOpts("zone_image_hover", delay = 0, delayType = "throttle", nullOutside = FALSE), 
                 height = "400px"),
      tags$hr(),
      h5(textOutput("current_selected_zones_text")),
      footer = tagList(
        modalButton("Close"),
        actionButton("clear_zone_filter", "Clear Selection", class = "btn-danger")
      ),
      size = "l"
    ))
  })
  
  # 3. Render the image and overlay selected zones
  output$zone_image_plot <- renderPlot({
    
    # Force dependency on hovered_zone so the plot stays locked on the highlighted zone
    current_hover <- hovered_zone()
    current_zones <- selected_zones()
    
    # Data frame defining the rectangles for each zone
    zone_rects <- data.frame(
      zone = c(1, 2, 2, 3, 4, 4, 5, 5, 6, 7, 8, 8),
      xmin = c(0, 0, 0, 6, 6, 6, 0, 0, 18, 27, 0, 0),
      xmax = c(6, 6, 6, 18, 18, 18, 6, 6, 27, 60, 27, 27),
      ymin = c(34.5, 26.5, 41.5, 26.5, 15.5, 48.5, 15.5, 48.5, 15.5, 0, 0, 59.5),
      ymax = c(41.5, 34.5, 48.5, 48.5, 26.5, 59.5, 26.5, 59.5, 59.5, 75, 15.5, 75)
    )
    
    # Map transparency (alpha) based on selection and hover state
    zone_rects$alpha_val <- sapply(zone_rects$zone, function(z) {
      if (z %in% current_zones) return(0.7)
      if (!is.na(current_hover) && z == current_hover) return(0.3)
      return(0.0)
    })
    
    zone_labels <- data.frame(
      zone = c(1, 2, 2, 3, 4, 4, 5, 5, 6, 7, 8, 8),
      x = c(3, 3, 3, 12, 12, 12, 3, 3, 22.5, 43.5, 13.5, 13.5),
      y = c(38, 30.5, 45, 37.5, 21, 54, 21, 54, 37.5, 37.5, 7.75, 67.25)
    )
    
    suppressMessages({
      soccer_pitch() +
        geom_rect(aes(xmin = -2, xmax = 0, ymin = 34.5, ymax = 41.5),
                  fill = "white", color = "black", linewidth = 1.2) +
        geom_rect(data = zone_rects, 
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = I(alpha_val)),
                  fill = "white", color = NA) +
        geom_text(data = zone_labels, aes(x = x, y = y, label = zone), 
                  color = "white", size = 6, fontface = "bold") +
        coord_fixed(xlim = c(-2, 60), ylim = c(0, FIELD_WIDTH), expand = FALSE) +
        theme(plot.margin = margin(0, 0, 0, 0))
    })
  })
  
  # 4. Handle clicks on the image and map to the 8 zones
  observeEvent(input$zone_image_click, {
    req(input$zone_image_click)
    
    zone <- get_zone_from_xy(input$zone_image_click$x, input$zone_image_click$y)
    
    # Toggle the zone in the reactive vector
    if (!is.na(zone)) {
      current <- selected_zones()
      if (zone %in% current) {
        selected_zones(current[current != zone]) 
      } else {
        selected_zones(c(current, zone)) 
      }
    }
  })
  
  # 5. Clear selections button
  observeEvent(input$clear_zone_filter, {
    selected_zones(integer(0))
  })
  
  # 6. Text output for the modal
  output$current_selected_zones_text <- renderText({
    if (length(selected_zones()) == 0) {
      "No zones selected (Showing all locations)."
    } else {
      paste("Currently Selected Zones:", paste(sort(selected_zones()), collapse = ", "))
    }
  })
  
  penalty_data <- reactive({
    advanced_data() |> 
      filter(typeOfAttack == "PENALTY") |>
      filter(if (input$penalty_team_filter == "Opponent") Team == "OPPONENT" else Team == "TEAM")
  })
  
  output$penalty_goal_rates <- renderDT({
    data <- penalty_data()
    if(nrow(data) == 0) return(datatable(tibble(Note = "No penalty data"), options = list(dom = "t"), rownames = FALSE))
    
    summary <- data |>
      summarise(Shots = n(), 
                Goals = sum(Result == "GOAL" | goals.x == 1, na.rm = TRUE),
                Goal_Percentage = round(100 * Goals / Shots, 2))
    datatable(summary, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$shooting_grid <- renderPlot(zone_grid_plot(advanced_data() |> filter(Team == "TEAM"), NULL, "Shooting net distribution"))
  output$gk_grid <- renderPlot(zone_grid_plot(advanced_data() |> filter(Team == "OPPONENT"), NULL, "Goalkeeper shots faced distribution", is_gk = TRUE))
  
  output$shooting_heat <- renderPlot(net_plot(advanced_data() |> filter(Team == "TEAM", !is.na(net_x), !is.na(net_y))))
  output$gk_heat <- renderPlot(net_plot(advanced_data() |> filter(Team == "OPPONENT", !is.na(net_x), !is.na(net_y))))
  
  output$penalty_grid <- renderPlot({ zone_grid_plot(penalty_data(), NULL, title = "Penalty Net Distribution") })
  output$penalty_heat <- renderPlot({ net_plot(penalty_data() |> filter(!is.na(net_x), !is.na(net_y))) })
  
  selected_game <- reactive(if (!is.null(input$single_game) && nzchar(input$single_game)) filter(game_data(), Game == input$single_game) else game_data())
  scoreline_text <- reactive({
    data <- selected_game(); paste0("TEAM ", sum(data$goals.x[data$Team == "TEAM"], na.rm = TRUE), " - ", sum(data$goals.x[data$Team == "OPPONENT"], na.rm = TRUE), " OPPONENT")
  })
  
  output$shot_chart <- renderPlot(soccer_pitch(selected_game() |> mutate(row_id = row_number()), aes(X, Y, colour = Result, size = goals.y)) + geom_point(alpha = .85) + annotate("label", x = FIELD_LENGTH / 2, y = FIELD_WIDTH - 4, label = scoreline_text(), size = 6, fill = "white") + labs(colour = "Result", size = "xG"))
  
  clicked_shot_data <- reactive({
    p <- input$shot_chart_click; data <- selected_game() |> mutate(row_id = row_number()); if (is.null(p) || nrow(data) == 0) return(data[0, ])
    data[which.min((data$X - p$x)^2 + (data$Y - p$y)^2), , drop = FALSE]
  })
  
  output$clicked_net <- renderPlot({ d <- clicked_shot_data(); net_plot(point = if (nrow(d) == 1 && !is.na(d$net_x[1]) && !is.na(d$net_y[1])) list(x = d$net_x[1], y = d$net_y[1]) else NULL) })
  output$clicked_shot <- renderDT({
    clicked_shot_data() |>
      round_shot_display() |>
      rename(xG = goals.y) |>
      select(any_of(c(
        "Minute", "Result", "player", "Team", "Opponent", 
        "xG", "PSxG", "shotType", "typeOfAssist", 
        "typeOfAttack", "sideOfAttack"
      ))) |>
      datatable(options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$xg_timeline <- renderPlot({ selected_game() |> arrange(Minute) |> mutate(team_xg = cumsum(ifelse(Team == "TEAM", goals.y, 0)), opp_xg = cumsum(ifelse(Team == "OPPONENT", goals.y, 0))) |> ggplot(aes(Minute)) + geom_step(aes(y = team_xg, colour = "Team")) + geom_step(aes(y = opp_xg, colour = "Opponent")) + labs(y = "Cumulative xG", colour = NULL) })
  
  net_click_point <- reactiveVal(NULL)
  
  output$field_click <- renderPlot({
    soccer_pitch() + labs(title = "Click anywhere on the field to place a shot")
  })
  output$field_point <- renderText({ p <- input$field_click; if (is.null(p)) "No field point selected" else sprintf("Shot location: X = %.1f, Y = %.1f", p$x, p$y) })
  output$net_click <- renderPlot(net_plot(point = net_click_point()))
  output$net_point <- renderText({
    p <- net_click_point()
    if (is.null(p)) "No net point selected" else sprintf("Net location: X = %.2f, Y = %.2f", p$x, p$y)
  })
  
  observeEvent(input$net_click, {
    req(!is.null(input$net_click$x), !is.null(input$net_click$y))
    net_click_point(list(x = input$net_click$x, y = input$net_click$y))
  }, ignoreInit = TRUE)
  
  shot_form <- function() {
    tagList(
      numericInput("minute", "Minute*", value = 1, min = 0, max = 130),
      selectInput("result", "Result*", choices = SHOT_RESULTS),
      selectInput("team", "Team*", choices = c("My Team", "Opponent")),
      selectInput("home_away", "Home vs away", choices = c("", "HOME", "AWAY")),
      textInput("opponent", "Opponent*", value = "OPPONENT"),
      textInput("player", "Player", value = ""),
      textInput("gk", "Goalkeeper", value = ""),
      selectInput("shot_type", "Shot type", choices = c("RIGHTFOOT", "LEFTFOOT", "HEADER", "RIGHTVOLLEY", "LEFTVOLLEY")),
      selectInput("assist_type", "Type of assist", choices = c("N/A", "PASS", "THROUGHBALL", "CROSSINAIR", "CUTBACK", "OTHER")),
      selectInput("attack_type", "Type of attack", choices = c("POSSESSION", "TRANSITION", "RESTART", "THROW-IN", "PENALTY", "OTHER")),
      selectInput("side_attack", "Side of attack", choices = c("LEFT", "RIGHT", "MIDDLE")),
      selectInput("dom", "Dominant/non-dominant", choices = c("", "DOMINANT", "NONDOMINANT")),
      actionButton("blocked", "Blocked shot")
    )
  }
  
  observeEvent(input$field_click, {
    req(profile()$role == "Coach")
    net_click_point(NULL)
    showModal(modalDialog(
      title = "Shot net location",
      p("Click where the shot ended in the net, or skip this step. The ball marker stays on your last click."),
      plotOutput("net_click", click = "net_click", hover = hoverOpts("net_hover"), width = "100%", height = "55vh"),
      textOutput("net_point"), shot_form(),
      footer = tagList(modalButton("Cancel"), actionButton("skip_net", "Skip net location"), actionButton("save_shot", "Save shot", class = "btn-primary")),
      size = "l", easyClose = TRUE
    ))
  })
  
  observeEvent(input$open_upload, {
    req(profile()$role == "Coach")
    showModal(modalDialog(
      title = "Upload shot data",
      fileInput("upload", "Upload CSV/XLSX/XLS", accept = c(".csv", ".xlsx", ".xls")),
      helpText("Uploaded files should include these column names: ", paste(REQUIRED_UPLOAD_COLUMNS, collapse = ", ")),
      footer = modalButton("Close"), easyClose = TRUE
    ))
  })
  
  observeEvent(input$blocked, updateSelectInput(session, "result", selected = "BLOCKED"))
  observeEvent(input$skip_net, {
    net_click_point(NULL)
    removeModal()
    showModal(modalDialog(title = "Shot information", shot_form(), footer = tagList(modalButton("Cancel"), actionButton("save_shot", "Save shot", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$upload, {
    uploaded$datetime_added <- as.character(Sys.time())
    req(profile()$role == "Coach")
    ext <- tools::file_ext(input$upload$name)
    uploaded <- if (tolower(ext) == "csv") readr::read_csv(input$upload$datapath, show_col_types = FALSE) else readxl::read_excel(input$upload$datapath)
    missing <- setdiff(REQUIRED_UPLOAD_COLUMNS, names(uploaded))
    if (length(missing) > 0) showNotification(paste("Warning: missing columns", paste(missing, collapse = ", ")), type = "warning", duration = 10)
    shots(bind_rows(shots(), standardize_shots(uploaded))); save_user_data(shots(), profile()$email, profile()$signed_in); removeModal()
  })
  
  observeEvent(input$save_shot, {
    req(profile()$role == "Coach")
    req(input$minute, input$result, input$opponent, input$field_click)
    team_value <- ifelse(input$team == "Opponent", "OPPONENT", "TEAM")
    game_label <- paste(format(Sys.Date(), "%Y"), input$opponent, input$home_away %||% "", sep = " - ")
    net_pt <- net_click_point()
    new_shot <- tibble(
      Minute = input$minute, Result = input$result, X = input$field_click$x, Y = input$field_click$y,
      player = input$player, h_a = input$home_away, situation = NA, shotType = input$shot_type,
      domVSnondom = input$dom, Opponent = input$opponent, playerAssist = NA, typeOfAssist = input$assist_type,
      sideOfAttack = input$side_attack, PossesionWon = NA, typeOfAttack = input$attack_type,
      year = as.integer(format(Sys.Date(), "%Y")), sideOfAttackGrouped = NA, Team = team_value,
      GK = input$gk, Game = game_label,
      net_x = if (is.null(net_pt)) NA_real_ else net_pt$x,
      net_y = if (is.null(net_pt)) NA_real_ else net_pt$y,
      datetime_added = as.character(Sys.time())
    )
    shots(bind_rows(shots(), standardize_shots(new_shot)))
    save_user_data(shots(), profile()$email, profile()$signed_in)
    net_click_point(NULL)
    removeModal()
    showNotification("Shot saved", type = "message")
  })
  
  output$recent_shots <- renderDT({
    req(profile()$role == "Coach")
    shots() |>
      mutate(row_id = row_number(), .before = 1) |>
      select(-any_of(c("zone", "goals.x", "goals.y", "sideOfAttackGrouped", "h_a"))) |> # Filter out noisy data to keep datetime_added visible
      round_shot_display() |>
      datatable(selection = "single", editable = TRUE, options = list(pageLength = 25, scrollX = TRUE))
  }, server = FALSE)
  
  observeEvent(input$recent_shots_cell_edit, {
    req(profile()$role == "Coach")
    info <- input$recent_shots_cell_edit
    display <- shots() |> mutate(row_id = row_number(), .before = 1) |> select(-any_of(c("zone")))
    col_name <- names(display)[info$col + 1]
    if (is.null(col_name) || col_name == "row_id") return()
    data <- shots()
    data[info$row, col_name] <- DT::coerceValue(info$value, data[[col_name]][info$row])
    shots(standardize_shots(data)); save_user_data(shots(), profile()$email, profile()$signed_in)
  })
  
  observeEvent(input$delete_shot, {
    req(profile()$role == "Coach")
    selected <- input$recent_shots_rows_selected
    req(length(selected) == 1)
    data <- shots()
    data <- data[-selected, , drop = FALSE]
    shots(standardize_shots(data)); save_user_data(shots(), profile()$email, profile()$signed_in); showNotification("Shot deleted", type = "message")
  })
}

shinyApp(ui, server)