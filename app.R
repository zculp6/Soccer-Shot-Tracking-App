# Soccer Shot Tracking Shiny App
# Run with: shiny::runApp()

required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "readr", "readxl", "DT",
  "tidyr", "digest", "scales", "tibble", "DBI", "jsonlite"
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
library(tibble)
library(DBI)

source("xg_model.R")

require_optional_package <- function(package, feature) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(feature, " requires the ", package, " package. Add it to the deployment dependencies.", call. = FALSE)
  }
}

get_db_connection <- function() {
  require_optional_package("RPostgres", "Database access")
  if (Sys.getenv("NEON_DB") == "" || Sys.getenv("NEON_USER") == "") {
    stop("Database access requires NEON_DB, NEON_USER, and NEON_PASS environment variables.", call. = FALSE)
  }
  host_env <- Sys.getenv("NEON_HOST")
  if (host_env == "") host_env <- "localhost"
  dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("NEON_DB"),
    host = host_env,
    port = 5432,
    user = Sys.getenv("NEON_USER"),
    password = Sys.getenv("NEON_PASS"),
    sslmode = "prefer"
  )
}

DATA_PATH <- "XGStats.csv"
USER_DATA_DIR <- "user_data"
FIELD_LENGTH <- 120
FIELD_WIDTH <- 75
GOAL_WIDTH <- 24
GOAL_HEIGHT <- 8
REQUIRED_UPLOAD_COLUMNS <- c(
  "Minute", "Result", "X", "Y", "player", "h_a", "situation", "shotType",
  "domVSnondom", "Opponent", "playerAssist", "typeOfAssist", "sideOfAttack", "PossesionWon",
  "typeOfAttack", "year", "sideOfAttackGrouped", "Team", "GK", "net_x", "net_y", "PSxG", "Date", "Notes"
)
SHOT_RESULTS <- c("GOAL", "SAVED", "MISSED", "BLOCKED")

recompute_game_labels <- function(data) {
  data <- as.data.frame(data)
  if (nrow(data) == 0) {
    data$Game <- character(0)
    return(data)
  }
  
  data <- data %>%
    arrange(Date) %>% 
    group_by(year, Opponent) %>%
    mutate(
      game_id = dense_rank(Date),
      suffix = ifelse(is.na(game_id) | game_id <= 1, "", paste0(" (", game_id, ")")),
      safe_year = ifelse(is.na(year), "Unknown", year),
      safe_opp = ifelse(is.na(Opponent), "Unknown", Opponent),
      Game = paste0(safe_year, " - ", safe_opp, suffix)
    ) %>%
    ungroup() %>%
    # Automatically enforce sorting by Match and Minute so duplicates group logically
    arrange(Game, Minute) %>%
    as.data.frame()
  
  data
}

standardize_shots <- function(data) {
  data <- as.data.frame(data)
  
  # Remove any auto-generated index columns from CSV/spreadsheet exports.
  data <- data[, !names(data) %in% c("", "X.1", "row_id"), drop = FALSE]
  
  for (col in REQUIRED_UPLOAD_COLUMNS) if (!col %in% names(data)) data[[col]] <- NA
  data[] <- lapply(data, clean_text)
  
  # Cast explicitly for necessary numeric/date columns
  data$Minute <- suppressWarnings(as.numeric(data$Minute))
  data$X <- suppressWarnings(as.numeric(data$X))
  data$Y <- suppressWarnings(as.numeric(data$Y))
  data$net_x <- suppressWarnings(as.numeric(data$net_x))
  data$net_y <- suppressWarnings(as.numeric(data$net_y))
  data$year <- suppressWarnings(as.integer(data$year))
  data$Date <- safe_parse_date(data$Date)
  data$Notes <- as.character(data$Notes) # Added Notes column parsing
  
  data$goals.x <- ifelse(data$Result == "GOAL", 1, suppressWarnings(as.numeric(data$goals.x %||% 0)))
  data$sideOfAttackGrouped <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", data$sideOfAttack)
  data$goals.y <- predict_xg(data, fallback = suppressWarnings(as.numeric(data$goals.y)))
  data$PSxG <- predict_psxg(data, fallback = suppressWarnings(as.numeric(data$PSxG)))
  data$PSxG <- ifelse(is.na(data$net_x) | is.na(data$net_y), NA, data$PSxG)
  data$Team <- ifelse(toupper(as.character(data$Team %||% "")) %in% c("OPPONENT", "OPP") | toupper(as.character(data$player %||% "")) == "OPP", "OPPONENT", "TEAM")
  data$GK <- ifelse(is.na(data$GK) | data$GK == "", NA_character_, as.character(data$GK))
  if (!"datetime_added" %in% names(data)) data$datetime_added <- NA_character_
  data$datetime_added <- as.character(data$datetime_added)
  data
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

clean_text <- function(x) {
  if (is.character(x)) toupper(trimws(x)) else x
}

# Parses a Date column that might come in as an actual Date, an ISO string,
# or one of a few common spreadsheet formats. Anything unparseable becomes NA
# rather than erroring, since Date is optional on upload.
safe_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(x_chr))
  need <- is.na(out) & !is.na(x_chr) & x_chr != ""
  for (fmt in c("%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%Y/%m/%d")) {
    if (!any(need)) break
    alt <- suppressWarnings(as.Date(x_chr[need], format = fmt))
    out[need][!is.na(alt)] <- alt[!is.na(alt)]
    need <- is.na(out) & !is.na(x_chr) & x_chr != ""
  }
  out
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
  data
}

load_demo_data <- function() {
  readr::read_csv(DATA_PATH, show_col_types = FALSE) %>%
    anonymize_demo_data() %>%
    standardize_shots()
}

# Strips anything that isn't a letter/number (spaces, hyphens, etc.) and
# uppercases, so team codes are clean and URL/share-friendly.
clean_team_code <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", x %||% ""))
}

# Base code = first 6 letters of the team name (non-letters stripped).
team_code_base <- function(team_name) {
  letters_only <- toupper(gsub("[^A-Za-z]", "", team_name %||% ""))
  substr(letters_only, 1, 6)
}

team_code_taken <- function(con, code, exclude_email = NULL) {
  if (is.null(con)) return(FALSE)
  if (!is.null(exclude_email)) {
    query <- "SELECT 1 FROM team_accounts WHERE team_code = $1 AND email != $2"
    res <- tryCatch(dbGetQuery(con, query, params = list(code, exclude_email)), error = function(e) data.frame())
  } else {
    query <- "SELECT 1 FROM team_accounts WHERE team_code = $1"
    res <- tryCatch(dbGetQuery(con, query, params = list(code)), error = function(e) data.frame())
  }
  nrow(res) > 0
}

# Generates a unique team code from the team name: first 6 letters, then
# appends 2, 3, 4, ... (giving a 7th, 8th, ... character) until it no longer
# collides with an existing team's code.
generate_unique_team_code <- function(team_name, exclude_email = NULL) {
  base_code <- team_code_base(team_name)
  if (nchar(base_code) < 4) base_code <- paste0(base_code, strrep("X", 4 - nchar(base_code)))
  
  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  code <- base_code
  if (!is.null(con)) {
    suffix <- 1
    while (team_code_taken(con, code, exclude_email)) {
      suffix <- suffix + 1
      code <- paste0(base_code, suffix)
    }
    dbDisconnect(con)
  }
  code
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
  # Build each column with the SAME type that standardize_shots() produces,
  # so an empty frame can always be safely bind_rows()'d with real data.
  # (The old matrix()-based construction made every column `logical`, which
  # crashed bind_rows() with a type-mismatch error once real character/
  # numeric data was combined with it -- e.g. Result: logical vs character.)
  numeric_cols <- c("Minute", "X", "Y", "net_x", "net_y", "PSxG")
  integer_cols <- c("year")
  date_cols <- c("Date")
  col_list <- lapply(REQUIRED_UPLOAD_COLUMNS, function(col) {
    if (col %in% numeric_cols) numeric(0)
    else if (col %in% integer_cols) integer(0)
    else if (col %in% date_cols) as.Date(character(0))
    else character(0)
  })
  names(col_list) <- REQUIRED_UPLOAD_COLUMNS
  df <- as.data.frame(col_list, stringsAsFactors = FALSE)
  df$datetime_added <- character(0)
  df$Game <- character(0)
  df
}

load_user_data <- function(email) {
  if (email == default_user_profile()$email) {
    return(load_demo_data())
  }
  
  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) return(empty_shots_frame())
  
  query <- "SELECT shot_data FROM team_accounts WHERE email = $1"
  res <- tryCatch(dbGetQuery(con, query, params = list(email)), error = function(e) data.frame())
  dbDisconnect(con)
  
  if (nrow(res) > 0 && !is.na(res$shot_data[1]) && res$shot_data[1] != "" && res$shot_data[1] != "[]") {
    df <- tryCatch(jsonlite::fromJSON(res$shot_data[1]), error = function(e) empty_shots_frame())
    if (nrow(df) > 0) return(standardize_shots(df))
  }
  
  return(empty_shots_frame())
}

save_user_data <- function(data, email, signed_in = TRUE) {
  if (!isTRUE(signed_in)) {
    # Demo mode: changes are shown to the user for the current session only
    # (via the `shots` reactiveVal) but are never persisted to disk or the DB.
    # This keeps the shared demo dataset (DATA_PATH) intact for all other users,
    # and any edits automatically revert once the user signs out or reloads.
    return(invisible(NULL))
  } else {
    # Serialize the R dataframe into a JSON string
    json_string <- as.character(jsonlite::toJSON(data, dataframe = "rows", na = "null"))
    
    con <- tryCatch(get_db_connection(), error = function(e) NULL)
    if (!is.null(con)) {
      query <- "UPDATE team_accounts SET shot_data = $1 WHERE email = $2"
      tryCatch(dbExecute(con, query, params = list(json_string, email)), error = function(e) NULL)
      dbDisconnect(con)
    }
  }
}


round_shot_display <- function(data) {
  data %>% mutate(across(any_of(c("Minute", "X", "Y", "net_x", "net_y", "goals.x", "goals.y", "PSxG", "xG", "xG_Against", "PSxG_Against", "Difference", "Goal_Percentage")), ~ {
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
    uiOutput(ns("player_filter_ui")), # Dynamic render
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
  data %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
}

descriptive_stats <- function(data, column, label) {
  data %>%
    mutate(value = ifelse(is.na(.data[[column]]) | .data[[column]] == "" | .data[[column]] == "N/A", "UNKNOWN", .data[[column]])) %>%
    count(value, name = "Shots") %>%
    mutate(Category = label, Percent = round(100 * Shots / sum(Shots), 2)) %>%
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
  rates <- clean_data %>%
    group_by(Group = as.character(.data[[column]])) %>%
    summarise(rate = mean(goal, na.rm = TRUE), .groups = "drop")
  max_rate <- max(rates$rate, 0, na.rm = TRUE)
  tk_df %>%
    filter(!is.na(group1), !is.na(group2)) %>%
    transmute(
      Comparison,
      group1,
      group2,
      p_adj = `p adj`,
      label = ifelse(p_adj < 0.10, sprintf("p=%.2f*", p_adj), sprintf("p=%.2f", p_adj)),
      y = max_rate + 0.08 + 0.07 * (seq_len(n()) - 1)
    )
}

run_advanced_tests <- function(data) {
  data <- standardize_xg_features(data)
  data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
  tests <- list()
  clean_for_test <- function(dataset, column) {
    dataset[!is.na(dataset[[column]]) & !dataset[[column]] %in% c("", "N/A", "NA", "UNKNOWN") & !is.na(dataset$goal), , drop = FALSE]
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
  foot_data <- foot_data[foot_data$domVSnondom %in% c("DOMINANT", "NONDOMINANT"), ]
  foot_data$domVSnondom <- factor(foot_data$domVSnondom) 
  tests$dominant_foot_t_test <- safe_t(goal ~ domVSnondom, foot_data)
  tests
}

goal_rate_by_group <- function(data, column) {
  data %>%
    filter(!is.na(.data[[column]]), !.data[[column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal)) %>%
    group_by(Group = as.character(.data[[column]])) %>%
    summarise(Shots = n(), Goals = sum(goal, na.rm = TRUE), Goal_Percentage = round(100 * mean(goal, na.rm = TRUE), 2), .groups = "drop") %>%
    arrange(desc(Goal_Percentage))
}

advanced_summary <- function(data, is_defense = FALSE) {
  data <- standardize_xg_features(data)
  data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
  tests <- run_advanced_tests(data)
  
  valid_specs <- analytics_specs %>% filter(sapply(column, function(col) !all(is.na(data[[col]]))))
  if(nrow(valid_specs) == 0) return(tibble(Test=character(), Test_ID=character(), Type=character(), P_Value=numeric(), Significant=logical(), Top_Group=character(), Takeaway=character()))
  bind_rows(lapply(seq_len(nrow(analytics_specs)), function(i) {
    spec <- analytics_specs[i, ]
    p_value <- extract_p_value(tests[[spec$id]])
    
    clean_data <- data %>%
      filter(!is.na(.data[[spec$column]]), !.data[[spec$column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal))
    
    group_counts <- table(clean_data[[spec$column]])
    valid_groups <- names(group_counts)[group_counts >= 10]
    clean_data <- clean_data %>% filter(.data[[spec$column]] %in% valid_groups)
    
    if (identical(spec$test_type, "t-test") && is.na(p_value) && nrow(clean_data) > 0) {
      if (length(unique(clean_data[[spec$column]])) == 2) {
        tt <- try(stats::t.test(stats::as.formula(paste("goal ~", spec$column)), data = clean_data), silent = TRUE)
        if (!inherits(tt, "try-error")) p_value <- tt$p.value
      } else {
        p_value <- NA_real_
      }
    }
    
    groups <- clean_data %>%
      group_by(Group = as.character(.data[[spec$column]])) %>%
      summarise(Shots = n(), Goal_Percentage = mean(goal, na.rm = TRUE), .groups = "drop") %>%
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
  sig_findings <- summary %>% filter(Significant)
  non_sig_findings <- summary %>% filter(!Significant)
  
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
    # Look for the geom_point lines inside net_plot and reduce size parameters
    {if (!is.null(data) && nrow(data) > 0) geom_point(data = filter(data, Result != "GOAL" & (is.na(goals.x) | goals.x != 1)), aes(net_x, net_y), colour = "white", alpha = .75, size = 1.33) else NULL} +
    # Reduced yellow goal dot from 5.33 to 3.0
    {if (!is.null(data) && nrow(data) > 0) geom_point(data = filter(data, Result == "GOAL" | goals.x == 1), aes(net_x, net_y), colour = "#FFD700", size = 3.0) else NULL} +
    {if (!is.null(point) && !is.null(point$x) && !is.null(point$y) && !is.na(point$x) && !is.na(point$y)) {
      list(
        # Reduced clicked dot outline and center size
        annotate("point", x = point$x, y = point$y, shape = 21, size = 5, fill = "white", colour = "black", stroke = 1.4),
        annotate("point", x = point$x, y = point$y, shape = 16, size = 3, colour = "black"),
        annotate("text", x = point$x, y = point$y, label = "⚽", size = 5, vjust = 0.35)
      )
    } else NULL} +
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
  data <- data %>% mutate(zone = net_zone(net_x, net_y)) %>% filter(!is.na(zone))
  if (!is.null(selected) && length(selected) > 0) data <- data %>% filter(zone %in% selected)
  
  zs <- data %>% 
    group_by(zone) %>% 
    summarise(Shots = n(), Goals = sum(goal_val, na.rm = TRUE), .groups = "drop") %>% 
    complete(zone = paste("Zone", 1:8), fill = list(Shots = 0, Goals = 0))
  
  if (is_gk) {
    zs %>% mutate(
      Percent = ifelse(Shots == 0, 0, round(100 * (Shots - Goals) / Shots, 0)),
      Label = sprintf("%.0f%%\n(%d GA, %d shots)", Percent, Goals, Shots)
    )
  } else {
    zs %>% mutate(
      Percent = ifelse(Shots == 0, 0, round(100 * Goals / Shots, 0)),
      Label = sprintf("%.0f%%\n(%d goals, %d shots)", Percent, Goals, Shots)
    )
  }
}

zone_grid_plot <- function(data, selected = NULL, title = "Net location distribution", is_gk = FALSE,
                           plot_width = NULL, plot_height = NULL) {
  zs <- zone_summary(data, selected, is_gk) %>% 
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
  
  # --- Dynamically size the zone labels so they always fit inside their box ---
  # Data-space dimensions of the plotted grid are fixed by the geometry above.
  data_w <- 36  # x range: -18 to 18
  data_h <- 16  # y range: -4 to 12
  box_w  <- 6   # width of a single zone box, in data units
  box_h  <- 4   # height of a single zone box, in data units
  
  if (!is.null(plot_width) && !is.null(plot_height) && plot_width > 0 && plot_height > 0) {
    # coord_fixed() enforces a 1:1 aspect ratio, so the panel gets
    # letterboxed to whichever screen dimension is the tighter constraint.
    px_per_unit <- min(plot_width / data_w, plot_height / data_h)
  } else {
    # Fallback used before client dimensions are available (e.g. first render)
    px_per_unit <- 15
  }
  
  box_w_px <- box_w * px_per_unit
  box_h_px <- box_h * px_per_unit
  
  # Labels are two lines: "xx%" / "(n goals, n shots)" - find the longest line
  # actually present so text is sized to the real content, not a guess.
  label_lines <- unlist(strsplit(zs$Label, "\n", fixed = TRUE))
  max_chars <- max(nchar(label_lines), 1)
  n_lines <- 2
  
  # Cap the font size (points) so both lines fit vertically AND the longest
  # line fits horizontally; ~0.58 char-width and ~1.25 line-height are
  # conservative approximations for a bold sans-serif face.
  size_from_height <- (box_h_px * 0.85) / (n_lines * 1.25)
  size_from_width  <- (box_w_px * 0.90) / (max_chars * 0.58)
  
  text_size_pt <- max(6, min(size_from_height, size_from_width, 16))
  text_size_mm <- text_size_pt / (72.27 / 25.4)  # geom_text's 'size' is in mm
  
  ggplot(zs) + 
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 12, fill = "#0b4f6c", colour = NA, alpha = 0.95) +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 0, fill = "#56a832", colour = NA) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Percent), colour = "black", linewidth = 0.5) + 
    geom_text(aes(x = x_center, y = y_center, label = Label), colour = "black", fontface = "bold", 
              size = text_size_mm, lineheight = 0.9) + 
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
  id = "main_nav",
  header = tags$head(
    tags$style(HTML("
      #shiny-notification-panel {
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        bottom: auto !important;
        right: auto !important;
        transform: translate(-50%, -50%) !important;
        z-index: 9999;
      }
      .dataTables_wrapper {
        width: 100%;
        overflow-x: auto;
      }
      .dataTables_wrapper .dataTables_scroll,
      .dataTables_wrapper .dataTables_scrollBody {
        border-bottom: 0 !important;
      }
      table.dataTable.no-footer {
        border-bottom: 0 !important;
      }
      #recent_shots .form-control,
      #recent_shots select,
      #recent_shots input {
        width: auto !important;
        min-width: 3rem;
        max-width: none;
      }
      #recent_shots th,
      #recent_shots td {
        white-space: nowrap;
      }
      #recent_shots .dataTables_scrollHeadInner,
      #recent_shots table.dataTable {
        width: max-content !important;
        table-layout: auto !important;
      }
      @media (max-width: 768px) {
        .bslib-sidebar-layout {
          --_sidebar-width: min(100%, 22rem);
        }
        .dataTables_wrapper .dataTables_paginate {
          float: none;
          text-align: center;
          margin-top: 0.5rem;
        }
        .dataTables_wrapper .dataTables_info {
          float: none;
          text-align: center;
        }
        .dataTables_wrapper {
          margin-bottom: 1.5rem !important; 
        }
        .tab-content {
          padding-bottom: 2rem;
        }
      }
    "))
  ),
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(title = textOutput("account_tab_title", inline = TRUE), value = "account_tab",
            uiOutput("account_panel")),
  nav_panel("User info", value = "user_info_tab",
            uiOutput("user_info_panel")),
  nav_panel("Basic stats", 
            conditionalPanel("!output.has_data", div(class = "alert alert-warning", role = "alert", "No data exists. Upload or enter data in the Add/Upload shots tab.")),
            conditionalPanel("output.has_data",
                             layout_sidebar(sidebar = sidebar(filter_ui("basic"), selectInput("basic_opponents", "Filter opponents (optional)", choices = NULL, multiple = TRUE), title = "Filter", open = FALSE),
                                            h4(textOutput("signed_in_as")), p("xG stands for expected goals. PSxG stands for post-shot expected goals. Values are rounded to hundredths."),
                                            navset_tab(id = "basic_tabs",
                                                       nav_panel("Team stats",
                                                                 radioButtons("team_stat_view", "Opponent View:", choices = c("Total Opponent", "Individual Opponent Teams"), inline = TRUE),
                                                                 DTOutput("team_stats"),
                                                                 br(),
                                                                 hr(),
                                                                 fluidRow(
                                                                   column(4, selectInput("team_pie_var", "Distribution Variable", choices = c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result")))
                                                                 ),
                                                                 plotOutput("team_pie", height = "45vh")
                                                       ),
                                                       nav_panel("Player stats", value = "player_stats_tab",
                                                                 DTOutput("player_stats"),
                                                                 br(),
                                                                 hr(),
                                                                 fluidRow(
                                                                   column(4, selectInput("player_pie_var", "Distribution Variable", choices = c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result")))
                                                                 ),
                                                                 plotOutput("player_pie", height = "45vh"))
                                            ),
                                            DTOutput("descriptive_stats"))
            )
  ),
  nav_panel("Advanced analytics", 
            conditionalPanel("!output.has_data", div(class = "alert alert-warning", role = "alert", "No data exists. Upload or enter data in the Add/Upload shots tab.")),
            conditionalPanel("output.has_data",
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
                                           uiOutput("shooting_targets_ui")),
                                 nav_panel("GK", 
                                           uiOutput("gk_targets_ui")),
                                 nav_panel("Penalties", 
                                           uiOutput("penalty_team_filter_ui"),
                                           fluidRow(
                                             column(12, h5("Penalty Goal %"), DTOutput("penalty_goal_rates"))
                                           ),
                                           uiOutput("penalty_targets_ui"))
                               )
                             )
            )
  ),
  nav_panel("Game stats", 
            conditionalPanel("!output.has_data", div(class = "alert alert-warning", role = "alert", "No data exists. Upload or enter data in the Add/Upload shots tab.")),
            conditionalPanel("output.has_data",
                             layout_sidebar(
                               sidebar = sidebar(
                                 selectInput("game_chart_scope", "Shot chart view", choices = c("All Games" = "all", "Filter by Year" = "year", "Individual Game" = "game")),
                                 conditionalPanel("input.game_chart_scope == 'year'", selectInput("single_game_year", "Year", choices = NULL)),
                                 conditionalPanel("input.game_chart_scope == 'game'", selectInput("single_game", "Individual game shot chart", choices = NULL)),
                                 title = "Game", open = TRUE
                               ), 
                               p(strong("Note: Click a point on the shot chart below to see shot details.")), 
                               plotOutput("shot_chart", click = "shot_chart_click", width = "100%", height = "60vh"), 
                               uiOutput("clicked_net_ui"), 
                               DTOutput("clicked_shot"), 
                               plotOutput("xg_timeline", width = "100%", height = "45vh")
                             )
            )
  ),
  nav_panel("Add/upload shots", value = "add_upload_tab", h4("Click shot location or upload data"), actionButton("open_upload", "Upload data", class = "btn-secondary"), plotOutput("field_click", click = "field_click", hover = hoverOpts("field_hover"), width = "100%", height = "70vh"), textOutput("field_point")),
  nav_panel("Edit shot data", value = "edit_shot_tab",
            p("Select a row and click 'Edit selected row' to modify data. Use 'Delete selected row' if a shot should be removed."), 
            actionButton("edit_row_btn", "Edit selected row", class = "btn-warning"), # Added Edit Button
            actionButton("delete_shot", "Delete selected row(s)", class = "btn-danger"),
            actionButton("delete_all", "Delete all data", class = "btn-danger"),
            DTOutput("recent_shots")),
  
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
  shots <- reactiveVal(recompute_game_labels(load_user_data(default_user_profile()$email)))
  # Every write to `shots` must go through here so the hidden Game grouping
  # label always reflects the FULL, current dataset (see recompute_game_labels).
  set_shots <- function(new_data) shots(recompute_game_labels(new_data))
  
  # Displays the coach's chosen team name wherever "My Team" used to be
  # hard-coded; falls back to "My Team" if no custom name has been set yet.
  team_display_name <- reactive({
    tn <- profile()$team_name
    if (is.null(tn) || !nzchar(trimws(tn))) "My Team" else tn
  })
  
  # Whether any shot has recorded net coordinates. When FALSE, PSxG is dropped
  # from the Basic stats and Game stats tables since it can't be computed.
  psxg_available <- reactive({
    d <- shots()
    isTRUE(!is.null(d) && nrow(d) > 0 && "PSxG" %in% names(d) && !all(is.na(d$PSxG)))
  })
  
  output$has_data <- reactive({
    isTRUE(!is.null(shots()) && nrow(shots()) > 0)
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  output[["basic-player_filter_ui"]] <- renderUI({
    if (profile()$role != "Player" || isTRUE(profile()$show_player_data)) {
      selectInput("basic-player", "Player", choices = unname(sort(unique(shots()$player))), multiple = TRUE)
    }
  })
  
  output[["advanced-player_filter_ui"]] <- renderUI({
    if (profile()$role != "Player" || isTRUE(profile()$show_player_data)) {
      selectInput("advanced-player", "Player", choices = unname(sort(unique(shots()$player))), multiple = TRUE)
    }
  })
  
  # Handle Player Mode UI Restrictions
  # Players never get access to adding/uploading or editing shots, regardless of
  # the "Show player-level data" setting. Everything else (including the Player
  # stats sub-tab) opens up to players once the coach enables that setting.
  observe({
    if (profile()$role == "Player") {
      nav_hide("main_nav", "add_upload_tab")
      nav_hide("main_nav", "edit_shot_tab")
      if (isTRUE(profile()$show_player_data)) {
        nav_show("basic_tabs", "player_stats_tab")
      } else {
        nav_hide("basic_tabs", "player_stats_tab")
      }
    } else {
      nav_show("main_nav", "add_upload_tab")
      nav_show("main_nav", "edit_shot_tab")
      nav_show("basic_tabs", "player_stats_tab")
    }
  })
  
  observe({
    data <- shots()
    for (id in c("basic", "advanced", "game")) {
      updateSelectInput(session, paste0(id, "-game"), choices = unname(sort(unique(data$Game))))
      updateSelectInput(session, paste0(id, "-year"), choices = unname(sort(unique(stats::na.omit(data$year)))))
      updateSelectInput(session, paste0(id, "-shotType"), choices = unname(sort(unique(data$shotType))))
      updateSelectInput(session, paste0(id, "-assist"), choices = unname(sort(unique(data$typeOfAssist))))
      updateSelectInput(session, paste0(id, "-attack"), choices = unname(sort(unique(data$typeOfAttack))))
    }
    updateSelectInput(session, "single_game", choices = unname(sort(unique(data$Game))))
    updateSelectInput(session, "single_game_year", choices = unname(sort(unique(stats::na.omit(data$year)))))
    updateSelectInput(session, "basic_opponents", choices = unname(sort(unique(stats::na.omit(data$Opponent)))))
    updateSelectInput(session, "player_pie_player", choices = unname(sort(unique(data$player[data$Team == "TEAM"]))))
    
    # Exclude columns that are entirely NA
    valid_cols <- names(data)[sapply(data, function(x) !all(is.na(x)))]
    
    # Filter Pie Charts
    pie_choices <- c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result")
    pie_choices <- pie_choices[pie_choices %in% valid_cols]
    updateSelectInput(session, "team_pie_var", choices = pie_choices)
    updateSelectInput(session, "player_pie_var", choices = pie_choices)
    
    # Filter Analytics Specs
    valid_specs <- analytics_specs %>% filter(column %in% valid_cols)
    test_choices <- setNames(valid_specs$id, valid_specs$label)
    updateSelectInput(session, "off_analytics_test", choices = test_choices)
    updateSelectInput(session, "def_analytics_test", choices = test_choices)
  })
  
  # Shown on Basic stats, Advanced analytics, and Game stats tabs whenever
  # there's no shot data yet, since their tables/plots would otherwise just
  # render blank with no explanation.
  output$no_data_banner <- renderUI({
    if (is.null(shots()) || nrow(shots()) == 0) {
      div(class = "alert alert-warning", role = "alert",
          "No data exists. Upload or enter data in the Add/Upload shots tab.")
    } else {
      NULL
    }
  })
  
  output$user_info <- renderText({
    pr <- profile(); paste("Email:", pr$email, "| Team:", pr$team_name, "| Level:", pr$level, "| Team code:", pr$team_code, "| Role:", pr$role)
  })
  output$signed_in_as <- renderText({ pr <- profile(); paste("Signed in as", if (isTRUE(pr$signed_in)) pr$email else "demo user", "| Team:", pr$team_name, "| Role:", pr$role) })
  
  # ---- Account tab: title + content switch between Sign In/Up and Signed-in/Sign-out ----
  output$account_tab_title <- renderText(if (isTRUE(profile()$signed_in)) "Sign out" else "Sign in / Sign up")
  
  output$account_panel <- renderUI({
    pr <- profile()
    if (isTRUE(pr$signed_in)) {
      card(
        card_header("Signed in"),
        p(strong("Signed in as: "), if (pr$role == "Coach") pr$email else "Player (team code access)"),
        p(strong("Team: "), pr$team_name, " | ", strong("Role: "), pr$role),
        actionButton("sign_out", "Sign out", class = "btn-secondary")
      )
    } else {
      tagList(
        h3("Sign in"),
        fluidRow(
          column(5,
                 card(
                   card_header("Coach Access"), 
                   radioButtons("auth_mode", "Select Action", choices = c("Sign In", "Sign Up"), inline = TRUE),
                   textInput("coach_email", "Email", value = ""), 
                   passwordInput("coach_password", "Password"), 
                   conditionalPanel(
                     condition = "input.auth_mode == 'Sign Up'",
                     passwordInput("coach_password_confirm", "Confirm Password")
                   ),
                   actionButton("coach_submit", "Submit", class = "btn-primary")
                 )
          ),
          column(2,
                 div(style = "display: flex; justify-content: center; align-items: center; height: 100%; min-height: 250px;",
                     h3(strong("OR"))
                 )
          ),
          column(5,
                 card(
                   card_header("Player Access"),
                   textInput("team_code_input", "Team Code", value = ""),
                   actionButton("player_submit", "Sign in with Code", class = "btn-info")
                 )
          )
        ),
        
      )
    }
  })
  
  # ---- User info tab: only rendered/visible for a signed-in coach ----
  output$user_info_panel <- renderUI({
    req(isTRUE(profile()$signed_in), profile()$role == "Coach")
    pr <- profile()
    tagList(
      h3("User info"),
      p(textOutput("user_info")),
      card(
        card_header("Team settings"),
        textInput("team_name", "Team name", value = pr$team_name),
        selectInput("team_level", "Level", choices = c("Youth", "High School", "College/University", "Professional"), selected = pr$level),
        textInput("team_code", "Team code (shared with players)", value = pr$team_code),
        checkboxInput("show_player_data", "Show player-level data to players", value = isTRUE(pr$show_player_data)),
        actionButton("save_profile", "Save", class = "btn-primary")
      )
    )
  })
  
  observe({
    if (isTRUE(profile()$signed_in) && profile()$role == "Coach") {
      nav_show("main_nav", target = "user_info_tab")
    } else {
      nav_hide("main_nav", target = "user_info_tab")
    }
  })
  
  observeEvent(input$coach_submit, {
    req(input$coach_email, input$coach_password)
    
    if (input$auth_mode == "Sign Up") {
      req(input$coach_password_confirm)
      if (input$coach_password != input$coach_password_confirm) {
        showNotification("Passwords do not match.", type = "error")
        return()
      }
    }
    
    con <- tryCatch(get_db_connection(), error = function(e) {
      showNotification(paste("Database Connection Error:", e$message), type = "error")
      return(NULL)
    })
    req(con)
    
    login_email <- trimws(input$coach_email)
    
    if (input$auth_mode == "Sign Up") {
      require_optional_package("bcrypt", "Coach sign up")
      hashed_pw <- bcrypt::hashpw(input$coach_password)
      new_team_code <- generate_unique_team_code("New Team")
      
      query <- "INSERT INTO team_accounts (email, password_hash, team_name, team_level, team_code, show_player_data, shot_data) VALUES ($1, $2, $3, $4, $5, $6, $7)"
      
      tryCatch({
        dbExecute(con, query, params = list(
          login_email, hashed_pw, "New Team", "High School", new_team_code, TRUE, "[]"
        ))
        showNotification("Sign up successful! Please sign in to continue.", type = "message")
        
        # Switch UI back to Sign In and populate credentials
        updateRadioButtons(session, "auth_mode", selected = "Sign In")
        updateTextInput(session, "coach_email", value = login_email)
        updateTextInput(session, "coach_password", value = input$coach_password)
        
      }, error = function(e) {
        if (grepl("unique constraint", tolower(e$message))) {
          showNotification("An account with this email already exists.", type = "error")
        } else {
          showNotification("Error creating account.", type = "error")
        }
      })
      
      dbDisconnect(con)
      return() # Exit early so it doesn't try to log them in automatically
      
    } else {
      # Log In Logic
      query <- "SELECT * FROM team_accounts WHERE email = $1"
      user_record <- dbGetQuery(con, query, params = list(login_email))
      
      if (nrow(user_record) == 0) {
        showNotification("Email not found. Please sign up or use a different email.", type = "error")
      } else {
        require_optional_package("bcrypt", "Coach sign in")
        if (nrow(user_record) == 1 && bcrypt::checkpw(input$coach_password, user_record$password_hash)) {
          pr <- list(
            email = user_record$email,
            team_name = user_record$team_name,
            level = user_record$team_level,
            team_code = user_record$team_code,
            role = "Coach",
            show_player_data = as.logical(user_record$show_player_data),
            signed_in = TRUE
          )
          profile(pr)
          set_shots(load_user_data(user_record$email))
          showNotification("Signed in successfully.", type = "message")
          nav_select("main_nav", "user_info_tab")
        } else {
          showNotification("Invalid password.", type = "error")
        }
      }
    }
    
    dbDisconnect(con)
  })
  
  observeEvent(input$player_submit, {
    req(input$team_code_input)
    login_code <- trimws(input$team_code_input)
    
    con <- tryCatch(get_db_connection(), error = function(e) {
      showNotification(paste("Database Connection Error:", e$message), type = "error")
      return(NULL)
    })
    req(con)
    
    query <- "SELECT * FROM team_accounts WHERE team_code = $1"
    user_record <- dbGetQuery(con, query, params = list(login_code))
    
    if (nrow(user_record) == 1) { 
      pr <- list(
        email = "hidden_from_players", 
        team_name = user_record$team_name,
        level = user_record$team_level,
        team_code = user_record$team_code,
        role = "Player",
        show_player_data = as.logical(user_record$show_player_data),
        signed_in = TRUE
      )
      profile(pr)
      set_shots(load_user_data(user_record$email)) # Fetches actual shots using the coach's real email from the query
      showNotification("Signed in successfully.", type = "message")
      nav_select("main_nav", "account_tab")
    } else {
      showNotification("Invalid team code.", type = "error")
    }
    
    dbDisconnect(con)
  })
  
  observeEvent(input$sign_out, {
    profile(default_user_profile())
    set_shots(load_demo_data())
    nav_select("main_nav", "account_tab")
  })
  
  observeEvent(input$save_profile, {
    req(input$team_name, input$team_level)
    pr_current <- profile()
    
    # If the user typed their own code, it must be at least 4 characters
    # (letters/numbers, after stripping spaces/hyphens/etc.). If they left it
    # blank, auto-generate one from the first 6 letters of the team name,
    # appending 2, 3, 4, ... if that code is already taken by another team.
    typed_code <- clean_team_code(input$team_code)
    
    if (nzchar(typed_code)) {
      if (nchar(typed_code) < 4) {
        showNotification("Team code must be at least 4 letters.", type = "error")
        return()
      }
      final_team_code <- typed_code
    } else {
      final_team_code <- generate_unique_team_code(input$team_name, exclude_email = pr_current$email)
    }
    
    pr <- modifyList(pr_current, list(
      team_name = input$team_name, 
      level = input$team_level, 
      team_code = final_team_code, 
      show_player_data = isTRUE(input$show_player_data)
    ))
    
    updateTextInput(session, "team_code", value = final_team_code)
    
    if (isTRUE(pr$signed_in) && pr$role == "Coach") {
      con <- tryCatch(get_db_connection(), error = function(e) {
        showNotification(paste("Database Connection Error:", e$message), type = "error")
        NULL
      })
      if (is.null(con)) return()
      
      # Make sure the code isn't already used by a different team's account
      if (team_code_taken(con, final_team_code, exclude_email = pr$email)) {
        dbDisconnect(con)
        showNotification("That team code is already in use by another team. Please choose a different one.", type = "error")
        return()
      }
      
      query <- "UPDATE team_accounts SET team_name = $1, team_level = $2, team_code = $3, show_player_data = $4 WHERE email = $5"
      save_ok <- tryCatch({
        dbExecute(con, query, params = list(pr$team_name, pr$level, pr$team_code, pr$show_player_data, pr$email))
        TRUE
      }, error = function(e) {
        showNotification(paste("Error saving to database:", e$message), type = "error")
        FALSE
      })
      dbDisconnect(con)
      
      if (isTRUE(save_ok)) {
        profile(pr)
        showNotification("User info saved to database", type = "message")
      }
    } else {
      # Demo mode: reflect the change in this session only, don't persist it
      profile(pr)
      showNotification("Demo mode: changes shown for preview only and will not be saved.", type = "warning", duration = 8)
    }
  })
  
  basic_data <- reactive({
    data <- apply_filters(shots(), input, "basic")
    if (length(input$basic_opponents %||% character()) > 0) {
      data <- data %>% filter(Team == "TEAM" | Opponent %in% input$basic_opponents)
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
    data <- basic_data()
    my_team <- data %>% filter(Team == "TEAM") %>% summarise(
      Side = team_display_name(), Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
      xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE)
    )
    
    if (input$team_stat_view == "Individual Opponent Teams") {
      opponent <- data %>% filter(Team == "OPPONENT") %>% group_by(Side = Opponent) %>% summarise(
        Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
        xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE), .groups = "drop"
      )
    } else {
      opponent <- data %>% filter(Team == "OPPONENT") %>% summarise(
        Side = "Total Opponent", Shots = n(), Goals = sum(goals.x, na.rm = TRUE),
        xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE)
      )
    }
    
    result <- round_xg_columns(bind_rows(my_team, opponent))
    if (!psxg_available()) result <- result %>% select(-any_of("PSxG"))
    
    # Added selection = "single" so we can capture the clicked row.
    datatable(result, selection = "single", options = list(dom = "t"), rownames = FALSE)
  })
  
  output$player_stats <- renderDT({
    req(isTRUE(profile()$show_player_data) || profile()$role == "Coach")
    result <- basic_data() %>% 
      filter(Team == "TEAM") %>% 
      group_by(player) %>% 
      summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE), Difference = Goals - xG, .groups = "drop") %>% 
      round_xg_columns()
    if (!psxg_available()) result <- result %>% select(-any_of("PSxG"))
    datatable(result, selection = "single", options = list(pageLength = 8, lengthChange = FALSE, scrollX = TRUE))
  })
  
  output$team_pie <- renderPlot({
    req(input$team_pie_var)
    selected_row <- input$team_stats_rows_selected
    
    if (is.null(selected_row)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Select a team from the table above") + theme_void())
    }
    
    data <- basic_data()
    # Recalculate the summary order exactly as it appears in the table
    my_team_summ <- data %>% filter(Team == "TEAM") %>% summarise(Side = team_display_name(), Shots = n())
    
    if (input$team_stat_view == "Individual Opponent Teams") {
      opp_summ <- data %>% filter(Team == "OPPONENT") %>% group_by(Side = Opponent) %>% summarise(Shots = n(), .groups = "drop")
    } else {
      opp_summ <- data %>% filter(Team == "OPPONENT") %>% summarise(Side = "Total Opponent", Shots = n())
    }
    summary_data <- bind_rows(my_team_summ, opp_summ)
    req(selected_row <= nrow(summary_data))
    
    team_label <- summary_data$Side[selected_row]
    
    if (team_label == team_display_name()) {
      p_data <- data %>% filter(Team == "TEAM")
    } else if (team_label == "Total Opponent") {
      p_data <- data %>% filter(Team == "OPPONENT")
    } else {
      p_data <- data %>% filter(Team == "OPPONENT", Opponent == team_label)
    }
    
    var_label <- names(c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result"))[c("shotType", "sideOfAttack", "typeOfAssist", "typeOfAttack", "Result") == input$team_pie_var]
    render_pie_chart(p_data, input$team_pie_var, paste(team_label, "-", var_label %||% input$team_pie_var))
  })
  
  output$player_pie <- renderPlot({
    req(isTRUE(profile()$show_player_data) || profile()$role == "Coach")
    req(input$player_pie_var)
    selected_row <- input$player_stats_rows_selected
    
    if (is.null(selected_row)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Select a player from the table above") + theme_void())
    }
    
    # Retrieve the summarized data in the exact same order as the table
    summary_data <- basic_data() %>% 
      filter(Team == "TEAM") %>% 
      group_by(player) %>% 
      summarise(Shots = n(), .groups = "drop")
    
    selected_player <- summary_data$player[selected_row]
    var_label <- names(c("Shot type" = "shotType", "Attack side" = "sideOfAttack", "Assist type" = "typeOfAssist", "Attack type" = "typeOfAttack", "Result" = "Result"))[c("shotType", "sideOfAttack", "typeOfAssist", "typeOfAttack", "Result") == input$player_pie_var]
    p_data <- basic_data() %>% filter(Team == "TEAM", player == selected_player)
    render_pie_chart(p_data, input$player_pie_var, paste(selected_player, "-", var_label %||% input$player_pie_var))
  })
  
  output$descriptive_stats <- renderDT({
    data <- basic_data()
    stats_list <- list()
    
    if (!all(is.na(data$typeOfAssist))) stats_list <- append(stats_list, list(descriptive_stats(data, "typeOfAssist", "Type of assist")))
    if (!all(is.na(data$typeOfAttack))) stats_list <- append(stats_list, list(descriptive_stats(data, "typeOfAttack", "Type of attack")))
    if (!all(is.na(data$shotType))) stats_list <- append(stats_list, list(descriptive_stats(data, "shotType", "Shot type")))
    
    if (length(stats_list) > 0) {
      bind_rows(stats_list) %>% datatable(options = list(pageLength = 10))
    } else {
      datatable(data.frame(Message = "No descriptive stats available for these selections."), options = list(dom = "t"), rownames = FALSE)
    }
  })
  
  output$offensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() %>% filter(Team == "TEAM"), "Offense (My Team)", is_defense = FALSE))
  output$defensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() %>% filter(Team == "OPPONENT"), "Defense (Opponents)", is_defense = TRUE))
  
  analytics_bundle <- function(team_filter, test_input) {
    req(test_input)
    spec <- analytics_specs %>% filter(id == test_input) %>% slice(1)
    data <- standardize_xg_features(advanced_data() %>% filter(Team == team_filter))
    data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
    clean <- data %>%
      filter(!is.na(.data[[spec$column]]), !.data[[spec$column]] %in% c("", "N/A", "NA", "UNKNOWN"), !is.na(goal))
    
    # FILTER: Require >= 10 shots per grouping variable
    group_counts <- table(clean[[spec$column]])
    valid_groups <- names(group_counts)[group_counts >= 10]
    clean <- clean %>% filter(.data[[spec$column]] %in% valid_groups)
    
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
    game_rates <- data %>% 
      group_by(Game, Group = as.character(.data[[spec$column]])) %>% 
      summarise(Goal_Percentage = 100 * mean(goal, na.rm = TRUE), Shots = n(), .groups = "drop") %>%
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
    wide <- rates %>%
      select(Group, Goal_Percentage, Shots, Goals) %>%
      tidyr::pivot_longer(c(Goal_Percentage, Shots, Goals), names_to = "Metric", values_to = "value") %>%
      tidyr::pivot_wider(names_from = Group, values_from = value)
    datatable(wide, options = list(dom = "t", pageLength = 15), rownames = FALSE)
  })
  output$def_goal_rates <- renderDT({
    rates <- def_bundle()$rates
    if (nrow(rates) == 0) return(datatable(tibble(Note = "No groups available"), options = list(dom = "t"), rownames = FALSE))
    wide <- rates %>%
      select(Group, Goal_Percentage, Shots, Goals) %>%
      tidyr::pivot_longer(c(Goal_Percentage, Shots, Goals), names_to = "Metric", values_to = "value") %>%
      tidyr::pivot_wider(names_from = Group, values_from = value)
    datatable(wide, options = list(dom = "t", pageLength = 15), rownames = FALSE)
  })
  output$off_pairwise <- renderDT({
    pairs <- off_bundle()$pairs
    if (nrow(pairs) == 0) {
      return(datatable(tibble(Note = "Pairwise comparisons shown on plot when 2+ groups are available"), options = list(dom = "t"), rownames = FALSE))
    }
    datatable(
      pairs %>% transmute(Comparison, P_Value = round(p_adj, 4), Significant = p_adj < 0.10),
      options = list(dom = "t", pageLength = 15), rownames = FALSE
    )
  })
  output$def_pairwise <- renderDT({
    pairs <- def_bundle()$pairs
    if (nrow(pairs) == 0) {
      return(datatable(tibble(Note = "Pairwise comparisons shown on plot when 2+ groups are available"), options = list(dom = "t"), rownames = FALSE))
    }
    datatable(
      pairs %>% transmute(Comparison, P_Value = round(p_adj, 4), Significant = p_adj < 0.10),
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
    team_filter <- input$penalty_team_filter %||% team_display_name()
    advanced_data() %>% 
      filter(typeOfAttack == "PENALTY") %>%
      filter(if (identical(team_filter, "Opponent")) Team == "OPPONENT" else Team == "TEAM")
  })
  
  output$penalty_goal_rates <- renderDT({
    data <- penalty_data()
    if(nrow(data) == 0) return(datatable(tibble(Note = "No penalty data"), options = list(dom = "t"), rownames = FALSE))
    
    summary <- data %>%
      summarise(Shots = n(), 
                Goals = sum(Result == "GOAL" | goals.x == 1, na.rm = TRUE),
                Goal_Percentage = round(100 * Goals / Shots, 2))
    datatable(summary, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$penalty_targets_ui <- renderUI({
    data <- penalty_data()
    
    if (all(is.na(data$net_x)) && all(is.na(data$net_y))) {
      h4("No shot location data was provided for penalties")
    } else {
      fluidRow(
        column(6, plotOutput("penalty_grid", height = "40vh")),
        column(6, plotOutput("penalty_heat", height = "40vh"))
      )
    }
  })
  
  output$shooting_targets_ui <- renderUI({
    data <- advanced_data() %>% filter(Team == "TEAM")
    
    # Check if all net_x and net_y values are missing
    if (all(is.na(data$net_x)) && all(is.na(data$net_y))) {
      h4("No shot location data was provided")
    } else {
      fluidRow(
        column(6, plotOutput("shooting_grid", height = "50vh")),
        column(6, plotOutput("shooting_heat", height = "50vh"))
      )
    }
  })
  
  output$gk_targets_ui <- renderUI({
    data <- advanced_data() %>% filter(Team == "OPPONENT")
    
    # Check if all net_x and net_y values are missing
    if (all(is.na(data$net_x)) && all(is.na(data$net_y))) {
      h4("No shot location data was provided")
    } else {
      fluidRow(
        column(6, plotOutput("gk_grid", height = "50vh")),
        column(6, plotOutput("gk_heat", height = "50vh"))
      )
    }
  })
  
  output$shooting_grid <- renderPlot({
    zone_grid_plot(advanced_data() %>% filter(Team == "TEAM"), NULL, "Shooting net distribution",
                   plot_width = session$clientData$output_shooting_grid_width,
                   plot_height = session$clientData$output_shooting_grid_height)
  })
  output$gk_grid <- renderPlot({
    zone_grid_plot(advanced_data() %>% filter(Team == "OPPONENT"), NULL, "Goalkeeper shots faced distribution", is_gk = TRUE,
                   plot_width = session$clientData$output_gk_grid_width,
                   plot_height = session$clientData$output_gk_grid_height)
  })
  
  output$shooting_heat <- renderPlot(net_plot(advanced_data() %>% filter(Team == "TEAM", !is.na(net_x), !is.na(net_y))))
  output$gk_heat <- renderPlot(net_plot(advanced_data() %>% filter(Team == "OPPONENT", !is.na(net_x), !is.na(net_y))))
  
  output$penalty_team_filter_ui <- renderUI({
    selectInput("penalty_team_filter", "Select Team", choices = c(team_display_name(), "Opponent"))
  })
  
  output$penalty_grid <- renderPlot({
    zone_grid_plot(penalty_data(), NULL, title = "Penalty Net Distribution",
                   plot_width = session$clientData$output_penalty_grid_width,
                   plot_height = session$clientData$output_penalty_grid_height)
  })
  output$penalty_heat <- renderPlot({ net_plot(penalty_data() %>% filter(!is.na(net_x), !is.na(net_y))) })
  
  selected_game <- reactive({
    data <- game_data()
    scope <- input$game_chart_scope %||% "all"
    if (identical(scope, "year") && !is.null(input$single_game_year) && nzchar(input$single_game_year)) {
      data <- data %>% filter(year == suppressWarnings(as.integer(input$single_game_year)))
    } else if (identical(scope, "game") && !is.null(input$single_game) && nzchar(input$single_game)) {
      data <- data %>% filter(Game == input$single_game)
    }
    data
  })
  scoreline_text <- reactive({
    data <- selected_game()
    opponent_label <- "Opponent"
    if (identical(input$game_chart_scope %||% "all", "game")) {
      opponents <- unique(stats::na.omit(data$Opponent[data$Team == "OPPONENT"]))
      if (length(opponents) == 1) opponent_label <- opponents[[1]]
    }
    # Stack names and scores vertically to prevent mobile cutoff
    paste0(
      team_display_name(), "\n",
      sum(data$goals.x[data$Team == "TEAM"], na.rm = TRUE),
      " - ",
      sum(data$goals.x[data$Team == "OPPONENT"], na.rm = TRUE), "\n",
      opponent_label
    )
  })
  
  output$shot_chart <- renderPlot({
    chart_data <- selected_game() %>% mutate(row_id = row_number())
    soccer_pitch(chart_data, aes(X, Y)) +
      geom_point(aes(colour = Result, size = goals.y), alpha = .85) +
      geom_point(
        data = chart_data %>% filter(Result == "GOAL", !is.na(net_x), !is.na(net_y)),
        aes(X, Y),
        colour = "#FFD700", size = 5, show.legend = FALSE
      ) +
      scale_colour_manual(values = c(GOAL = "#FFD700", SAVED = "#2C7FB8", MISSED = "#666666", BLOCKED = "#D95F0E"), drop = FALSE) +
      scale_size_continuous(range = c(2.67, 8)) +
      # Updated to accommodate vertically stacked text
      annotate("label", x = FIELD_LENGTH / 2, y = FIELD_WIDTH - 5, label = scoreline_text(), size = 5, fill = "white", lineheight = 0.9) +
      labs(colour = "Result", size = "xG") +
      # Moves legend to bottom to prevent field shrinking on mobile
      theme(legend.position = "bottom", legend.title = element_text(size = 14), legend.text = element_text(size = 14))
  })
  
  clicked_shot_data <- reactive({
    p <- input$shot_chart_click; data <- selected_game() %>% mutate(row_id = row_number()); if (is.null(p) || nrow(data) == 0) return(data[0, ])
    data[which.min((data$X - p$x)^2 + (data$Y - p$y)^2), , drop = FALSE]
  })
  
  output$clicked_net_ui <- renderUI({
    data <- selected_game()
    if (all(is.na(data$net_x)) && all(is.na(data$net_y))) return(NULL)
    plotOutput("clicked_net", width = "100%", height = "35vh")
  })
  output$clicked_net <- renderPlot({
    d <- clicked_shot_data()
    net_plot(point = if (nrow(d) == 1 && !is.na(d$net_x[1]) && !is.na(d$net_y[1])) list(x = d$net_x[1], y = d$net_y[1]) else NULL)
  })
  output$clicked_shot <- renderDT({
    d <- clicked_shot_data() %>% round_shot_display()
    
    if (nrow(d) > 0 && profile()$role == "Player" && !isTRUE(profile()$show_player_data) && "player" %in% names(d)) {
      d$player <- "Anonymous"
    }
    
    d %>%
      rename(xG = goals.y) %>%
      select(any_of(c(
        "Minute", "Result", "player", "Team", "Opponent", 
        "xG", "PSxG", "shotType", "typeOfAssist", 
        "typeOfAttack", "sideOfAttack"
      ))) %>%
      datatable(options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$xg_timeline <- renderPlot({ selected_game() %>% arrange(Minute) %>% mutate(team_xg = cumsum(ifelse(Team == "TEAM", goals.y, 0)), opp_xg = cumsum(ifelse(Team == "OPPONENT", goals.y, 0))) %>% ggplot(aes(Minute)) + geom_step(aes(y = team_xg, colour = "Team")) + geom_step(aes(y = opp_xg, colour = "Opponent")) + labs(y = "Cumulative xG", colour = NULL) })
  
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
      selectInput("team", "Team*", choices = c(team_display_name(), "Opponent")),
      selectInput("home_away", "Home vs away", choices = c("", "HOME", "AWAY")),
      dateInput("game_date", "Game date*", value = Sys.Date()),
      textInput("opponent", "Opponent*", value = "OPPONENT"),
      textInput("player", "Player", value = ""),
      textInput("gk", "Goalkeeper", value = ""),
      selectInput("shot_type", "Shot type", choices = c("RIGHTFOOT", "LEFTFOOT", "HEADER", "RIGHTVOLLEY", "LEFTVOLLEY")),
      selectInput("assist_type", "Type of assist", choices = c("N/A", "PASS", "THROUGHBALL", "CROSSINAIR", "CUTBACK", "OTHER")),
      selectInput("attack_type", "Type of attack", choices = c("POSSESSION", "TRANSITION", "RESTART", "THROW-IN", "PENALTY", "OTHER")),
      selectInput("side_attack", "Side of attack", choices = c("LEFT", "RIGHT", "MIDDLE")),
      selectInput("dom", "Dominant/non-dominant", choices = c("", "DOMINANT", "NONDOMINANT")),
      textInput("notes", "Notes", value = ""), # Added Notes input
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
      tags$p("Uploaded files must include these required columns (case insensitive):"),
      tags$ul(
        tags$li(strong("Minute*: "), "Whole number minute of the game when a shot took place."),
        tags$li(strong("Result*: "), "The outcome of the shot (e.g., GOAL, SAVED, MISSED, BLOCKED)."),
        tags$li(strong("X*: "), "The X coordinate of the shot origin (0 to 120)."),
        tags$li(strong("Y*: "), "The Y coordinate of the shot origin (0 to 75)."),
        tags$li(strong("player: "), "Name of the player who took the shot."),
        tags$li(strong("h_a: "), "Was your team Home or Away (HOME/AWAY)."),
        tags$li(strong("situation: "), "The play situation (e.g., OPEN PLAY, SET PIECE)."),
        tags$li(strong("shotType: "), "The body part/type (e.g., RIGHTFOOT, LEFTFOOT, HEADER)."),
        tags$li(strong("domVSnondom: "), "Was it on their DOMINANT or NONDOMINANT foot."),
        tags$li(strong("Opponent*: "), "The name of the opposing team."),
        tags$li(strong("playerAssist: "), "The name of the player who assisted."),
        tags$li(strong("typeOfAssist: "), "Type of assist (e.g., PASS, CUTBACK, THROUGHBALL)."),
        tags$li(strong("sideOfAttack: "), "Side the attack came from (LEFT, RIGHT, MIDDLE)."),
        tags$li(strong("PossesionWon: "), "Where possession was won."),
        tags$li(strong("typeOfAttack: "), "The build-up type (e.g., POSSESSION, TRANSITION)."),
        tags$li(strong("year*: "), "The year of the season/game."),
        tags$li(strong("sideOfAttackGrouped: "), "Grouped attack side."),
        tags$li(strong("Team*: "), "TEAM or OPPONENT."),
        tags$li(strong("GK: "), "The Goalkeeper's name."),
        tags$li(strong("net_x: "), "X coordinate of where the ball crossed the goal line."),
        tags$li(strong("net_y: "), "Y coordinate of where the ball crossed the goal line."),
        tags$li(strong("Date: "), "Date of the match (e.g., MM/DD/YYYY)."),
        tags$li(strong("Notes: "), "Comments about the goal or shot context.")
      ),
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
    req(profile()$role == "Coach")
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    uploaded <- if (tolower(ext) == "csv") readr::read_csv(input$upload$datapath, show_col_types = FALSE) else readxl::read_excel(input$upload$datapath)
    
    # Case-insensitive required column check
    uploaded_upper <- toupper(names(uploaded))
    minimal_reqs <- c("Minute", "Result", "X", "Y", "Opponent", "year", "team")
    missing_idx <- which(!toupper(minimal_reqs) %in% uploaded_upper)
    
    if (length(missing_idx) > 0) {
      # Determine exactly which columns were missing (in their original casing)
      missing_names <- minimal_reqs[missing_idx]
      showNotification(
        paste("Upload failed. Missing required column(s):", paste(missing_names, collapse = ", ")), 
        type = "error", 
        duration = 10
      )
      return() # Do not upload the data
    }
    
    # Force the names of the uploaded data to match the script's strict casing exactly
    for (col in REQUIRED_UPLOAD_COLUMNS) {
      match_idx <- which(toupper(names(uploaded)) == toupper(col))
      if (length(match_idx) > 0) {
        names(uploaded)[match_idx[1]] <- col
      }
    }
    
    uploaded$datetime_added <- as.character(Sys.time())
    
    set_shots(bind_rows(shots(), standardize_shots(uploaded)))
    save_user_data(shots(), profile()$email, profile()$signed_in)
    
    if (!isTRUE(profile()$signed_in)) {
      showNotification("Demo mode: upload shown for preview only and will not be saved.", type = "warning", duration = 8)
    }
    removeModal()
  })
  
  observeEvent(input$save_shot, {
    req(profile()$role == "Coach")
    req(input$minute, input$result, input$opponent, input$field_click, input$game_date)
    team_value <- ifelse(input$team == "Opponent", "OPPONENT", "TEAM")
    net_pt <- net_click_point()
    new_shot <- tibble(
      Minute = input$minute, Result = input$result, X = input$field_click$x, Y = input$field_click$y,
      player = input$player, h_a = input$home_away, situation = NA, shotType = input$shot_type,
      domVSnondom = input$dom, Opponent = input$opponent, playerAssist = NA, typeOfAssist = input$assist_type,
      sideOfAttack = input$side_attack, PossesionWon = NA, typeOfAttack = input$attack_type,
      year = as.integer(format(input$game_date, "%Y")), Date = input$game_date, sideOfAttackGrouped = NA, Team = team_value,
      GK = input$gk,
      net_x = if (is.null(net_pt)) NA_real_ else net_pt$x,
      net_y = if (is.null(net_pt)) NA_real_ else net_pt$y,
      datetime_added = as.character(Sys.time()),
      Notes = input$notes # Mapped newly added notes variable
    )
    set_shots(bind_rows(shots(), standardize_shots(new_shot)))
    save_user_data(shots(), profile()$email, profile()$signed_in)
    net_click_point(NULL)
    removeModal()
    if (isTRUE(profile()$signed_in)) {
      showNotification("Shot saved", type = "message")
    } else {
      showNotification("Demo mode: shot shown for preview only and will not be saved.", type = "warning", duration = 8)
    }
  })
  
  
  observeEvent(input$recent_shots_cell_edit, {
    req(profile()$role == "Coach")
    info <- input$recent_shots_cell_edit
    display <- shots() %>% 
      select(-any_of(c("", "X.1", "row_id", "zone", "goals.x", "goals.y", "sideOfAttackGrouped", "h_a", "Game", "safe_year", "suffix", "game_id", "safe_opp")))
    col_name <- names(display)[info$col + 1]
    if (is.null(col_name)) return()
    data <- shots()
    data[info$row, col_name] <- DT::coerceValue(info$value, data[[col_name]][info$row])
    set_shots(standardize_shots(data)); save_user_data(shots(), profile()$email, profile()$signed_in)
  })
  
  # Edit Button logic
  observeEvent(input$edit_row_btn, {
    req(profile()$role == "Coach")
    selected <- input$recent_shots_rows_selected
    if (length(selected) != 1) {
      showNotification("Please select exactly one row to edit.", type = "warning")
      return()
    }
    
    row_data <- shots()[selected, ]
    
    showModal(modalDialog(
      title = "Edit Shot",
      fluidRow(
        column(6,
               textInput("edit_opponent", "Opponent", value = row_data$Opponent),
               numericInput("edit_minute", "Minute", value = row_data$Minute),
               selectInput("edit_result", "Result", choices = c("GOAL", "SAVED", "MISSED", "BLOCKED"), selected = row_data$Result),
               numericInput("edit_x", "Field X", value = row_data$X),
               numericInput("edit_y", "Field Y", value = row_data$Y),
               textInput("edit_player", "Player", value = row_data$player),
               selectInput("edit_h_a", "Home/Away", choices = c("", "HOME", "AWAY"), selected = row_data$h_a),
               selectInput("edit_situation", "Situation", choices = c("OPEN PLAY", "SET PIECE", "CORNER", "FREE KICK", "PENALTY", "OTHER"), selected = row_data$situation),
               selectInput("edit_shotType", "Shot type", choices = c("RIGHTFOOT", "LEFTFOOT", "HEADER", "RIGHTVOLLEY", "LEFTVOLLEY", "OTHER"), selected = row_data$shotType),
               selectInput("edit_domVSnondom", "Dom/Non-dom", choices = c("", "DOMINANT", "NONDOMINANT"), selected = row_data$domVSnondom)
        ),
        column(6,
               textInput("edit_playerAssist", "Player Assist", value = row_data$playerAssist),
               selectInput("edit_typeOfAssist", "Type of assist", choices = c("N/A", "PASS", "THROUGHBALL", "CROSSINAIR", "CUTBACK", "OTHER"), selected = row_data$typeOfAssist),
               selectInput("edit_sideOfAttack", "Side of attack", choices = c("LEFT", "RIGHT", "MIDDLE", "OTHER"), selected = row_data$sideOfAttack),
               selectInput("edit_possesionWon", "Possession won", choices = c("DEFENSIVE THIRD", "MIDDLE THIRD", "ATTACKING THIRD", "OTHER"), selected = row_data$PossesionWon),
               selectInput("edit_typeOfAttack", "Type of attack", choices = c("POSSESSION", "TRANSITION", "RESTART", "THROW-IN", "PENALTY", "OTHER"), selected = row_data$typeOfAttack),
               numericInput("edit_year", "Year", value = row_data$year),
               dateInput("edit_date", "Date", value = row_data$Date),
               textInput("edit_gk", "Goalkeeper", value = row_data$GK),
               numericInput("edit_net_x", "Net X", value = row_data$net_x),
               numericInput("edit_net_y", "Net Y", value = row_data$net_y)
        )
      ),
      textInput("edit_notes", "Notes", value = row_data$Notes, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit_btn", "Save Changes", class = "btn-primary")
      ),
      size = "l" # Make modal larger to fit rows nicely
    ))
  })
  
  # Save button logic
  observeEvent(input$save_edit_btn, {
    req(profile()$role == "Coach")
    selected <- input$recent_shots_rows_selected
    data <- shots()
    
    # Inject inputs back into the dataframe row
    data[selected, "Opponent"] <- input$edit_opponent
    data[selected, "Minute"] <- input$edit_minute
    data[selected, "Result"] <- input$edit_result
    data[selected, "X"] <- input$edit_x
    data[selected, "Y"] <- input$edit_y
    data[selected, "player"] <- input$edit_player
    data[selected, "h_a"] <- input$edit_h_a
    data[selected, "situation"] <- input$edit_situation
    data[selected, "shotType"] <- input$edit_shotType
    data[selected, "domVSnondom"] <- input$edit_domVSnondom
    data[selected, "playerAssist"] <- input$edit_playerAssist
    data[selected, "typeOfAssist"] <- input$edit_typeOfAssist
    data[selected, "sideOfAttack"] <- input$edit_sideOfAttack
    data[selected, "PossesionWon"] <- input$edit_possesionWon
    data[selected, "typeOfAttack"] <- input$edit_typeOfAttack
    data[selected, "year"] <- input$edit_year
    data[selected, "Date"] <- input$edit_date
    data[selected, "GK"] <- input$edit_gk
    data[selected, "net_x"] <- input$edit_net_x
    data[selected, "net_y"] <- input$edit_net_y
    data[selected, "Notes"] <- input$edit_notes
    
    set_shots(standardize_shots(data))
    save_user_data(shots(), profile()$email, profile()$signed_in)
    removeModal()
    showNotification("Shot updated successfully", type = "message")
  })
  
  # Updated Datatable
  output$recent_shots <- renderDT({
    req(profile()$role == "Coach")
    shots() %>%
      mutate(row_id = row_number(), .before = 1) %>%
      # Shift 'Date' to the very end of the selector
      select(everything(), -any_of(c("", "X.1", "row_id", "zone", "goals.x", "goals.y", "sideOfAttackGrouped", "h_a", "Game", "safe_year", "suffix", "game_id", "safe_opp")), Date) %>%
      mutate(across(where(is.character), as.factor)) %>%
      round_shot_display() %>%
      datatable(
        selection = "multiple", 
        editable = FALSE, 
        filter = "top",   
        options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE)
      )
  }, server = FALSE)
  
  # Update logic to delete multiple rows
  observeEvent(input$delete_shot, {
    req(profile()$role == "Coach")
    selected <- input$recent_shots_rows_selected
    
    data <- shots()
    
    # Clean indices to prevent NA/NaN out-of-bounds subsetting
    selected <- selected[!is.na(selected) & selected >= 1 & selected <= nrow(data)]
    req(length(selected) >= 1) # Support 1 or more rows securely
    
    data <- data[-selected, , drop = FALSE]
    
    set_shots(standardize_shots(data))
    save_user_data(shots(), profile()$email, profile()$signed_in)
    showNotification("Shot(s) deleted", type = "message")
  })
  
  # Add logic for Delete All confirmation modal
  observeEvent(input$delete_all, {
    req(profile()$role == "Coach")
    showModal(modalDialog(
      title = "WARNING",
      "WARNING: This will delete all data. Do you still wish to continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_all", "Yes", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  # Process actual deletion of all data
  observeEvent(input$confirm_delete_all, {
    removeModal()
    set_shots(empty_shots_frame()) # Wipe data by setting it to the empty frame
    save_user_data(shots(), profile()$email, profile()$signed_in)
    showNotification("All data deleted", type = "message")
  })
}

shinyApp(ui, server)