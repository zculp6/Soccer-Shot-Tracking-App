# Soccer Shot Tracking Shiny App
# Run with: shiny::runApp()

required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "readr", "readxl", "DT", "tidyr", "digest", "scales"
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
  data$Team <- ifelse(data$player == "OPP", "OPPONENT", "TEAM")
  if ("Opponent" %in% names(data)) {
    opponents <- sort(unique(stats::na.omit(data$Opponent)))
    opponent_map <- setNames(paste("OPPONENT", seq_along(opponents)), opponents)
    data$Opponent <- unname(opponent_map[data$Opponent])
  }
  if ("player" %in% names(data)) {
    players <- sort(unique(stats::na.omit(data$player)))
    player_map <- setNames(paste("PLAYER", seq_along(players)), players)
    data$player <- unname(player_map[data$player])
  }
  data$Game <- paste(data$year %||% "UNKNOWN", data$Opponent %||% "OPPONENT", data$h_a %||% "", sep = " - ")
  data
}

load_demo_data <- function() {
  readr::read_csv(DATA_PATH, show_col_types = FALSE) |>
    anonymize_demo_data() |>
    standardize_shots()
}

current_user <- function() {
  # Production Google sign-in is enabled by setting SHINY_GOOGLE_USER_EMAIL after OAuth
  # middleware/proxy validation (for example shinyproxy, posit connect, or googleAuthR).
  Sys.getenv("SHINY_GOOGLE_USER_EMAIL", unset = "demo.user@example.com")
}

user_file <- function(email) {
  dir.create(USER_DATA_DIR, showWarnings = FALSE)
  file.path(USER_DATA_DIR, paste0(digest::digest(email, algo = "sha256"), ".csv"))
}

load_user_data <- function(email) {
  path <- user_file(email)
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) |> standardize_shots() else load_demo_data()
}

save_user_data <- function(data, email) {
  readr::write_csv(data, user_file(email), na = "")
}

standardize_shots <- function(data) {
  data <- as.data.frame(data)
  for (col in REQUIRED_UPLOAD_COLUMNS) if (!col %in% names(data)) data[[col]] <- NA
  data[] <- lapply(data, clean_text)
  data$Minute <- suppressWarnings(as.numeric(data$Minute))
  data$X <- suppressWarnings(as.numeric(data$X))
  data$Y <- suppressWarnings(as.numeric(data$Y))
  data$year <- suppressWarnings(as.integer(data$year))
  data$goals.x <- ifelse(data$Result == "GOAL", 1, suppressWarnings(as.numeric(data$goals.x %||% 0)))
  data$sideOfAttackGrouped <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", data$sideOfAttack)
  data$goals.y <- predict_xg(data, fallback = suppressWarnings(as.numeric(data$goals.y)))
  data$PSxG <- predict_psxg(data, fallback = suppressWarnings(as.numeric(data$PSxG)))
  data
}

soccer_pitch <- function(data = NULL, aes_extra = aes()) {
  ggplot(data, aes_extra) +
    annotate("rect", xmin = 0, xmax = FIELD_LENGTH, ymin = 0, ymax = FIELD_WIDTH, fill = "#4f9f50", colour = "white") +
    annotate("segment", x = FIELD_LENGTH / 2, xend = FIELD_LENGTH / 2, y = 0, yend = FIELD_WIDTH, colour = "white") +
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
  data |> mutate(across(any_of(c("xG", "xG_Against", "PSxG", "PSxG_Against", "Difference")), ~ round(.x, 3)))
}

descriptive_stats <- function(data, column, label) {
  data |>
    mutate(value = ifelse(is.na(.data[[column]]) | .data[[column]] == "", "UNKNOWN", .data[[column]])) |>
    count(value, name = "Shots") |>
    mutate(Category = label, Percent = round(100 * Shots / sum(Shots), 1)) |>
    select(Category, Type = value, Shots, Percent)
}

coach_statement <- function(data, group_col, label) {
  summary <- data |>
    filter(!is.na(.data[[group_col]]), .data[[group_col]] != "") |>
    group_by(.data[[group_col]]) |>
    summarise(Shots = n(), Goal_Rate = mean(goal, na.rm = TRUE), .groups = "drop")
  if (nrow(summary) < 2) return(paste(label, "needs more variety before drawing a coaching takeaway."))
  best <- summary[[group_col]][which.max(summary$Goal_Rate)]
  paste(label, "shows the strongest finishing pattern from", best, "shots. Use the chart to guide training emphasis.")
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

advanced_summary <- function(data) {
  data <- standardize_xg_features(data)
  data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
  tests <- run_advanced_tests(data)
  bind_rows(lapply(seq_len(nrow(analytics_specs)), function(i) {
    spec <- analytics_specs[i, ]
    p_value <- extract_p_value(tests[[spec$id]])
    groups <- data |>
      filter(!is.na(.data[[spec$column]]), .data[[spec$column]] != "") |>
      group_by(Group = .data[[spec$column]]) |>
      summarise(Shots = n(), Goal_Percentage = mean(goal, na.rm = TRUE), .groups = "drop")
    top <- if (nrow(groups) > 0) groups$Group[which.max(groups$Goal_Percentage)] else NA_character_
    tibble(Test = spec$label, Test_ID = spec$id, Type = spec$test_type, P_Value = p_value, Significant = !is.na(p_value) & p_value < 0.1, Top_Group = top)
  }))
}

coach_takeaway_ui <- function(data, perspective) {
  summary <- advanced_summary(data) |> filter(Significant)
  if (nrow(summary) == 0) return(tags$p("No significant findings at p < 0.10 for the current filters."))
  tags$ul(lapply(seq_len(nrow(summary)), function(i) {
    tags$li(sprintf("%s: %s is significant (p = %.3f). Highest observed goal percentage: %s.", perspective, summary$Test[i], summary$P_Value[i], summary$Top_Group[i] %||% "N/A"))
  }))
}

net_plot <- function() {
  v_lines <- seq(-GOAL_WIDTH / 2, GOAL_WIDTH / 2, by = 2)
  h_lines <- seq(0, GOAL_HEIGHT, by = 1)
  diag_a <- data.frame(x = seq(-GOAL_WIDTH / 2, GOAL_WIDTH / 2 - 4, by = 4))
  ggplot() +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 12, fill = "#0b4f6c", colour = NA, alpha = .95) +
    annotate("rect", xmin = -18, xmax = 18, ymin = -4, ymax = 0, fill = "#56a832", colour = NA) +
    annotate("rect", xmin = -GOAL_WIDTH / 2, xmax = GOAL_WIDTH / 2, ymin = 0, ymax = GOAL_HEIGHT, fill = "#102a43", colour = NA, alpha = .55) +
    annotate("segment", x = v_lines, xend = v_lines, y = 0, yend = GOAL_HEIGHT, colour = "grey85", alpha = .65) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = h_lines, yend = h_lines, colour = "grey85", alpha = .65) +
    geom_segment(data = diag_a, aes(x = x, xend = x + 4, y = 0, yend = GOAL_HEIGHT), colour = "grey85", alpha = .45) +
    geom_segment(data = diag_a, aes(x = x, xend = x + 4, y = GOAL_HEIGHT, yend = 0), colour = "grey85", alpha = .45) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = -GOAL_WIDTH / 2, y = 0, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("segment", x = GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = 0, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("segment", x = -GOAL_WIDTH / 2, xend = GOAL_WIDTH / 2, y = GOAL_HEIGHT, yend = GOAL_HEIGHT, colour = "white", linewidth = 2) +
    annotate("point", x = c(-GOAL_WIDTH / 2, GOAL_WIDTH / 2), y = c(0, 0), colour = "white", size = 5) +
    annotate("text", x = 0, y = -2.5, label = "Click misses outside the frame", colour = "white", size = 4) +
    coord_fixed(xlim = c(-18, 18), ylim = c(-4, 12), expand = FALSE) +
    theme_void() + labs(x = "Net width (yds)", y = "Net height (yds)")
}

ui <- page_navbar(
  title = "Soccer Shot Tracking",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel("Basic stats", layout_sidebar(sidebar = sidebar(filter_ui("basic"), title = "Filters", open = FALSE),
                                          h4(textOutput("signed_in_as")), h3("Basic Team Stats"), DTOutput("team_stats"), h3("Basic Player Stats"), DTOutput("player_stats"),
                                          h4("Descriptive shot breakdowns"), DTOutput("descriptive_stats"))),
  nav_panel("Advanced analytics", layout_sidebar(sidebar = sidebar(filter_ui("advanced"), title = "Filters", open = FALSE),
                                                 h4("Coach-ready takeaways"),
                                                 navset_tab(
                                                   nav_panel("Offensive", uiOutput("offensive_takeaways")),
                                                   nav_panel("Defensive", uiOutput("defensive_takeaways"))
                                                 ),
                                                 selectInput("analytics_test", "Analytics test", choices = NULL),
                                                 plotOutput("analytics_plot", width = "100%", height = "55vh"),
                                                 DTOutput("analytics_results"))),
  nav_panel("Game stats", layout_sidebar(sidebar = sidebar(filter_ui("game"), title = "Filters", open = FALSE),
                                         selectInput("single_game", "Individual game shot chart", choices = NULL),
                                         plotOutput("shot_chart", width = "100%", height = "60vh"),
                                         plotOutput("heat_map", width = "100%", height = "60vh"),
                                         plotOutput("xg_timeline", width = "100%", height = "45vh"))),
  nav_panel("Add/upload shots",
            h4("Click shot location or upload data"),
            actionButton("open_upload", "Upload data", class = "btn-secondary"),
            plotOutput("field_click", click = "field_click", hover = hoverOpts("field_hover"), width = "100%", height = "70vh"),
            textOutput("field_point")),
  nav_panel("Edit shot data",
            p("Select a row to edit and double click the input to change it. Use Delete selected row if a shot should be removed."),
            actionButton("delete_shot", "Delete selected row", class = "btn-danger"),
            DTOutput("recent_shots"))
)

server <- function(input, output, session) {
  email <- current_user()
  shots <- reactiveVal(load_user_data(email))
  observe({
    data <- shots()
    for (id in c("basic", "advanced", "game")) {
      updateSelectInput(session, paste0(id, "-game"), choices = sort(unique(data$Game)))
      updateSelectInput(session, paste0(id, "-year"), choices = sort(unique(stats::na.omit(data$year))))
      updateSelectInput(session, paste0(id, "-player"), choices = sort(unique(data$player)))
      updateSelectInput(session, paste0(id, "-shotType"), choices = sort(unique(data$shotType)))
      updateSelectInput(session, paste0(id, "-assist"), choices = sort(unique(data$typeOfAssist)))
      updateSelectInput(session, paste0(id, "-attack"), choices = sort(unique(data$typeOfAttack)))
    }
    updateSelectInput(session, "single_game", choices = sort(unique(data$Game)))
  })
  output$signed_in_as <- renderText(paste("Signed in as", email))
  basic_data <- reactive(apply_filters(shots(), input, "basic"))
  advanced_data <- reactive(apply_filters(shots(), input, "advanced"))
  game_data <- reactive(apply_filters(shots(), input, "game"))
  
  output$team_stats <- renderDT({
    data <- basic_data()
    for_stats <- data |> filter(Team == "TEAM") |> summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE))
    against_stats <- data |> filter(Team != "TEAM") |> summarise(Shots_Against = n(), Goals_Against = sum(goals.x, na.rm = TRUE), xG_Against = sum(goals.y, na.rm = TRUE), PSxG_Against = sum(PSxG, na.rm = TRUE))
    datatable(round_xg_columns(bind_cols(for_stats, against_stats)), options = list(dom = "t"))
  })
  output$player_stats <- renderDT({
    basic_data() |> group_by(player) |> summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE), PSxG = sum(PSxG, na.rm = TRUE), Difference = Goals - xG, .groups = "drop") |> round_xg_columns() |> datatable()
  })
  output$descriptive_stats <- renderDT({
    data <- basic_data()
    bind_rows(
      descriptive_stats(data, "typeOfAssist", "Type of assist"),
      descriptive_stats(data, "typeOfAttack", "Type of attack"),
      descriptive_stats(data, "shotType", "Shot type")
    ) |> datatable(options = list(pageLength = 10))
  })
  observe({
    updateSelectInput(session, "analytics_test", choices = setNames(analytics_specs$id, analytics_specs$label), selected = analytics_specs$id[1])
  })
  output$offensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() |> filter(Team == "TEAM"), "Offense"))
  output$defensive_takeaways <- renderUI(coach_takeaway_ui(advanced_data() |> filter(Team != "TEAM"), "Defense"))
  output$analytics_results <- renderDT({
    advanced_summary(advanced_data()) |>
      mutate(P_Value = round(P_Value, 4)) |>
      select(Test, Type, P_Value, Significant, Top_Group) |>
      datatable(options = list(pageLength = 10))
  })
  output$analytics_plot <- renderPlot({
    req(input$analytics_test)
    spec <- analytics_specs |> filter(id == input$analytics_test) |> slice(1)
    data <- standardize_xg_features(advanced_data())
    data$side_group <- ifelse(data$sideOfAttack %in% c("LEFT", "RIGHT"), "SIDE", as.character(data$sideOfAttack))
    data |>
      filter(!is.na(.data[[spec$column]]), .data[[spec$column]] != "") |>
      ggplot(aes(x = .data[[spec$column]], fill = factor(goal))) +
      geom_bar(position = "fill") + coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = spec$label, y = "Goal percentage", fill = "Goal")
  })
  selected_game <- reactive(if (!is.null(input$single_game) && nzchar(input$single_game)) filter(game_data(), Game == input$single_game) else game_data())
  scoreline_text <- reactive({
    data <- selected_game(); paste0("TEAM ", sum(data$goals.x[data$Team == "TEAM"], na.rm = TRUE), " - ", sum(data$goals.x[data$Team != "TEAM"], na.rm = TRUE), " OPPONENT")
  })
  output$shot_chart <- renderPlot(soccer_pitch(selected_game(), aes(X, Y, colour = Result, size = goals.y)) + geom_point(alpha = .85) + annotate("label", x = FIELD_LENGTH / 2, y = FIELD_WIDTH - 4, label = scoreline_text(), size = 6, fill = "white") + labs(colour = "Result", size = "xG"))
  output$heat_map <- renderPlot(soccer_pitch(selected_game(), aes(X, Y)) + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = .55) + scale_fill_viridis_c())
  output$xg_timeline <- renderPlot({ selected_game() |> arrange(Minute) |> mutate(team_xg = cumsum(ifelse(Team == "TEAM", goals.y, 0)), opp_xg = cumsum(ifelse(Team != "TEAM", goals.y, 0))) |> ggplot(aes(Minute)) + geom_step(aes(y = team_xg, colour = "Team")) + geom_step(aes(y = opp_xg, colour = "Opponent")) + labs(y = "Cumulative xG", colour = NULL) })
  output$field_click <- renderPlot(soccer_pitch())
  output$net_click <- output$net_click <- renderPlot(net_plot())
  output$field_point <- renderText({ p <- input$field_click; if (is.null(p)) "No field point selected" else sprintf("Shot: %.1f, %.1f", p$x, p$y) })
  output$net_point <- renderText({ p <- input$net_click; if (is.null(p)) "No net point selected" else sprintf("Net: %.1f, %.1f", p$x, p$y) })
  shot_form <- function() {
    tagList(
      numericInput("minute", "Minute*", value = 1, min = 0, max = 130),
      selectInput("result", "Result*", choices = SHOT_RESULTS),
      selectInput("team", "Team*", choices = c("My Team", "Opponent")),
      selectInput("home_away", "Home vs away", choices = c("", "HOME", "AWAY")),
      textInput("opponent", "Opponent*", value = "OPPONENT"),
      textInput("player", "Player", value = "PLAYER 1"),
      textInput("gk", "Goalkeeper", value = "GK 1"),
      selectInput("shot_type", "Shot type", choices = c("RIGHTFOOT", "LEFTFOOT", "HEADER", "RIGHTVOLLEY", "LEFTVOLLEY")),
      selectInput("assist_type", "Type of assist", choices = c("N/A", "PASS", "THROUGHBALL", "CROSSINAIR", "CUTBACK", "OTHER")),
      selectInput("attack_type", "Type of attack", choices = c("POSSESSION", "TRANSITION", "RESTART", "THROW-IN", "PENALTY", "OTHER")),
      selectInput("side_attack", "Side of attack", choices = c("LEFT", "RIGHT", "MIDDLE")),
      selectInput("dom", "Dominant/non-dominant", choices = c("", "DOM", "NONDOM")),
      actionButton("blocked", "Blocked shot")
    )
  }
  observeEvent(input$field_click, {
    showModal(modalDialog(
      title = "Shot net location",
      p("Click where the shot ended in the net, or skip this step."),
      plotOutput("net_click", click = "net_click", hover = hoverOpts("net_hover"), width = "100%", height = "55vh"),
      textOutput("net_point"), shot_form(),
      footer = tagList(modalButton("Cancel"), actionButton("skip_net", "Skip net location"), actionButton("save_shot", "Save shot", class = "btn-primary")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input$open_upload, {
    showModal(modalDialog(
      title = "Upload shot data",
      fileInput("upload", "Upload CSV/XLSX/XLS", accept = c(".csv", ".xlsx", ".xls")),
      helpText("Uploaded files should include these column names: ", paste(REQUIRED_UPLOAD_COLUMNS, collapse = ", ")),
      footer = modalButton("Close"), easyClose = TRUE
    ))
  })
  observeEvent(input$blocked, updateSelectInput(session, "result", selected = "BLOCKED"))
  observeEvent(input$skip_net, { removeModal(); showModal(modalDialog(title = "Shot information", shot_form(), footer = tagList(modalButton("Cancel"), actionButton("save_shot", "Save shot", class = "btn-primary")), easyClose = TRUE)) })
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$name)
    uploaded <- if (tolower(ext) == "csv") readr::read_csv(input$upload$datapath, show_col_types = FALSE) else readxl::read_excel(input$upload$datapath)
    missing <- setdiff(REQUIRED_UPLOAD_COLUMNS, names(uploaded))
    if (length(missing) > 0) showNotification(paste("Warning: missing columns", paste(missing, collapse = ", ")), type = "warning", duration = 10)
    shots(bind_rows(shots(), standardize_shots(uploaded))); save_user_data(shots(), email); removeModal()
  })
  observeEvent(input$save_shot, {
    req(input$minute, input$result, input$opponent, input$field_click)
    team_value <- ifelse(input$team == "Opponent", "OPPONENT", "TEAM")
    game_label <- paste(format(Sys.Date(), "%Y"), input$opponent, input$home_away %||% "", sep = " - ")
    new_shot <- tibble(Minute = input$minute, Result = input$result, X = input$field_click$x, Y = input$field_click$y, player = input$player, h_a = input$home_away, situation = NA, shotType = input$shot_type, domVSnondom = input$dom, Opponent = input$opponent, playerAssist = NA, typeOfAssist = input$assist_type, sideOfAttack = input$side_attack, PossesionWon = NA, typeOfAttack = input$attack_type, year = as.integer(format(Sys.Date(), "%Y")), sideOfAttackGrouped = NA, Team = team_value, GK = input$gk, Game = game_label, net_x = input$net_click$x %||% NA_real_, net_y = input$net_click$y %||% NA_real_)
    shots(bind_rows(shots(), standardize_shots(new_shot))); save_user_data(shots(), email); removeModal(); showNotification("Shot saved", type = "message")
  })
  output$recent_shots <- renderDT({
    shots() |> mutate(row_id = row_number(), .before = 1) |> select(-any_of(c("zone"))) |> datatable(selection = "single", editable = TRUE, options = list(pageLength = 25, scrollX = TRUE))
  }, server = FALSE)
  observeEvent(input$recent_shots_cell_edit, {
    info <- input$recent_shots_cell_edit
    data <- shots() |> mutate(row_id = row_number(), .before = 1)
    col_name <- names(data)[info$col + 1]
    if (col_name == "row_id") return()
    data[info$row, col_name] <- DT::coerceValue(info$value, data[info$row, col_name])
    data$row_id <- NULL
    shots(standardize_shots(data)); save_user_data(shots(), email)
  })
  observeEvent(input$delete_shot, {
    selected <- input$recent_shots_rows_selected
    req(length(selected) == 1)
    data <- shots()
    data <- data[-selected, , drop = FALSE]
    shots(standardize_shots(data)); save_user_data(shots(), email); showNotification("Shot deleted", type = "message")
  })
}

shinyApp(ui, server)