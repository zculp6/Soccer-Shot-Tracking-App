# Soccer Shot Tracking Shiny App
# Run with: shiny::runApp()

required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "readr", "readxl", "DT", "tidyr", "digest"
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
FIELD_WIDTH <- 80
GOAL_WIDTH <- 8
GOAL_HEIGHT <- 8
REQUIRED_UPLOAD_COLUMNS <- c(
  "zone", "Minute", "Result", "X", "Y", "player", "h_a", "situation", "shotType",
  "domVSnondom", "Opponent", "playerAssist", "typeOfAssist", "sideOfAttack", "PossesionWon",
  "typeOfAttack", "year", "goals.x", "goals.y", "sideOfAttackGrouped"
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
  data
}

soccer_pitch <- function(data = NULL, aes_extra = aes()) {
  ggplot(data, aes_extra) +
    annotate("rect", xmin = 0, xmax = FIELD_LENGTH, ymin = 0, ymax = FIELD_WIDTH, fill = "#4f9f50", colour = "white") +
    annotate("segment", x = FIELD_LENGTH / 2, xend = FIELD_LENGTH / 2, y = 0, yend = FIELD_WIDTH, colour = "white") +
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62, fill = NA, colour = "white") +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white") +
    annotate("point", x = c(12, 108, 60), y = c(40, 40, 40), colour = "white") +
    coord_fixed(xlim = c(0, FIELD_LENGTH), ylim = c(0, FIELD_WIDTH), expand = FALSE) +
    theme_void()
}

filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("game"), "Game", choices = NULL, multiple = TRUE),
    selectInput(ns("player"), "Player", choices = NULL, multiple = TRUE),
    selectInput(ns("shotType"), "Shot type", choices = NULL, multiple = TRUE),
    selectInput(ns("assist"), "Type of assist", choices = NULL, multiple = TRUE),
    selectInput(ns("attack"), "Type of attack", choices = NULL, multiple = TRUE)
  )
}

apply_filters <- function(data, input, id) {
  for (field in c("game", "player", "shotType", "assist", "attack")) {
    value <- input[[paste0(id, "-", field)]]
    if (!is.null(value) && length(value) > 0) {
      column <- c(game = "Game", player = "player", shotType = "shotType", assist = "typeOfAssist", attack = "typeOfAttack")[[field]]
      data <- data[data[[column]] %in% value, , drop = FALSE]
    }
  }
  data
}

ui <- page_navbar(
  title = "Soccer Shot Tracking",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel("Basic stats", layout_sidebar(sidebar = sidebar(filter_ui("basic")),
                                          h4(textOutput("signed_in_as")), DTOutput("team_stats"), DTOutput("player_stats"))),
  nav_panel("Advanced analytics", layout_sidebar(sidebar = sidebar(filter_ui("advanced")),
                                                 h4("ANOVA and t-test summaries"), verbatimTextOutput("anova_output"), plotOutput("analytics_plot"))),
  nav_panel("Game stats", layout_sidebar(sidebar = sidebar(filter_ui("game")),
                                         selectInput("single_game", "Individual game shot chart", choices = NULL),
                                         h3(textOutput("scoreline")), plotOutput("shot_chart"), plotOutput("heat_map"), plotOutput("xg_timeline"))),
  nav_panel("Add/upload shots", layout_sidebar(sidebar = sidebar(
    fileInput("upload", "Upload CSV/XLSX/XLS", accept = c(".csv", ".xlsx", ".xls")),
    helpText("Uploaded files must include these column names: ", paste(REQUIRED_UPLOAD_COLUMNS, collapse = ", ")),
    numericInput("minute", "Minute*", value = 1, min = 0, max = 130),
    selectInput("result", "Result*", choices = SHOT_RESULTS),
    selectInput("home_away", "Home vs away*", choices = c("HOME", "AWAY")),
    textInput("opponent", "Opponent*", value = "OPPONENT"),
    textInput("player", "Player", value = "PLAYER 1"),
    selectInput("shot_type", "Shot type", choices = c("RIGHTFOOT", "LEFTFOOT", "HEADER", "RIGHTVOLLEY", "LEFTVOLLEY")),
    selectInput("assist_type", "Type of assist", choices = c("N/A", "PASS", "THROUGHBALL", "CROSSINAIR", "CUTBACK", "OTHER")),
    selectInput("attack_type", "Type of attack", choices = c("POSSESSION", "TRANSITION", "RESTART", "THROW-IN", "PENALTY", "OTHER")),
    selectInput("side_attack", "Side of attack", choices = c("LEFT", "RIGHT", "MIDDLE")),
    selectInput("dom", "Dominant/non-dominant", choices = c("", "DOM", "NONDOM")),
    actionButton("blocked", "Blocked shot"), actionButton("skip_net", "Skip net location"), actionButton("save_shot", "Save shot", class = "btn-primary")
  ),
  h4("Click shot location"), plotOutput("field_click", click = "field_click"),
  textOutput("field_point"), h4("Click where the shot ended in the net"), plotOutput("net_click", click = "net_click"), textOutput("net_point"), DTOutput("recent_shots")))
)

server <- function(input, output, session) {
  email <- current_user()
  shots <- reactiveVal(load_user_data(email))
  observe({
    data <- shots()
    for (id in c("basic", "advanced", "game")) {
      updateSelectInput(session, paste0(id, "-game"), choices = sort(unique(data$Game)))
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
    for_stats <- data |> filter(Team == "TEAM") |> summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE))
    against_stats <- data |> filter(Team != "TEAM") |> summarise(Shots_Against = n(), Goals_Against = sum(goals.x, na.rm = TRUE), xG_Against = sum(goals.y, na.rm = TRUE))
    datatable(bind_cols(for_stats, against_stats), options = list(dom = "t"))
  })
  output$player_stats <- renderDT({
    basic_data() |> group_by(player) |> summarise(Shots = n(), Goals = sum(goals.x, na.rm = TRUE), xG = sum(goals.y, na.rm = TRUE), Difference = Goals - xG, .groups = "drop") |> datatable()
  })
  output$anova_output <- renderPrint(run_advanced_tests(advanced_data()))
  output$analytics_plot <- renderPlot({
    advanced_data() |> count(typeOfAssist, Result) |> ggplot(aes(typeOfAssist, n, fill = Result)) + geom_col(position = "dodge") + coord_flip() + labs(x = "Assist type", y = "Shots")
  })
  selected_game <- reactive(if (!is.null(input$single_game) && nzchar(input$single_game)) filter(game_data(), Game == input$single_game) else game_data())
  output$scoreline <- renderText({
    data <- selected_game(); paste0("TEAM ", sum(data$goals.x[data$Team == "TEAM"], na.rm = TRUE), " - ", sum(data$goals.x[data$Team != "TEAM"], na.rm = TRUE), " OPPONENT")
  })
  output$shot_chart <- renderPlot(soccer_pitch(selected_game(), aes(X, Y, colour = Result, size = goals.y)) + geom_point(alpha = .85) + labs(colour = "Result", size = "xG"))
  output$heat_map <- renderPlot(soccer_pitch(selected_game(), aes(X, Y)) + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = .55) + scale_fill_viridis_c())
  output$xg_timeline <- renderPlot({ selected_game() |> arrange(Minute) |> mutate(team_xg = cumsum(ifelse(Team == "TEAM", goals.y, 0)), opp_xg = cumsum(ifelse(Team != "TEAM", goals.y, 0))) |> ggplot(aes(Minute)) + geom_step(aes(y = team_xg, colour = "Team")) + geom_step(aes(y = opp_xg, colour = "Opponent")) + labs(y = "Cumulative xG", colour = NULL) })
  output$field_click <- renderPlot(soccer_pitch())
  output$net_click <- renderPlot(ggplot() + annotate("rect", xmin = -GOAL_WIDTH / 2, xmax = GOAL_WIDTH / 2, ymin = 0, ymax = GOAL_HEIGHT, fill = "grey95", colour = "black") + coord_fixed(xlim = c(-5, 5), ylim = c(0, 9)) + theme_minimal() + labs(x = "Net width", y = "Net height"))
  output$field_point <- renderText({ p <- input$field_click; if (is.null(p)) "No field point selected" else sprintf("Shot: %.1f, %.1f", p$x, p$y) })
  output$net_point <- renderText({ p <- input$net_click; if (is.null(p)) "No net point selected" else sprintf("Net: %.1f, %.1f", p$x, p$y) })
  observeEvent(input$blocked, updateSelectInput(session, "result", selected = "BLOCKED"))
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$name)
    uploaded <- if (tolower(ext) == "csv") readr::read_csv(input$upload$datapath, show_col_types = FALSE) else readxl::read_excel(input$upload$datapath)
    missing <- setdiff(REQUIRED_UPLOAD_COLUMNS, names(uploaded))
    if (length(missing) > 0) showNotification(paste("Warning: missing columns", paste(missing, collapse = ", ")), type = "warning", duration = 10)
    shots(bind_rows(shots(), standardize_shots(uploaded))); save_user_data(shots(), email)
  })
  observeEvent(input$save_shot, {
    req(input$minute, input$result, input$home_away, input$opponent, input$field_click)
    new_shot <- tibble(zone = NA, Minute = input$minute, Result = input$result, X = input$field_click$x, Y = input$field_click$y, player = input$player, h_a = input$home_away, situation = NA, shotType = input$shot_type, domVSnondom = input$dom, Opponent = input$opponent, playerAssist = NA, typeOfAssist = input$assist_type, sideOfAttack = input$side_attack, PossesionWon = NA, typeOfAttack = input$attack_type, year = as.integer(format(Sys.Date(), "%Y")), goals.x = ifelse(input$result == "GOAL", 1, 0), goals.y = NA, sideOfAttackGrouped = NA, Team = "TEAM", Game = paste(format(Sys.Date(), "%Y"), input$opponent, input$home_away, sep = " - "), net_x = input$net_click$x %||% NA_real_, net_y = input$net_click$y %||% NA_real_)
    shots(bind_rows(shots(), standardize_shots(new_shot))); save_user_data(shots(), email); showNotification("Shot saved", type = "message")
  })
  output$recent_shots <- renderDT(tail(shots(), 10) |> datatable())
}

shinyApp(ui, server)