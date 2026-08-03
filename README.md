# Soccer Shot Tracking App

An R Shiny application for exploring soccer shot data, adding new shots from an interactive field/net UI, and training an updated expected-goals (xG) model as more data is collected.

## Features

- **Basic stats**: team shots/goals/xG for and against, plus player-level goals, xG, and goal-minus-xG difference.
- **Advanced analytics**: ANOVA and t-test summaries for assist type, side of attack, type of attack, and dominant vs. non-dominant foot effects.
- **Game stats**: filterable shot charts, shot heat maps, cumulative xG timelines, and an individual-game scoreline.
- **Shot entry**: click a soccer field to set shot location, optionally click a goal mouth to set where the shot ended, mark blocked shots, and save required shot details.
- **Uploads**: accepts `.csv`, `.xlsx`, and `.xls` files and warns when expected columns are missing.
- **User isolation**: saves each signed-in user's shots to a separate hashed CSV in `user_data/`.
- **Modeling**: `xg_model.R` contains reusable cross-validation training and prediction helpers for xG model updates.

## Demo data

`XGStats.csv` is loaded as demo data. The app anonymizes it on load by replacing the original team/opponent context with `Team` and `Opponent` labels and remapping player names to `Player 1`, `Player 2`, etc.

## Required upload columns

Uploaded files should match these column names:

```text
zone, Minute, Result, X, Y, player, h_a, situation, shotType, domVSnondom, Opponent, playerAssist, typeOfAssist, sideOfAttack, PossesionWon, typeOfAttack, year, goals.x, goals.y, sideOfAttackGrouped
```

## Running locally

Install the app packages:

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "readr", "readxl", "DT", "tidyr", "digest"))
```

Optional model-training packages:

```r
install.packages(c("caret", "pROC", "randomForest"))
```

Start the app:

```r
shiny::runApp()
```

## Google sign-in

In production, put the app behind Google OAuth (for example Posit Connect, ShinyProxy, or `googleAuthR`) and expose the verified user email as `SHINY_GOOGLE_USER_EMAIL`. The app uses that email to load/save only that user's shot file. Without this environment variable, local development uses `demo.user@example.com`.

## xG model updates

`xg_model.R` provides:

- `train_xg_model(data, folds = 5, seed = 42, model_path = "xg_model.rds")`
- `predict_xg(data, model_path = "xg_model.rds", fallback = NULL)`
- `run_advanced_tests(data)`

To retrain manually:

```r
library(readr)
source("xg_model.R")
shots <- read_csv("XGStats.csv", show_col_types = FALSE)
train_xg_model(shots, folds = 5, model_path = "xg_model.rds")
```

Schedule that command externally at **3:00 AM Eastern** (cron, Posit Connect scheduler, GitHub Actions, or a server task scheduler). The Shiny app automatically uses `xg_model.rds` for predictions when the file exists and falls back to a distance/angle heuristic when it does not.

A helper is included for schedulers that run more frequently but should only retrain at 3:00 AM Eastern and only when the shot count has increased:

```r
source("xg_model.R")
retrain_xg_if_new_shots("XGStats.csv")
