# ============================================================
# Create fake World Cup results for testing the bolão dashboard
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

root_dir <- getwd()

matches_path <- file.path(root_dir, "data", "matches.csv")

if (!file.exists(matches_path)) {
  stop("Could not find data/matches.csv. Put the full World Cup schedule there first.")
}

dir.create(file.path(root_dir, "data", "results"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

set.seed(2026)

# Change this to test different situations.
# 10  = only first 10 group-stage matches have fake results.
# 72  = full fake group stage, so group rankings and 3rd-place ranking can be tested.
n_group_matches <- 72

# Keep this at 0 for now unless you want to fake knockout games too.
# If you set this to, say, 4, matches 73-76 will get fake results.
n_knockout_matches <- 0

max_goals <- 6

fake_goals <- function(n) {
  pmin(rpois(n, lambda = 1.35), max_goals)
}

# ------------------------------------------------------------
# Read schedule
# ------------------------------------------------------------

matches <- read_csv(matches_path, show_col_types = FALSE)

required_cols <- c("match_id", "stage", "group", "date", "team1", "team2")
missing_cols <- setdiff(required_cols, names(matches))

if (length(missing_cols) > 0) {
  stop("data/matches.csv is missing: ", paste(missing_cols, collapse = ", "))
}

optional_cols <- c("time", "venue", "stadium_name", "city", "country")

for (col in optional_cols) {
  if (!col %in% names(matches)) {
    matches[[col]] <- NA_character_
  }
}

# ------------------------------------------------------------
# Create fake results
# ------------------------------------------------------------

fake_results <- matches %>%
  mutate(
    match_id = as.integer(match_id),
    
    fake_group_played =
      match_id >= 1 &
      match_id <= pmin(n_group_matches, 72),
    
    fake_knockout_played =
      match_id >= 73 &
      match_id < 73 + n_knockout_matches,
    
    played = fake_group_played | fake_knockout_played,
    
    team1_goals = if_else(
      played,
      as.numeric(fake_goals(n())),
      NA_real_
    ),
    
    team2_goals = if_else(
      played,
      as.numeric(fake_goals(n())),
      NA_real_
    )
  ) %>%
  rowwise() %>%
  mutate(
    advance_team = case_when(
      !played ~ NA_character_,
      
      # Group-stage games do not need an advance_team.
      match_id <= 72 ~ NA_character_,
      
      # Knockout games need an advance_team.
      team1_goals > team2_goals ~ team1,
      team2_goals > team1_goals ~ team2,
      
      # If a fake knockout game is tied, randomly choose who advances.
      TRUE ~ sample(c(team1, team2), 1)
    )
  ) %>%
  ungroup() %>%
  mutate(
    source = "fake",
    source_match_id = NA_character_,
    status = if_else(played, "FAKE_FINISHED", "SCHEDULED")
  ) %>%
  select(
    match_id,
    source_match_id,
    source,
    stage,
    group,
    date,
    time,
    venue,
    stadium_name,
    city,
    country,
    team1,
    team2,
    team1_goals,
    team2_goals,
    advance_team,
    status,
    played
  )

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

fake_path <- file.path(root_dir, "data", "results", "fake_actual_results.csv")

write_csv(fake_results, fake_path)

message("Fake results written to: ", fake_path)
message("Fake group-stage matches played: ", sum(fake_results$played & fake_results$match_id <= 72))
message("Fake knockout matches played: ", sum(fake_results$played & fake_results$match_id >= 73))