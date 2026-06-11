# ============================================================
# Score World Cup Bolão predictions
# ============================================================
# Inputs:
#   1. predictions_input: one prediction CSV, a vector of CSV paths,
#      or a folder containing prediction CSVs.
#   2. results_path: CSV with true results.
#
# Main output:
#   score_bolao(...) returns a list with:
#     - leaderboard
#     - match_points
#     - group_position_points
#     - final_position_points
#     - third_place_ranking_points
#     - actual_group_standings
#     - predicted_group_standings
#     - actual_third_place_ranking
#     - predicted_third_place_ranking
#
# Required packages:
#   install.packages(c("dplyr", "readr", "purrr", "stringr", "tidyr"))
# ============================================================

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# ------------------------------------------------------------
# 1. Scoring parameters
# ------------------------------------------------------------

match_points <- list(
  one_team_score = 1,
  correct_outcome = 2,
  exact_score_component = 3
)

# The exact score total is 3 + 2 = 5.
# The 2026 World Cup has a Round of 32. The weights below are the
# agreed weights for the bolão.
stage_weights <- c(
  group_stage   = 1,
  round_of_32   = 1.25,
  round_of_16   = 1.5,
  quarterfinal  = 2,
  semifinal     = 2.5,
  third_place   = 3,
  final         = 3
)

group_rank_points <- c(
  `1` = 2,
  `2` = 5,
  `3` = 5,
  `4` = 2
)

final_position_points <- c(
  third_place = 25,
  runner_up   = 50,
  champion    = 100
)

third_place_ranking_points_per_correct <- 1

# ------------------------------------------------------------
# 2. Helpers for reading files
# ------------------------------------------------------------

read_prediction_files <- function(predictions_input) {
  if (length(predictions_input) == 1 && dir.exists(predictions_input)) {
    files <- list.files(
      predictions_input,
      pattern = "\\.csv$",
      full.names = TRUE
    )
  } else {
    files <- predictions_input
  }

  if (length(files) == 0) {
    stop("No prediction CSV files found.")
  }

  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("These prediction files do not exist: ", paste(missing_files, collapse = ", "))
  }

  map_dfr(
    files,
    ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
      mutate(prediction_file = basename(.x))
  )
}

as_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_team <- function(x) {
  str_squish(as.character(x))
}

# ------------------------------------------------------------
# 3. Stage mapping
# ------------------------------------------------------------

stage_key_from_match_id <- function(match_id) {
  case_when(
    match_id >= 1   & match_id <= 72  ~ "group_stage",
    match_id >= 73  & match_id <= 88  ~ "round_of_32",
    match_id >= 89  & match_id <= 96  ~ "round_of_16",
    match_id >= 97  & match_id <= 100 ~ "quarterfinal",
    match_id >= 101 & match_id <= 102 ~ "semifinal",
    match_id == 103                  ~ "third_place",
    match_id == 104                  ~ "final",
    TRUE                             ~ NA_character_
  )
}

# ------------------------------------------------------------
# 4. Match-level scoring
# ------------------------------------------------------------

score_match_rows <- function(predictions, results) {
  required_pred <- c(
    "participant", "match_id", "team1", "team2",
    "pred_team1_goals", "pred_team2_goals"
  )

  missing_pred <- setdiff(required_pred, names(predictions))
  if (length(missing_pred) > 0) {
    stop("Prediction file is missing columns: ", paste(missing_pred, collapse = ", "))
  }

  required_results <- c("match_id", "team1_goals", "team2_goals")
  missing_results <- setdiff(required_results, names(results))
  if (length(missing_results) > 0) {
    stop("Results file is missing columns: ", paste(missing_results, collapse = ", "))
  }

  predictions2 <- predictions %>%
    mutate(source = if ("source" %in% names(.)) source else NA_character_) %>%
    filter(!is.na(match_id), is.na(source) | source %in% c("group_stage", "knockout")) %>%
    mutate(
      match_id = as.integer(match_id),
      pred_team1_goals = as_numeric_safe(pred_team1_goals),
      pred_team2_goals = as_numeric_safe(pred_team2_goals),
      stage_key = stage_key_from_match_id(match_id)
    )

  results2 <- results %>%
    mutate(
      match_id = as.integer(match_id),
      team1_goals = as_numeric_safe(team1_goals),
      team2_goals = as_numeric_safe(team2_goals)
    ) %>%
    select(match_id, true_team1_goals = team1_goals, true_team2_goals = team2_goals, everything())

  predictions2 %>%
    left_join(
      results2 %>% select(match_id, true_team1_goals, true_team2_goals),
      by = "match_id"
    ) %>%
    mutate(
      played = !is.na(true_team1_goals) & !is.na(true_team2_goals),

      pred_diff = pred_team1_goals - pred_team2_goals,
      true_diff = true_team1_goals - true_team2_goals,

      pred_outcome = case_when(
        pred_diff > 0 ~ "team1",
        pred_diff < 0 ~ "team2",
        pred_diff == 0 ~ "draw",
        TRUE ~ NA_character_
      ),
      true_outcome = case_when(
        true_diff > 0 ~ "team1",
        true_diff < 0 ~ "team2",
        true_diff == 0 ~ "draw",
        TRUE ~ NA_character_
      ),

      exact_score = played &
        pred_team1_goals == true_team1_goals &
        pred_team2_goals == true_team2_goals,

      correct_outcome = played & pred_outcome == true_outcome,

      one_team_score = played & (
        pred_team1_goals == true_team1_goals |
          pred_team2_goals == true_team2_goals
      ),

      base_points = case_when(
        exact_score ~
          match_points$exact_score_component + match_points$correct_outcome,

        correct_outcome & one_team_score ~
          match_points$correct_outcome + match_points$one_team_score,

        correct_outcome ~
          match_points$correct_outcome,

        one_team_score ~
          match_points$one_team_score,

        TRUE ~ 0
      ),

      stage_weight = unname(stage_weights[stage_key]),
      stage_weight = if_else(is.na(stage_weight), 1, stage_weight),
      match_points = base_points * stage_weight
    )
}

# ------------------------------------------------------------
# 5. Group standings
# ------------------------------------------------------------

compute_standings <- function(match_df, g1_col, g2_col) {
  g1_col <- rlang::ensym(g1_col)
  g2_col <- rlang::ensym(g2_col)

  long1 <- match_df %>%
    transmute(
      group,
      team = clean_team(team1),
      gf = as_numeric_safe(!!g1_col),
      ga = as_numeric_safe(!!g2_col)
    )

  long2 <- match_df %>%
    transmute(
      group,
      team = clean_team(team2),
      gf = as_numeric_safe(!!g2_col),
      ga = as_numeric_safe(!!g1_col)
    )

  bind_rows(long1, long2) %>%
    filter(!is.na(gf), !is.na(ga)) %>%
    mutate(
      win = as.integer(gf > ga),
      draw = as.integer(gf == ga),
      loss = as.integer(gf < ga),
      points = 3 * win + draw
    ) %>%
    group_by(group, team) %>%
    summarise(
      played = n(),
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss),
      gf = sum(gf),
      ga = sum(ga),
      gd = gf - ga,
      points = sum(points),
      .groups = "drop"
    ) %>%
    arrange(group, desc(points), desc(gd), desc(gf), team) %>%
    group_by(group) %>%
    mutate(position = row_number()) %>%
    ungroup()
}

compute_predicted_group_standings <- function(predictions) {
  # The current HTML exports explicit group rankings.
  # The script uses these rows when they are present:
  # source == "group_ranking", participant, group, position, team.
  if (
    all(c("source", "participant", "group", "position", "team") %in% names(predictions)) &&
      any(predictions$source == "group_ranking", na.rm = TRUE)
  ) {
    return(
      predictions %>%
        filter(source == "group_ranking") %>%
        transmute(
          participant,
          group,
          position = as.integer(position),
          team = clean_team(team)
        )
    )
  }

  # Otherwise, recompute from predicted scores using the simplified
  # tie-breakers: points, goal difference, goals for, alphabetical order.
  predictions %>%
    filter(stage_key_from_match_id(as.integer(match_id)) == "group_stage") %>%
    mutate(
      pred_team1_goals = as_numeric_safe(pred_team1_goals),
      pred_team2_goals = as_numeric_safe(pred_team2_goals)
    ) %>%
    group_by(participant) %>%
    group_modify(~ compute_standings(.x, pred_team1_goals, pred_team2_goals)) %>%
    ungroup() %>%
    select(participant, group, position, team)
}

compute_actual_group_standings <- function(predictions, results, actual_group_standings_path = NULL) {
  if (!is.null(actual_group_standings_path)) {
    if (!file.exists(actual_group_standings_path)) {
      stop("actual_group_standings_path does not exist: ", actual_group_standings_path)
    }

    return(
      read_csv(actual_group_standings_path, show_col_types = FALSE) %>%
        transmute(
          group,
          position = as.integer(position),
          team = clean_team(team)
        )
    )
  }

  group_schedule <- predictions %>%
    filter(stage_key_from_match_id(as.integer(match_id)) == "group_stage") %>%
    distinct(match_id, group, team1, team2)

  results2 <- results %>%
    mutate(
      match_id = as.integer(match_id),
      team1_goals = as_numeric_safe(team1_goals),
      team2_goals = as_numeric_safe(team2_goals)
    ) %>%
    select(match_id, team1_goals, team2_goals)

  group_schedule %>%
    left_join(results2, by = "match_id") %>%
    compute_standings(team1_goals, team2_goals) %>%
    select(group, position, team)
}

score_group_positions <- function(predictions, results, actual_group_standings_path = NULL) {
  predicted <- compute_predicted_group_standings(predictions)
  actual <- compute_actual_group_standings(predictions, results, actual_group_standings_path)

  predicted %>%
    left_join(
      actual %>% rename(actual_team = team),
      by = c("group", "position")
    ) %>%
    mutate(
      correct = clean_team(team) == clean_team(actual_team),
      points = if_else(
        correct,
        unname(group_rank_points[as.character(position)]),
        0
      )
    )
}


score_third_place_ranking <- function(predictions, results, actual_third_place_ranking_path = NULL) {
  # Scores the ranking of the third-placed teams.
  # A participant receives 1 point for each exact position in the overall
  # ranking of third-placed teams.

  if (!all(c("source", "participant", "position", "team") %in% names(predictions)) ||
      !any(predictions$source == "third_place_ranking", na.rm = TRUE)) {
    return(
      tibble(
        participant = character(),
        position = integer(),
        predicted_team = character(),
        actual_team = character(),
        correct = logical(),
        points = numeric()
      )
    )
  }

  predicted <- predictions %>%
    filter(source == "third_place_ranking") %>%
    transmute(
      participant,
      position = as.integer(position),
      predicted_team = clean_team(team),
      predicted_group = if ("third_group" %in% names(.)) as.character(third_group) else NA_character_,
      predicted_qualified = if ("qualified" %in% names(.)) as_numeric_safe(qualified) else NA_real_
    )

  actual <- compute_actual_third_place_ranking(
    predictions = predictions,
    results = results,
    actual_third_place_ranking_path = actual_third_place_ranking_path
  )

  predicted %>%
    left_join(
      actual %>% rename(actual_team = team, actual_group = group),
      by = "position"
    ) %>%
    mutate(
      correct = clean_team(predicted_team) == clean_team(actual_team),
      points = if_else(correct, third_place_ranking_points_per_correct, 0)
    )
}

compute_actual_third_place_ranking <- function(
  predictions,
  results,
  actual_third_place_ranking_path = NULL
) {
  # Preferred option: manually provide the actual ranking of third-placed teams.
  # CSV must contain at least: position, team. Optional: group, qualified.
  if (!is.null(actual_third_place_ranking_path)) {
    if (!file.exists(actual_third_place_ranking_path)) {
      stop("actual_third_place_ranking_path does not exist: ", actual_third_place_ranking_path)
    }

    return(
      read_csv(actual_third_place_ranking_path, show_col_types = FALSE) %>%
        transmute(
          position = as.integer(position),
          team = clean_team(team),
          group = if ("group" %in% names(.)) as.character(group) else NA_character_,
          qualified = if ("qualified" %in% names(.)) as_numeric_safe(qualified) else if_else(position <= 8, 1, 0)
        ) %>%
        arrange(position)
    )
  }

  # Otherwise, compute from actual group-stage match results using the same
  # simplified criteria used in the app: points, goal difference, goals for,
  # then alphabetical order.
  group_schedule <- predictions %>%
    filter(stage_key_from_match_id(as.integer(match_id)) == "group_stage") %>%
    distinct(match_id, group, team1, team2)

  results2 <- results %>%
    mutate(
      match_id = as.integer(match_id),
      team1_goals = as_numeric_safe(team1_goals),
      team2_goals = as_numeric_safe(team2_goals)
    ) %>%
    select(match_id, team1_goals, team2_goals)

  group_schedule %>%
    left_join(results2, by = "match_id") %>%
    compute_standings(team1_goals, team2_goals) %>%
    filter(position == 3) %>%
    arrange(desc(points), desc(gd), desc(gf), team) %>%
    mutate(
      position = row_number(),
      qualified = if_else(position <= 8, 1, 0)
    ) %>%
    select(position, team, group, qualified, points, gd, gf)
}

compute_predicted_third_place_ranking <- function(predictions) {
  if (!all(c("source", "participant", "position", "team") %in% names(predictions)) ||
      !any(predictions$source == "third_place_ranking", na.rm = TRUE)) {
    return(
      tibble(
        participant = character(),
        position = integer(),
        team = character(),
        group = character(),
        qualified = numeric()
      )
    )
  }

  predictions %>%
    filter(source == "third_place_ranking") %>%
    transmute(
      participant,
      position = as.integer(position),
      team = clean_team(team),
      group = if ("third_group" %in% names(.)) as.character(third_group) else NA_character_,
      qualified = if ("qualified" %in% names(.)) as_numeric_safe(qualified) else if_else(position <= 8, 1, 0)
    )
}

# ------------------------------------------------------------
# 6. Champion, runner-up, and third-place bonuses
# ------------------------------------------------------------

winner_from_row <- function(team1, team2, g1, g2, advance_team = NA_character_) {
  g1 <- as_numeric_safe(g1)
  g2 <- as_numeric_safe(g2)

  if (is.na(g1) || is.na(g2)) return(NA_character_)
  if (g1 > g2) return(clean_team(team1))
  if (g2 > g1) return(clean_team(team2))

  advance_team <- clean_team(advance_team)
  if (!is.na(advance_team) && advance_team != "") return(advance_team)

  NA_character_
}

loser_from_row <- function(team1, team2, winner) {
  team1 <- clean_team(team1)
  team2 <- clean_team(team2)
  winner <- clean_team(winner)

  case_when(
    is.na(winner) ~ NA_character_,
    winner == team1 ~ team2,
    winner == team2 ~ team1,
    TRUE ~ NA_character_
  )
}

derive_predicted_positions <- function(predictions) {
  ko <- predictions %>%
    filter(match_id %in% c(103, 104)) %>%
    mutate(match_id = as.integer(match_id))

  final_pred <- ko %>% filter(match_id == 104)
  third_pred <- ko %>% filter(match_id == 103)

  final_positions <- final_pred %>%
    rowwise() %>%
    mutate(
      predicted_champion = winner_from_row(
        team1, team2, pred_team1_goals, pred_team2_goals, advance_team
      ),
      predicted_runner_up = loser_from_row(team1, team2, predicted_champion)
    ) %>%
    ungroup() %>%
    select(participant, predicted_champion, predicted_runner_up)

  third_positions <- third_pred %>%
    rowwise() %>%
    mutate(
      predicted_third_place = winner_from_row(
        team1, team2, pred_team1_goals, pred_team2_goals, advance_team
      )
    ) %>%
    ungroup() %>%
    select(participant, predicted_third_place)

  full_join(final_positions, third_positions, by = "participant")
}

derive_actual_positions <- function(results) {
  if (!"advance_team" %in% names(results)) {
    results$advance_team <- NA_character_
  }

  final_actual <- results %>%
    filter(as.integer(match_id) == 104)

  third_actual <- results %>%
    filter(as.integer(match_id) == 103)

  if (nrow(final_actual) == 0) {
    warning("No actual final result found: match_id == 104.")
  }

  if (nrow(third_actual) == 0) {
    warning("No actual third-place result found: match_id == 103.")
  }

  champion <- if (nrow(final_actual) > 0) {
    winner_from_row(
      final_actual$team1[1],
      final_actual$team2[1],
      final_actual$team1_goals[1],
      final_actual$team2_goals[1],
      final_actual$advance_team[1]
    )
  } else {
    NA_character_
  }

  runner_up <- if (nrow(final_actual) > 0) {
    loser_from_row(final_actual$team1[1], final_actual$team2[1], champion)
  } else {
    NA_character_
  }

  third_place <- if (nrow(third_actual) > 0) {
    winner_from_row(
      third_actual$team1[1],
      third_actual$team2[1],
      third_actual$team1_goals[1],
      third_actual$team2_goals[1],
      third_actual$advance_team[1]
    )
  } else {
    NA_character_
  }

  tibble(
    actual_champion = champion,
    actual_runner_up = runner_up,
    actual_third_place = third_place
  )
}

score_final_positions <- function(predictions, results) {
  predicted <- derive_predicted_positions(predictions)
  actual <- derive_actual_positions(results)

  if (nrow(predicted) == 0) {
    return(
      tibble(
        participant = character(),
        champion_points = numeric(),
        runner_up_points = numeric(),
        third_place_points = numeric(),
        position_points = numeric()
      )
    )
  }

  predicted %>%
    mutate(dummy_join = 1) %>%
    left_join(actual %>% mutate(dummy_join = 1), by = "dummy_join") %>%
    select(-dummy_join) %>%
    mutate(
      champion_points = if_else(
        clean_team(predicted_champion) == clean_team(actual_champion),
        final_position_points[["champion"]],
        0
      ),
      runner_up_points = if_else(
        clean_team(predicted_runner_up) == clean_team(actual_runner_up),
        final_position_points[["runner_up"]],
        0
      ),
      third_place_points = if_else(
        clean_team(predicted_third_place) == clean_team(actual_third_place),
        final_position_points[["third_place"]],
        0
      ),
      position_points = champion_points + runner_up_points + third_place_points
    )
}

# ------------------------------------------------------------
# 7. Main scoring function
# ------------------------------------------------------------

score_bolao <- function(
  predictions_input,
  results_path,
  actual_group_standings_path = NULL,
  actual_third_place_ranking_path = NULL,
  output_dir = NULL
) {
  predictions <- read_prediction_files(predictions_input)
  results <- read_csv(results_path, show_col_types = FALSE)

  predictions <- predictions %>%
    mutate(
      match_id = as.integer(match_id),
      participant = as.character(participant),
      team1 = clean_team(team1),
      team2 = clean_team(team2),
      advance_team = if ("advance_team" %in% names(.)) clean_team(advance_team) else NA_character_
    )

  results <- results %>%
    mutate(
      match_id = as.integer(match_id),
      team1 = if ("team1" %in% names(.)) clean_team(team1) else NA_character_,
      team2 = if ("team2" %in% names(.)) clean_team(team2) else NA_character_,
      advance_team = if ("advance_team" %in% names(.)) clean_team(advance_team) else NA_character_
    )

  match_scored <- score_match_rows(predictions, results)

  group_position_scored <- score_group_positions(
    predictions,
    results,
    actual_group_standings_path = actual_group_standings_path
  )

  third_place_ranking_scored <- score_third_place_ranking(
    predictions,
    results,
    actual_third_place_ranking_path = actual_third_place_ranking_path
  )

  final_position_scored <- score_final_positions(predictions, results)

  match_summary <- match_scored %>%
    group_by(participant) %>%
    summarise(
      pontos_partidas = sum(match_points, na.rm = TRUE),
      pontos_fase_grupos_partidas = sum(match_points[stage_key == "group_stage"], na.rm = TRUE),
      pontos_mata_mata_partidas = sum(match_points[stage_key != "group_stage"], na.rm = TRUE),
      placares_exatos = sum(exact_score, na.rm = TRUE),
      vencedores_ou_empates = sum(correct_outcome, na.rm = TRUE),
      .groups = "drop"
    )

  group_position_summary <- group_position_scored %>%
    group_by(participant) %>%
    summarise(
      pontos_posicoes_grupos = sum(points, na.rm = TRUE),
      acertos_posicoes_grupos = sum(correct, na.rm = TRUE),
      .groups = "drop"
    )

  third_place_ranking_summary <- third_place_ranking_scored %>%
    group_by(participant) %>%
    summarise(
      pontos_ranking_terceiros = sum(points, na.rm = TRUE),
      acertos_ranking_terceiros = sum(correct, na.rm = TRUE),
      .groups = "drop"
    )

  final_position_summary <- final_position_scored %>%
    transmute(
      participant,
      pontos_posicoes_finais = position_points,
      champion_points,
      runner_up_points,
      third_place_points,
      predicted_champion,
      predicted_runner_up,
      predicted_third_place,
      actual_champion,
      actual_runner_up,
      actual_third_place
    )

  leaderboard <- match_summary %>%
    full_join(group_position_summary, by = "participant") %>%
    full_join(third_place_ranking_summary, by = "participant") %>%
    full_join(final_position_summary, by = "participant") %>%
    mutate(
      across(
        c(
          pontos_partidas,
          pontos_fase_grupos_partidas,
          pontos_mata_mata_partidas,
          pontos_posicoes_grupos,
          pontos_ranking_terceiros,
          pontos_posicoes_finais,
          champion_points,
          runner_up_points,
          third_place_points,
          placares_exatos,
          vencedores_ou_empates,
          acertos_posicoes_grupos,
          acertos_ranking_terceiros
        ),
        ~ replace_na(.x, 0)
      ),
      pontos_total = pontos_partidas + pontos_posicoes_grupos + pontos_ranking_terceiros + pontos_posicoes_finais
    ) %>%
    arrange(desc(pontos_total), desc(placares_exatos), desc(vencedores_ou_empates))

  output <- list(
    leaderboard = leaderboard,
    match_points = match_scored,
    group_position_points = group_position_scored,
    third_place_ranking_points = third_place_ranking_scored,
    final_position_points = final_position_scored,
    actual_group_standings = compute_actual_group_standings(
      predictions,
      results,
      actual_group_standings_path = actual_group_standings_path
    ),
    predicted_group_standings = compute_predicted_group_standings(predictions),
    actual_third_place_ranking = compute_actual_third_place_ranking(
      predictions,
      results,
      actual_third_place_ranking_path = actual_third_place_ranking_path
    ),
    predicted_third_place_ranking = compute_predicted_third_place_ranking(predictions)
  )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    write_csv(output$leaderboard, file.path(output_dir, "leaderboard.csv"))
    write_csv(output$match_points, file.path(output_dir, "match_points.csv"))
    write_csv(output$group_position_points, file.path(output_dir, "group_position_points.csv"))
    write_csv(output$third_place_ranking_points, file.path(output_dir, "third_place_ranking_points.csv"))
    write_csv(output$final_position_points, file.path(output_dir, "final_position_points.csv"))
    write_csv(output$actual_group_standings, file.path(output_dir, "actual_group_standings.csv"))
    write_csv(output$predicted_group_standings, file.path(output_dir, "predicted_group_standings.csv"))
    write_csv(output$actual_third_place_ranking, file.path(output_dir, "actual_third_place_ranking.csv"))
    write_csv(output$predicted_third_place_ranking, file.path(output_dir, "predicted_third_place_ranking.csv"))
  }

  output
}

# ------------------------------------------------------------
# 8. Example usage
# ------------------------------------------------------------
# Put all prediction CSVs in a folder, e.g.:
#   bolao_app/predictions/
#
# Put the real results in:
#   bolao_app/actual_results.csv
#
# Then run:
#
# result <- score_bolao(
#   predictions_input = "bolao_app/predictions",
#   results_path = "bolao_app/actual_results.csv",
#   output_dir = "bolao_app/scored_outputs"
# )
#
# result$leaderboard
#
# If FIFA tie-breakers make the actual group standings differ from the
# simplified points/GD/GF/alphabetical rule, create a CSV with:
#   group,position,team
# and pass it as actual_group_standings_path.
#
# If FIFA tie-breakers make the actual ranking of third-placed teams differ
# from the simplified points/GD/GF/alphabetical rule, create a CSV with:
#   position,team,group,qualified
# and pass it as actual_third_place_ranking_path.
#
# result <- score_bolao(
#   predictions_input = "bolao_app/predictions",
#   results_path = "bolao_app/actual_results.csv",
#   actual_group_standings_path = "bolao_app/actual_group_standings_manual.csv",
#   output_dir = "bolao_app/scored_outputs"
# )
