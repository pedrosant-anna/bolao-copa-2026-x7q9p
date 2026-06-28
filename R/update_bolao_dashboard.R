# ============================================================
# Update World Cup Bolão dashboard
# ============================================================
# What this script does:
#   1. Fetches current World Cup results.
#   2. Reads all participant prediction CSVs.
#   3. Scores the bolão using score_bolao_v2.R.
#   4. Writes a visually nice static dashboard to docs/index.html.
#
# Recommended project structure:
#   bolao-copa/
#     R/update_bolao_dashboard.R
#     R/score_bolao_v2.R
#     data/matches.csv
#     data/predictions/predictions_pedro.csv
#     data/predictions/predictions_friend.csv
#     data/predictions_corrected/            # corrected knockout CSVs, optional
#     data/knockout_correct_path.csv         # corrected knockout fixture/path CSV, optional
#     data/results/actual_results.csv        # generated automatically
#     docs/index.html                        # generated automatically
#
# Packages:
#   install.packages(c("dplyr", "readr", "purrr", "stringr", "tidyr", "jsonlite", "httr2", "lubridate", "stringi"))
#
# Results source:
#   Preferred: ESPN public scoreboard endpoint, no API key.
#   Fallback: openfootball/worldcup.json, no API key, but not live-guaranteed.
#
# Notes:
#   ESPN endpoint is public/undocumented and can change without notice.
#   For robust match_id mapping, put the official schedule at data/matches.csv
#   or matches.csv in the project root.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(jsonlite)
  library(httr2)
  library(lubridate)
})

# ------------------------------------------------------------
# 0. Paths
# ------------------------------------------------------------

root_dir <- getwd()

possible_score_paths <- c(
  file.path(root_dir, "R", "score_bolao_v2.R"),
  file.path(root_dir, "score_bolao_v2.R"),
  file.path(root_dir, "score_bolao_with_rankings.R")
)

score_path <- possible_score_paths[file.exists(possible_score_paths)][1]
if (is.na(score_path)) {
  stop("Could not find score_bolao_v2.R. Put it in R/score_bolao_v2.R or the project root.")
}

source(score_path)

paths <- list(
  predictions_dir = file.path(root_dir, "data", "predictions"),
  predictions_original_fixed_dir = file.path(root_dir, "data", "_predictions_original_fixed"),
  predictions_corrected_dir = file.path(root_dir, "data", "predictions_corrected"),
  predictions_corrected_merged_dir = file.path(root_dir, "data", "_predictions_corrected_merged"),
  knockout_correct_path_csv = dplyr::case_when(
    file.exists(file.path(root_dir, "data", "knockout_correct_path.csv")) ~ file.path(root_dir, "data", "knockout_correct_path.csv"),
    file.exists(file.path(root_dir, "knockout_correct_path.csv")) ~ file.path(root_dir, "knockout_correct_path.csv"),
    TRUE ~ NA_character_
  ),
  matches_csv = dplyr::case_when(
    file.exists(file.path(root_dir, "data", "matches.csv")) ~ file.path(root_dir, "data", "matches.csv"),
    file.exists(file.path(root_dir, "matches.csv")) ~ file.path(root_dir, "matches.csv"),
    TRUE ~ NA_character_
  ),
  results_dir = file.path(root_dir, "data", "results"),
  results_csv = file.path(root_dir, "data", "results", "actual_results.csv"),
  docs_dir = file.path(root_dir, "docs"),
  dashboard_html = file.path(root_dir, "docs", "index.html")
)

dir.create(paths$results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$docs_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. Team-name normalization
# ------------------------------------------------------------

clean_ascii <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

normalize_team <- function(x) {
  z <- clean_ascii(x)

  recode(
    z,
    "usa" = "united states",
    "u s a" = "united states",
    "us" = "united states",
    "united states of america" = "united states",
    "czechia" = "czech republic",
    "czech republic" = "czech republic",
    "korea republic" = "south korea",
    "republic of korea" = "south korea",
    "south korea" = "south korea",
    "ir iran" = "iran",
    "cote d ivoire" = "ivory coast",
    "cote divoire" = "ivory coast",
    "turkiye" = "turkey",
    "bosnia and herzegovina" = "bosnia herzegovina",
    "bosnia herzegovina" = "bosnia herzegovina",
    "dr congo" = "congo dr",
    "congo dr" = "congo dr",
    "democratic republic of congo" = "congo dr",
    .default = z
  )
}

# ------------------------------------------------------------
# 2. Fetch true results
# ------------------------------------------------------------

fetch_results_openfootball <- function() {
  url <- "https://raw.githubusercontent.com/openfootball/worldcup.json/master/2026/worldcup.json"

  x <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  matches <- x$matches

  purrr::imap_dfr(matches, function(m, i) {
    ft <- m$score$ft %||% NULL
    et <- m$score$et %||% NULL
    p  <- m$score$p  %||% NULL

    team1_goals <- if (!is.null(ft)) ft[[1]] else NA_real_
    team2_goals <- if (!is.null(ft)) ft[[2]] else NA_real_

    # For knockout matches tied in the score field, use penalties/ET only to
    # identify the team that advanced.
    advance_team <- NA_character_
    if (!is.na(team1_goals) && !is.na(team2_goals)) {
      if (team1_goals > team2_goals) advance_team <- m$team1
      if (team2_goals > team1_goals) advance_team <- m$team2
    }
    if (!is.null(et) && length(et) == 2 && et[[1]] != et[[2]]) {
      advance_team <- if (et[[1]] > et[[2]]) m$team1 else m$team2
    }
    if (!is.null(p) && length(p) == 2 && p[[1]] != p[[2]]) {
      advance_team <- if (p[[1]] > p[[2]]) m$team1 else m$team2
    }

    tibble(
      match_id = i,
      source_match_id = NA_character_,
      source = "openfootball",
      stage = m$round %||% NA_character_,
      group = m$group %||% NA_character_,
      date = m$date %||% NA_character_,
      time = m$time %||% NA_character_,
      team1 = m$team1 %||% NA_character_,
      team2 = m$team2 %||% NA_character_,
      team1_goals = team1_goals,
      team2_goals = team2_goals,
      advance_team = advance_team,
      status = if (!is.null(ft)) "FINISHED" else "SCHEDULED"
    )
  })
}

fetch_results_espn_raw <- function(
  dates = "20260611-20260720",
  limit = 200
) {
  url <- "https://site.api.espn.com/apis/site/v2/sports/soccer/fifa.world/scoreboard"

  resp <- request(url) %>%
    req_url_query(
      lang = "en",
      limit = limit,
      dates = dates
    ) %>%
    req_user_agent("bolao-copa-2026-personal/1.0") %>%
    req_perform()

  body <- resp_body_json(resp, simplifyVector = FALSE)
  events <- body$events %||% list()

  if (length(events) == 0) {
    stop("ESPN returned zero events.")
  }

  purrr::map_dfr(events, function(ev) {
    comp <- ev$competitions[[1]] %||% list()
    competitors <- comp$competitors %||% list()

    if (length(competitors) < 2) {
      return(tibble())
    }

    c1 <- competitors[[1]]
    c2 <- competitors[[2]]

    team_name <- function(c) {
      c$team$displayName %||% c$team$shortDisplayName %||% c$team$name %||% NA_character_
    }

    team_abbrev <- function(c) {
      c$team$abbreviation %||% NA_character_
    }

    score_num <- function(c) {
      suppressWarnings(as.numeric(c$score %||% NA_real_))
    }

    winner_bool <- function(c) {
      w <- c$winner %||% NA
      isTRUE(w)
    }

    status_type <- ev$status$type %||% list()
    status_name <- status_type$name %||% NA_character_
    status_desc <- status_type$description %||% NA_character_
    completed <- isTRUE(status_type$completed %||% FALSE)

    g1 <- score_num(c1)
    g2 <- score_num(c2)

    advance_team <- NA_character_
    if (winner_bool(c1)) advance_team <- team_name(c1)
    if (winner_bool(c2)) advance_team <- team_name(c2)

    tibble(
      source_match_id = as.character(ev$id %||% NA_character_),
      source = "espn",
      utc_datetime = ev$date %||% NA_character_,
      date = as.character(as.Date(ev$date %||% NA_character_)),
      name = ev$name %||% NA_character_,
      short_name = ev$shortName %||% NA_character_,
      team_a = team_name(c1),
      team_b = team_name(c2),
      team_a_abbrev = team_abbrev(c1),
      team_b_abbrev = team_abbrev(c2),
      team_a_goals = g1,
      team_b_goals = g2,
      advance_team_raw = advance_team,
      status = status_name,
      status_long = status_desc,
      completed = completed
    )
  }) %>%
    arrange(utc_datetime, source_match_id)
}

align_results_to_schedule <- function(raw_results, schedule_path) {
  if (is.na(schedule_path) || !file.exists(schedule_path)) {
    warning(
      "No data/matches.csv or root matches.csv found. ",
      "Assigning match_id by chronological order. For safer mapping, add data/matches.csv."
    )

    return(
      raw_results %>%
        arrange(utc_datetime, source_match_id) %>%
        mutate(
          match_id = row_number(),
          team1 = team_a,
          team2 = team_b,
          team1_goals = if_else(completed, team_a_goals, NA_real_),
          team2_goals = if_else(completed, team_b_goals, NA_real_),
          advance_team = advance_team_raw,
          stage = NA_character_,
          group = NA_character_,
          time = NA_character_
        ) %>%
        select(
          match_id, source_match_id, source, stage, group, date, time,
          team1, team2, team1_goals, team2_goals, advance_team, status
        )
    )
  }

  schedule <- read_csv(schedule_path, show_col_types = FALSE) %>%
    mutate(
      match_id = as.integer(match_id),
      date = as.character(date),
      team1_norm = normalize_team(team1),
      team2_norm = normalize_team(team2)
    )

  # If available, use the corrected knockout path for matches 73-104.
  # This keeps ESPN result matching aligned with the corrected Round of 32 bracket.
  if (!is.na(paths$knockout_correct_path_csv) && file.exists(paths$knockout_correct_path_csv)) {
    ko_schedule <- read_csv(paths$knockout_correct_path_csv, show_col_types = FALSE) %>%
      mutate(
        match_id = as.integer(match_id),
        date = as.character(date),
        team1_norm = normalize_team(team1),
        team2_norm = normalize_team(team2)
      )

    missing_cols <- setdiff(names(schedule), names(ko_schedule))
    for (col in missing_cols) ko_schedule[[col]] <- NA

    ko_schedule <- ko_schedule %>% select(all_of(names(schedule)))

    schedule <- bind_rows(
      schedule %>% filter(!(match_id %in% ko_schedule$match_id)),
      ko_schedule
    ) %>% arrange(match_id)
  }

  raw <- raw_results %>%
    mutate(
      date = as.character(date),
      team_a_norm = normalize_team(team_a),
      team_b_norm = normalize_team(team_b),
      raw_row = row_number()
    )

  aligned <- purrr::pmap_dfr(schedule, function(...) {
    sch <- tibble(...)

    # First try exact date + teams. Then allow date mismatch as a fallback,
    # because source dates can differ near midnight/time zones.
    candidates <- raw %>%
      mutate(
        forward = team_a_norm == sch$team1_norm & team_b_norm == sch$team2_norm,
        reverse = team_a_norm == sch$team2_norm & team_b_norm == sch$team1_norm,
        same_date = date == sch$date
      ) %>%
      filter(forward | reverse) %>%
      arrange(desc(same_date), utc_datetime, source_match_id)

    if (nrow(candidates) == 0) {
      return(tibble(
        match_id = sch$match_id,
        source_match_id = NA_character_,
        source = "espn",
        stage = sch$stage %||% NA_character_,
        group = sch$group %||% NA_character_,
        date = sch$date %||% NA_character_,
        time = sch$time %||% NA_character_,
        team1 = sch$team1 %||% NA_character_,
        team2 = sch$team2 %||% NA_character_,
        team1_goals = NA_real_,
        team2_goals = NA_real_,
        advance_team = NA_character_,
        status = "SCHEDULED"
      ))
    }

    m <- candidates[1, ]
    forward <- isTRUE(m$team_a_norm == sch$team1_norm & m$team_b_norm == sch$team2_norm)

    if (forward) {
      team1_goals <- if (isTRUE(m$completed)) m$team_a_goals else NA_real_
      team2_goals <- if (isTRUE(m$completed)) m$team_b_goals else NA_real_
      advance_team <- dplyr::case_when(
        is.na(m$advance_team_raw) ~ NA_character_,
        normalize_team(m$advance_team_raw) == sch$team1_norm ~ sch$team1,
        normalize_team(m$advance_team_raw) == sch$team2_norm ~ sch$team2,
        TRUE ~ m$advance_team_raw
      )
    } else {
      team1_goals <- if (isTRUE(m$completed)) m$team_b_goals else NA_real_
      team2_goals <- if (isTRUE(m$completed)) m$team_a_goals else NA_real_
      advance_team <- dplyr::case_when(
        is.na(m$advance_team_raw) ~ NA_character_,
        normalize_team(m$advance_team_raw) == sch$team1_norm ~ sch$team1,
        normalize_team(m$advance_team_raw) == sch$team2_norm ~ sch$team2,
        TRUE ~ m$advance_team_raw
      )
    }

    tibble(
      match_id = sch$match_id,
      source_match_id = m$source_match_id,
      source = "espn",
      stage = sch$stage %||% NA_character_,
      group = sch$group %||% NA_character_,
      date = sch$date %||% NA_character_,
      time = sch$time %||% NA_character_,
      team1 = sch$team1 %||% NA_character_,
      team2 = sch$team2 %||% NA_character_,
      team1_goals = team1_goals,
      team2_goals = team2_goals,
      advance_team = advance_team,
      status = m$status
    )
  })

  aligned %>% arrange(match_id)
}

fetch_results_espn <- function(schedule_path = NA_character_) {
  message("Fetching results from ESPN public scoreboard...")

  raw <- fetch_results_espn_raw()

  if (nrow(raw) == 0) {
    stop("ESPN returned no parseable events.")
  }

  align_results_to_schedule(raw, schedule_path)
}

fetch_results <- function() {
  out <- tryCatch(
    fetch_results_espn(paths$matches_csv),
    error = function(e) {
      warning("ESPN failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(out)) {
    message("Fetching results from openfootball fallback...")
    out <- fetch_results_openfootball()
  }

  out %>%
    mutate(
      match_id = as.integer(match_id),
      team1_goals = suppressWarnings(as.numeric(team1_goals)),
      team2_goals = suppressWarnings(as.numeric(team2_goals)),
      played = !is.na(team1_goals) & !is.na(team2_goals)
    )
}

# ------------------------------------------------------------
# 3. Dashboard HTML helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

html_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x <- str_replace_all(x, '"', "&quot;")
  x <- str_replace_all(x, "'", "&#039;")
  x
}

fmt_points <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "0", format(round(x, 2), nsmall = ifelse(abs(x %% 1) < 1e-8, 0, 2), trim = TRUE))
}

fmt_score <- function(g1, g2) {
  g1 <- suppressWarnings(as.numeric(g1))
  g2 <- suppressWarnings(as.numeric(g2))
  ifelse(is.na(g1) | is.na(g2), "–", paste0(g1, " x ", g2))
}

stage_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("group_stage", "Group Stage", "Fase de grupos") ~ "Fase de grupos",
    x %in% c("round_of_32", "Round of 32") ~ "Fase de 32",
    x %in% c("round_of_16", "Round of 16") ~ "Oitavas",
    x %in% c("quarterfinal", "Quarterfinal") ~ "Quartas",
    x %in% c("semifinal", "Semifinal") ~ "Semifinal",
    x %in% c("third_place", "Third-place match") ~ "3º lugar",
    x %in% c("final", "Final") ~ "Final",
    TRUE ~ x
  )
}

team_pt <- c(
  "Algeria" = "Argélia", "Argentina" = "Argentina", "Australia" = "Austrália",
  "Austria" = "Áustria", "Belgium" = "Bélgica", "Bosnia and Herzegovina" = "Bósnia e Herzegovina",
  "Brazil" = "Brasil", "Canada" = "Canadá", "Cape Verde" = "Cabo Verde",
  "Colombia" = "Colômbia", "Congo DR" = "RD Congo", "DR Congo" = "RD Congo", "Croatia" = "Croácia",
  "Curaçao" = "Curaçao", "Czechia" = "Tchéquia", "Ecuador" = "Equador",
  "Egypt" = "Egito", "England" = "Inglaterra", "France" = "França",
  "Germany" = "Alemanha", "Ghana" = "Gana", "Haiti" = "Haiti",
  "Iran" = "Irã", "Iraq" = "Iraque", "Ivory Coast" = "Costa do Marfim",
  "Japan" = "Japão", "Jordan" = "Jordânia", "Mexico" = "México",
  "Morocco" = "Marrocos", "Netherlands" = "Holanda", "New Zealand" = "Nova Zelândia",
  "Norway" = "Noruega", "Panama" = "Panamá", "Paraguay" = "Paraguai",
  "Portugal" = "Portugal", "Qatar" = "Catar", "Saudi Arabia" = "Arábia Saudita",
  "Scotland" = "Escócia", "Senegal" = "Senegal", "South Africa" = "África do Sul",
  "South Korea" = "Coreia do Sul", "Spain" = "Espanha", "Sweden" = "Suécia",
  "Switzerland" = "Suíça", "Tunisia" = "Tunísia", "Türkiye" = "Turquia",
  "Turkey" = "Turquia", "United States" = "Estados Unidos", "Uruguay" = "Uruguai",
  "Uzbekistan" = "Uzbequistão",
  "Group A runner-up" = "2º do Grupo A", "Group B runner-up" = "2º do Grupo B",
  "Group C winner" = "1º do Grupo C", "Group F runner-up" = "2º do Grupo F",
  "Group E winner" = "1º do Grupo E", "Group F winner" = "1º do Grupo F",
  "Group C runner-up" = "2º do Grupo C", "Group E runner-up" = "2º do Grupo E",
  "Group I runner-up" = "2º do Grupo I", "Group I winner" = "1º do Grupo I",
  "Group A winner" = "1º do Grupo A", "Group L winner" = "1º do Grupo L",
  "Group G winner" = "1º do Grupo G", "Group D winner" = "1º do Grupo D",
  "Group H winner" = "1º do Grupo H", "Group J runner-up" = "2º do Grupo J",
  "Group K runner-up" = "2º do Grupo K", "Group L runner-up" = "2º do Grupo L",
  "Group B winner" = "1º do Grupo B", "Group D runner-up" = "2º do Grupo D",
  "Group G runner-up" = "2º do Grupo G", "Group J winner" = "1º do Grupo J",
  "Group H runner-up" = "2º do Grupo H", "Group K winner" = "1º do Grupo K",
  "TBD" = "A definir"
)

team_code <- c(
  "Algeria" = "ALG", "Argentina" = "ARG", "Australia" = "AUS", "Austria" = "AUT",
  "Belgium" = "BEL", "Bosnia and Herzegovina" = "BIH", "Brazil" = "BRA", "Canada" = "CAN",
  "Cape Verde" = "CPV", "Colombia" = "COL", "Congo DR" = "COD", "DR Congo" = "COD", "Croatia" = "CRO",
  "Curaçao" = "CUW", "Czechia" = "CZE", "Ecuador" = "ECU", "Egypt" = "EGY",
  "England" = "ENG", "France" = "FRA", "Germany" = "GER", "Ghana" = "GHA",
  "Haiti" = "HAI", "Iran" = "IRN", "Iraq" = "IRQ", "Ivory Coast" = "CIV",
  "Japan" = "JPN", "Jordan" = "JOR", "Mexico" = "MEX", "Morocco" = "MAR",
  "Netherlands" = "NED", "New Zealand" = "NZL", "Norway" = "NOR", "Panama" = "PAN",
  "Paraguay" = "PAR", "Portugal" = "POR", "Qatar" = "QAT", "Saudi Arabia" = "KSA",
  "Scotland" = "SCO", "Senegal" = "SEN", "South Africa" = "RSA", "South Korea" = "KOR",
  "Spain" = "ESP", "Sweden" = "SWE", "Switzerland" = "SUI", "Tunisia" = "TUN",
  "Türkiye" = "TUR", "Turkey" = "TUR", "United States" = "USA", "Uruguay" = "URU",
  "Uzbekistan" = "UZB"
)

flag_code <- c(
  "Algeria" = "dz", "Argentina" = "ar", "Australia" = "au", "Austria" = "at",
  "Belgium" = "be", "Bosnia and Herzegovina" = "ba", "Brazil" = "br", "Canada" = "ca",
  "Cape Verde" = "cv", "Colombia" = "co", "Congo DR" = "cd", "DR Congo" = "cd", "Croatia" = "hr",
  "Curaçao" = "cw", "Czechia" = "cz", "Ecuador" = "ec", "Egypt" = "eg",
  "England" = "gb-eng", "France" = "fr", "Germany" = "de", "Ghana" = "gh",
  "Haiti" = "ht", "Iran" = "ir", "Iraq" = "iq", "Ivory Coast" = "ci",
  "Japan" = "jp", "Jordan" = "jo", "Mexico" = "mx", "Morocco" = "ma",
  "Netherlands" = "nl", "New Zealand" = "nz", "Norway" = "no", "Panama" = "pa",
  "Paraguay" = "py", "Portugal" = "pt", "Qatar" = "qa", "Saudi Arabia" = "sa",
  "Scotland" = "gb-sct", "Senegal" = "sn", "South Africa" = "za", "South Korea" = "kr",
  "Spain" = "es", "Sweden" = "se", "Switzerland" = "ch", "Tunisia" = "tn",
  "Türkiye" = "tr", "Turkey" = "tr", "United States" = "us", "Uruguay" = "uy",
  "Uzbekistan" = "uz"
)

normalize_key <- function(x) clean_ascii(x)

lookup_by_clean <- function(x, map, default = NA_character_) {
  if (is.na(x) || x == "") return(default)
  nm <- normalize_key(names(map))
  key <- normalize_key(x)
  hit <- which(nm == key)[1]
  if (is.na(hit)) default else unname(map[[hit]])
}

display_team_one <- function(team) {
  if (is.na(team) || team == "") return("")
  lookup_by_clean(team, team_pt, default = team)
}

display_team <- function(team) {
  vapply(team, display_team_one, character(1), USE.NAMES = FALSE)
}

team_abbr_one <- function(team) {
  if (is.na(team) || team == "") return("—")
  lookup_by_clean(team, team_code, default = str_to_upper(str_sub(display_team_one(team), 1, 3)))
}

team_abbr <- function(team) {
  vapply(team, team_abbr_one, character(1), USE.NAMES = FALSE)
}

flag_html_one <- function(team) {
  code <- lookup_by_clean(team, flag_code, default = NA_character_)
  if (is.na(code) || code == "") return("<span class='flag-spacer'></span>")
  paste0("<img class='flag-img' src='https://flagcdn.com/w40/", html_escape(code), ".png' alt=''>")
}

flag_html <- function(team) {
  vapply(team, flag_html_one, character(1), USE.NAMES = FALSE)
}

team_badge_one <- function(team, compact = FALSE) {
  if (is.na(team) || team == "") return("<span class='team-badge muted'>—</span>")
  label <- if (compact) team_abbr_one(team) else display_team_one(team)
  paste0("<span class='team-badge'>", flag_html_one(team), "<span>", html_escape(label), "</span></span>")
}

team_badge <- function(team, compact = FALSE) {
  vapply(team, function(z) team_badge_one(z, compact = compact), character(1), USE.NAMES = FALSE)
}

safe_tab_id <- function(x) {
  x <- clean_ascii(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  ifelse(x == "", "participante", x)
}

prediction_outcome_class <- function(g1, g2, side) {
  g1 <- suppressWarnings(as.numeric(g1)); g2 <- suppressWarnings(as.numeric(g2))
  if (is.na(g1) || is.na(g2)) return("")
  if (g1 == g2) return("draw")
  if (side == 1 && g1 > g2) return("win")
  if (side == 2 && g2 > g1) return("win")
  "lose"
}

make_score_pill <- function(g1, g2, points = NULL) {
  pts <- if (!is.null(points)) paste0("<span class='mini-points'>+", fmt_points(points), "</span>") else ""
  paste0("<span class='score-pill'>", fmt_score(g1, g2), "</span>", pts)
}

# ------------------------------------------------------------
# 4. Main dashboard sections
# ------------------------------------------------------------

make_tabs_nav <- function(participants) {
  participant_buttons <- paste0(
    purrr::map_chr(participants, ~ paste0(
      "<button class='tab-button' onclick=\"showTab('player_", html_escape(safe_tab_id(.x)), "', this)\">",
      "Palpites: ", html_escape(.x), "</button>"
    )),
    collapse = "\n"
  )

  paste0(
    "<nav class='tabs'>",
    "<button class='tab-button active' onclick=\"showTab('tab_score', this)\">Placar</button>",
    participant_buttons,
    "<button class='tab-button' onclick=\"showTab('tab_rules', this)\">Regras</button>",
    "</nav>"
  )
}

make_leaderboard_html <- function(leaderboard) {
  rows <- leaderboard %>%
    mutate(rank = row_number()) %>%
    transmute(
      html = paste0(
        "<tr>",
        "<td class='rank'>", rank, "</td>",
        "<td class='player'>", html_escape(participant), "</td>",
        "<td class='points'>", fmt_points(pontos_total), "</td>",
        "<td>", fmt_points(pontos_partidas), "</td>",
        "<td>", fmt_points(pontos_posicoes_grupos), "</td>",
        "<td>", fmt_points(pontos_ranking_terceiros), "</td>",
        "<td>", fmt_points(pontos_posicoes_finais), "</td>",
        "<td>", placares_exatos, "</td>",
        "</tr>"
      )
    ) %>%
    pull(html) %>%
    paste(collapse = "\n")

  paste0(
    "<section class='card hero'>",
    "<h2>Placar atual</h2>",
    "<table class='leaderboard'>",
    "<thead><tr>",
    "<th>#</th><th>Participante</th><th>Total</th><th>Jogos</th><th>Grupos</th><th>3ºs</th><th>Final</th><th>Placares exatos</th>",
    "</tr></thead><tbody>", rows, "</tbody></table>",
    "</section>"
  )
}

make_score_breakdown_html <- function(leaderboard) {
  cards <- leaderboard %>%
    mutate(rank = row_number()) %>%
    transmute(
      html = paste0(
        "<div class='summary-card'>",
        "<div class='summary-rank'>#", rank, "</div>",
        "<h3>", html_escape(participant), "</h3>",
        "<div class='summary-total'>", fmt_points(pontos_total), " pts</div>",
        "<div class='summary-grid'>",
        "<span>Jogos</span><strong>", fmt_points(pontos_partidas), "</strong>",
        "<span>Posições nos grupos</span><strong>", fmt_points(pontos_posicoes_grupos), "</strong>",
        "<span>Ranking dos 3ºs</span><strong>", fmt_points(pontos_ranking_terceiros), "</strong>",
        "<span>Campeão/finalistas</span><strong>", fmt_points(pontos_posicoes_finais), "</strong>",
        "</div></div>"
      )
    ) %>%
    pull(html) %>%
    paste(collapse = "\n")

  paste0("<section class='summary-grid-wrap'>", cards, "</section>")
}

make_match_cards_html <- function(match_points, results) {
  played_ids <- results %>% filter(played) %>% pull(match_id)

  if (length(played_ids) == 0) {
    return("<section class='card'><h2>Resultados e palpites</h2><p class='muted'>Nenhum jogo finalizado ainda.</p></section>")
  }

  mp <- match_points %>%
    filter(match_id %in% played_ids) %>%
    mutate(
      pred_score = paste0(pred_team1_goals, " x ", pred_team2_goals),
      true_score = paste0(true_team1_goals, " x ", true_team2_goals),
      match_points_fmt = fmt_points(match_points),
      stage_lbl = stage_label(stage_key)
    )

  match_rows <- mp %>%
    group_by(match_id, team1, team2, true_score, stage_lbl) %>%
    summarise(
      participant_html = paste0(
        "<div class='bet-row'><span>", html_escape(participant), "</span>",
        "<span class='bet'>", pred_score, "</span>",
        "<span class='mini-points'>+", match_points_fmt, "</span></div>",
        collapse = ""
      ),
      .groups = "drop"
    ) %>%
    arrange(match_id) %>%
    mutate(
      html = paste0(
        "<div class='match-card'>",
        "<div class='match-title'><span class='match-id'>Jogo ", match_id, " · ", html_escape(stage_lbl), "</span></div>",
        "<div class='true-result'>", team_badge(team1), " <strong>", true_score, "</strong> ", team_badge(team2), "</div>",
        "<div class='bets'>", participant_html, "</div>",
        "</div>"
      )
    ) %>%
    pull(html) %>%
    paste(collapse = "\n")

  paste0(
    "<section class='card'><h2>Resultados e palpites</h2>",
    "<p class='muted'>Mostra os jogos já finalizados, com os palpites e os pontos de cada participante.</p>",
    "<div class='match-grid'>", match_rows, "</div></section>"
  )
}

# ------------------------------------------------------------
# 5. Participant prediction views
# ------------------------------------------------------------

prediction_rows_for_participant <- function(predictions, participant_name) {
  predictions %>%
    mutate(
      source = if ("source" %in% names(.)) source else NA_character_,
      match_id = suppressWarnings(as.integer(match_id)),
      pred_team1_goals = suppressWarnings(as.numeric(pred_team1_goals)),
      pred_team2_goals = suppressWarnings(as.numeric(pred_team2_goals)),
      position = suppressWarnings(as.integer(position)),
      qualified = suppressWarnings(as.numeric(qualified))
    ) %>%
    filter(participant == participant_name)
}

make_group_prediction_card <- function(group_name, games, ranking) {
  game_html <- games %>%
    arrange(match_id) %>%
    mutate(
      c1 = purrr::map_chr(seq_len(n()), ~ prediction_outcome_class(pred_team1_goals[.x], pred_team2_goals[.x], 1)),
      c2 = purrr::map_chr(seq_len(n()), ~ prediction_outcome_class(pred_team1_goals[.x], pred_team2_goals[.x], 2)),
      html = paste0(
        "<div class='guess-match'>",
        "<div class='mini-meta'>Jogo ", match_id, "</div>",
        "<div class='guess-line'>",
        "<span class='team-chip ", c1, "'>", team_badge(team1, compact = TRUE), "</span>",
        make_score_pill(pred_team1_goals, pred_team2_goals),
        "<span class='team-chip ", c2, "'>", team_badge(team2, compact = TRUE), "</span>",
        "</div></div>"
      )
    ) %>%
    pull(html) %>%
    paste(collapse = "\n")

  if (nrow(ranking) > 0) {
    ranking_html <- ranking %>%
      arrange(position) %>%
      mutate(
        q = if_else(!is.na(qualified) & qualified == 1, "<span class='q'>classifica</span>", ""),
        html = paste0(
          "<tr><td>", position, "º</td><td>", team_badge(team, compact = TRUE), "</td><td>", q, "</td></tr>"
        )
      ) %>%
      pull(html) %>%
      paste(collapse = "\n")
  } else {
    ranking_html <- "<tr><td colspan='3' class='muted'>Ranking não exportado.</td></tr>"
  }

  paste0(
    "<section class='group-card'>",
    "<h3>Grupo ", html_escape(group_name), "</h3>",
    "<div class='group-card-grid'><div>", game_html, "</div>",
    "<div><h4>Classificação prevista</h4><table class='mini-table'><tbody>", ranking_html, "</tbody></table></div></div>",
    "</section>"
  )
}

make_groups_view_html <- function(p) {
  group_games <- p %>%
    filter((source == "group_stage") | (is.na(source) & match_id >= 1 & match_id <= 72)) %>%
    filter(match_id >= 1, match_id <= 72)

  rankings <- p %>% filter(source == "group_ranking")

  if (nrow(group_games) == 0) {
    return("<p class='muted'>Não encontrei palpites da fase de grupos para este participante.</p>")
  }

  groups <- sort(unique(group_games$group))
  paste0(
    "<div class='groups-grid'>",
    purrr::map_chr(groups, function(g) {
      make_group_prediction_card(
        g,
        group_games %>% filter(group == g),
        rankings %>% filter(group == g)
      )
    }) %>% paste(collapse = "\n"),
    "</div>"
  )
}

winner_from_prediction <- function(team1, team2, g1, g2, advance_team = NA_character_) {
  g1 <- suppressWarnings(as.numeric(g1)); g2 <- suppressWarnings(as.numeric(g2))
  if (is.na(g1) || is.na(g2)) return(NA_character_)
  if (g1 > g2) return(team1)
  if (g2 > g1) return(team2)
  if (!is.na(advance_team) && advance_team != "") return(advance_team)
  NA_character_
}

make_compact_ko_card <- function(row) {
  if (nrow(row) == 0) return("<div class='compact-match empty'>—</div>")

  r <- row[1, ]
  g1 <- suppressWarnings(as.numeric(r$pred_team1_goals))
  g2 <- suppressWarnings(as.numeric(r$pred_team2_goals))
  winner <- winner_from_prediction(r$team1, r$team2, g1, g2, r$advance_team)
  c1 <- prediction_outcome_class(g1, g2, 1)
  c2 <- prediction_outcome_class(g1, g2, 2)

  adv <- ""
  if (!is.na(winner) && winner != "" && !is.na(g1) && !is.na(g2) && g1 == g2) {
    adv <- paste0("<div class='compact-advance'>classifica: ", team_badge(winner, compact = TRUE), "</div>")
  }

  paste0(
    "<div class='compact-match'>",
    "<div class='compact-title'>Jogo ", r$match_id, "</div>",
    "<div class='compact-row ", c1, "'><span>", team_badge(r$team1, compact = TRUE), "</span><strong>", ifelse(is.na(g1), "–", g1), "</strong></div>",
    "<div class='compact-row ", c2, "'><span>", team_badge(r$team2, compact = TRUE), "</span><strong>", ifelse(is.na(g2), "–", g2), "</strong></div>",
    adv,
    "</div>"
  )
}

make_tree_row <- function(rows, ids, span) {
  paste0(
    "<div class='tree-row'>",
    paste0(
      purrr::map_chr(ids, ~ paste0(
        "<div class='tree-cell span-", span, "'>",
        make_compact_ko_card(rows %>% filter(match_id == .x)),
        "</div>"
      )),
      collapse = "\n"
    ),
    "</div>"
  )
}

make_connector_row <- function(n, span, direction = c("down", "up")) {
  direction <- match.arg(direction)
  paste0(
    "<div class='connector-row'>",
    paste0(
      rep(paste0("<div class='connector-cell connector-", direction, " span-", span, "'></div>"), n),
      collapse = "\n"
    ),
    "</div>"
  )
}

make_round_label_row <- function(labels, spans) {
  paste0(
    "<div class='round-label-row'>",
    paste0(
      purrr::map2_chr(labels, spans, ~ paste0(
        "<div class='round-label span-", .y, "'>", html_escape(.x), "</div>"
      )),
      collapse = "\n"
    ),
    "</div>"
  )
}

make_top_tree <- function(ko) {
  paste0(
    "<div class='natural-bracket'><h3>Metade superior do chaveamento</h3><div class='tree'>",
    make_round_label_row(c("Fase de 32", rep("", 7)), rep(1, 8)),
    make_tree_row(ko, c(74, 77, 73, 75, 83, 84, 81, 82), 1),
    make_connector_row(4, 2, "down"),
    make_round_label_row(c("Oitavas", "", "", ""), rep(2, 4)),
    make_tree_row(ko, c(89, 90, 93, 94), 2),
    make_connector_row(2, 4, "down"),
    make_round_label_row(c("Quartas", ""), rep(4, 2)),
    make_tree_row(ko, c(97, 98), 4),
    make_connector_row(1, 8, "down"),
    make_round_label_row("Semifinal", 8),
    make_tree_row(ko, 101, 8),
    "</div></div>"
  )
}

make_bottom_tree <- function(ko) {
  paste0(
    "<div class='natural-bracket'><h3>Metade inferior do chaveamento</h3><div class='tree'>",
    make_round_label_row("Semifinal", 8),
    make_tree_row(ko, 102, 8),
    make_connector_row(1, 8, "up"),
    make_round_label_row(c("Quartas", ""), rep(4, 2)),
    make_tree_row(ko, c(99, 100), 4),
    make_connector_row(2, 4, "up"),
    make_round_label_row(c("Oitavas", "", "", ""), rep(2, 4)),
    make_tree_row(ko, c(91, 92, 95, 96), 2),
    make_connector_row(4, 2, "up"),
    make_round_label_row(c("Fase de 32", rep("", 7)), rep(1, 8)),
    make_tree_row(ko, c(76, 78, 79, 80, 86, 88, 85, 87), 1),
    "</div></div>"
  )
}

make_center_bracket <- function(ko) {
  final_row <- ko %>% filter(match_id == 104)
  champ <- if (nrow(final_row) > 0) {
    winner_from_prediction(final_row$team1[1], final_row$team2[1], final_row$pred_team1_goals[1], final_row$pred_team2_goals[1], final_row$advance_team[1])
  } else {
    NA_character_
  }

  champ_html <- if (!is.na(champ) && champ != "") {
    paste0("<div class='champ'>Campeão previsto:<br>", team_badge(champ, compact = TRUE), "</div>")
  } else {
    ""
  }

  paste0(
    "<div class='bracket-center'><h3>Final</h3><div class='center-layout'>",
    "<div class='center-line'></div>",
    "<div class='center-cards'>", make_compact_ko_card(final_row), champ_html, "</div>",
    "<div><div class='round-title'>3º lugar</div>", make_compact_ko_card(ko %>% filter(match_id == 103)), "</div>",
    "</div></div>"
  )
}

make_knockout_view_html <- function(p) {
  ko <- p %>%
    filter((source == "knockout") | (!is.na(match_id) & match_id >= 73 & match_id <= 104)) %>%
    filter(match_id >= 73, match_id <= 104) %>%
    arrange(match_id)

  if (nrow(ko) == 0) {
    return("<p class='muted'>Não encontrei palpites do mata-mata para este participante. Talvez ele tenha baixado o CSV antes de completar o chaveamento.</p>")
  }

  paste0(
    "<div class='bracket-wrap'>",
    make_top_tree(ko),
    make_center_bracket(ko),
    make_bottom_tree(ko),
    "</div>"
  )
}

make_third_place_view_html <- function(p) {
  thirds <- p %>% filter(source == "third_place_ranking") %>% arrange(position)
  if (nrow(thirds) == 0) return("")

  rows <- thirds %>%
    mutate(
      cls = if_else(!is.na(qualified) & qualified == 1, "qualified", "not-qualified"),
      html = paste0(
        "<tr class='", cls, "'><td>", position, "º</td><td>", team_badge(team), "</td><td>", html_escape(third_group), "</td><td>",
        if_else(!is.na(qualified) & qualified == 1, "classifica", "não classifica"), "</td></tr>"
      )
    ) %>%
    pull(html) %>%
    paste(collapse = "\n")

  paste0(
    "<section class='card'><h2>Ranking previsto dos terceiros colocados</h2>",
    "<table class='third-table'><thead><tr><th>Pos.</th><th>Seleção</th><th>Grupo</th><th>Status</th></tr></thead><tbody>",
    rows,
    "</tbody></table></section>"
  )
}

make_participant_tab_html <- function(predictions, participant_name) {
  p <- prediction_rows_for_participant(predictions, participant_name)
  tab_id <- paste0("player_", safe_tab_id(participant_name))

  paste0(
    "<section id='", html_escape(tab_id), "' class='tab-panel'>",
    "<div class='card player-header'><h2>Palpites de ", html_escape(participant_name), "</h2>",
    "<p class='muted'>Esta aba mostra o CSV enviado por ", html_escape(participant_name), ": fase de grupos, classificação prevista, ranking dos terceiros e mata-mata.</p></div>",
    "<section class='card'><h2>Fase de grupos</h2>", make_groups_view_html(p), "</section>",
    make_third_place_view_html(p),
    "<section class='card'><h2>Mata-mata previsto</h2>", make_knockout_view_html(p), "</section>",
    "</section>"
  )
}


make_participant_tab_html_versioned <- function(predictions_original, predictions_corrected, participant_name) {
  p_original <- prediction_rows_for_participant(predictions_original, participant_name)
  p_corrected <- prediction_rows_for_participant(predictions_corrected, participant_name)
  tab_id <- paste0("player_", safe_tab_id(participant_name))
  original_id <- paste0(tab_id, "_original")
  corrected_id <- paste0(tab_id, "_corrected")

  paste0(
    "<section id='", html_escape(tab_id), "' class='tab-panel'>",
    "<div class='card player-header'><h2>Palpites de ", html_escape(participant_name), "</h2>",
    "<p class='muted'>Compare o CSV original com a versão que mantém a fase de grupos original e troca apenas o mata-mata pelo chaveamento corrigido.</p>",
    "<div class='version-toggle'>",
    "<button class='version-button active' onclick=\"showPredictionVersion('", html_escape(original_id), "','", html_escape(corrected_id), "', this)\">Original</button>",
    "<button class='version-button' onclick=\"showPredictionVersion('", html_escape(corrected_id), "','", html_escape(original_id), "', this)\">Mata-mata corrigido</button>",
    "</div></div>",

    "<div id='", html_escape(original_id), "' class='prediction-version-panel'>",
    "<section class='card'><h2>Fase de grupos</h2>", make_groups_view_html(p_original), "</section>",
    make_third_place_view_html(p_original),
    "<section class='card'><h2>Mata-mata previsto</h2>", make_knockout_view_html(p_original), "</section>",
    "</div>",

    "<div id='", html_escape(corrected_id), "' class='prediction-version-panel hidden'>",
    "<section class='card'><h2>Fase de grupos</h2>", make_groups_view_html(p_corrected), "</section>",
    make_third_place_view_html(p_corrected),
    "<section class='card'><h2>Mata-mata previsto corrigido</h2>", make_knockout_view_html(p_corrected), "</section>",
    "</div>",
    "</section>"
  )
}

make_rules_html <- function() {
  paste0(
    "<section id='tab_rules' class='tab-panel'>",
    "<section class='card rules'>",
    "<h2>Regras do bolão</h2>",
    "<h3>Pontuação por jogo</h3>",
    "<ul>",
    "<li><strong>Placar exato:</strong> 5 pontos × peso da fase.</li>",
    "<li><strong>Resultado correto</strong> (vencedor/empate): 2 pontos × peso da fase.</li>",
    "<li><strong>Resultado correto + gols de uma seleção:</strong> 3 pontos × peso da fase.</li>",
    "<li><strong>Gols de uma seleção correta, mas resultado errado:</strong> 1 ponto × peso da fase.</li>",
    "</ul>",
    "<h3>Pesos por fase</h3>",
    "<div class='weights'><span>Grupos: 1</span><span>Fase de 32: 1,25</span><span>Oitavas: 1,5</span><span>Quartas: 2</span><span>Semis: 2,5</span><span>Final e 3º lugar: 3</span></div>",
    "<h3>Bônus de classificação</h3>",
    "<ul>",
    "<li><strong>Classificação dos grupos:</strong> 2 pontos por acertar 1º ou 4º; 5 pontos por acertar 2º ou 3º.</li>",
    "<li><strong>Ranking dos terceiros colocados:</strong> 1 ponto por seleção na posição exata.</li>",
    "<li><strong>Campeão:</strong> 100 pontos.</li>",
    "<li><strong>Vice-campeão:</strong> 50 pontos.</li>",
    "<li><strong>Terceiro lugar:</strong> 25 pontos.</li>",
    "</ul>",
    "<p class='muted'>Empates técnicos nas classificações previstas são resolvidos manualmente por quem preencheu o bolão. Para resultados reais, o script usa os critérios disponíveis automaticamente; se a FIFA decidir posições por critérios que não estão nos dados, podemos corrigir manualmente depois.</p>",
    "</section></section>"
  )
}

make_dashboard <- function(scored_original, scored_corrected, results, predictions_original, predictions_corrected, output_path) {
  updated_at <- format(Sys.time(), "%d/%m/%Y %H:%M:%S %Z")
  played_n <- sum(results$played, na.rm = TRUE)
  participants <- scored_original$leaderboard %>% pull(participant) %>% as.character()
  if (length(participants) == 0) {
    participants <- bind_rows(predictions_original, predictions_corrected) %>% pull(participant) %>% unique() %>% na.omit() %>% as.character()
  }

  tabs <- make_tabs_nav(participants)

  score_panel_html <- paste0(
    "<div class='version-toggle score-version-toggle'>",
    "<button id='score_btn_original' class='version-button active' onclick=\"showScoreVersion('original', this)\">Pontuação: original</button>",
    "<button id='score_btn_corrected' class='version-button' onclick=\"showScoreVersion('corrected', this)\">Pontuação: mata-mata corrigido</button>",
    "</div>",
    "<div id='score_version_original' class='score-version-panel'>",
    make_leaderboard_html(scored_original$leaderboard),
    make_score_breakdown_html(scored_original$leaderboard),
    make_match_cards_html(scored_original$match_points, results),
    "</div>",
    "<div id='score_version_corrected' class='score-version-panel hidden'>",
    make_leaderboard_html(scored_corrected$leaderboard),
    make_score_breakdown_html(scored_corrected$leaderboard),
    make_match_cards_html(scored_corrected$match_points, results),
    "</div>"
  )

  participant_tabs <- paste0(
    purrr::map_chr(
      participants,
      ~ make_participant_tab_html_versioned(predictions_original, predictions_corrected, .x)
    ),
    collapse = "\n"
  )

  rules_html <- make_rules_html()

  html <- paste0("<!DOCTYPE html>
<html lang='pt-BR'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <meta name='robots' content='noindex, nofollow'>
  <title>Bolão da Copa 2026</title>
  <style>
    :root { --bg:#f6f7fb; --card:#ffffff; --text:#172026; --muted:#68707a; --line:#e5e7eb; --accent:#155e63; --accent2:#0f766e; --gold:#f59e0b; --green:#dcfce7; --green-text:#166534; --red:#fee2e2; --red-text:#991b1b; --orange:#ffedd5; --orange-text:#9a3412; }
    * { box-sizing:border-box; }
    body { margin:0; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif; background:var(--bg); color:var(--text); }
    .wrap { max-width:1200px; margin:0 auto; padding:24px; }
    header { background:linear-gradient(135deg,#155e63,#0f766e); color:white; border-radius:22px; padding:24px; margin-bottom:18px; box-shadow:0 10px 24px rgba(0,0,0,.08); }
    header h1 { margin:0 0 8px; font-size:34px; }
    header p { margin:4px 0; opacity:.94; }
    .pill { display:inline-block; background:rgba(255,255,255,.16); padding:6px 10px; border-radius:999px; margin-top:10px; font-weight:700; }
    .tabs { position:sticky; top:0; z-index:20; display:flex; gap:8px; flex-wrap:wrap; background:rgba(246,247,251,.94); backdrop-filter:blur(8px); padding:10px 0; margin-bottom:10px; }
    .tab-button { border:1px solid var(--line); background:white; color:var(--text); border-radius:999px; padding:10px 13px; cursor:pointer; font-weight:800; }
    .tab-button.active { background:var(--accent); border-color:var(--accent); color:white; }
    .version-toggle { display:flex; gap:8px; flex-wrap:wrap; margin:8px 0 14px; }
    .version-button { border:1px solid var(--line); background:white; color:var(--text); border-radius:999px; padding:10px 13px; cursor:pointer; font-weight:900; }
    .version-button.active { background:var(--gold); border-color:var(--gold); color:#111827; }
    .player-header { position:sticky; top:64px; z-index:18; border-top:5px solid var(--gold); }
    .player-header .version-toggle { margin-bottom:0; }
    .score-version-panel.hidden, .prediction-version-panel.hidden { display:none; }
    .tab-panel { display:none; }
    .tab-panel.active { display:block; }
    .card { background:var(--card); border:1px solid var(--line); border-radius:18px; padding:18px; margin:16px 0; box-shadow:0 6px 18px rgba(0,0,0,.04); }
    .hero { border-top:5px solid var(--gold); }
    h2 { margin:0 0 14px; }
    h3 { margin:0 0 10px; }
    h4 { margin:0 0 8px; }
    table { width:100%; border-collapse:collapse; }
    th, td { padding:10px 8px; border-bottom:1px solid var(--line); text-align:right; }
    th:first-child, td:first-child, th:nth-child(2), td:nth-child(2) { text-align:left; }
    th { color:var(--muted); font-size:13px; text-transform:uppercase; letter-spacing:.03em; }
    .rank { font-weight:900; color:var(--gold); font-size:20px; }
    .player { font-weight:900; }
    .points { font-weight:900; font-size:20px; color:var(--accent); }
    .muted { color:var(--muted); }
    .summary-grid-wrap { display:grid; grid-template-columns:repeat(auto-fit,minmax(230px,1fr)); gap:12px; margin:16px 0; }
    .summary-card { background:white; border:1px solid var(--line); border-radius:18px; padding:16px; box-shadow:0 6px 18px rgba(0,0,0,.04); position:relative; overflow:hidden; }
    .summary-rank { position:absolute; top:12px; right:12px; color:var(--gold); font-weight:900; }
    .summary-total { font-size:30px; font-weight:950; color:var(--accent); margin:8px 0 12px; }
    .summary-grid { display:grid; grid-template-columns:1fr auto; gap:6px 10px; font-size:14px; }
    .match-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(300px,1fr)); gap:12px; }
    .match-card { border:1px solid var(--line); border-radius:14px; padding:12px; background:#fbfdff; }
    .match-title { display:flex; justify-content:space-between; gap:10px; align-items:center; margin-bottom:10px; }
    .match-id, .mini-meta { color:var(--muted); font-size:12px; font-weight:800; text-transform:uppercase; }
    .true-result { display:flex; align-items:center; justify-content:space-between; gap:8px; margin:8px 0 10px; }
    .bet-row { display:grid; grid-template-columns:1fr auto auto; gap:8px; align-items:center; padding:6px 0; border-top:1px solid #edf0f3; }
    .bet { font-weight:800; }
    .mini-points { background:#e6f4f1; color:#0f766e; border-radius:999px; padding:3px 8px; font-weight:900; margin-left:6px; }
    .team-badge { display:inline-flex; align-items:center; gap:6px; font-weight:900; }
    .flag-img { width:22px; height:15px; object-fit:cover; border-radius:2px; box-shadow:0 0 0 1px rgba(0,0,0,.08); }
    .flag-spacer { width:22px; height:15px; display:inline-block; }
    .groups-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(330px,1fr)); gap:12px; }
    .group-card { border:1px solid var(--line); background:#fbfdff; border-radius:16px; padding:11px; }
    .group-card-grid { display:grid; grid-template-columns:minmax(0,1.35fr) minmax(145px,.65fr); gap:10px; align-items:start; }
    .guess-match { padding:8px 0; border-top:1px solid #edf0f3; }
    .guess-match:first-child { border-top:0; }
    .guess-line { display:grid; grid-template-columns:1fr auto 1fr; gap:6px; align-items:center; }
    .team-chip { border:1px solid var(--line); border-radius:999px; padding:3px 5px; background:white; display:flex; align-items:center; justify-content:center; min-height:28px; font-size:10px; }
    .team-chip.win, .bracket-team.win { background:var(--green); color:var(--green-text); border-color:#86efac; }
    .team-chip.lose, .bracket-team.lose { background:var(--red); color:var(--red-text); border-color:#fecaca; }
    .team-chip.draw, .bracket-team.draw { background:var(--orange); color:var(--orange-text); border-color:#fed7aa; }
    .score-pill { display:inline-block; font-weight:950; background:#111827; color:white; border-radius:999px; padding:5px 8px; min-width:48px; text-align:center; font-size:13px; }
    .mini-table td { padding:5px 3px; font-size:12px; }
    .q { color:var(--green-text); background:var(--green); border-radius:999px; padding:2px 6px; font-size:12px; font-weight:900; }
    .third-table tr.qualified { background:#f0fdf4; }
    .third-table tr.not-qualified { color:var(--muted); }
    .bracket-wrap { display:flex; flex-direction:column; gap:18px; overflow-x:auto; padding-bottom:8px; }
    .natural-bracket { background:#fff; border:1px solid var(--line); border-radius:16px; padding:14px; min-width:980px; }
    .natural-bracket h3 { margin:0 0 8px; font-size:15px; }
    .tree { min-width:980px; }
    .tree-row { display:grid; grid-template-columns:repeat(8,minmax(92px,1fr)); gap:9px; align-items:center; margin:5px 0; }
    .tree-cell { display:flex; justify-content:center; min-width:0; }
    .span-1 { grid-column:span 1; }
    .span-2 { grid-column:span 2; }
    .span-4 { grid-column:span 4; }
    .span-8 { grid-column:span 8; }
    .connector-row { display:grid; grid-template-columns:repeat(8,minmax(92px,1fr)); gap:9px; height:24px; margin:0; }
    .connector-cell { position:relative; }
    .connector-down::before { content:''; position:absolute; left:24%; right:24%; top:7px; border-top:2px solid #cbd5e1; }
    .connector-down::after { content:''; position:absolute; left:50%; top:7px; bottom:0; border-left:2px solid #cbd5e1; }
    .connector-up::before { content:''; position:absolute; left:24%; right:24%; bottom:7px; border-top:2px solid #cbd5e1; }
    .connector-up::after { content:''; position:absolute; left:50%; top:0; bottom:7px; border-left:2px solid #cbd5e1; }
    .round-label-row { display:grid; grid-template-columns:repeat(8,minmax(92px,1fr)); gap:9px; min-width:980px; margin:5px 0 7px; }
    .round-label { font-size:11px; text-transform:uppercase; letter-spacing:.05em; color:var(--muted); font-weight:900; text-align:center; }
    .compact-match { width:100%; max-width:118px; border:1px solid #d7dce2; border-radius:12px; background:#fff; padding:7px; box-shadow:0 1px 3px rgba(0,0,0,.04); }
    .compact-match.empty { color:var(--muted); text-align:center; }
    .compact-title { font-size:10px; color:var(--muted); text-align:center; margin-bottom:5px; text-transform:uppercase; font-weight:900; }
    .compact-row { display:grid; grid-template-columns:1fr 24px; gap:5px; align-items:center; margin:4px 0; border:1px solid transparent; border-radius:8px; padding:4px 5px; font-size:12px; }
    .compact-row .team-badge { font-size:12px; gap:4px; }
    .compact-row .flag-img { width:20px; height:14px; }
    .compact-row strong { text-align:right; font-size:13px; }
    .compact-row.win { background:var(--green); color:var(--green-text); border-color:#86efac; }
    .compact-row.lose { background:var(--red); color:var(--red-text); border-color:#fecaca; }
    .compact-row.draw { background:var(--orange); color:var(--orange-text); border-color:#fed7aa; }
    .compact-advance { font-size:11px; margin-top:5px; color:var(--muted); }
    .bracket-center { background:#fff; border:1px solid var(--line); border-radius:16px; padding:14px; min-width:680px; }
    .center-layout { display:grid; grid-template-columns:1fr 210px 1fr; gap:16px; align-items:center; }
    .center-line { height:2px; background:#cbd5e1; }
    .center-cards { display:flex; flex-direction:column; gap:12px; align-items:center; }
    .center-cards .compact-match { max-width:180px; background:#fffbeb; border-color:#fde68a; }
    .champ { font-size:15px; font-weight:900; background:#fef9c3; border:1px solid #fde68a; padding:10px; border-radius:12px; text-align:center; width:100%; box-sizing:border-box; }
    .bracket-team { display:flex; align-items:center; justify-content:space-between; gap:8px; border:1px solid transparent; border-radius:10px; padding:5px 6px; margin-top:4px; }
    .adv { margin-top:6px; color:var(--muted); font-size:12px; }
    .rules ul { margin-top:6px; }
    .rules li { margin:7px 0; }
    .weights { display:flex; gap:8px; flex-wrap:wrap; }
    .weights span { background:#eef7f6; color:#0f766e; border:1px solid #cde7e4; border-radius:999px; padding:6px 10px; font-weight:900; }
    footer { color:var(--muted); text-align:center; padding:18px; }
    @media(max-width:800px) { .wrap{padding:12px;} header h1{font-size:26px;} .tabs{position:static;} .player-header{top:0;} th,td{font-size:13px;padding:8px 5px;} .leaderboard th:nth-child(4), .leaderboard td:nth-child(4), .leaderboard th:nth-child(5), .leaderboard td:nth-child(5), .leaderboard th:nth-child(6), .leaderboard td:nth-child(6), .leaderboard th:nth-child(7), .leaderboard td:nth-child(7){display:none;} .groups-grid{grid-template-columns:1fr;} .group-card-grid{grid-template-columns:1fr;} .guess-line{grid-template-columns:1fr; text-align:center;} .score-pill{justify-self:center;} .natural-bracket,.tree,.round-label-row,.bracket-center{min-width:0;} .tree-row,.connector-row,.round-label-row{grid-template-columns:1fr;} .tree-cell,.round-label{grid-column:span 1!important;} .connector-row{display:none;} .center-layout{grid-template-columns:1fr;} .center-line{display:none;} }
  </style>
</head>
<body>
  <div class='wrap'>
    <header>
      <h1>Bolão da Copa 2026</h1>
      <p>Placar, resultados e palpites de todos os participantes.</p>
      <span class='pill'>", played_n, " jogos finalizados</span>
      <p style='color:rgba(255,255,255,.75)'>Atualizado em ", html_escape(updated_at), "</p>
    </header>
    ", tabs, "

    <section id='tab_score' class='tab-panel active'>
      ", score_panel_html, "
    </section>

    ", participant_tabs, "
    ", rules_html, "

    <footer>Gerado automaticamente em R.</footer>
  </div>

  <script>
    function showTab(id, button) {
      document.querySelectorAll('.tab-panel').forEach(function(x) { x.classList.remove('active'); });
      document.querySelectorAll('.tab-button').forEach(function(x) { x.classList.remove('active'); });
      var panel = document.getElementById(id);
      if (panel) panel.classList.add('active');
      if (button) button.classList.add('active');
      window.scrollTo({top: 0, behavior: 'smooth'});
    }

    function showScoreVersion(version, button) {
      var original = document.getElementById('score_version_original');
      var corrected = document.getElementById('score_version_corrected');
      if (original) original.classList.toggle('hidden', version !== 'original');
      if (corrected) corrected.classList.toggle('hidden', version !== 'corrected');
      document.querySelectorAll('.score-version-toggle .version-button').forEach(function(x) { x.classList.remove('active'); });
      if (button) button.classList.add('active');
    }

    function showPredictionVersion(showId, hideId, button) {
      var showPanel = document.getElementById(showId);
      var hidePanel = document.getElementById(hideId);
      if (showPanel) showPanel.classList.remove('hidden');
      if (hidePanel) hidePanel.classList.add('hidden');
      var container = button ? button.closest('.tab-panel') : null;
      if (container) {
        container.querySelectorAll('.version-toggle .version-button').forEach(function(x) { x.classList.remove('active'); });
      }
      if (button) button.classList.add('active');
    }
  </script>
</body>
</html>")

  writeLines(html, output_path)
  message("Dashboard written to: ", output_path)
}


# ------------------------------------------------------------
# 6. Corrected knockout prediction helpers
# ------------------------------------------------------------

read_prediction_csv_chr <- function(path) {
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
}

get_prediction_participant <- function(df, fallback_path = NULL) {
  if ("participant" %in% names(df) && any(!is.na(df$participant) & df$participant != "")) {
    return(df$participant[which(!is.na(df$participant) & df$participant != "")[1]])
  }

  if (!is.null(fallback_path)) {
    nm <- tools::file_path_sans_ext(basename(fallback_path))
    nm <- str_remove(nm, "^predictions[_-]?")
    nm <- str_remove(nm, "^palpites[_-]?mata[_-]?mata[_-]?corrigido[_-]?")
    return(nm)
  }

  "participante"
}

repair_original_knockout_89_90 <- function(df) {
  if (!("match_id" %in% names(df))) return(df)

  out <- df %>%
    mutate(match_id_num = suppressWarnings(as.integer(match_id)))

  is_knockout_match <- !is.na(out$match_id_num) & out$match_id_num >= 73 & out$match_id_num <= 104

  # In the first version of the form, the two top Round-of-16 slots were
  # exported with match IDs 89 and 90 flipped. Repair the slot IDs so
  # scores are compared with the correct official match.
  out$match_id_num[is_knockout_match & out$match_id_num == 89] <- -90L
  out$match_id_num[is_knockout_match & out$match_id_num == 90] <- -89L
  out$match_id_num[out$match_id_num == -90L] <- 90L
  out$match_id_num[out$match_id_num == -89L] <- 89L

  if (any(is_knockout_match & suppressWarnings(as.integer(df$match_id)) == 97, na.rm = TRUE)) {
    idx <- which(is_knockout_match & suppressWarnings(as.integer(df$match_id)) == 97)

    if (all(c("team1", "team2") %in% names(out))) {
      tmp <- out$team1[idx]
      out$team1[idx] <- out$team2[idx]
      out$team2[idx] <- tmp
    }

    if (all(c("pred_team1_goals", "pred_team2_goals") %in% names(out))) {
      tmp <- out$pred_team1_goals[idx]
      out$pred_team1_goals[idx] <- out$pred_team2_goals[idx]
      out$pred_team2_goals[idx] <- tmp
    }
  }

  out %>%
    mutate(match_id = if_else(!is.na(match_id_num), as.character(match_id_num), as.character(match_id))) %>%
    select(-match_id_num)
}

build_original_fixed_prediction_folder <- function(original_dir, fixed_dir) {
  dir.create(fixed_dir, recursive = TRUE, showWarnings = FALSE)

  old_files <- list.files(fixed_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)

  original_files <- list.files(original_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(original_files) == 0) stop("No original prediction CSV files found in: ", original_dir)

  for (fp in original_files) {
    original <- read_prediction_csv_chr(fp)
    fixed <- repair_original_knockout_89_90(original)
    write_csv(fixed, file.path(fixed_dir, basename(fp)))
  }

  invisible(fixed_dir)
}

build_corrected_prediction_folder <- function(original_dir, corrected_dir, merged_dir) {
  dir.create(merged_dir, recursive = TRUE, showWarnings = FALSE)

  old_files <- list.files(merged_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)

  original_files <- list.files(original_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(original_files) == 0) stop("No original prediction CSV files found in: ", original_dir)

  corrected_files <- if (dir.exists(corrected_dir)) {
    list.files(corrected_dir, pattern = "\\.csv$", full.names = TRUE)
  } else {
    character()
  }

  corrected_by_participant <- list()
  if (length(corrected_files) > 0) {
    for (fp in corrected_files) {
      df <- read_prediction_csv_chr(fp)
      part <- get_prediction_participant(df, fp)
      corrected_by_participant[[normalize_key(part)]] <- df
    }
  }

  for (fp in original_files) {
    original <- read_prediction_csv_chr(fp)
    part <- get_prediction_participant(original, fp)
    corrected <- corrected_by_participant[[normalize_key(part)]]

    if (!is.null(corrected)) {
      original_without_knockout <- original %>%
        mutate(match_id_num = suppressWarnings(as.integer(match_id))) %>%
        # Keep all non-match rows exported by the original form, such as
        # source == "group_ranking" and source == "third_place_ranking".
        # These often have match_id == NA, so a plain filter(!(between(...)))
        # would accidentally drop them and zero out group-ranking points.
        filter(is.na(match_id_num) | !(match_id_num >= 73 & match_id_num <= 104)) %>%
        select(-match_id_num)

      corrected_knockout <- corrected %>%
        mutate(match_id_num = suppressWarnings(as.integer(match_id))) %>%
        filter(!is.na(match_id_num), match_id_num >= 73, match_id_num <= 104) %>%
        select(-match_id_num) %>%
        mutate(
          participant = part,
          source = if ("source" %in% names(.)) source else "knockout"
        )

      merged <- bind_rows(original_without_knockout, corrected_knockout)
    } else {
      merged <- original
    }

    write_csv(merged, file.path(merged_dir, basename(fp)))
  }

  invisible(merged_dir)
}

# ------------------------------------------------------------
# 6. Run everything
# ------------------------------------------------------------

use_fake_results <- Sys.getenv("BOLAO_USE_FAKE_RESULTS") %in% c("1", "true", "TRUE", "yes", "YES")

if (use_fake_results) {
  fake_results_csv <- file.path(root_dir, "data", "results", "fake_actual_results.csv")

  if (!file.exists(fake_results_csv)) {
    stop(
      "BOLAO_USE_FAKE_RESULTS is on, but I could not find: ",
      fake_results_csv,
      "\nRun: Rscript R/create_fake_results.R"
    )
  }

  message("Using fake results from: ", fake_results_csv)
  results <- read_csv(fake_results_csv, show_col_types = FALSE)
} else {
  results <- fetch_results()
}

results <- results %>%
  mutate(
    match_id = as.integer(match_id),
    team1_goals = suppressWarnings(as.numeric(team1_goals)),
    team2_goals = suppressWarnings(as.numeric(team2_goals)),
    played = if ("played" %in% names(.)) as.logical(played) else (!is.na(team1_goals) & !is.na(team2_goals))
  )

write_csv(results, paths$results_csv)
message("Results written to: ", paths$results_csv)

if (!dir.exists(paths$predictions_dir)) {
  stop("Prediction folder not found: ", paths$predictions_dir)
}

prediction_files <- list.files(paths$predictions_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(prediction_files) == 0) {
  stop("No prediction CSV files found in: ", paths$predictions_dir)
}

build_original_fixed_prediction_folder(
  original_dir = paths$predictions_dir,
  fixed_dir = paths$predictions_original_fixed_dir
)

build_corrected_prediction_folder(
  original_dir = paths$predictions_original_fixed_dir,
  corrected_dir = paths$predictions_corrected_dir,
  merged_dir = paths$predictions_corrected_merged_dir
)

predictions_original <- read_prediction_files(paths$predictions_original_fixed_dir) %>%
  mutate(
    participant = as.character(participant),
    source = if ("source" %in% names(.)) as.character(source) else NA_character_
  )

predictions_corrected <- read_prediction_files(paths$predictions_corrected_merged_dir) %>%
  mutate(
    participant = as.character(participant),
    source = if ("source" %in% names(.)) as.character(source) else NA_character_
  )

scored_original <- score_bolao(
  predictions_input = paths$predictions_original_fixed_dir,
  results_path = paths$results_csv,
  output_dir = file.path(root_dir, "data", "scored_outputs_original")
)

scored_corrected <- score_bolao(
  predictions_input = paths$predictions_corrected_merged_dir,
  results_path = paths$results_csv,
  output_dir = file.path(root_dir, "data", "scored_outputs_corrected")
)

make_dashboard(
  scored_original = scored_original,
  scored_corrected = scored_corrected,
  results = results,
  predictions_original = predictions_original,
  predictions_corrected = predictions_corrected,
  output_path = paths$dashboard_html
)

message("Done.")