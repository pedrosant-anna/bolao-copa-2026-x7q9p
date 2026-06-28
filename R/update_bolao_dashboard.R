# Dashboard versionado do bolão (original x mata-mata corrigido)
# Requisitos:
#   - R/score_bolao_v2.R
#   - data/results/actual_results.csv
#   - data/predictions/
#   - data/predictions_corrected/
# Saída:
#   - docs/index.html

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(jsonlite)
})

root_dir <- getwd()
source(file.path(root_dir, "R", "score_bolao_v2.R"))

pred_dir <- file.path(root_dir, "data", "predictions")
corrected_dir <- file.path(root_dir, "data", "predictions_corrected")
results_path <- file.path(root_dir, "data", "results", "actual_results.csv")
out_dir <- file.path(root_dir, "docs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_csv_chr <- function(path) {
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
}

get_participant_name <- function(df, fallback_path = NULL) {
  if ("participant" %in% names(df) && any(df$participant != "", na.rm = TRUE)) {
    return(df$participant[which(df$participant != "")[1]])
  }
  nm <- tools::file_path_sans_ext(basename(fallback_path))
  nm <- str_remove(nm, "^predictions[_-]?")
  nm <- str_remove(nm, "^palpites[_-]?mata[_-]?mata[_-]?corrigido[_-]?")
  nm
}

files_original <- list.files(pred_dir, pattern = "\\.csv$", full.names = TRUE)
files_corrected <- if (dir.exists(corrected_dir)) list.files(corrected_dir, pattern = "\\.csv$", full.names = TRUE) else character()

corrected_by_participant <- list()
for (fp in files_corrected) {
  df <- read_csv_chr(fp)
  corrected_by_participant[[str_to_lower(get_participant_name(df, fp))]] <- df
}

merged_dir <- file.path(root_dir, "data", "_predictions_corrected_merged")
dir.create(merged_dir, recursive = TRUE, showWarnings = FALSE)

for (fp in files_original) {
  orig <- read_csv_chr(fp)
  part <- get_participant_name(orig, fp)
  corr <- corrected_by_participant[[str_to_lower(part)]]

  merged <- if (!is.null(corr)) {
    bind_rows(
      orig %>%
        mutate(match_id_num = suppressWarnings(as.integer(match_id))) %>%
        filter(!(match_id_num >= 73 & match_id_num <= 104)) %>%
        select(-match_id_num),
      corr %>% mutate(participant = part, source = if ("source" %in% names(.)) source else "knockout")
    )
  } else {
    orig
  }

  write_csv(merged, file.path(merged_dir, basename(fp)))
}

score_original <- score_bolao(pred_dir, results_path)
score_corrected <- score_bolao(merged_dir, results_path)

orig_all <- read_prediction_files(pred_dir)
corr_all <- read_prediction_files(merged_dir)

participants <- sort(unique(c(score_original$leaderboard$participant, score_corrected$leaderboard$participant)))

prep_knockout <- function(df) {
  df %>%
    mutate(match_id = suppressWarnings(as.integer(match_id))) %>%
    filter(match_id >= 73, match_id <= 104) %>%
    transmute(
      match_id,
      stage,
      team1,
      team2,
      pred_team1_goals,
      pred_team2_goals,
      advance_team = if ("advance_team" %in% names(.)) advance_team else NA_character_
    ) %>%
    arrange(match_id)
}

player_payload <- map(participants, function(p) {
  list(
    participant = p,
    original = prep_knockout(orig_all %>% filter(participant == p)),
    corrected = prep_knockout(corr_all %>% filter(participant == p))
  )
})

team_display <- list(
  "South Africa"="África do Sul","Canada"="Canadá","Brazil"="Brasil","Japan"="Japão","Germany"="Alemanha","Paraguay"="Paraguai",
  "Netherlands"="Holanda","Morocco"="Marrocos","Ivory Coast"="Costa do Marfim","Norway"="Noruega","France"="França","Sweden"="Suécia",
  "Mexico"="México","Ecuador"="Equador","England"="Inglaterra","DR Congo"="RD Congo","Belgium"="Bélgica","Senegal"="Senegal",
  "United States"="Estados Unidos","Bosnia and Herzegovina"="Bósnia e Herzegovina","Spain"="Espanha","Austria"="Áustria","Portugal"="Portugal",
  "Croatia"="Croácia","Switzerland"="Suíça","Algeria"="Argélia","Australia"="Austrália","Egypt"="Egito","Argentina"="Argentina",
  "Cape Verde"="Cabo Verde","Colombia"="Colômbia","Ghana"="Gana"
)
flags <- list(
  "South Africa"="🇿🇦","Canada"="🇨🇦","Brazil"="🇧🇷","Japan"="🇯🇵","Germany"="🇩🇪","Paraguay"="🇵🇾","Netherlands"="🇳🇱","Morocco"="🇲🇦",
  "Ivory Coast"="🇨🇮","Norway"="🇳🇴","France"="🇫🇷","Sweden"="🇸🇪","Mexico"="🇲🇽","Ecuador"="🇪🇨","England"="🏴","DR Congo"="🇨🇩",
  "Belgium"="🇧🇪","Senegal"="🇸🇳","United States"="🇺🇸","Bosnia and Herzegovina"="🇧🇦","Spain"="🇪🇸","Austria"="🇦🇹","Portugal"="🇵🇹",
  "Croatia"="🇭🇷","Switzerland"="🇨🇭","Algeria"="🇩🇿","Australia"="🇦🇺","Egypt"="🇪🇬","Argentina"="🇦🇷","Cape Verde"="🇨🇻","Colombia"="🇨🇴","Ghana"="🇬🇭"
)

payload <- list(
  leaderboard_original = score_original$leaderboard,
  leaderboard_corrected = score_corrected$leaderboard,
  players = player_payload,
  team_display = team_display,
  flags = flags
)

payload_json <- toJSON(payload, dataframe = "rows", auto_unbox = TRUE, na = "null")

html_template <- r"---(
<!DOCTYPE html>
<html lang='pt-BR'>
<head>
<meta charset='UTF-8'>
<meta name='viewport' content='width=device-width, initial-scale=1.0'>
<title>Bolão Copa 2026</title>
<style>
body{font-family:Arial,Helvetica,sans-serif;background:#f7f8fb;color:#18212b;margin:0}.page{max-width:1200px;margin:0 auto;padding:18px}.box{background:#fff;border:1px solid #dde3ea;border-radius:16px;padding:14px 16px;margin-bottom:16px}.tabs button{border:none;border-radius:999px;padding:10px 14px;font-weight:700;cursor:pointer;background:#eef2f7;margin-right:8px;margin-bottom:8px}.tabs button.active{background:#0f766e;color:#fff}.hidden{display:none}table{width:100%;border-collapse:collapse}th,td{border-bottom:1px solid #edf0f3;padding:8px 10px;text-align:left}.small{font-size:13px;color:#667085}.pill{display:inline-block;background:#111827;color:white;border-radius:999px;padding:3px 7px;font-weight:700;font-size:12px}.team{display:flex;align-items:center;gap:6px}.section-title{margin:0 0 8px}.subtabs button{border:none;border-radius:999px;padding:8px 12px;font-weight:700;cursor:pointer;background:#eef2f7;margin-right:8px}.subtabs button.active{background:#0f766e;color:#fff}
</style>
</head>
<body>
<div class='page'>
  <div class='box'>
    <h1 class='section-title'>Bolão Copa 2026</h1>
    <div class='small'>Escolha a versão usada na pontuação. Abaixo, em cada participante, você pode alternar entre o palpite original e o mata-mata corrigido.</div>
  </div>

  <div class='box'>
    <div class='tabs'>
      <button id='btn-board-orig' class='active' onclick='switchBoard("original")'>Pontuação: original</button>
      <button id='btn-board-corr' onclick='switchBoard("corrected")'>Pontuação: mata-mata corrigido</button>
    </div>
    <div id='board-original'></div>
    <div id='board-corrected' class='hidden'></div>
  </div>

  <div class='box'>
    <h2 class='section-title'>Palpites por participante</h2>
    <div class='tabs' id='player-tabs'></div>
    <div id='player-view'></div>
  </div>
</div>

<script>
const DATA = __PAYLOAD_JSON__;
function esc(x){return String(x ?? '').replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;').replaceAll('"','&quot;')}
function dTeam(t){return DATA.team_display[t] || t || ''}
function flag(t){return DATA.flags[t] || '🏳️'}
function teamHtml(t){return `<span class="team"><span>${flag(t)}</span><span>${esc(dTeam(t))}</span></span>`}
function leaderboardTable(rows){
  const sorted=[...rows].sort((a,b)=>Number(b.total_points)-Number(a.total_points));
  return `<table><thead><tr><th>Pos.</th><th>Participante</th><th>Pontos</th></tr></thead><tbody>${sorted.map((r,i)=>`<tr><td>${i+1}</td><td>${esc(r.participant)}</td><td><strong>${esc(r.total_points)}</strong></td></tr>`).join('')}</tbody></table>`;
}
function switchBoard(v){
  document.getElementById('board-original').classList.toggle('hidden', v !== 'original');
  document.getElementById('board-corrected').classList.toggle('hidden', v !== 'corrected');
  document.getElementById('btn-board-orig').classList.toggle('active', v === 'original');
  document.getElementById('btn-board-corr').classList.toggle('active', v === 'corrected');
}
let currentPlayer = DATA.players.length ? DATA.players[0].participant : null;
let currentVersion = 'original';
function renderPlayerTabs(){
  document.getElementById('player-tabs').innerHTML = DATA.players.map(p => `<button class='${p.participant===currentPlayer?'active':''}' onclick='selectPlayer(${JSON.stringify(p.participant)})'>${esc(p.participant)}</button>`).join('');
}
function selectPlayer(p){ currentPlayer = p; renderPlayerTabs(); renderPlayerView(); }
function findPlayer(){ return DATA.players.find(x => x.participant === currentPlayer); }
function switchPlayerVersion(v){ currentVersion = v; renderPlayerView(); }
function predictionTable(rows){
  if(!rows || !rows.length) return `<div class='small'>Sem arquivo para esta versão.</div>`;
  return `<table><thead><tr><th>Jogo</th><th>Fase</th><th>Time 1</th><th>Placar</th><th>Time 2</th><th>Desempate</th></tr></thead><tbody>${rows.map(r => `<tr><td>${esc(r.match_id)}</td><td>${esc(r.stage)}</td><td>${teamHtml(r.team1)}</td><td><span class='pill'>${esc(r.pred_team1_goals)} × ${esc(r.pred_team2_goals)}</span></td><td>${teamHtml(r.team2)}</td><td>${r.advance_team ? teamHtml(r.advance_team) : ''}</td></tr>`).join('')}</tbody></table>`;
}
function renderPlayerView(){
  const p = findPlayer();
  if(!p){ document.getElementById('player-view').innerHTML = `<div class='small'>Sem participantes.</div>`; return; }
  const rows = currentVersion === 'original' ? p.original : p.corrected;
  document.getElementById('player-view').innerHTML = `
    <div style='margin-bottom:12px'><strong>${esc(p.participant)}</strong></div>
    <div class='subtabs' style='margin-bottom:12px'>
      <button class='${currentVersion==='original'?'active':''}' onclick='switchPlayerVersion("original")'>Original</button>
      <button class='${currentVersion==='corrected'?'active':''}' onclick='switchPlayerVersion("corrected")'>Mata-mata corrigido</button>
    </div>
    ${predictionTable(rows)}
  `;
}

document.getElementById('board-original').innerHTML = leaderboardTable(DATA.leaderboard_original);
document.getElementById('board-corrected').innerHTML = leaderboardTable(DATA.leaderboard_corrected);
renderPlayerTabs();
renderPlayerView();
</script>
</body>
</html>
)---"

parts <- strsplit(html_template, "__PAYLOAD_JSON__", fixed = TRUE)[[1]]
html <- paste0(parts[1], payload_json, parts[2])
writeLines(html, file.path(out_dir, "index.html"))
message("Dashboard salvo em: ", file.path(out_dir, "index.html"))
