# Bolão Copa 2026 — Dashboard automático

## Estrutura sugerida

```text
bolao-copa/
  R/
    update_bolao_dashboard.R
    score_bolao_v2.R
  data/
    predictions/
      predictions_pedro.csv
      predictions_amigo.csv
    results/
      actual_results.csv      # gerado automaticamente
    scored_outputs/           # gerado automaticamente
  docs/
    index.html                # dashboard final
  .github/
    workflows/
      update-bolao-dashboard.yml
```

## Como rodar localmente

```r
install.packages(c(
  "dplyr", "readr", "purrr", "stringr", "tidyr", "jsonlite",
  "httr2", "lubridate", "stringi"
))
```

Depois:

```bash
Rscript R/update_bolao_dashboard.R
```

Abra:

```text
docs/index.html
```

## Fonte dos resultados

O script tenta usar `API_FOOTBALL_KEY`, se essa variável existir. Se não existir, usa o `openfootball/worldcup.json` como fallback.

Para usar API-Football no GitHub Actions:

1. Crie uma conta no API-Football/API-SPORTS.
2. Copie sua chave de API.
3. No GitHub: `Settings > Secrets and variables > Actions > New repository secret`.
4. Nome: `API_FOOTBALL_KEY`.
5. Valor: sua chave.

## Publicar no GitHub Pages

1. Suba o repositório para o GitHub.
2. Vá em `Settings > Pages`.
3. Em `Build and deployment`, escolha `Deploy from a branch`.
4. Escolha a branch `main` e a pasta `/docs`.
5. O dashboard ficará disponível como uma página estática.

## Atualização automática

O workflow em `.github/workflows/update-bolao-dashboard.yml` roda automaticamente a cada 30 minutos e também pode ser rodado manualmente em `Actions > Update Bolao Dashboard > Run workflow`.
