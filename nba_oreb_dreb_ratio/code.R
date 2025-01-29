pacman::p_load('tidyverse', 'hoopR', 'gt')


nba_player_box <- load_nba_player_box(2023:hoopR::most_recent_nba_season())


nba_player_box <- nba_player_box %>%
  replace_na(replace = list(offensive_rebounds = 0, defensive_rebounds = 0))

test <- nba_player_box %>%
  group_by(season, athlete_display_name, athlete_headshot_href) %>%
  summarise(
    OREB = sum(offensive_rebounds)
    ,DREB = sum(defensive_rebounds)
  ) %>%
  filter(
    DREB >= 100
  ) %>%
  mutate(
    # REB = OREB + DREB
    ,OREB_DREB_ratio = OREB / DREB
    # ,OREB_ratio = OREB / (OREB + DREB)  # OREB / REB の比率
    # ,athlete_headshot_href = gt::html(paste0('<img src="', athlete_headshot_href, '" height="80">'))
  ) %>%
  arrange(desc(OREB_DREB_ratio)) %>%
  head(n = 10)

# gtテーブルの作成
table_ <- test %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = vars(athlete_headshot_href)),  # vars() を使う
    fn = function(x) {
      mapply(function(url) {
        gt::html(paste0('<img src="', url, '" height="80">'))
      }, x)
    }
  ) %>%
  fmt_percent(
    columns = OREB_DREB_ratio,
    decimals = 1  # 小数点1桁まで
  ) %>%
  cols_move(columns = athlete_headshot_href, after = athlete_display_name) %>%  # 画像を名前の隣に配置
  cols_label(
    athlete_headshot_href = "Player",
    OREB = "OREB",
    DREB = "DREB",
    OREB_DREB_ratio = "OREB / DREB (%)",
  ) %>%
  cols_align(
    align = "center",
    columns = c(athlete_headshot_href, OREB, DREB, OREB_DREB_ratio)
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.font.size = px(14)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_header(
    title = md("**???? NBA Rebounding Analysis**"),
    subtitle = md("*The top 10 players with the highest OREB/DREB ratio in the last 3 seasons are ranked in descending order*")
  ) %>%
  tab_footnote(
    footnote = "Only players with at least 100 DREB are included.",
    locations = cells_column_labels(columns = DREB)
  ) %>%
  tab_source_note(
    source_note = md("**Data Source:** [hoopR](https://hoopr.sportsdataverse.org/)")
  ) 

table_
# PNG 形式で保存
gtsave(table_, "table_output.png")



