
# ライブラリ読み込みとチーム名マスタ作成 -----------------------------------------------------


pacman::p_load("hoopR", "tidyverse", "lubridate")

conf <- data.frame(
  team_display_name = c(
    "Atlanta Hawks"
    ,"Boston Celtics"
    ,"Brooklyn Nets"
    ,"Charlotte Hornets"
    ,"Chicago Bulls"
    ,"Cleveland Cavaliers"
    ,"Dallas Mavericks"
    ,"Denver Nuggets"
    ,"Detroit Pistons"
    ,"Golden State Warriors"
    ,"Houston Rockets"
    ,"Indiana Pacers"
    ,"LA Clippers"
    ,"Los Angeles Lakers"
    ,"Memphis Grizzlies"
    ,"Miami Heat"
    ,"Milwaukee Bucks"
    ,"Minnesota Timberwolves"
    ,"New Orleans Pelicans"
    ,"New York Knicks"
    ,"Oklahoma City Thunder"
    ,"Orlando Magic"
    ,"Philadelphia 76ers"
    ,"Phoenix Suns"
    ,"Portland Trail Blazers"
    ,"Sacramento Kings"
    ,"San Antonio Spurs"
    ,"Toronto Raptors"
    ,"Utah Jazz"
    ,"Washington Wizards"
  )
  ,conference = c(
    "east"
    ,"east"
    ,"east"
    ,"east"
    ,"east"
    ,"east"
    ,"west"
    ,"west"
    ,"east"
    ,"west"
    ,"west"
    ,"east"
    ,"west"
    ,"west"
    ,"west"
    ,"east"
    ,"east"
    ,"west"
    ,"west"
    ,"east"
    ,"west"
    ,"east"
    ,"east"
    ,"west"
    ,"west"
    ,"west"
    ,"west"
    ,"east"
    ,"west"
    ,"east"
  )
)


# データの読み込み ----------------------------------------------------------------


# raw <- load_nba_team_box(seasons = seq(2004, 2025, by = 1)) #2025-01-10読み込み
# write_csv(raw, "./01_data/box_score.csv")

raw <- read_csv("./01_data/box_score.csv") %>%
  filter(season != 2004) # キリが悪いので除外


# データ確認コード ----------------------------------------------------------------


# カンファレンス確認用
# raw %>%
#   distinct(team_display_name) %>%
#   inner_join(
#     conf
#     ,by = "team_display_name"
#   ) %>%
#   view()

# データ確認用
# raw %>%
#   group_by(season_type, season) %>%
#   summarise(
#     cnt = n()
#     ,min_date = min(game_date)
#     ,max_date = max(game_date)
#   )
# 2025シーズンを2025-01-09時点で見ると、season_type = 2はレギュラーシーズンで3はプレイオフ。2の中にはおそらくインシーズントーナメントも含まれる


# ベースとなるデータフレーム作成 ---------------------------------------------------------

nba_team_box <- raw %>%
  mutate(
    win_flag = if_else(team_winner == TRUE, 1, 0)
    ,game_flag = 1
  ) %>%
  filter(
    season_type == 2#レギュラーシーズンに限定
    ,team_display_name != "Eastern Conf All-Stars"
    ,team_display_name != "Western Conf All-Stars"
    ,game_date != "2024-12-17" # インシーズントーナメント除外
    ,game_date != "2023-12-09" # インシーズントーナメント除外
  ) %>% 
  select(
    game_id
    ,season
    ,game_date
    ,team_display_name
    ,win_flag
    ,game_flag
  ) %>%
  left_join(
    conf
    ,by = "team_display_name"
  ) %>%
  group_by(season, team_display_name) %>%
  arrange(game_date) %>%
  mutate(
    win_cnt = cumsum(win_flag) - win_flag #試合前のステータスにする
    ,game_cnt = cumsum(game_flag) - 1 #試合前のステータスにする
    ,win_ratio = win_cnt / game_cnt
  ) %>%
  ungroup()

# 試合日時点での順位算出
## 試合日のリスト作成
game_date_df <- nba_team_box %>%
  select(season, game_date_base = game_date)

test <- game_date_df %>%
  left_join(
    nba_team_box
    ,by = "season"
  ) %>%
  filter(
    game_date_base >= game_date
  ) %>%
  mutate(
    game_date_diff = game_date_base - game_date
  ) %>%
  distinct()

test2 <- test %>%
  group_by(game_date_base, team_display_name) %>%
  summarise(
    min_game_date_diff = min(game_date_diff)
  )

test3 <- test %>% #NBAで何かしらの試合日がある時点での各チームの勝率リスト(試合開始前)
  inner_join(
    test2
    ,by = c("game_date_base", "team_display_name", "game_date_diff" = "min_game_date_diff")
  )

test4 <- test3 %>% # 各チームの試合前時点でのカンファレンス内順位
  group_by(game_date_base, conference) %>%
  mutate(
    win_ratio_ranking = min_rank(desc(win_ratio))
  ) %>%
  select(
    season
    ,game_date_base
    ,game_id
    ,team_display_name
    ,conference
    ,win_cnt
    ,win_flag
    ,win_ratio_ranking
  )

t <- nba_team_box %>%
  left_join(
    test4
    ,by = c("season", "game_id", "game_date" = "game_date_base", "team_display_name", "conference", "win_cnt")
  ) %>%
  select(
    game_id, season, game_date, team_display_name, conference, win_ratio, win_ratio_ranking, win_cnt
  )
wide_t <- t %>%
  left_join(
    t %>% select(game_id, team_display_name_opp = team_display_name, conference_opp = conference, win_ratio_opp = win_ratio, win_ratio_ranking_opp = win_ratio_ranking, win_cnt_opp = win_cnt)
    ,by = "game_id"
  ) %>%
  filter(
    team_display_name != team_display_name_opp
    , month(game_date) %in% c(1, 2, 3, 4, 5)
  )

top_battle <- wide_t %>%
  mutate(
    top_battle_flag = case_when(
      conference != conference_opp & win_ratio_ranking == 1 & win_ratio_ranking_opp == 1 ~ 1
      ,TRUE ~ 0
    )
    ,total_win_ratio = win_ratio + win_ratio_opp
  ) %>%
  filter(top_battle_flag == 1)

top_second_battle <- wide_t %>%
  mutate(
    top_battle_flag = case_when(
      conference != conference_opp & win_ratio_ranking %in% c(1,2) & win_ratio_ranking_opp %in% c(1,2) ~ 1
      ,TRUE ~ 0
    )
    ,total_win_ratio = win_ratio + win_ratio_opp
  ) %>%
  filter(top_battle_flag == 1)

# 推移作成用のデータフレームマスタ
seasons <- 2005:2025
df <- data.frame(
  season = rep(seasons, each = 2)
  ,game_type = rep(c("top_1_vs_top_1_matches", "top_2_vs_top_2_matches"), times = length(years))
)
game_cnt_result <- bind_rows(
  top_battle %>% group_by(season) %>% summarise(game_cnt = n() / 2) %>% mutate(game_type = "top_1_vs_top_1_matches")
  ,top_second_battle %>% group_by(season) %>% summarise(game_cnt = n() / 2) %>% mutate(game_type = "top_2_vs_top_2_matches")
)

g_en <- df %>%
  left_join(
    game_cnt_result
    ,by = c("season", "game_type")
  ) %>%
  mutate(game_cnt = if_else(is.na(game_cnt), 0, game_cnt))%>%
  ggplot(aes(x = season, y = game_cnt, colour = game_type))+
  geom_line(size = 1)+
  geom_point(size = 2.5)+
  scale_color_manual(
    values = c("#FF9999", "#99CCFF")
    ,labels = c("Top 1 vs Top 1 Matches", "Top 2 vs Top 2 Matches") 
  ) + # 薄めの赤 (#FF9999) と青 (#99CCFF)
  scale_x_continuous(breaks = seq(2005, 2025, 5)) + # x軸のラベル間隔を調整
  scale_y_continuous(breaks = seq(0, 15, 3)) + 
  labs(
    title = "Trends in Matchups Between Top NBA Conference Teams",
    subtitle = "Visualizing the number of matches between top 1 teams\nand top 2 teams in Eastern and Western Conferences",
    x = "Season",
    y = "Game Count",
    colour = "Matchup Type"
    ,caption = "Data represents NBA conference matchups from 2005 to 2025"
  ) +
  theme_classic(base_size = 18) + # 全体のフォントサイズを調整
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # タイトルを強調
    plot.subtitle = element_text(size = 16, hjust = 0.5), # サブタイトルのスタイル
    plot.caption = element_text(size = 14, hjust = 1, margin = margin(t = 10)), # Position and style caption
    legend.position = "top", # 凡例の位置を上部に
    legend.title = element_text(size = 16), # 凡例タイトルのフォントサイズ
    legend.text = element_text(size = 14) # 凡例ラベルのフォントサイズ
    ,axis.text = element_text(size = 14), # Adjust axis text font size
    axis.title = element_text(size = 16) # Adjust axis title font size
  )
g_en
ggsave(
  filename = "./02_output/top_team_matchup_en.png",           # 保存するファイル名
  plot = g_en,               # 最後に作成したプロットを保存
  width = 1200 / 96,                # 幅（ピクセル -> インチ: 1200ピクセル）
  height = 675 / 96,                # 高さ（ピクセル -> インチ: 675ピクセル）
  dpi = 96,                         # 解像度（96dpiがTwitterで推奨）
  units = "in"                      # 単位をインチで指定
)

