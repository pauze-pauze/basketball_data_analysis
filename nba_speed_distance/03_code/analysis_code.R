
# import library and data -------------------------------------------------

pacman::p_load("tidyverse", "showtext", "ggpubr", "gridExtra", "scales")

r_team <- read_csv("./01_data/r_team.csv") # チームの移動距離データ
r_player <- read_csv("./01_data/r_player.csv") # 選手の移動距離データ

# Set parameters ----------------------------------------------------------

mile_to_km <- 1.60934 # マイルをキロメートルに変換する値
# フォント追加と有効化
font_add_google("Noto Sans JP")
showtext_auto()
# Avg SpeedとDist Milesと試合時間から算出されるAvg Speedや移動距離の数字のズレの確認 -------------------------------------
# # 平均時速推移確認用
# r_team %>%
#   mutate(
#     Avg_Speed_cal = Dist_Miles / MIN * 60 # Dist_MilesとMINから平均時速を計算
#   ) %>%
#   group_by(season) %>%
#   summarise(
#     Avg_Speed = mean(Avg_Speed) * mile_to_km
#     ,Avg_Speed_cal = mean(Avg_Speed_cal) * mile_to_km
#   ) %>%
#   ggplot(aes(x = season))+
#   geom_line(aes(y = Avg_Speed, color = "サイト上の平均時速", group = 1), size = 1) +  # 線の太さを1.2に設定
#   geom_point(aes(y = Avg_Speed, color = "サイト上の平均時速"), size = 2) +  # 点を追加、サイズを3に設定
#   # Avg_Speed_calの線と点
#   geom_line(aes(y = Avg_Speed_cal, color = "計算上の平均時速", group = 1), size = 1) +  # 線の太さを1.2に設定
#   geom_point(aes(y = Avg_Speed_cal, color = "計算上の平均時速"), size = 2) +  # 点を追加、サイズを3に設定
#   labs(
#     title = "公式サイト上の平均時速と計算上の平均時速の違い",
#     x = "シーズン",
#     y = "平均時速(km)",
#     color = "凡例"  # 凡例のタイトルを設定
#   )+
#   theme_minimal()+
#   theme(text = element_text(family = "Noto Sans JP"))
# 


## 1試合あたりの移動距離換算
g <- r_team %>%
  mutate(
    Distance = Dist_Miles * mile_to_km
    ,Distance_cal = MIN * Avg_Speed / 60 * mile_to_km #計算上の総移動距離算出
  ) %>%
  group_by(season) %>%
  summarise(
    Distance = sum(Distance)
    ,MIN = sum(MIN)
    ,Distance_cal = sum(Distance_cal)
  ) %>%
  mutate(
    Distance_per_game = Distance / MIN * 48
    ,Distance_cal_per_game = Distance_cal / MIN * 48
  ) %>%
  ggplot(aes(x = season))+
  geom_line(aes(y = Distance_per_game, color = "サイト上の平均移動距離", group = 1), size = 1) +  # 線の太さを1.2に設定
  geom_point(aes(y = Distance_per_game, color = "サイト上の平均移動距離"), size = 2) +  # 点を追加、サイズを3に設定
  # Avg_Speed_calの線と点
  geom_line(aes(y = Distance_cal_per_game, color = "計算上の平均移動距離", group = 1), size = 1) +  # 線の太さを1.2に設定
  geom_point(aes(y = Distance_cal_per_game, color = "計算上の平均移動距離"), size = 2) +  # 点を追加、サイズを3に設定
  labs(
    title = "公式サイト上の平均移動距離と計算上の平均移動距離の違い",
    x = "シーズン",
    y = "1試合の平均移動距離(km)",
    color = "凡例"  # 凡例のタイトルを設定
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    # ,axis.text.x = element_text(size = 12)
    # ,axis.text.y = element_text(size = 12)
  )
g
ggsave(file=  "./02_output/check_definition.png", plot = g, , dpi = 100, width = 9, height = 6)


# 決め手はないけど、公式サイト上のAvg Speedを採用する。移動距離を算出する場合も。
# Preprocess data ---------------------------------------------------------
r_team <- r_team %>%
  mutate(
    Dist_km = Dist_Miles * mile_to_km
    ,Dist_km_Off = Dist_Miles_Off * mile_to_km
    ,Dist_km_Def = Dist_Miles_Def * mile_to_km
    ,Avg_Speed_km = Avg_Speed * mile_to_km
    ,Avg_Speed_km_Off = Avg_Speed_Off * mile_to_km
    ,Avg_Speed_km_Def = Avg_Speed_Def * mile_to_km
  )
r_player <- r_player %>%
  mutate(
    Dist_km = Dist_Miles * mile_to_km
    ,Dist_km_Off = Dist_Miles_Off * mile_to_km
    ,Dist_km_Def = Dist_Miles_Def * mile_to_km
    ,Avg_Speed_km = Avg_Speed * mile_to_km
    ,Avg_Speed_km_Off = Avg_Speed_Off * mile_to_km
    ,Avg_Speed_km_Def = Avg_Speed_Def * mile_to_km
  )



# シーズンごとのリーグ全体の移動距離推移 -----------------------------------------------------

## 1試合あたりの移動距離
g <- r_team %>%
  mutate(
    Distance =  MIN * Avg_Speed_km / 60  #計算上の総移動距離算出
  ) %>%
  group_by(season) %>%
  summarise(
    Distance = sum(Distance)
    ,MIN = sum(MIN)
  ) %>%
  mutate(
    Distance_per_game = Distance / MIN * 48
  ) %>%
  ggplot(aes(x = season))+
  geom_line(aes(y = Distance_per_game, group = 1), size = 1) +  # 線の太さを1.2に設定
  geom_point(aes(y = Distance_per_game, group = 1), size = 2) +  # 点を追加、サイズを3に設定
  scale_y_continuous(
    breaks = seq(5.3, 5.6, length = 4)
    ,limits = c(5.3, 5.6)
  )+
  labs(
    title = "1試合の平均移動距離のシーズン推移",
    subtitle = "試合中(48分換算)にチームが平均で移動する距離",
    x = "シーズン",
    y = "1試合の平均移動距離(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/distance_per_game_league.png", plot = g, , dpi = 100, width = 8, height = 6)

# シーズンごとの分布も踏まえた平均時速の推移
g <- r_team %>%
  ggplot(aes(x = season, y = Avg_Speed_km))+
  geom_violin(fill = "lightgreen", color = "darkgreen", alpha = 0.7) +  # バイオリンプロット
  # geom_boxplot(fill = "lightblue", color = "darkblue") +  # 箱ひげ図
  stat_summary(
    fun = mean, geom = "line", aes(group = 1), size = 0.8  # 平均値の線
  ) +
  stat_summary(
    fun = mean, geom = "point", size = 1.5  # 平均値の点
  ) +
  # scale_y_continuous(
  #   breaks = seq(6.4, 7.6, length = 3)
  #   ,limits = c(6.4, 7.6)
  # )+
  scale_y_continuous(
    breaks = seq(6.2, 7.8, length = 3)
    ,limits = c(6.2, 7.8)
  )+
  labs(
    title = "平均時速推移",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_league.png", plot = g, , dpi = 100, width = 8, height = 6)

# 移動スピードランキング_チーム ---------------------------------------------------------

# 平均時速の上位10チームと下位10チームを出力する
## 上位10チーム
g <- r_team %>%
  select(season, Team, Avg_Speed_km)%>%
  arrange(desc(Avg_Speed_km)) %>%
  mutate(Avg_Speed_km = round(Avg_Speed_km, 2)) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速(km)" = Avg_Speed_km
  ) %>%
  head(n = 10) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "移動スピード上位10チーム", face = "plain", size = 12)
g
# ggsave(file=  "./02_output/average_speed_rank_top10.png", plot = g, dpi = 100, width = 12, height = 9)

## 下位10チーム
g <- r_team %>%
  select(season, Team, Avg_Speed_km)%>%
  arrange(Avg_Speed_km) %>%
  mutate(Avg_Speed_km = round(Avg_Speed_km, 2)) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速(km)" = Avg_Speed_km
  ) %>%
  head(n = 10) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  ) %>%
  tab_add_title(text = "移動スピード下位10チーム", face = "plain", size = 12)
g
# ggsave(file=  "./02_output/average_speed_rank_bottom10.png", plot = g, dpi = 100, width = 12, height = 9)


# オフェンスの平均時速の上位10チームと下位10チームを出力する
## 上位10チーム
g <- r_team %>%
  select(season, Team, Avg_Speed_km_Off)%>%
  arrange(desc(Avg_Speed_km_Off)) %>%
  mutate(Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速(km)" = Avg_Speed_km_Off
  ) %>%
  head(n = 10) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "OFFでの移動スピード上位10チーム", face = "plain", size = 12)
g
# ggsave(file=  "./02_output/average_speed_OFF_rank_top10.png", plot = g, dpi = 100, width = 12, height = 9)

## 下位10チーム
g <- r_team %>%
  select(season, Team, Avg_Speed_km_Off)%>%
  arrange(Avg_Speed_km_Off) %>%
  mutate(Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速(km)" = Avg_Speed_km_Off
  ) %>%
  head(n = 10) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "OFFでの移動スピード下位10チーム", face = "plain", size = 12)
g
# ggsave(file=  "./02_output/average_speed_OFF_rank_bottom10.png", plot = g, dpi = 100, width = 12, height = 9)


#移動スピード上位30チームを全体、OFF、DEFの順位合わせて作成
g <- r_team %>%
  select(season, Team, Avg_Speed_km, Avg_Speed_km_Off, Avg_Speed_km_Def)%>%
  mutate(
    rank_avg_speed = min_rank(desc(Avg_Speed_km))
    ,rank_avg_speed_Off = min_rank(desc(Avg_Speed_km_Off))
    ,rank_avg_speed_Def = min_rank(desc(Avg_Speed_km_Def))
  ) %>%
  arrange(desc(Avg_Speed_km)) %>%
  mutate(
    Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速_全体" = Avg_Speed_km
    ,"順位_全体" = rank_avg_speed
    ,"時速_OFF" = Avg_Speed_km_Off
    ,"順位_OFF" = rank_avg_speed_Off
    ,"時速_DEF" = Avg_Speed_km_Def
    ,"順位_DEF" = rank_avg_speed_Def
  ) %>%
  head(n = 20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "移動スピード上位20チーム", face = "plain", size = 12)
g
# g %>% ggexport(filename =  "./02_output/average_speed_rank_top20_all_num.png",width = 800, height = 600)

#移動スピード下位30チームを全体、OFF、DEFの順位合わせて作成
g <- r_team %>%
  select(season, Team, Avg_Speed_km, Avg_Speed_km_Off, Avg_Speed_km_Def)%>%
  mutate(
    rank_avg_speed = min_rank(Avg_Speed_km)
    ,rank_avg_speed_Off = min_rank(Avg_Speed_km_Off)
    ,rank_avg_speed_Def = min_rank(Avg_Speed_km_Def)
  ) %>%
  arrange(Avg_Speed_km) %>%
  mutate(
    Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  rename(
    "シーズン" = season
    ,"チーム名" = Team
    ,"時速_全体" = Avg_Speed_km
    ,"順位_全体" = rank_avg_speed
    ,"時速_OFF" = Avg_Speed_km_Off
    ,"順位_OFF" = rank_avg_speed_Off
    ,"時速_DEF" = Avg_Speed_km_Def
    ,"順位_DEF" = rank_avg_speed_Def
  ) %>%
  head(n = 20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "移動スピード下位20チーム", face = "plain", size = 12)

# タイトルとテーブルを組み合わせ
g
# g %>% ggexport(filename =  "./02_output/average_speed_rank_bottom20_all_num.png",width = 800, height = 600)


# 移動スピードランキング_選手 ----------------------------------------------------------

#移動スピード上位20チームを全体、OFF、DEFの順位合わせて作成
g <- r_player %>%
  filter(MIN >= 500) %>% #数値は仮だが、数分しか出ていない選手を除ければよしとする
  select(season, PLAYER, Avg_Speed_km, Avg_Speed_km_Off, Avg_Speed_km_Def)%>%
  mutate(
    rank_avg_speed = min_rank(desc(Avg_Speed_km))
    ,rank_avg_speed_Off = min_rank(desc(Avg_Speed_km_Off))
    ,rank_avg_speed_Def = min_rank(desc(Avg_Speed_km_Def))
  ) %>%
  arrange(desc(Avg_Speed_km)) %>%
  mutate(
    Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  rename(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"時速_全体" = Avg_Speed_km
    ,"順位_全体" = rank_avg_speed
    ,"時速_OFF" = Avg_Speed_km_Off
    ,"順位_OFF" = rank_avg_speed_Off
    ,"時速_DEF" = Avg_Speed_km_Def
    ,"順位_DEF" = rank_avg_speed_Def
  ) %>%
  head(n = 20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "移動スピード上位20選手", face = "plain", size = 12)
g
g %>% ggexport(filename =  "./02_output/average_speed_rank_top20_player_all_num.png",width = 800, height = 600)

#移動スピード下位20選手を全体、OFF、DEFの順位合わせて作成
g <- r_player %>%
  filter(MIN >= 500) %>% #数値は仮だが、数分しか出ていない選手を除ければよしとする
  select(season, PLAYER, Avg_Speed_km, Avg_Speed_km_Off, Avg_Speed_km_Def)%>%
  mutate(
    rank_avg_speed = min_rank(Avg_Speed_km)
    ,rank_avg_speed_Off = min_rank(Avg_Speed_km_Off)
    ,rank_avg_speed_Def = min_rank(Avg_Speed_km_Def)
  ) %>%
  arrange(Avg_Speed_km) %>%
  mutate(
    Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  rename(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"時速_全体" = Avg_Speed_km
    ,"順位_全体" = rank_avg_speed
    ,"時速_OFF" = Avg_Speed_km_Off
    ,"順位_OFF" = rank_avg_speed_Off
    ,"時速_DEF" = Avg_Speed_km_Def
    ,"順位_DEF" = rank_avg_speed_Def
  ) %>%
  head(n = 20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "移動スピード下位20選手", face = "plain", size = 12)
# タイトルとテーブルを組み合わせ
g
# g %>% ggexport(filename =  "./02_output/average_speed_rank_bottom20_player_all_num.png",width = 800, height = 600)



# 移動スピードと勝率の相関 ------------------------------------------------------------

g <- r_team %>%
  mutate(
    win_ratio = W / GP
    ,label = case_when(
      season == "2017-18" & Team == "Houston Rockets" ~ "HOU"
      ,season == "2018-19" & Team == "Houston Rockets" ~ "HOU"
      ,season == "2024-25"& Team == "Memphis Grizzlies" ~ "MEM"
      ,TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x = Avg_Speed_km, y = win_ratio))+
  geom_point()+
  geom_text(aes(label = label), hjust = 0.5, vjust = -0.3)+
  facet_wrap(~season)+
  labs(
    title = "移動スピードと勝率の散布図",
    x = "平均時速(km)",
    y = "勝率",
  )+
  scale_y_continuous(labels = percent)+
  scale_x_continuous(
    breaks = seq(6.3, 7.5, length = 3)
    ,limits = c(6.3, 7.5)
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 10)
    ,axis.text.y = element_text(size = 10)
  )
g
# ggsave(file=  "./02_output/average_speed_win_ratio.png", plot = g, dpi = 100, width = 12, height = 9)



# チームの移動スピード推移 ------------------------------------------------------------
## 単純な推移 -----------------------------------------------------------------
g <- r_team %>%
  mutate(season = str_extract(season, "^\\d{4}"))%>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  facet_wrap(~Team, nrow = 6)+
  labs(
    title = "チームごとの移動スピードのシーズン推移",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    # ,axis.text.x = element_text(size = 12)
    # ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_team_season.png", plot = g, , dpi = 100, width = 18, height = 12)


## 前年からの変化が大きいチーム抽出 -----------------------------------------------------------------
### 上位20チーム -----------------------------------------------------------------

g <- r_team %>%
  arrange(Team, season) %>%
  group_by(Team) %>%
  mutate(
    Avg_Speed_ly = lag(Avg_Speed_km)
    ,yoy_ratio = Avg_Speed_km / Avg_Speed_ly
  ) %>%
  ungroup() %>%
  mutate(
    yoy_rank = min_rank(desc(yoy_ratio))
    ,Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_ly = round(Avg_Speed_ly, 2)
  ) %>%
  filter(is.na(yoy_ratio) == FALSE) %>%
  arrange(desc(yoy_ratio)) %>%
  mutate(yoy_ratio = percent(round(yoy_ratio, 2))) %>%
  select(
    "シーズン" = season
    ,"チーム名" = Team
    ,"平均時速(km)" = Avg_Speed_km
    ,"前シーズンの平均時速" = Avg_Speed_ly
    ,"前シーズン比" = yoy_ratio
    ,"順位" = yoy_rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "前シーズンから移動スピードが伸びた上位20チーム", face = "plain", size = 12)
g
# 直近2シーズンはやっぱり移動スピード伸びがちな傾向？
g %>% ggexport(filename =  "./02_output/average_speed_yoy_rank_top20_team_all_num.png",width = 800, height = 600)

### 下位20チーム -----------------------------------------------------------------

g <- r_team %>%
  arrange(Team, season) %>%
  group_by(Team) %>%
  mutate(
    Avg_Speed_ly = lag(Avg_Speed_km)
    ,yoy_ratio = Avg_Speed_km / Avg_Speed_ly
  ) %>%
  ungroup() %>%
  mutate(
    yoy_rank = min_rank(yoy_ratio)
    ,Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_ly = round(Avg_Speed_ly, 2)
  ) %>%
  filter(is.na(yoy_ratio) == FALSE) %>%
  arrange(yoy_ratio) %>%
  mutate(yoy_ratio = percent(round(yoy_ratio, 2))) %>%
  select(
    "シーズン" = season
    ,"チーム名" = Team
    ,"平均時速(km)" = Avg_Speed_km
    ,"前シーズンの平均時速" = Avg_Speed_ly
    ,"前シーズン比" = yoy_ratio
    ,"順位" = yoy_rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "前シーズンから移動スピードが減った下位20チーム", face = "plain", size = 12)
g
# 2024-25のSASは特徴的？
# g %>% ggexport(filename =  "./02_output/average_speed_yoy_rank_bottom20_team_all_num.png",width = 800, height = 600)



# 選手の移動スピード推移 -------------------------------------------------------------
## レブロン -------------------------------------------------------------
g <- r_player %>%
  filter(PLAYER == "LeBron James") %>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  geom_text(aes(label = MIN), vjust = 0.3, hjust = 1.2, size = 3.5)+
  labs(
    title = "レブロン・ジェームズの移動スピードのシーズン推移",
    subtitle = "グラフ内の数値は出場時間(分)",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_player_lbj_season.png", plot = g, , dpi = 100, width = 9, height = 6)
## カリー -------------------------------------------------------------
g <- r_player %>%
  filter(PLAYER == "Stephen Curry", season != "2019-20") %>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  geom_text(aes(label = MIN), vjust = 0.3, hjust = 1.2, size = 3.5)+
  labs(
    title = "ステフィン・カリーの移動スピードのシーズン推移",
    subtitle = "グラフ内の数値は出場時間(分)。2019-20は出場時間が短いため除外。",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_player_curry_season.png", plot = g, , dpi = 100, width = 9, height = 6)


## ヨキッチ -------------------------------------------------------------
g <- r_player %>%
  filter(PLAYER == "Nikola Joki??") %>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  geom_text(aes(label = MIN), vjust = 0.3, hjust = 1.2, size = 3.5)+
  labs(
    title = "ニコラ・ヨキッチの移動スピードのシーズン推移",
    subtitle = "グラフ内の数値は出場時間(分)。",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_player_jokic_season.png", plot = g, dpi = 100, width = 9, height = 6)

## SGA -------------------------------------------------------------
g <- r_player %>%
  filter(PLAYER == "Shai Gilgeous-Alexander") %>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  geom_text(aes(label = MIN), vjust = 0.3, hjust = 1.2, size = 3.5)+
  labs(
    title = "SGAの移動スピードのシーズン推移",
    subtitle = "グラフ内の数値は出場時間(分)。",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_player_sga_season.png", plot = g, dpi = 100, width = 9, height = 6)

## KD -------------------------------------------------------------
g <- r_player %>%
  filter(PLAYER == "Kevin Durant") %>%
  ggplot(aes(x = season, y = Avg_Speed_km, group = 1))+
  geom_line()+
  geom_text(aes(label = MIN), vjust = 0.3, hjust = 1.2, size = 3.5)+
  labs(
    title = "KDの移動スピードのシーズン推移",
    subtitle = "グラフ内の数値は出場時間(分)。",
    x = "シーズン",
    y = "平均時速(km)",
  )+
  theme_bw()+
  theme(
    text = element_text(family = "Noto Sans JP")
    ,axis.text.x = element_text(size = 12)
    ,axis.text.y = element_text(size = 12)
  )
g
# ggsave(file=  "./02_output/average_speed_player_kd_season.png", plot = g, dpi = 100, width = 9, height = 6)


## 前年からの変化が大きい選手 -------------------------------------------------------------
### 上位20選手 -------------------------------------------------------------
g <- r_player %>%
  arrange(PLAYER, season) %>%
  group_by(PLAYER) %>%
  mutate(
    Avg_Speed_ly = lag(Avg_Speed_km)
    ,MIN_ly = lag(MIN)
    ,yoy_ratio = Avg_Speed_km / Avg_Speed_ly
  ) %>%
  ungroup() %>%
  filter(
    MIN >= 500
    ,MIN_ly >= 500
  ) %>%
  mutate(
    yoy_rank = min_rank(desc(yoy_ratio))
    ,Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_ly = round(Avg_Speed_ly, 2)
  ) %>%
  filter(is.na(yoy_ratio) == FALSE) %>%
  arrange(desc(yoy_ratio)) %>%
  mutate(yoy_ratio = percent(round(yoy_ratio, 2))) %>%
  select(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"平均時速(km)" = Avg_Speed_km
    ,"前シーズンの平均時速" = Avg_Speed_ly
    ,"前シーズン比" = yoy_ratio
    ,"順位" = yoy_rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "前シーズンと当シーズンの両方で500分以上出場した選手が対象", size = 8) %>%
  tab_add_title(text = "前シーズンから移動スピードが伸びた上位20選手", face = "plain", size = 12, padding = unit(0.1, "line"))
g
# KDは面白そう
# g %>% ggexport(filename =  "./02_output/average_speed_yoy_rank_top20_player_all_num.png",width = 800, height = 600)

### 下位20選手 -------------------------------------------------------------
g <- r_player %>%
  arrange(PLAYER, season) %>%
  group_by(PLAYER) %>%
  mutate(
    Avg_Speed_ly = lag(Avg_Speed_km)
    ,MIN_ly = lag(MIN)
    ,yoy_ratio = Avg_Speed_km / Avg_Speed_ly
  ) %>%
  ungroup() %>%
  filter(
    MIN >= 500
    ,MIN_ly >= 500
  ) %>%
  mutate(
    yoy_rank = min_rank(yoy_ratio)
    ,Avg_Speed_km = round(Avg_Speed_km, 2)
    ,Avg_Speed_ly = round(Avg_Speed_ly, 2)
  ) %>%
  filter(is.na(yoy_ratio) == FALSE) %>%
  arrange(yoy_ratio) %>%
  mutate(yoy_ratio = percent(round(yoy_ratio, 2))) %>%
  select(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"平均時速(km)" = Avg_Speed_km
    ,"前シーズンの平均時速" = Avg_Speed_ly
    ,"前シーズン比" = yoy_ratio
    ,"順位" = yoy_rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "前シーズンと当シーズンの両方で500分以上出場した選手が対象", size = 8) %>%
  tab_add_title(text = "前シーズンから移動スピードが減った下位20選手", face = "plain", size = 12, padding = unit(0.1, "line"))
g
# SGAは面白そう
# g %>% ggexport(filename =  "./02_output/average_speed_yoy_rank_bottom20_player_all_num.png",width = 800, height = 600)



# オフェンスとディフェンスの移動スピード比率 -------------------------------------------------------------
## チーム -------------------------------------------------------------

### 上位20チーム -------------------------------------------------------------
g <- r_team %>%
  mutate(
    OFF_DEF_ratio = Avg_Speed_km_Off / Avg_Speed_km_Def
    ,rank = min_rank(desc(OFF_DEF_ratio))
  ) %>%
  arrange(desc(OFF_DEF_ratio)) %>%
  mutate(
    OFF_DEF_ratio = percent(round(OFF_DEF_ratio, 2))
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  select(
    "シーズン" = season
    ,"チーム名" = Team
    ,"OFF平均時速(km)" = Avg_Speed_km_Off
    ,"DEF平均時速(km)" = Avg_Speed_km_Def
    ,"OFF÷DEF" = OFF_DEF_ratio
    ,"順位" = rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "OFFスピード÷DEFスピードの上位20チーム", face = "plain", size = 12)
g
# 察せるものがある
g %>% ggexport(filename =  "./02_output/average_speed_off_def_ratio_rank_top20_team.png",width = 800, height = 600)

### 下位20選手 -------------------------------------------------------------
g <- r_team %>%
  mutate(
    OFF_DEF_ratio = Avg_Speed_km_Off / Avg_Speed_km_Def
    ,rank = min_rank(OFF_DEF_ratio)
  ) %>%
  arrange(OFF_DEF_ratio) %>%
  mutate(
    OFF_DEF_ratio = percent(round(OFF_DEF_ratio, 2))
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  select(
    "シーズン" = season
    ,"チーム名" = Team
    ,"OFF平均時速(km)" = Avg_Speed_km_Off
    ,"DEF平均時速(km)" = Avg_Speed_km_Def
    ,"OFF÷DEF" = OFF_DEF_ratio
    ,"順位" = rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "OFFスピード÷DEFスピードの下位20チーム", face = "plain", size = 12)
g
# 3&D系かな？しかも3に関してはコーナー待機多め？
# g %>% ggexport(filename =  "./02_output/average_speed_off_def_ratio_rank_bottom20_player.png",width = 800, height = 600)



## 選手 -------------------------------------------------------------

### 上位20選手 -------------------------------------------------------------
g <- r_player %>%
  filter(
    MIN >= 500
  ) %>%
  mutate(
    OFF_DEF_ratio = Avg_Speed_km_Off / Avg_Speed_km_Def
    ,rank = min_rank(desc(OFF_DEF_ratio))
  ) %>%
  arrange(desc(OFF_DEF_ratio)) %>%
  mutate(
    OFF_DEF_ratio = percent(round(OFF_DEF_ratio, 2))
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  select(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"OFF平均時速(km)" = Avg_Speed_km_Off
    ,"DEF平均時速(km)" = Avg_Speed_km_Def
    ,"OFF÷DEF" = OFF_DEF_ratio
    ,"順位" = rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mGreen"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "500分以上出場した選手が対象", size = 8) %>%
  tab_add_title(text = "OFFスピード÷DEFスピードの上位20選手", face = "plain", size = 12, padding = unit(0.1, "line"))
g
# 察せるものがある
g %>% ggexport(filename =  "./02_output/average_speed_off_def_ratio_rank_top20_player.png",width = 800, height = 600)

### 下位20選手 -------------------------------------------------------------
g <- r_player %>%
  filter(
    MIN >= 500
  ) %>%
  mutate(
    OFF_DEF_ratio = Avg_Speed_km_Off / Avg_Speed_km_Def
    ,rank = min_rank(OFF_DEF_ratio)
  ) %>%
  arrange(OFF_DEF_ratio) %>%
  mutate(
    OFF_DEF_ratio = percent(round(OFF_DEF_ratio, 2))
    ,Avg_Speed_km_Off = round(Avg_Speed_km_Off, 2)
    ,Avg_Speed_km_Def = round(Avg_Speed_km_Def, 2)
  ) %>%
  select(
    "シーズン" = season
    ,"選手名" = PLAYER
    ,"OFF平均時速(km)" = Avg_Speed_km_Off
    ,"DEF平均時速(km)" = Avg_Speed_km_Def
    ,"OFF÷DEF" = OFF_DEF_ratio
    ,"順位" = rank
  ) %>%
  head(20) %>%
  ggtexttable(
    theme = ttheme(
      base_style = "mBlue"
    )
    ,rows = NULL
  )%>%
  tab_add_title(text = "500分以上出場した選手が対象", size = 8) %>%
  tab_add_title(text = "OFFスピード÷DEFスピードの下位20選手", face = "plain", size = 12, padding = unit(0.1, "line"))
g
# 3&D系かな？しかも3に関してはコーナー待機多め？
# g %>% ggexport(filename =  "./02_output/average_speed_off_def_ratio_rank_bottom20_player.png",width = 800, height = 600)
