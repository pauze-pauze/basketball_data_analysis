
# import library ----------------------------------------------------------

library(tidyverse)
library(bleaguer)
library(geosphere)
library(gt)
install.packages("webshot")
library(webshot2)

# read data ---------------------------------------------------------------

# read arena location data
arena_locate <- read_csv("./01_data/arena_location.csv", locale = locale(encoding = "cp932"))

# read games and teams
game_outline <- b.games %>%
  filter(EventId %in% c(2, 7)) # filter games into B1 regular season and B2 regular season

game_team <- b.games.summary

team <- b.teams %>%
  mutate(NameShort = if_else(as.character(NameShort) == "栃木", "宇都宮", as.character(NameShort))) # チーム名変更に関する処理


# manipulate data ---------------------------------------------------------

# Identify main arena by season
main_arena <- game_outline %>%
  left_join(
    game_team
    ,by = "ScheduleKey"
  ) %>%
  left_join(
    team
    ,by = c("TeamId", "Season")
  ) %>%
  group_by(Season, NameShort, Arena) %>%
  summarise(game_cnt = n()) %>% # check game count by team and season
  mutate(row_n = row_number(desc(game_cnt))) %>% # identify the arena where games were most held
  filter(row_n == 1) %>%
  mutate(Arena = as.character(Arena))

# check last game's arena
game_arena <- game_outline %>%
  left_join(
    game_team
    ,by = "ScheduleKey"
  ) %>%
  left_join(
    team
    ,by = c("TeamId", "Season")
  ) %>%
  group_by(Season, League, NameShort) %>%
  select(Season, League, NameShort, Date, Arena) %>%
  mutate(last_game_arena = lag(Arena, order_by = Date)) %>%
  ungroup() %>%
  mutate(Arena = as.character(Arena), last_game_arena = as.character(last_game_arena))
## First game of the season has NA in "last_game_arena" row, so cope with it appropriately.



# calculate moving distance -----------------------------------------------


## pattern 1 節ごとにホームに戻るケース --------------------------------------------------
#前の試合とアリーナが違ったら、一旦ホームに帰ってると扱う。シーズン初ゲーム時の前回アリーナにはホームアリーナ入れる

df_pattern1 <- game_arena %>%
  left_join(
    main_arena %>%
      select(Season, NameShort, Arena) %>%
      rename(m_arena = Arena)
    ,by = c("Season", "NameShort")
  ) %>%
  left_join(
    arena_locate %>% select(Arena, lat, lng)
    ,by = "Arena"
  ) %>%
  rename(
    game_arena_lat = lat
    ,game_arena_lng = lng
  ) %>%
  left_join(
    arena_locate %>% select(Arena, lat, lng)
    ,by = c("m_arena" = "Arena")
  ) %>%
  rename(
    m_arena_lat = lat
    ,m_arena_lng = lng
  ) %>%
  mutate(
    last_game_arena = if_else(is.na(last_game_arena), m_arena, last_game_arena)
    ,move_flag = if_else(Arena == last_game_arena, 0, 1)
    ,arena_distance = NA
  )

for (i in 1:nrow(df_pattern1)){
  game_arena_lat = df_pattern1 %>% select(game_arena_lat) %>% slice(i) %>% as.numeric()
  game_arena_lng = df_pattern1 %>% select(game_arena_lng) %>% slice(i) %>% as.numeric()
  m_arena_lat = df_pattern1 %>% select(m_arena_lat) %>% slice(i) %>% as.numeric()
  m_arena_lng = df_pattern1 %>% select(m_arena_lng) %>% slice(i) %>% as.numeric()
  game_arena_loc = c(game_arena_lng, game_arena_lat)
  m_arena_loc = c(m_arena_lng, m_arena_lat)
  md = distGeo(game_arena_loc, m_arena_loc)
  df_pattern1[i, "arena_distance"] <- md
}


df_pattern1 <- df_pattern1 %>%
  mutate(
    moving_distance = if_else(
      move_flag == 1
      ,2 * arena_distance # アウェイとホームの往復のために2倍にする。アウェイからホームに帰ってきたときもmove_flag = 1になるが、move_distanceが0になってるので無問題
      ,0
    )
  ) %>%
  filter(move_flag == 1)

# # 参考の出力
# df_pattern1 %>%
#   filter(League == "B1", Season == "2016-17") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
#   ggplot(aes(x = NameShort, y = moving_distance))+
#   geom_bar(stat = "identity") +
#   scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
#   labs(x = "チーム"
#        , y = "移動の直線距離の合計(km)"
#        , title = "2016-17のB1レギュラーシーズンのチーム別移動距離"
#        , subtitle = "毎節ごとにホームに帰っている場合"
#        , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
#   )+
#   theme_classic()

# B1
df_pattern1 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = NameShort, y = moving_distance))+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
  facet_wrap( ~ Season, ncol = 1, scales = "free")+
  labs(x = "チーム"
       , y = "移動の直線距離の合計(km)"
       , title = "B1レギュラーシーズンのチーム別移動距離"
       , subtitle = "毎節ごとにホームに帰っている場合"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/チーム×シーズンごとの移動距離_B1_毎節ホームに帰る場合.png", width = 9, height = 16)

# B2
df_pattern1 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = NameShort, y = moving_distance))+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 30000))+
  facet_wrap( ~ Season, ncol = 1, scales = "free")+
  labs(x = "チーム"
       , y = "移動の直線距離の合計(km)"
       , title = "B2レギュラーシーズンのチーム別移動距離"
       , subtitle = "毎節ごとにホームに帰っている場合"
       , caption = "B2レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/チーム×シーズンごとの移動距離_B2_毎節ホームに帰る場合.png", width = 9, height = 16)


## pattern 2 そのまま次の試合地に向かうケース --------------------------------------------------
#前の試合とアリーナが違ったら、そのまま向かってると扱う。シーズン初ゲーム時の前回アリーナにはホームアリーナ入れる

df_pattern2 <- game_arena %>%
  left_join(
    main_arena %>%
      select(Season, NameShort, Arena) %>%
      rename(m_arena = Arena)
    ,by = c("Season", "NameShort")
  ) %>%
  left_join(
    arena_locate %>% select(Arena, lat, lng)
    ,by = "Arena"
  ) %>%
  rename(
    game_arena_lat = lat
    ,game_arena_lng = lng
  ) %>%
  mutate(
    last_game_arena = if_else(is.na(last_game_arena), m_arena, last_game_arena)
  ) %>%
  left_join(
    arena_locate %>% select(Arena, lat, lng)
    ,by = c("last_game_arena" = "Arena")
  ) %>%
  rename(
    lg_arena_lat = lat
    ,lg_arena_lng = lng
  ) %>%
  mutate(
    move_flag = if_else(Arena == last_game_arena, 0, 1)
    ,arena_distance = NA
  )

for (i in 1:nrow(df_pattern2)){
  game_arena_lat = df_pattern2 %>% select(game_arena_lat) %>% slice(i) %>% as.numeric()
  game_arena_lng = df_pattern2 %>% select(game_arena_lng) %>% slice(i) %>% as.numeric()
  lg_arena_lat = df_pattern2 %>% select(lg_arena_lat) %>% slice(i) %>% as.numeric()
  lg_arena_lng = df_pattern2 %>% select(lg_arena_lng) %>% slice(i) %>% as.numeric()
  game_arena_loc = c(game_arena_lng, game_arena_lat)
  m_arena_loc = c(lg_arena_lng, lg_arena_lat)
  md = distGeo(game_arena_loc, m_arena_loc)
  df_pattern2[i, "arena_distance"] <- md
}


df_pattern2 <- df_pattern2 %>%
  mutate(
    moving_distance = if_else(
      move_flag == 1
      ,arena_distance # アウェイとホームの往復のために2倍にする。アウェイからホームに帰ってきたときもmove_flag = 1になるが、move_distanceが0になってるので無問題
      ,0
    )
  ) %>%
  filter(move_flag == 1)

# # 参考の出力
# df_pattern2 %>%
#   filter(League == "B1", Season == "2016-17") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
#   ggplot(aes(x = NameShort, y = moving_distance))+
#   geom_bar(stat = "identity") +
#   scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
#   labs(x = "チーム"
#        , y = "移動の直線距離の合計(km)"
#        , title = "2016-17のB1レギュラーシーズンのチーム別移動距離"
#        , subtitle = "次の試合会場にそのまま赴く場合(アウェー続きの場合はアウェー→アウェーでの移動)"
#        , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
#   )+
#   theme_classic()

# B1
df_pattern2 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = NameShort, y = moving_distance))+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
  facet_wrap( ~ Season, ncol = 1, scales = "free")+
  labs(x = "チーム"
       , y = "移動の直線距離の合計(km)"
       , title = "B1レギュラーシーズンのチーム別移動距離"
       , subtitle = "次の試合会場にそのまま赴く場合(アウェー続きの場合はアウェー→アウェーでの移動)"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/チーム×シーズンごとの移動距離_B1_次のアリーナに直接向かう場合.png", width = 9, height = 16)

# B2
df_pattern2 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = NameShort, y = moving_distance))+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 30000))+
  facet_wrap( ~ Season, ncol = 1, scales = "free")+
  labs(x = "チーム"
       , y = "移動の直線距離の合計(km)"
       , title = "B2レギュラーシーズンのチーム別移動距離"
       , subtitle = "次の試合会場にそのまま赴く場合(アウェー続きの場合はアウェー→アウェーでの移動)"
       , caption = "B2レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/チーム×シーズンごとの移動距離_B2_次のアリーナに直接向かう場合.png", width = 9, height = 16)


# Analysis ------------------------------------------------------------------

## 2.シーズンごとの移動距離の分布 --------------------------------------------------------
## 毎回ホームに帰る場合_B1
df_pattern1 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = Season, y = moving_distance))+
  geom_violin() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
  labs(x = "シーズン"
       , y = "移動の直線距離の合計(km)"
       , title = "B1レギュラーシーズンのチーム別移動距離の分布"
       , subtitle = "毎節ごとにホームに帰っている場合"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/シーズンごとの移動距離分布_B1_毎節ホームに帰る場合.png")


df_pattern1 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(row_num = row_number(desc(moving_distance))) %>%
  filter(row_num %in% c(1, 2, 3)) %>%
  arrange(Season, row_num) %>%
  ungroup()%>%
  select(-row_num) %>%
  rename(
    シーズン = Season
    ,チーム = NameShort
    ,"移動距離(km)" = moving_distance
  ) %>%
  gt() %>%
  fmt_number(columns = "移動距離(km)", decimals = 1) %>%
  tab_header(title = md("**シーズンごとの移動距離長いチームTOP3**<br>B1"))

df_pattern1 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(row_num = row_number((moving_distance))) %>%
  filter(row_num %in% c(1, 2, 3)) %>%
  arrange(Season, row_num) %>%
  ungroup()%>%
  select(-row_num) %>%
  rename(
    シーズン = Season
    ,チーム = NameShort
    ,"移動距離(km)" = moving_distance
  ) %>%
  gt() %>%
  fmt_number(columns = "移動距離(km)", decimals = 1) %>%
  tab_header(title = md("**シーズンごとの移動距離短いチームTOP3**<br>B1"))

## 毎回ホームに帰る場合_B2
df_pattern1 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = Season, y = moving_distance))+
  geom_violin() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 30000))+
  labs(x = "シーズン"
       , y = "移動の直線距離の合計(km)"
       , title = "B2レギュラーシーズンのチーム別移動距離の分布"
       , subtitle = "毎節ごとにホームに帰っている場合"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/シーズンごとの移動距離分布_B2_毎節ホームに帰る場合.png")

df_pattern1 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(row_num = row_number(desc(moving_distance))) %>%
  filter(row_num %in% c(1, 2, 3)) %>%
  arrange(Season, row_num) %>%
  ungroup()%>%
  select(-row_num) %>%
  rename(
    シーズン = Season
    ,チーム = NameShort
    ,"移動距離(km)" = moving_distance
  ) %>%
  gt() %>%
  fmt_number(columns = "移動距離(km)", decimals = 1) %>%
  tab_header(title = md("**シーズンごとの移動距離長いチームTOP3**<br>B2"))

df_pattern1 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(row_num = row_number((moving_distance))) %>%
  filter(row_num %in% c(1, 2, 3)) %>%
  arrange(Season, row_num) %>%
  ungroup()%>%
  select(-row_num) %>%
  rename(
    シーズン = Season
    ,チーム = NameShort
    ,"移動距離(km)" = moving_distance
  ) %>%
  gt() %>%
  fmt_number(columns = "移動距離(km)", decimals = 1) %>%
  tab_header(title = md("**シーズンごとの移動距離短いチームTOP3**<br>B2"))


## 前回のアリーナから直行する場合_B1
df_pattern2 %>%
  filter(League == "B1") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = Season, y = moving_distance))+
  geom_violin() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000))+
  labs(x = "シーズン"
       , y = "移動の直線距離の合計(km)"
       , title = "B1レギュラーシーズンのチーム別移動距離の分布"
       , subtitle = "次の試合会場にそのまま赴く場合(アウェー続きの場合はアウェー→アウェーでの移動)"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/シーズンごとの移動距離分布_B1_次のアリーナに直接向かう場合.png")

# 上側が削れていて、端っこのチームが楽できてる感。次のパートでふかぼりましょう

# 省略
# df_pattern1 %>%
#   filter(League == "B1") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>%
#   ungroup() %>%
#   group_by(Season) %>%
#   mutate(row_num = row_number(desc(moving_distance))) %>%
#   filter(row_num %in% c(1, 2, 3)) %>%
#   arrange(Season, row_num)
# 
# df_pattern1 %>%
#   filter(League == "B1") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>%
#   ungroup() %>%
#   group_by(Season) %>%
#   mutate(row_num = row_number((moving_distance))) %>%
#   filter(row_num %in% c(1, 2, 3)) %>%
#   arrange(Season, row_num)

## 前回のアリーナから直行する場合_B2
# 省略
df_pattern2 %>%
  filter(League == "B2") %>%
  group_by(Season, NameShort) %>%
  summarise(moving_distance = sum(moving_distance) / 1000) %>% # km単位に変換するために1000で割る
  ggplot(aes(x = Season, y = moving_distance))+
  geom_violin() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 30000))+
  labs(x = "シーズン"
       , y = "移動の直線距離の合計(km)"
       , title = "B2レギュラーシーズンのチーム別移動距離の分布"
       , subtitle = "次の試合会場にそのまま赴く場合(アウェー続きの場合はアウェー→アウェーでの移動)"
       , caption = "B1レギュラーシーズンのみ対象。プレーオフや天皇杯は対象外です。\n 移動距離はホームアリーナからの直線距離としています。\n交通手段による距離の変動は未考慮&サブアリーナの場合はホームでも距離カウントされることにご注意ください"
  )+
  theme_classic()+
  ggsave("./02_output/シーズンごとの移動距離分布_B2_次のアリーナに直接向かう場合.png")
# 
# df_pattern1 %>%
#   filter(League == "B2") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>%
#   ungroup() %>%
#   group_by(Season) %>%
#   mutate(row_num = row_number(desc(moving_distance))) %>%
#   filter(row_num %in% c(1, 2, 3)) %>%
#   arrange(Season, row_num)
# 
# df_pattern1 %>%
#   filter(League == "B2") %>%
#   group_by(Season, NameShort) %>%
#   summarise(moving_distance = sum(moving_distance) / 1000) %>%
#   ungroup() %>%
#   group_by(Season) %>%
#   mutate(row_num = row_number((moving_distance))) %>%
#   filter(row_num %in% c(1, 2, 3)) %>%
#   arrange(Season, row_num)

# 環太平洋ベルト地帯は移動距離少ない感じ。なんか思い浮かんだので使いたい



## 1.集計タイプ別の移動距離の違い -------------------------------------------------------
df_analysis_1 <- df_pattern1 %>%
  group_by(Season, League, NameShort) %>%
  summarise(moving_distance_return_home_every_round = sum(moving_distance) / 1000) %>%
  left_join(
    df_pattern2 %>%
      group_by(Season, League, NameShort) %>%
      summarise(moving_distance_from_last_game_arena = sum(moving_distance) / 1000)
    ,by = c("Season", "League", "NameShort")
  )
df_analysis_1 %>%
  mutate(distance_ratio = moving_distance_from_last_game_arena / moving_distance_return_home_every_round) %>%
  view()

df_analysis_1 %>%
  mutate(distance_ratio = moving_distance_from_last_game_arena / moving_distance_return_home_every_round) %>%
  ggplot(aes(x = Season, y = distance_ratio))+
  geom_violin() +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "シーズン", y = "移動距離の比率(A ÷ B)", title = "毎節ホームに帰る場合の距離(A)に対する次の試合会場にそのまま赴く場合(B)の比率の分布")+
  facet_wrap(.~League)+
  theme_bw()+
  ggsave("./02_output/毎節ホームに帰る場合と次の試合会場にそのまま赴く場合の距離の比率分布.png")

# 比率が小さいチームToP15
df_analysis_1 %>%
  mutate(distance_ratio = moving_distance_from_last_game_arena / moving_distance_return_home_every_round) %>%
  ungroup() %>%
  arrange(distance_ratio) %>%
  head(n = 15) %>%
  rename(
    シーズン = Season
    ,リーグ = League
    ,チーム = NameShort
    ,"移動距離_毎節ホームに帰る場合(km)" = moving_distance_return_home_every_round
    ,"移動距離_次の試合会場に赴く場合(km)" = moving_distance_from_last_game_arena
    ,比率 = distance_ratio
  ) %>%
  gt() %>%
  fmt_number(columns = c("移動距離_毎節ホームに帰る場合(km)", "移動距離_次の試合会場に赴く場合(km)"), decimals = 0) %>%
  fmt_percent(columns = "比率", decimals = 1)
# gtsave(t, "./02_output/算出方法による移動距離の比率_小さい順.png")ができなかったので手動
  

# 比率が大きいチームToP15
df_analysis_1 %>%
  mutate(distance_ratio = moving_distance_from_last_game_arena / moving_distance_return_home_every_round) %>%
  ungroup() %>%
  arrange(desc(distance_ratio)) %>%
  head(n = 15) %>%
  rename(
    シーズン = Season
    ,リーグ = League
    ,チーム = NameShort
    ,"移動距離_毎節ホームに帰る場合(km)" = moving_distance_return_home_every_round
    ,"移動距離_次の試合会場に赴く場合(km)" = moving_distance_from_last_game_arena
    ,比率 = distance_ratio
  ) %>%
  gt() %>%
  fmt_number(columns = c("移動距離_毎節ホームに帰る場合(km)", "移動距離_次の試合会場に赴く場合(km)"), decimals = 0) %>%
  fmt_percent(columns = "比率", decimals = 1)

df_analysis_1 %>%
  mutate(distance_ratio = moving_distance_from_last_game_arena / moving_distance_return_home_every_round) %>%
  arrange(desc(distance_ratio)) %>%
  head(n = 20) %>%
  view()

# 端っこのエリアのチームは前の試合のアリーナから直接出向くことによる移動距離の削減割合が大きい傾向。真ん中あたりは、直接向かうのとホームに戻ってから向かうのであまりサがないのかも


## 3.チームごとの移動距離のシーズン推移 ---------------------------------------------------
# 毎回アリーナ戻るわけではないけど、毎回次のアリーナに直行するわけでもないという塩梅にするために、2パターンの移動距離の平均を採用する

df_analysis_3 <- df_analysis_1 %>%
  mutate(moving_distance_mean = (moving_distance_return_home_every_round + moving_distance_from_last_game_arena) / 2) %>%
  left_join(
    game_arena %>% group_by(Season, NameShort) %>% summarise(game_cnt = n())
    ,by = c("Season", "NameShort")
  ) %>%
  mutate(Season = as.numeric(str_sub(as.character(Season), start = 1, end = 4))) %>%
  mutate(moving_distance_mean_per_game = moving_distance_mean / game_cnt)
  
df_analysis_3 %>%
  filter(League == "B1") %>%
  ggplot(aes(x = Season, y = moving_distance_mean_per_game, color = NameShort))+
  geom_line()+
  geom_point()+
  labs(x = "シーズン", y = "1試合あたりの平均移動距離(km)", title = "チームごとの1試合あたり平均移動距離のシーズン推移_B1")+
  theme_classic()+
  ggsave("./02_output/チームごとの1試合あたり平均移動距離のシーズン推移_B1.png")
# 系列多すぎると思いつつ、上下動してるチームもちらほらあるので、抽出してみる

df_analysis_3 %>%
  select(-c(moving_distance_return_home_every_round, moving_distance_from_last_game_arena)) %>%
  group_by(NameShort) %>%
  mutate(
    last_season_md = lag(moving_distance_mean_per_game, n = 1, default = NA, order_by = Season)
    ,yoy = moving_distance_mean_per_game / last_season_md
    ,yoy_abs = abs(1 - yoy)
  ) %>%
  filter(yoy_abs >= 0.2) %>%
  select(Season, League, NameShort, moving_distance_mean_per_game, last_season_md, yoy, yoy_abs) %>%
  arrange(desc(yoy)) %>%
  ungroup() %>%
  rename(
    シーズン = Season
    ,リーグ = League
    ,チーム名 = NameShort
    ,"移動距離_1試合平均(km)" = moving_distance_mean_per_game
    ,"昨シーズンの移動距離(km)" = last_season_md
    ,前年比 = yoy
  ) %>%
  select(-yoy_abs) %>%
  gt() %>%
  fmt_number(columns = c("移動距離_1試合平均(km)", "昨シーズンの移動距離(km)"), decimals = 2) %>%
  fmt_percent(columns = "前年比", decimals = 1) %>%
  gtsave("./02_output/移動距離の前年比で変動の大きいチーム.png")
  

# yoy大きい組
## 2017と2020のの川崎は地区移動影響っぽい。
## 2019の三遠と三河は地区変わってなくて、同地区のチームもほぼ変更なし。移動距離の長い組み合わせが中断前に固まってた or もしくはそもそも少なかった？
## 2021の越谷はよく分からん

# yoy小さい組
## 2019の川崎も上述の三遠や三河と同様？

# 移動距離がもともと長い組
## 2017北海道は同地区から秋田と仙台がいなくなってしまって同地区対戦の移動距離増えてそう
## 2018福岡もB2時は九州や中国四国地方が同地区だけど、B1ではそうは行かなくなってる