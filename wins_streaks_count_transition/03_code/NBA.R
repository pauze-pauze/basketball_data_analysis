
# import library ----------------------------------------------------------

library(tidyverse)
library(hoopR)
library(scales)


# read data ---------------------------------------------------------------

season_vec <- c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23")

df <- NA
for (i in 1:length(season_vec)){
  t <- nba_leaguegamelog(season = season_vec[i])
  tt <- t$LeagueGameLog
  df <- df %>%
    rbind(tt)
}
# na含む行が3つあったので調査
df[!complete.cases(df),] %>% View()
# 試合が中止だったらしいので、対象から外す https://www.sbnation.com/nba/2013/4/15/4228770/celtics-vs-pacers-april-16-game-canceled

df <- df %>%
  na.omit()

df <- df %>%
  select(SEASON_ID, team = TEAM_ABBREVIATION, game_date = GAME_DATE, WL) %>%
  mutate(
    season = as.numeric(str_sub(SEASON_ID, start = 2, end = 5))
    ,win_flag = if_else(WL == "W", 1, 0)
  )

streaks_df <- df %>%
  group_by(season, team) %>%
  arrange(game_date) %>%
  mutate(
    last_game_result = lag(win_flag, n = 1, order_by = game_date)
    ,result_change_flag = case_when(
      is.na(last_game_result) ~ 1
      ,win_flag == last_game_result ~ 0
      ,TRUE ~ 1
    )
    ,result_change_flag_cumsum = cumsum(result_change_flag)
  ) %>%
  group_by(season, team, win_flag, result_change_flag_cumsum) %>%
  summarise(
    streaks = n()
    ,.groups = "drop" # 100%棒グラフでfactorの順番を連勝数の大小と同じにするための処理。これがないとgroup内での順番が優先されてそう
  ) %>%
  group_by(season, team, win_flag, streaks) %>%
  summarise(
    streaks_cnt = n()
    ,.groups = "drop"
  ) %>%
  mutate(
    game_cnt_in_streaks = streaks * streaks_cnt
    ,streaks = if_else(streaks >= 10, "over_10", as.character(streaks)) # 10連勝は一旦10の枠に収める
  ) %>%
  arrange(streaks) %>% # 100%棒グラフでfactorの順番を連勝数の大小と同じにするための処理
  mutate(streaks = as.factor(streaks)) %>%
  group_by(season, team, win_flag, streaks) %>%
  summarise(
    game_cnt_in_streaks = sum(game_cnt_in_streaks)
  )
  
streaks_df %>%
  filter(win_flag == 1) %>%
  ggplot(aes(x = season, y = game_cnt_in_streaks, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "シーズン", y = "連勝数の構成比", title = "NBAでの連勝数の構成比")+
  theme_bw()+
  ggsave("./02_output/wins_streaks_NBA.png")

streaks_df %>%
  filter(win_flag == 0) %>%
  ggplot(aes(x = season, y = game_cnt_in_streaks, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "シーズン", y = "連敗数の構成比", title = "NBAでの連敗数の構成比")+
  theme_bw()+
  ggsave("./02_output/loses_streaks_NBA.png")


# 英語ver
streaks_df %>%
  filter(win_flag == 1) %>%
  ggplot(aes(x = season, y = game_cnt_in_streaks, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "season", y = "wins_streaks_ratio", title = "wins_streaks_ratio_in_NBA")+
  theme_bw()+
  ggsave("./02_output/wins_streaks_NBA_in_English.png")

streaks_df %>%
  filter(win_flag == 0) %>%
  ggplot(aes(x = season, y = game_cnt_in_streaks, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "シーズン", y = "loses_streaks_ratio", title = "loses_streaks_ratio_in_NBA")+
  theme_bw()+
  ggsave("./02_output/loses_streaks_NBAin_English.png")

  
  