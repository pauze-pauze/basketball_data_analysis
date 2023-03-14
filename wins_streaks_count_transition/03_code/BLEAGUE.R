
# import library ----------------------------------------------------------

library(tidyverse)
library(bleaguer)
library(scales)


# read data ---------------------------------------------------------------

source("../B_data.R")


# create data frame for count wins streaks --------------------------------

df <- game_summary %>%
  left_join(
    game_summary %>% select(ScheduleKey, TeamId, PTS)
    ,by = "ScheduleKey"
  ) %>%
  filter(TeamId.x != TeamId.y) %>%
  select(ScheduleKey, TeamId = TeamId.x, PTS = PTS.x, opp_TeamId = TeamId.y, opp_PTS = PTS.y) %>%
  mutate(win_flag = if_else(PTS > opp_PTS, 1, 0)) %>%
  inner_join(
    game %>% filter(EventId %in% c(2, 7)) %>% select(ScheduleKey, Season, Date) # B1とB2のレギュラーシーズンに絞る
    ,by  = "ScheduleKey"
  ) %>%
  left_join(
    team %>% select(TeamId, NameShort, Season, League)
    ,by = c("TeamId", "Season")
  ) %>%
  left_join(
    team %>% select(TeamId, opp_NameShort = NameShort, Season)
    ,by = c("opp_TeamId" = "TeamId", "Season")
  ) 


# count wins streaks ------------------------------------------------------
# この記事で同じ値が連続する件数の出し方把握した https://algorithm.joho.info/programming/python/count-continuous-values/

streaks_df <- df %>%
  group_by(Season, NameShort) %>%
  arrange(Date) %>%
  mutate(
    last_game_result = lag(win_flag, n = 1, order_by = Date)
    ,result_change_flag = case_when(
      is.na(last_game_result) ~ 1
      ,win_flag == last_game_result ~ 0
      ,TRUE ~ 1
    )
    ,result_change_flag_cumsum = cumsum(result_change_flag)
  ) %>%
  group_by(Season, NameShort, League, win_flag, result_change_flag_cumsum) %>%
  summarise(
    streaks = n()
    ,.groups = "drop" # 100%棒グラフでfactorの順番を連勝数の大小と同じにするための処理。これがないとgroup内での順番が優先されてそう
  ) %>%
  arrange(streaks) %>% # 100%棒グラフでfactorの順番を連勝数の大小と同じにするための処理
  mutate(streaks = as.factor(streaks)) %>%
  group_by(Season, NameShort, League, win_flag, streaks) %>%
  summarise(
    cnt = n()
  )

streaks_df %>%
  filter(League == "B1", win_flag == 1) %>%
  ggplot(aes(x = Season, y = cnt, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "シーズン", y = "連勝数の構成比", title = "B1での連勝数の構成比")+
  theme_bw()
  
streaks_df %>%
  filter(League == "B1", win_flag == 0) %>%
  ggplot(aes(x = Season, y = cnt, fill = streaks))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.2, end = 0.8)+
  labs(x = "シーズン", y = "連敗数の構成比", title = "B1での連敗数の構成比")+
  theme_bw()

X連勝 / 連敗に含まれる試合数の構成比のほうがいいかも