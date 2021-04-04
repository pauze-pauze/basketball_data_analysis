
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)
library(BradleyTerry2)


# load data ---------------------------------------------------------------

source("../B_data.R")


# manipulate data ---------------------------------------------------------

# pick up game type

season_type <- event %>%
  filter(ShortName == "B1RS") %>%
  select(EventId)

# filter game schedule
game_schedule <- game %>%
  inner_join(
    season_type
    ,by = "EventId"
  ) %>%
  filter(
    Season %in% c("2016-17", "2017-18","2018-19", "2019-20")
  ) %>%
  select(
    ScheduleKey
    ,Season
    ,Date
  )
table(game_schedule$Season)

game_data <- game_summary %>%
  inner_join(
    game_schedule
    ,by = "ScheduleKey"
  ) %>%
  select(
    ScheduleKey
    ,Season
    ,Date
    ,TeamId
    ,PTS
  )

win_team_pts <- game_data %>% # to detect which teams won
  group_by(ScheduleKey) %>%
  summarise(win_team_pts = max(PTS), .groups = "drop")

game_data <- game_data %>% # make base data
  left_join(
    win_team_pts
    ,by = "ScheduleKey"
  ) %>%
  mutate(
    win_flag = if_else(PTS == win_team_pts, 1, 0)
  )

# win ratio by season and team
team_win_ratio <- game_data %>%
  group_by(
    Season
    ,TeamId
  ) %>% 
  summarise(
    game_cnt = n()
    ,win_ratio = mean(win_flag)
    ,.groups = "drop"
  )

### pick up 2 streak games
game_data <- game_data %>%
  group_by(Season, TeamId) %>%
  mutate(
    lag_1_day_flag = Date - lag(Date, default = ymd("2010-01-01"), order_by = Date)
    ,lead_1_day_flag = lead(Date, default = ymd("2010-01-01"), order_by = Date) - Date
  ) %>%
  filter(
    lag_1_day_flag == 1 | lead_1_day_flag == 1
  ) %>%
  ungroup()

game_data %>%
  group_by(TeamId, Season) %>%
  summarise(
    win_ratio_in_2_streak = mean(win_flag)
    ,game_cnt_in_2_streak = n()
    ,.groups = "drop"
  ) %>%
  left_join(
    team_win_ratio
    ,by = c("Season", "TeamId")
  ) %>%
  ggplot(aes(x = win_ratio, y = win_ratio_in_2_streak))+
  geom_point()

game_data %>%
  group_by(TeamId, Season) %>%
  summarise(
    win_ratio_in_2_streak = mean(win_flag)
    ,game_cnt_in_2_streak = n()
    ,.groups = "drop"
  ) %>%
  left_join(
    team_win_ratio
    ,by = c("Season", "TeamId")
  ) %>%
  ggplot(aes(x = game_cnt, y = game_cnt_in_2_streak))+
  geom_point()

game_data %>%
  group_by(TeamId, Season) %>%
  summarise(
    win_ratio_in_2_streak = mean(win_flag)
    ,game_cnt_in_2_streak = n()
    ,.groups = "drop"
  ) %>%
  left_join(
    team_win_ratio
    ,by = c("Season", "TeamId")
  )

game_data <- game_data %>%
  left_join(
    game_data %>%
      select(
        ScheduleKey
        ,TeamId2 = TeamId
        ,win_flag2 = win_flag
      )
    ,by = "ScheduleKey"
  ) %>%
  filter(
    TeamId != TeamId2
  )

game_data %>%
  filter(
    TeamId < TeamId2
  ) %>%
  group_by(
    TeamId
    ,TeamId2
    ,Season
  ) %>%
  summarise(
    win_cnt_1 = sum(win_flag)
    ,win_cnt_2 = sum(win_flag2)
  ) %>%
  view()

team_fct_set <- c(game_data$TeamId, game_data$TeamId2) %>%
  unique()

df_for_bt_model <- game_data %>%
  filter(
    TeamId < TeamId2
  ) %>%
  group_by(
    TeamId
    ,TeamId2
    ,Season
  ) %>%
  summarise(
    win_cnt_1 = sum(win_flag)
    ,win_cnt_2 = sum(win_flag2)
    ,.groups = "drop"
  ) %>%
  mutate(
    TeamId = factor(TeamId, levels = team_fct_set)
    ,TeamId2 = factor(TeamId2, levels = team_fct_set)
  )

split_data <- df_for_bt_model %>%
  split(df_for_bt_model$Season)
split_data <- split_data[-5]
split_data[1:4]

model_df <- split_data[1:4] %>%
  map(
    ~BTm(
      cbind(win_cnt_1, win_cnt_2)
      ,TeamId
      ,TeamId2
      ,~Team
      ,id = "Team"
      ,data = .x
    )
  )

model_df[1]

test <- BTm(
    cbind(win_cnt_1, win_cnt_2)
    ,TeamId
    ,TeamId2
    ,~Team
    ,id = "Team"
    ,data = (df_for_bt_model %>% filter(Season == "2016-17"))
  )

BTabilities(test)

test <- BTabilities(model_df[[1]]) %>%
  as.data.frame() 
  





# https://moratoriamuo.hatenablog.com/entry/2019/07/02/225946
# TeamIdの小さい数字だけ件数多く表示されるように調整する
# seasonデータもあるから、purrrのmap機能使ってseasonごとにデータ出したい
# 後はこの強さデータを使って戦闘力パラメータを推測する
# この戦闘力データで実際の勝率との差異を分析






































