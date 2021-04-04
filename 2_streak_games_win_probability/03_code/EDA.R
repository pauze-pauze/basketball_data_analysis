
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


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


# see data distribution ---------------------------------------------------

# make win ratio bin

team_win_ratio_10_per_bin <- team_win_ratio %>%
  mutate(
    win_ratio_10_per_bin = ceiling(10 * win_ratio) / 10
  )

table(team_win_ratio_10_per_bin$win_ratio_10_per_bin)

game_data <- game_data %>%
  left_join(
    game_data %>%
      select(
        ScheduleKey
        ,TeamId2 = TeamId
      )
    ,by = "ScheduleKey"
  ) %>%
  filter(
    TeamId != TeamId2
  ) %>%
  left_join(
    team_win_ratio_10_per_bin %>%
      select(Season, TeamId, win_ratio_10_per_bin_former = win_ratio_10_per_bin)
    ,by = c("Season", "TeamId")
  ) %>%
  left_join(
    team_win_ratio_10_per_bin %>%
      select(Season, TeamId, win_ratio_10_per_bin_latter = win_ratio_10_per_bin)
    ,by = c("Season", "TeamId2" = "TeamId")
  )
  

game_data %>%
  group_by(win_ratio_10_per_bin_former, win_ratio_10_per_bin_latter) %>%
  summarise(
    game_cnt_2_streak = n()
    #,win_ratio_2_streak = mean(win_flag)
  ) %>%
  pivot_wider(
    names_from = "win_ratio_10_per_bin_former"
    ,values_from = "game_cnt_2_streak"
  )
game_data %>%
  group_by(win_ratio_10_per_bin_former, win_ratio_10_per_bin_latter) %>%
  summarise(
    #game_cnt_2_streak = n()
    win_ratio_2_streak = mean(win_flag)
  ) %>%
  pivot_wider(
    names_from = "win_ratio_10_per_bin_latter"
    ,values_from = "win_ratio_2_streak"
  )























