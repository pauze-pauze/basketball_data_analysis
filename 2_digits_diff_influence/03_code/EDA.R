
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


# load data ---------------------------------------------------------------

source("../B_data.R")


# check data --------------------------------------------------------------

# data manipulate

# extract needed data
df <- game_summary %>%
  select(ScheduleKey, TeamId, PTS, Q1, Q2) %>%
  mutate(half_time_pts = Q1 + Q2)
# extract lose team pts to make lose flag
df_lose_team_pts <- df %>%
  group_by(ScheduleKey) %>%
  summarise(win_team_pts = max(PTS) ,lose_team_pts = min(PTS), .groups = "drop")
# extract half time behind team pts to make half time behind flag
df_behind_team_pts <- df %>%
  mutate(half_time_pts = Q1 + Q2) %>%
  group_by(ScheduleKey) %>%
  summarise(exceed_team_pts = max(half_time_pts), behind_team_pts = min(half_time_pts), .groups = "drop")

df %>%
  left_join(
    df_lose_team_pts
    ,by = "ScheduleKey"
  ) %>%
  left_join(
    df_behind_team_pts
    ,by = "ScheduleKey"
  ) %>%
  mutate(
    win_flag = if_else(PTS == lose_team_pts, 0, 1)
    ,half_time_behind_flag = if_else(half_time_pts == behind_team_pts, "behind", "exceed")
    ,pts_diff = win_team_pts - lose_team_pts
    ,half_time_pts_diff = exceed_team_pts - behind_team_pts
  ) %>%
  filter(half_time_behind_flag == "behind") %>%
  group_by(half_time_pts_diff) %>%
  summarise(
    game_cnt = n()
    ,lose_cnt = sum(win_flag)
    ,.groups = "drop"
  ) %>%
  mutate(
    win_ratio = 100 * lose_cnt / game_cnt
  ) %>%
  ggplot(aes(x = half_time_pts_diff, y = win_ratio))+
  geom_line()+
  labs(x = "ハーフタイムでのビハインド点数", y = "勝率", title = "ハーフタイムでのビハインド点数ごとの勝率")+
  theme_bw()
