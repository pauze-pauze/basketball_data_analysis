
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



# create base data frame
df_base <- df %>%
  left_join( #join game information
    game[,c("ScheduleKey", "Season", "EventId")]
    ,by = "ScheduleKey"
  ) %>%
  filter(
    Season %in% c("2016-17", "2017-18", "2018-19", "2019-20") # filter season
    ,EventId %in% c(2, 3, 7, 8) # filter game type
  ) %>%
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
  filter(half_time_behind_flag == "behind")





# analize data ------------------------------------------------------------


# B1B2 all 
df_base %>%
  group_by(half_time_pts_diff) %>%
  summarise(
    game_cnt = n()
    ,lose_cnt = sum(win_flag)
    ,.groups = "drop"
  ) %>%
  mutate(
    win_ratio = 100 * lose_cnt / game_cnt
  )%>%
  ggplot(aes(x = half_time_pts_diff, y = win_ratio))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  #geom_text_repel(aes(label = game_cnt), size = 3) +
  labs(x = "ハーフタイムでのビハインド点数", y = "勝率(%)", title = "ハーフタイムでのビハインド点数ごとの勝率", caption = "B1・B2の2016~2019シーズンデータ")+
  theme_bw()

# B1 all 
df_base %>%
  filter(EventId %in% c(2, 3)) %>%
  group_by(half_time_pts_diff) %>%
  summarise(
    game_cnt = n()
    ,lose_cnt = sum(win_flag)
    ,.groups = "drop"
  ) %>%
  mutate(
    win_ratio = 100 * lose_cnt / game_cnt
  )%>%
  ggplot(aes(x = half_time_pts_diff, y = win_ratio))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  #geom_text_repel(aes(label = game_cnt), size = 3) +
  labs(x = "ハーフタイムでのビハインド点数", y = "勝率(%)", title = "ハーフタイムでのビハインド点数ごとの勝率(B1)", caption = "B1の2016~2019シーズンデータ")+
  theme_bw()

# B2 all 
df_base %>%
  filter(EventId %in% c(7, 8)) %>%
  group_by(half_time_pts_diff) %>%
  summarise(
    game_cnt = n()
    ,lose_cnt = sum(win_flag)
    ,.groups = "drop"
  ) %>%
  mutate(
    win_ratio = 100 * lose_cnt / game_cnt
  )%>%
  ggplot(aes(x = half_time_pts_diff, y = win_ratio))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  #geom_text_repel(aes(label = game_cnt), size = 3) +
  labs(x = "ハーフタイムでのビハインド点数", y = "勝率(%)", title = "ハーフタイムでのビハインド点数ごとの勝率(B2)", caption = "B2の2016~2019シーズンデータ")+
  theme_bw()

# plot half time pts diff and final pts diff
df_base %>%
  mutate(pts_diff = if_else(win_flag == 1, pts_diff, -pts_diff))%>%
  group_by(pts_diff, half_time_pts_diff) %>%
  summarise(game_cnt = n(), .groups = "drop") %>%
  ggplot(aes(x = half_time_pts_diff, y = pts_diff, size = game_cnt))+
  geom_point()+
  scale_size_area(max_size = 5)+
  labs(x = "ハーフタイムでのビハインド点数", y = "最終的な点差", title = "ハーフタイムでのビハインド点数と最終的な点差の分布", caption = "B1・B2の2016~2019シーズンデータ")+
  theme_bw()

#check big difference game
# type1
df_base %>%
  mutate(pts_diff = if_else(win_flag == 1, pts_diff, -pts_diff))%>%
  filter(pts_diff == max(pts_diff))
#type2
df_base %>%
  mutate(pts_diff = if_else(win_flag == 1, pts_diff, -pts_diff))%>%
  mutate(diff_in_diff = pts_diff + half_time_pts_diff) %>%
  filter(win_flag == 1 & diff_in_diff == max(diff_in_diff))
  