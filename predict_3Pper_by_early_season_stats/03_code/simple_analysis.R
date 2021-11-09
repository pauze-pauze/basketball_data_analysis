
# import libraries --------------------------------------------------------

library(tidyverse)
library(scales)
library(ggrepel)

# load data ---------------------------------------------------------------

source("../B_data.R")


# select players who shot many times --------------------------------------

game %>% 
  filter(Season == "2018-19", EventId == 2) %>%
  count()
# 一旦18-19シーズンにする。bleaguerのアップデートが終わったら20-21シーズン対象

base <- boxscore %>%
  inner_join(
    game %>%
      filter(EventId == 2, Season == "2018-19") %>% # choose specific event (B1 Regular Season) and season
      select(ScheduleKey, Season)
    ,by = "ScheduleKey"
  )

player_3P_rank <- 20
most_3pa_players <- base %>%
  group_by(PlayerId, Player, TeamId) %>%
  summarise(P3A_all = sum(F3GA), P3M_all = sum(F3GM)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(P3A_all))) %>%
  filter(rank <= player_3P_rank) %>%
  mutate(P3per_all = P3M_all / P3A_all)
  
# select games which are the first xxxx times -----------------------------


first_xx_times_game <- game_summary %>%
  select(ScheduleKey, TeamId) %>%
  inner_join(
    game %>%
      filter(EventId == 2, Season == "2018-19") %>% # choose specific event (B1 Regular Season) and season
      select(ScheduleKey, Season, Date)
    ,by = "ScheduleKey"
  ) %>%
  group_by(TeamId) %>%
  mutate(xx_times = dense_rank(Date)) %>%
  filter(xx_times <= 10) # choose specific times (when I saw the 2020-21 season game times)
  

# make data to compare first xx times and final 3P% -----------------------

first_xx_times_stats <- first_xx_times_game %>%
  left_join(
    base %>%
      select(ScheduleKey, TeamId, PlayerId, F3GA, F3GM)
    ,by = c("ScheduleKey", "TeamId")
  ) %>%
  inner_join(
    most_3pa_players
    ,by = c("PlayerId", "TeamId")
  ) %>%
  group_by(PlayerId, Player) %>%
  summarise(P3A_xx_times = sum(F3GA), P3M_xx_times = sum(F3GM)) %>%
  mutate(P3per_xx_times = P3M_xx_times / P3A_xx_times) %>%
  left_join(
    most_3pa_players
    ,by = c("PlayerId", "Player")
  )

lim_range_min = 0.22
lim_range_max = 0.52
# scatter plot (normal)
p1 <- first_xx_times_stats %>%
  ggplot(aes(x = P3per_xx_times, y = P3per_all, label = Player)) +
  geom_point()+
  geom_text_repel() +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5, linetype = 2) +
  scale_y_continuous(labels = percent, limits = c(lim_range_min, lim_range_max)) + 
  scale_x_continuous(labels = percent, limits = c(lim_range_min, lim_range_max)) +
  labs(x = "レギュラーシーズン開始10試合の3P%"
       , y = "レギュラーシーズン全体の3P%"
       , title = str_interp("シーズン序盤の3P%と全体の3P%の分布(最終的な3PA上位${player_3P_rank}名対象)")
       , caption = "シーズンは2018-19が対象\n欠場していた試合も含めて10試合とする\n点線はx = yの線"
  ) +
  theme_classic()
plot(p1)
#ggsave(file = "./02_output/scatter_firstXXtimes_final_3Pper.png", plot = p1)

# scappter plot (complex)
p2 <- first_xx_times_stats %>%
  mutate(Player_p3a = str_c(as.character(Player), as.character(P3A_xx_times), sep = "_")) %>%
  ggplot(aes(x = P3per_xx_times, y = P3per_all, label = Player_p3a)) +
  geom_point()+
  geom_text_repel() +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5, linetype = 2) +
  scale_y_continuous(labels = percent, limits = c(lim_range_min, lim_range_max)) + 
  scale_x_continuous(labels = percent, limits = c(lim_range_min, lim_range_max)) +
  labs(x = "レギュラーシーズン開始10試合の3P%"
       , y = "レギュラーシーズン全体の3P%"
       , title = "シーズン序盤の3P%と全体の3P%の分布(最終的な3PA上位${player_3P_rank}名対象)"
       , caption = "シーズンは2018-19が対象\n欠場していた試合も含めて10試合とする\n点線はx = yの線\n選手の末尾の数字は序盤の3PA"
  ) +
  theme_classic()
plot(p2)
#ggsave(file = "./02_output/scatter_firstXXtimes_final_3Pper_complex.png", plot = p2)


# 3P% time series by player -----------------------------------------------

box_score_team_master <- base %>% # pick up games which most 3p attempt players belongs to
  inner_join(
    most_3pa_players %>%
      distinct(TeamId)
    ,by = "TeamId"
  ) %>%
  distinct(ScheduleKey, TeamId) %>%
  left_join(
    most_3pa_players %>%
      select(PlayerId, Player, TeamId)
    ,by = "TeamId"
  )

time_series <- box_score_team_master %>%
  left_join(
    base %>%
      select(ScheduleKey, TeamId, PlayerId, F3GA, F3GM)
    ,by = c("ScheduleKey", "TeamId", "PlayerId")
  ) %>%
  left_join(
    game %>% 
      filter(Season == "2018-19", EventId == 2) %>%
      select(ScheduleKey, Date)
    ,by = "ScheduleKey"
  ) %>%
  mutate_if(
    is.numeric, ~replace_na(.x, 0)
  ) %>%
  group_by(TeamId, PlayerId) %>%
  arrange(Date) %>%
  mutate(
    cumsum_3PA = cumsum(F3GA)
    ,cumsum_3PM = cumsum(F3GM)
    ,cumsum_3Pper = cumsum_3PM / cumsum_3PA
    ,game_row_number = row_number(Date)
  )
# 未出場の内、NAはベンチ入りしてないパターン。それ以外はベンチ入りしてるけど出場してないパターン


# plot time series --------------------------------------------------------

lim_range_min = 0.00
lim_range_max = 0.65

p3 <- time_series %>%
  ggplot(aes(x = game_row_number, y = cumsum_3Pper))+
  geom_line()+
  geom_point(size = 0.5) +
  scale_y_continuous(labels = percent, limits = c(lim_range_min, lim_range_max)) + 
  labs(x = "経過試合数"
       , y = "各試合時点でのの3P%"
       , title = str_interp("各試合終了時点での3P%推移(最終的な3PA上位${player_3P_rank}名対象)")
       , caption = "シーズンは2018-19が対象\n欠場していた試合は3PAと3PMいずれも0とする"
  ) +
  facet_wrap(~Player, ncol = 3) +
  theme_bw()
plot(p3)
#ggsave(file = "./02_output/time_series_3Pper_cumsum_per_game.png", plot = p3, width = 6.4, height = 12.8)
