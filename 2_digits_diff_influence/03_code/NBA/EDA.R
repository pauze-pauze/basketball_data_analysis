
# import library ----------------------------------------------------------
pacman::p_load(tidyverse, skimr, ggrepel, scales)
library(tidyverse)

# load data ---------------------------------------------------------------

df <- readRDS("./01_data/quarter_log.rds") 
df <- df %>%
  mutate(pts_diff = hs - vs)

summary(df)
skim(df)

length(unique(df$g_id))

# 2Q ----------------------------------------------------------------------


# manipulate data ---------------------------------------------------------
## extract 2Q and final results by each game.
df_result_2q <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == 2) %>%
      select(pts_diff_2q = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_2q < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_2q = if_else(pts_diff_2q < 0, pts_diff_2q, -pts_diff_2q)
  )


# Check the transition of result by each points difference ----------------

df_result_2q %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  mutate(pts_diff_2q = if_else(pts_diff_2q < -25, -25, pts_diff_2q)) %>%
  group_by(pts_diff_2q) %>%
  summarise(
    game_cnt = n()
    ,win_cnt = sum(win_flag)
  ) %>%
  mutate(win_ratio = win_cnt / game_cnt) %>%
  mutate(win_ratio = if_else(pts_diff_2q == 0, 0.5, win_ratio)) %>% # special process
  ggplot(aes(x = -pts_diff_2q, y = win_ratio))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 25, by = 5))+
  scale_y_continuous(labels = percent, breaks = seq(0, 0.6, length = 7), limits = c(0, 0.6))+
  geom_line(size = 0.5)+
  geom_point(size = 1)+
  #geom_text_repel(aes(label = game_cnt), size = 3)+
  labs(
    x = "Each behind points at halftime"
    , y = "Win%"
    , title = "Winning percentage of each behind points at halftime"
    , subtitle = "4800 games of 4 seasons data(2020-2023) in NBA"
    , caption = "over 25 points behind games are aggregated 25 points."
  )+
  theme_bw()+
  theme(panel.grid.minor = element_blank())



# 3Q ----------------------------------------------------------------------

# manipulate data ---------------------------------------------------------
## extract 3Q and final results by each game.
df_result_3q <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == 3) %>%
      select(pts_diff_3q = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_3q < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_3q = if_else(pts_diff_3q < 0, pts_diff_3q, -pts_diff_3q)
  )


# Check the transition of result by each points difference ----------------

df_result_3q %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  mutate(pts_diff_3q = if_else(pts_diff_3q < -25, -25, pts_diff_3q)) %>%
  group_by(pts_diff_3q) %>%
  summarise(
    game_cnt = n()
    ,win_cnt = sum(win_flag)
  ) %>%
  mutate(win_ratio = win_cnt / game_cnt) %>%
  mutate(win_ratio = if_else(pts_diff_3q == 0, 0.5, win_ratio)) %>% # special process
  ggplot(aes(x = -pts_diff_3q, y = win_ratio))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 25, by = 5))+
  scale_y_continuous(labels = percent, breaks = seq(0, 0.6, length = 7), limits = c(0, 0.6))+
  geom_line(size = 0.5)+
  geom_point(size = 1)+
  #geom_text_repel(aes(label = game_cnt), size = 3)+
  labs(
    x = "Each behind points at the end of 3Q"
    , y = "Win%"
    , title = "Winning percentage of each behind points at the end of 3Q"
    , subtitle = "4800 games of 4 seasons data(2020-2023) in NBA"
    , caption = "over 25 points behind games are aggregated 25 points."
  )+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
