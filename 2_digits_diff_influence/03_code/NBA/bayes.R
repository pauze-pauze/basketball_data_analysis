
# import library ----------------------------------------------------------
pacman::p_load(tidyverse, scales, broom, rstan, brms)

# load data ---------------------------------------------------------------

df <- readRDS("./01_data/quarter_log.rds") 
df <- df %>%
  mutate(pts_diff = hs - vs)

# 2Q ----------------------------------------------------------------------
period_param <- 2

# manipulate data ---------------------------------------------------------
## extract 2Q and final results by each game.
df_result <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == period_param) %>%
      select(pts_diff_tq = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_tq < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_tq = if_else(pts_diff_tq < 0, pts_diff_tq, -pts_diff_tq)
  )

# bayes analysis ----------------------------------------------
# setting option
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

df_log <- df_result %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  filter(pts_diff_tq >= -15 & pts_diff_tq <= -5) %>% #  To focus on near 10 points behind(If I use scores far from 10 points, they would be impacted by other effect like gabage time.)
  mutate(two_digits_flag = if_else(pts_diff_tq <= -10, 1, 0))

df_under_9pts <- df_log %>%
  select(win_flag, pts_diff_tq) %>%
  filter(pts_diff_tq >= -9)
glm_under_9pts <- brm(
  win_flag ~ pts_diff_tq
  ,family = bernoulli()
  ,data = df_under_9pts
  ,seed = 1031
  ,prior = c(set_prior("", class = "Intercept"))
)

glm_under_9pts

set.seed(1031)
eff <- marginal_effects(
  glm_under_9pts
)
plot(glm_under_9pts)

# predict over 10pts
diff_data <- data.frame(pts_diff_tq = seq(-10, -20,1))
#fitted(glm_under_9pts, diff_data) # なんでこれだとだめなんだろう
linear_fit <- fitted(glm_under_9pts, diff_data, scale = "linear")[,1]
fit <- 1 / (1 + exp(linear_fit))
fit[1]

# simulation
game_cnt <- df_base %>% # game cnt of 10 pts diff
  filter(half_time_pts_diff == 10) %>%
  count() %>%
  as.numeric()

# 3Q ----------------------------------------------------------------------
period_param <- 3

# manipulate data ---------------------------------------------------------
## extract 2Q and final results by each game.
df_result <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == period_param) %>%
      select(pts_diff_tq = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_tq < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_tq = if_else(pts_diff_tq < 0, pts_diff_tq, -pts_diff_tq)
  )


# check the num of games by each points difference ------------------------
df_result %>%
  mutate(pts_diff_tq = if_else(pts_diff_tq < -25, -25, pts_diff_tq)) %>%
  group_by(pts_diff_tq) %>%
  summarise(
    game_cnt = n()
  ) %>%
  ggplot(aes(x = -pts_diff_tq, y = game_cnt))+
  geom_bar(stat = "identity")+
  labs(
    x = "Each behind points at the end of 3Q"
    , y = "Game cnt"
    , title = "Game count of each behind points at the end of 3Q"
    , subtitle = "4800 games of 4 seasons data(2020-2023) in NBA"
    , caption = "over 25 points behind games are aggregated 25 points."
  )+
  theme_bw()+
  theme(panel.grid.minor = element_blank())


# Check the transition of result by each points difference ----------------

df_result %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  mutate(pts_diff_tq = if_else(pts_diff_tq < -25, -25, pts_diff_tq)) %>%
  group_by(pts_diff_tq) %>%
  summarise(
    game_cnt = n()
    ,win_cnt = sum(win_flag)
  ) %>%
  mutate(win_ratio = win_cnt / game_cnt) %>%
  mutate(win_ratio = if_else(pts_diff_tq == 0, 0.5, win_ratio)) %>% # special process
  ggplot(aes(x = -pts_diff_tq, y = win_ratio))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 25, by = 5))+
  scale_y_continuous(labels = percent, breaks = seq(0, 0.5, length = 6), limits = c(0, 0.5))+
  geom_line(size = 1)+
  geom_point(size = 2)+
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



# linear regression analysis ----------------------------------------------

df_log <- df_result %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  filter(pts_diff_tq >= -15 & pts_diff_tq <= -5) %>%
  mutate(two_digits_flag = if_else(pts_diff_tq <= -10, 1, 0))

glm_result <- glm(win_flag ~ pts_diff_tq + two_digits_flag, data = df_log, family = binomial)  
summary(glm_result)


































