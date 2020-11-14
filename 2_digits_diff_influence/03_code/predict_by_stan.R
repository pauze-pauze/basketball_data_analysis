library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)
library(rstan)
library(brms)

package_version("bleaguer")
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


# bayes predict -----------------------------------------------------------

# setting option
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# create data set
df_under_9pts_behind <- df_base %>%
  select(win_flag, half_time_pts_diff) %>%
  filter(half_time_pts_diff <= 9)
skimr::skim(df_under_9pts_behind)

glm_under_9pts <- brm(
  win_flag ~ half_time_pts_diff
  ,family = bernoulli()
  ,data = df_under_9pts_behind
  ,seed = 1031
  ,prior = c(set_prior("", class = "Intercept"))
)
glm_under_9pts

set.seed(1031)
eff <- marginal_effects(
  glm_under_9pts
  ,method = "predict"
)
plot(eff)
plot(glm_under_9pts)

# predict over 10pts
diff_data <- data.frame(half_time_pts_diff = seq(10, 20,1))
set.seed(1031)
predict(glm_under_9pts, diff_data, probs = c(0.3, 0.7))



# base function predict ---------------------------------------------------
# create data set
df_under_9pts_behind <- df_base %>%
  select(win_flag, half_time_pts_diff) %>%
  filter(half_time_pts_diff <= 9)
skimr::skim(df_under_9pts_behind)

glm_model <- glm(win_flag ~ half_time_pts_diff, data = df_under_9pts_behind, family = binomial)
summary(glm_model)

# predict over 10pts
over_10pts_data <- df_base %>%
  select(half_time_pts_diff) %>%
  filter(half_time_pts_diff >= 10) %>%
  arrange(half_time_pts_diff)
pred_lm <- predict(glm_model, over_10pts_data,se.fit = TRUE,  interval = "prediction")

lines(over_10pts_data, pred_lm$fit[,2], col = "blue")
pred_lm$fit

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

