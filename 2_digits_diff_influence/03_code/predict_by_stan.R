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
)
plot(glm_under_9pts)

# predict over 10pts
diff_data <- data.frame(half_time_pts_diff = seq(10, 20,1))
fitted(glm_under_9pts, diff_data) # なんでこれだとだめなんだろう
linear_fit <- fitted(glm_under_9pts, diff_data, scale = "linear")[,1]
fit <- 1 / (1 + exp(-linear_fit))
fit[1]

# simulation
game_cnt <- df_base %>% # game cnt of 10 pts diff
  filter(half_time_pts_diff == 10) %>%
  count() %>%
  as.numeric()
win_cnt <- df_base %>% # win cnt of 10 pts diff
  filter(half_time_pts_diff == 10 & win_flag == 1) %>%
  count() %>%
  as.numeric()
pbinom(q = win_cnt, prob = fit[1],size = game_cnt,lower.tail = TRUE)

set.seed(1031)
data.frame(win_cnt = rbinom(n =1000000 ,size = game_cnt, prob = fit[1])) %>%
  ggplot(aes(x = win_cnt)) +
  geom_histogram(binwidth = 1) +
  theme_bw()


# base function predict ---------------------------------------------------
# create data set
df_under_9pts_behind <- df_base %>%
  select(win_flag, half_time_pts_diff) %>%
  filter(half_time_pts_diff <= 9)
skimr::skim(df_under_9pts_behind)

glm_model <- glm(win_flag ~ half_time_pts_diff, data = df_under_9pts_behind, family = binomial)
summary(glm_model)

# predict over 10pts
diff_data <- data.frame(half_time_pts_diff = seq(10, 20,1))
fit_lm <- predict(glm_model, diff_data, interval = "confidence")
fit_lm[1]
