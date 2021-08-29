
# import libraries --------------------------------------------------------

library(nbastatR)
library(future)
library(tidyverse)

# load data ---------------------------------------------------------------

plan(multiprocess) 
t <- game_logs(seasons = 2021)


# manipulate data ---------------------------------------------------------

view(t)
summary(t)
# The data is per player, so I convert it into per game

No_OverTime_game <- t %>%
  group_by(idGame, nameTeam) %>%
  summarise(
    game_time = sum(minutes)
  ) %>%
  filter(game_time <= 250) %>% # Due to rounding up, the number does not always add up to exactly 240,  anything less will be considered a match without overtime.
  distinct(idGame)

  base <- t %>%
    inner_join( # exclude OT game.
      No_OverTime_game
      ,by = "idGame"
    )
    
  # filter(typeSeason == "Regular Season") # It has already filtered to regular season, so this process is not necessary
  #group_by(idGame, nameTeam) %>% # group the data per game and team group in order to make per game stats.
# 後で延長のなかった試合のみにする(game単位でminが一定以上を超えた試合に絞る)
actual_pts_dist <- base %>%
  group_by(idGame, nameTeam) %>%
  summarise(PTS = sum(pts))
  

# simulateion -------------------------------------------------------------


f3ga = sum(base$fg3a)
f2ga = sum(base$fg2a)
fta = sum(base$fta)
f3g_per <- sum(base$fg3m) / sum(base$fg3a)
f2g_per <- sum(base$fg2m) / sum(base$fg2a)
ft_per <- sum(base$ftm) / sum(base$fta)
game_team_cnt <- nrow(No_OverTime_game) * 2
atmpt_num <- (f3ga + f2ga + fta) / game_team_cnt # Average number of atempts.
exp_num <- 250 # parameter n of exponential distribution.
pts_dist <- rep(NA, game_team_cnt)


base_df <- data.frame(
  rowname = rep(seq(exp_num), each = 1),
  pattern = rep(c("f3ga", "f2ga", "fta"), each = exp_num)
) %>%
  mutate(
    atmpt_pt = case_when(
      pattern == "f3ga" ~ 3
      ,pattern == "f2ga" ~ 2
      ,pattern == "fta" ~ 1
    )
  )
flag_prob <- c(f3ga = f3ga, f2ga = f2ga, fta = fta) / sum(f3ga, f2ga, fta)
pts_prob <- c(f3ga = f3g_per, f2ga = f2g_per, fta = ft_per)

score <- function() {
  exp_vec <- rexp(n = exp_num, rate = atmpt_num) # create an interval of time between the previous shot.
  atmpt_pred <- sum(cumsum(exp_vec) < 1)
  t <- as.tibble(
    rownames_to_column(
      as.data.frame(
        t(rmultinom(n = exp_num, size = 1, prob = c(f3ga, f2ga, fta)))
      )
    )
  ) %>%
    mutate(
      rowname = as.numeric(rowname)
    ) %>%
    rename(
      ,f3ga = V1
      ,f2ga = V2
      ,fta = V3
    ) %>%
    pivot_longer(
      -rowname
      ,names_to = "pattern"
      ,values_to = "flag"
    ) %>%
    filter(
      flag == 1
    )
  
  
  base_df %>%
    filter(rowname <= atmpt_pred) %>%
    inner_join(t, by = c("rowname", "pattern")) %>%
    #    filter(runif(n()) <= flag_prob[pattern]) %>% # flag相当
    mutate(pts = atmpt_pt * rbinom(n(), size = 1, prob = pts_prob[pattern])) %>% 
    pull(pts) %>%
    sum()
}

# Simulate the same number of game count ver
set.seed(1031)
pts_dist <- map_dbl(seq(game_team_cnt), ~score())

pts_dist_tibble <- as_tibble(pts_dist) %>%
  mutate(
    type = "simulation"
  ) %>%
  rename(PTS = value)


#Visualise
p_same_num <- actual_pts_dist %>%
  select(PTS) %>%
  mutate(
    type = "actual"
  ) %>%
  bind_rows(pts_dist_tibble) %>%
  ggplot(aes(x = PTS, fill = type))+
  geom_histogram(position = "dodge", bins = 10)+
  labs(x = "points", y = "count", title = "Compare actual points distribution and simulated one.(seed = 1031)")+
  theme_classic()
plot(p_same_num)
ggsave(file = "./02_output/same_game_cnt_NBA.png", p_same_num)


# Simulate 10 times number of game count ver
pts_dist <- furrr::future_map_dbl(seq(game_team_cnt * 10), ~ score(), .options = furrr::future_options(seed = 1031L)) 

pts_dist_tibble <- as_tibble(pts_dist) %>%
  mutate(
    type = "simulation"
  ) %>%
  rename(PTS = value)


p_10times_num <- actual_pts_dist %>%
  select(PTS) %>%
  mutate(
    type = "actual"
  ) %>%
  bind_rows(pts_dist_tibble) %>%
  ggplot(aes(x = PTS, fill = type))+
  geom_density(alpha=0.5)+
  labs(x = "points", y = "probability density", title = "Compare actual points distribution and 10 times simulated one(seed = 1031)")+
  theme_classic()
plot(p_10times_num)
ggsave(file = "./02_output/same_10times_cnt_NBA.png", p_10times_num)






