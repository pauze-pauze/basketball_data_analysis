
# import libraries --------------------------------------------------------

library(tidyverse)
library(scales)
library(ggrepel)
library(rstan)
options(mc.cores = parallel::detectCores())
seed = 1031

options(buildtools.check = function(action) TRUE)
# popupのブロック
# https://discourse.mc-stan.org/t/building-r-package-from-source-requires-installation-of-additional-build-tools/15218/2

# load data ---------------------------------------------------------------

source("../B_data.R")



# select players to make prior distribution -------------------------------

game %>% 
  filter(Season %in% c("2016-17", "2017-18"), EventId == 2) %>%
  count()

prior_base <- boxscore %>%
  inner_join(
    game %>%
      filter(Season %in% c("2016-17", "2017-18"), EventId == 2) %>% # choose specific event (B1 Regular Season) and season
      select(ScheduleKey, Season)
    ,by = "ScheduleKey"
  )

player_3P_rank_prior <- 40
most_3pa_players_prior <- prior_base %>%
  group_by(PlayerId, Player, TeamId, Season) %>%
  summarise(P3A_all = sum(F3GA), P3M_all = sum(F3GM)) %>%
  ungroup() %>%
  group_by(Season) %>% # to choose each season's top 3PA players
  mutate(rank = dense_rank(desc(P3A_all))) %>%
  filter(rank <= player_3P_rank_prior) %>%
  mutate(P3per_all = P3M_all / P3A_all)

# check prior distribution
most_3pa_players_prior %>%
  ggplot(aes(x = P3per_all)) +
  geom_histogram(bins = 10)
### use this data to make prior distrobution (normal distribution)


prior_mean = mean(most_3pa_players_prior$P3per_all)
prior_mean
prior_sd = sd(most_3pa_players_prior$P3per_all) # 意図的に不偏分散にする
prior_sd



# select players to predict --------------------------------------

base <- boxscore %>%
  inner_join(
    game %>%
      filter(EventId == 2, Season == "2018-19") %>% # choose specific event (B1 Regular Season) and season
      select(ScheduleKey, Season)
    ,by = "ScheduleKey"
  )

player_3P_rank <- 40
most_3pa_players <- base %>%
  group_by(PlayerId, Player, TeamId) %>%
  summarise(P3A_all = sum(F3GA), P3M_all = sum(F3GM)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(P3A_all))) %>%
  filter(rank <= player_3P_rank) %>%
  mutate(P3per_all = P3M_all / P3A_all)


first_xx_times_param = 10 # choose specific times (when I saw the 2020-21 season game times)
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
  filter(xx_times <= first_xx_times_param) # choose specific times (when I saw the 2020-21 season game times)


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
  ) %>%
  ungroup() %>%
  mutate(rn = row_number())


# sim code ----------------------------------------------------------------

df <- as_tibble()
for (i in 1:nrow(first_xx_times_stats)) { # 本番はこれnrow(first_xx_times_stats)
  f3g_made <- first_xx_times_stats %>%
    filter(rn == i) %>%
    select(P3M_xx_times) %>%
    as.numeric()
  f3g_miss <- first_xx_times_stats %>%
    mutate(P3miss_xx_times = P3A_xx_times - P3M_xx_times) %>%
    filter(rn == i) %>%
    select(P3miss_xx_times) %>%
    as.numeric()
  
  Y <-  rep(c(1, 0), c(f3g_made, f3g_miss)) # 3PM
  sum(Y)
  N <-  f3g_made + f3g_miss #3PA
  data <-  list(Y = Y, N = N, prior_mean = prior_mean, prior_sd = prior_sd)
  
  fit <- stan(file='./03_code/train_test.stan', data=data, seed=seed)
  tmp <- rstan::extract(fit, permuted = FALSE)
  length(tmp[,,"p"])
  tmp_vec <- as.vector(tmp[,,"p"])
  test <- quantile(tmp_vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  df[i,1] <- i
  df[i,2] <- test[1]
  df[i,3] <- test[2]
  df[i,4] <- test[3]
  df[i,5] <- test[4]
  df[i,6] <- test[5]
}

df <- df %>%
  rename(
    rn = ...1
    ,tile_10per = ...2
    ,tile_25per = ...3
    ,tile_50per = ...4
    ,tile_75per = ...5
    ,tile_90per = ...6
  )

result <- first_xx_times_stats %>%
  left_join(
    df
    ,by = "rn"
  )
result %>%
  mutate(
    within_80per = case_when(
      P3per_all >= tile_10per & P3per_xx_times <= tile_90per ~ 1
      ,TRUE ~ 0
    )
    ,within_50per = case_when(
      P3per_all >= tile_25per & P3per_xx_times <= tile_75per ~ 1
      ,TRUE ~ 0
    )
  ) %>%
  summarise(cnt_within_80 = sum(within_80per), cnt_within_50 = sum(within_50per))


# アテンプト数ごとの信頼区間の幅
# 

# test code ---------------------------------------------------------------
i <- 1

f3g_made <- first_xx_times_stats %>%
  filter(rn == i) %>%
  select(P3M_xx_times) %>%
  as.numeric()
f3g_miss <- first_xx_times_stats %>%
  mutate(P3miss_xx_times = P3A_xx_times - P3M_xx_times) %>%
  filter(rn == i)
  select(P3miss_xx_times) %>%
  as.numeric()

Y <-  rep(c(1, 0), c(f3g_made, f3g_miss)) # 3PM
sum(Y)
N <-  f3g_made + f3g_miss #3PA
data <-  list(Y = Y, N = N, prior_mean = prior_mean, prior_sd = prior_sd)

fit <- stan(file='./03_code/train_test.stan', data=data, seed=seed) #chainsは3つにして時短
tmp <- rstan::extract(fit, permuted = FALSE)
length(tmp[,,"p"])
tmp_vec <- as.vector(tmp[,,"p"])
test <- quantile(tmp_vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

df <- as_tibble() %>%
  mutate(
    rn = NULL
    ,tile_10per = NULL
    ,tile_25per = NULL
    ,tile_50per = NULL
    ,tile_75per = NULL
    ,tile_90per = NULL
  )
df[i,1] <- i
df[i,2] <- test[1]
df[i,3] <- test[2]
df[i,4] <- test[3]
df[i,5] <- test[4]
df[i,6] <- test[5]




df <- df %>%
  rename(
    rn = ...1
    ,tile_10per = ...2
    ,tile_25per = ...3
    ,tile_50per = ...4
    ,tile_75per = ...5
    ,tile_90per = ...6
  )

test %>%
  as_list() %>%
  t() #%>%
  mutate(rn = 1)

print(fit, probs = c(0.025, 0.1, 0.25, 0.5, 0.75,  0.9, 0.975))
traceplot(fit, inc_warmup = TRUE)
stan_hist(fit,pars="p")










