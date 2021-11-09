
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
  filter(Season %in% c("2019-20", "2020-21"), EventId == 2) %>%
  count()

prior_base <- boxscore %>%
  inner_join(
    game %>%
      filter(Season %in% c("2019-20", "2020-21"), EventId == 2) %>% # choose specific event (B1 Regular Season) and season
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

# make data frame 2021/11/09 check(11 games) ---------------------------------------------------------

水曜の試合終わったら更新するかも
name <- c(
  "ディージェイ・ニュービル"
  ,"ジュリアン・マブンガ"
  ,"富樫 勇樹"
  ,"安藤 誓哉"
  ,"ジョーダン・グリン"
  ,"金丸 晃輔"
  ,"コティ・クラーク"
  ,"辻 直人"
  ,"福澤 晃平"
  ,"古川 孝敏"
  ,"岸本 隆一"
  ,"岡田 侑大"
  ,"ジョシュ・ハレルソン"
  ,"今村 佳太"
  ,"クリストファー・スミス"
)

P3A <- c(
  87
  ,82
  ,79
  ,72
  ,69
  ,69
  ,68
  ,68
  ,64
  ,59
  ,59
  ,58
  ,58
  ,58
  ,57
)

P3M <- c(
  29
  ,25
  ,26
  ,23
  ,33
  ,33
  ,25
  ,29
  ,24
  ,25
  ,22
  ,23
  ,21
  ,19
  ,23
)

base_df <- data.frame(name = name, P3A = P3A, P3M = P3M) %>%
  mutate(rn = row_number(), P3per = P3M / P3A)




# sim code ----------------------------------------------------------------

df <- as_tibble()
for (i in 1:nrow(base_df)) { 
  f3g_made <- base_df %>%
    filter(rn == i) %>%
    select(P3M) %>%
    as.numeric()
  f3g_miss <- base_df %>%
    mutate(P3miss = P3A - P3M) %>%
    filter(rn == i) %>%
    select(P3miss) %>%
    as.numeric()
  
  Y <-  rep(c(1, 0), c(f3g_made, f3g_miss)) # 3PM
  sum(Y)
  N <-  f3g_made + f3g_miss #3PA
  data <-  list(Y = Y, N = N, prior_mean = prior_mean, prior_sd = prior_sd)
  
  fit <- stan(file='./03_code/predict.stan', data=data, seed=seed)
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

result <- base_df %>%
  left_join(
    df
    ,by = "rn"
  )
