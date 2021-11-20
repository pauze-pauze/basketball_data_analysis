
# import libraries --------------------------------------------------------

library(tidyverse)
library(scales)
library(ggrepel)
library(rstan)
library(nbastatR)
options(mc.cores = parallel::detectCores())
seed = 1031

options(buildtools.check = function(action) TRUE)
# block popup
# https://discourse.mc-stan.org/t/building-r-package-from-source-requires-installation-of-additional-build-tools/15218/2

# load data ---------------------------------------------------------------

# extract Regular Season boxscore from 2018 to 2022
df_base <- game_logs(
  seasons = c(2018:2022),
  league = "NBA",
  result_types = "player",
  season_types = "Regular Season"
)


# Prior distribution pattern 1 (Entire League 3P% distribution ver) ---------


# * select players to make prior distribution -------------------------------

player_3P_rank_prior <- 100 # Set how many players who shoot 3P most I pick up.(I set arbitrary)

most_3pa_players_prior <- df_base %>% # pick up players who shoot 3P most past 2 season
  filter(yearSeason %in% c(2018, 2019)) %>%
  group_by(yearSeason, namePlayer) %>%
  summarise(fg3a_all = sum(fg3a), fg3m_all = sum(fg3m)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(fg3a_all))) %>%
  filter(rank <= player_3P_rank_prior) %>%
  mutate(fg3Per_all = fg3m_all / fg3a_all)


# check prior distribution
prior_distribution <- most_3pa_players_prior %>%
  ggplot(aes(x = fg3Per_all)) +
  geom_histogram(bins = 10) +
  labs(x = "3P%", y = "cnt", title = "Distribution of 3P% which top 100 3PA players' stats in 2018-19,2019-20 season") +
  theme_classic()
plot(prior_distribution)
#ggsave(file = "./02_output/example_prior_distribution.png", plot = prior_distribution)
prior_mean = mean(most_3pa_players_prior$fg3Per_all)
prior_mean
prior_sd = sd(most_3pa_players_prior$fg3Per_all) 
prior_sd

# * select players to predict --------------------------------------
player_3P_rank <- 40 # Set how many players, whom I predict and check accuracy.
season_param <- 2020 # The start of 2020-21 season is delayed, so I select 2019-20 season

most_3pa_players <- df_base %>%
  filter(yearSeason == season_param) %>%
  group_by(yearSeason, namePlayer) %>%
  summarise(fg3a_all = sum(fg3a), fg3m_all = sum(fg3m)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(fg3a_all))) %>%
  filter(rank <= player_3P_rank) %>%
  mutate(fg3Per_all = fg3m_all / fg3a_all)

early_date <- as.Date("2019-11-17") # 今年と同タイミングまでの実績抽出用のパラメータ
early_time_stats <- df_base %>%
  filter(yearSeason == season_param, dateGame <= early_date) %>%
  group_by(namePlayer) %>%
  summarise(fg3a_early = sum(fg3a), fg3m_early = sum(fg3m)) %>%
  ungroup() %>%
  mutate(fg3per_early = fg3m_early / fg3a_early) %>%
  right_join(
    most_3pa_players
    ,by = "namePlayer"
  ) %>%
  ungroup() %>%
  mutate(rn = row_number())


# * execute stan ----------------------------------------------------------

df1 <- as_tibble()
for (i in 1:nrow(early_time_stats)) { 
  f3g_made <- early_time_stats %>%
    filter(rn == i) %>%
    select(fg3m_early) %>%
    as.numeric()
  f3g_miss <- early_time_stats %>%
    mutate(f3gmiss_early = fg3a_early - fg3m_early) %>%
    filter(rn == i) %>%
    select(f3gmiss_early) %>%
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
  df1[i,1] <- i
  df1[i,2] <- test[1]
  df1[i,3] <- test[2]
  df1[i,4] <- test[3]
  df1[i,5] <- test[4]
  df1[i,6] <- test[5]
}

df1 <- df1 %>%
  rename(
    rn = ...1
    ,tile_10per = ...2
    ,tile_25per = ...3
    ,tile_50per = ...4
    ,tile_75per = ...5
    ,tile_90per = ...6
  )

result1 <- early_time_stats %>%
  left_join(
    df1
    ,by = "rn"
  ) %>%
  mutate(
    within_80per = case_when(
      fg3Per_all >= tile_10per & fg3Per_all <= tile_90per ~ 1
      ,TRUE ~ 0
    )
    ,within_50per = case_when(
      fg3Per_all >= tile_25per & fg3Per_all <= tile_75per ~ 1
      ,TRUE ~ 0
    )
  )
  
# write_csv(result1, "./02_output/train_test_result_league.csv")


# * appendix --------------------------------------------------------------

# The percentage which final 3P% stats between x% credible interval
result1 %>%
  summarise(cnt_within_80 = sum(within_80per), cnt_within_50 = sum(within_50per)) %>%
  mutate(rate_80 = cnt_within_80 / nrow(result1), rate_50 = cnt_within_50 / nrow(result1))


# credible interval by 3PA
pattern1_plot <- result1 %>%
  mutate(
    width_80perTile = tile_90per - tile_10per
    ,width_50perTile = tile_75per - tile_25per
  ) %>%
  pivot_longer(
    cols = c("width_80perTile", "width_50perTile")
    ,names_to = "interval_type"
    ,values_to = "interval_num"
  ) %>%
  ggplot(aes(x = fg3a_early, y = interval_num, color = interval_type))+
  geom_point()+
  scale_y_continuous(label = percent)+
  labs(y = "Interval_width", title = "Scatter plot of F3GA and credible interval by pattern1")+
  theme_classic()
pattern1_plot
# ggsave(plot = pattern1_plot, file = "./02_output/pattern1_plot_credibleIntervalWidth.png")

# Prior distribution pattern 2 (last season's stats by each player) ---------


# * select players to predict ---------------------------------------------

player_3P_rank <- 40 # Set how many players, whom I predict and check accuracy.
season_param <- 2020 # The start of 2020-21 season is delayed, so I select 2019-20 season

most_3pa_players <- df_base %>%
  filter(yearSeason == season_param) %>%
  group_by(yearSeason, namePlayer) %>%
  summarise(fg3a_all = sum(fg3a), fg3m_all = sum(fg3m)) %>%
  ungroup() %>%
  mutate(rank = rank(desc(fg3a_all))) %>%
  filter(rank <= player_3P_rank) %>%
  mutate(fg3Per_all = fg3m_all / fg3a_all)

early_date <- as.Date("2019-11-17") # 今年と同タイミングまでの実績抽出用のパラメータ
early_time_stats <- df_base %>%
  filter(yearSeason == season_param, dateGame <= early_date) %>%
  group_by(namePlayer) %>%
  summarise(fg3a_early = sum(fg3a), fg3m_early = sum(fg3m)) %>%
  ungroup() %>%
  mutate(fg3per_early = fg3m_early / fg3a_early) %>%
  right_join(
    most_3pa_players
    ,by = "namePlayer"
  ) %>%
  ungroup() 


# * extract last season 3PA and 3PM stats of each players -----------------

last_season_3p_stats <- df_base %>%
  filter(yearSeason == season_param - 1) %>%
  group_by(namePlayer) %>%
  summarise(fg3a_all_ls = sum(fg3a), fg3m_all_ls = sum(fg3m)) %>%
  mutate(fg3Per_all_ls = fg3m_all_ls / fg3a_all_ls)

early_time_stats <- early_time_stats %>%
  left_join(
    last_season_3p_stats
    ,by = "namePlayer"
  )
is.na(early_time_stats)
early_time_stats <- na.omit(early_time_stats)%>%
  mutate(rn = row_number())

# * execute stan ----------------------------------------------------------

df2 <- as_tibble()
for (i in 1:nrow(early_time_stats)) { 
  f3g_made <- early_time_stats %>%
    filter(rn == i) %>%
    select(fg3m_early) %>%
    as.numeric()
  f3g_miss <- early_time_stats %>%
    mutate(f3gmiss_early = fg3a_early - fg3m_early) %>%
    filter(rn == i) %>%
    select(f3gmiss_early) %>%
    as.numeric()
  f3gper_ls <- early_time_stats %>%
    filter(rn == i) %>%
    select(fg3Per_all_ls) %>%
    as.numeric()
  f3ga_ls <- early_time_stats %>%
    filter(rn == i) %>%
    select(fg3a_all_ls) %>%
    as.numeric()
    
  
  Y <-  rep(c(1, 0), c(f3g_made, f3g_miss)) # 3PM
  sum(Y)
  N <-  f3g_made + f3g_miss #3PA
  data <-  list(Y = Y, N = N, prior_mean = f3gper_ls, prior_sd = sqrt(f3gper_ls * (1 - f3gper_ls) * f3ga_ls) / f3ga_ls)
  
  fit <- stan(file='./03_code/train_test.stan', data=data, seed=seed)
  tmp <- rstan::extract(fit, permuted = FALSE)
  length(tmp[,,"p"])
  tmp_vec <- as.vector(tmp[,,"p"])
  test <- quantile(tmp_vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  df2[i,1] <- i
  df2[i,2] <- test[1]
  df2[i,3] <- test[2]
  df2[i,4] <- test[3]
  df2[i,5] <- test[4]
  df2[i,6] <- test[5]
}

df2 <- df2 %>%
  rename(
    rn = ...1
    ,tile_10per = ...2
    ,tile_25per = ...3
    ,tile_50per = ...4
    ,tile_75per = ...5
    ,tile_90per = ...6
  )

result2 <- early_time_stats %>%
  left_join(
    df2
    ,by = "rn"
  ) %>%
  mutate(
    within_80per = case_when(
      fg3Per_all >= tile_10per & fg3Per_all <= tile_90per ~ 1
      ,TRUE ~ 0
    )
    ,within_50per = case_when(
      fg3Per_all >= tile_25per & fg3Per_all <= tile_75per ~ 1
      ,TRUE ~ 0
    )
  )

# write_csv(result2, "./02_output/train_test_result_player.csv")

# * appendix --------------------------------------------------------------

# The percentage which final 3P% stats between x% credible interval
result2  %>%
  summarise(cnt_within_80 = sum(within_80per), cnt_within_50 = sum(within_50per)) %>%
  mutate(rate_80 = cnt_within_80 / nrow(result2), rate_50 = cnt_within_50 / nrow(result2))



# credible interval by 3PA
pattern2_plot <- result2 %>%
  mutate(
    width_80perTile = tile_90per - tile_10per
    ,width_50perTile = tile_75per - tile_25per
  ) %>%
  pivot_longer(
    cols = c("width_80perTile", "width_50perTile")
    ,names_to = "interval_type"
    ,values_to = "interval_num"
  ) %>%
  ggplot(aes(x = fg3a_early, y = interval_num, color = interval_type))+
  geom_point()+
  scale_y_continuous(label = percent)+
  labs(y = "Interval_width", title = "Scatter plot of F3GA and credible interval by pattern2")+
  theme_classic()
pattern2_plot
# ggsave(plot = pattern2_plot, file = "./02_output/pattern2_plot_credibleIntervalWidth.png")

# Prior distribution pattern 3 (last season's 3P% by each player and past 2 season's 3P% standard deviation of entire League stats) ---------

# * execute stan ----------------------------------------------------------

df3 <- as_tibble()
for (i in 1:nrow(early_time_stats)) { 
  f3g_made <- early_time_stats %>%
    filter(rn == i) %>%
    select(fg3m_early) %>%
    as.numeric()
  f3g_miss <- early_time_stats %>%
    mutate(f3gmiss_early = fg3a_early - fg3m_early) %>%
    filter(rn == i) %>%
    select(f3gmiss_early) %>%
    as.numeric()
  
  Y <-  rep(c(1, 0), c(f3g_made, f3g_miss)) # 3PM
  sum(Y)
  N <-  f3g_made + f3g_miss #3PA
  data <-  list(Y = Y, N = N, prior_mean = f3gper_ls, prior_sd = prior_sd)
  
  fit <- stan(file='./03_code/train_test.stan', data=data, seed=seed)
  tmp <- rstan::extract(fit, permuted = FALSE)
  length(tmp[,,"p"])
  tmp_vec <- as.vector(tmp[,,"p"])
  test <- quantile(tmp_vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  df3[i,1] <- i
  df3[i,2] <- test[1]
  df3[i,3] <- test[2]
  df3[i,4] <- test[3]
  df3[i,5] <- test[4]
  df3[i,6] <- test[5]
}

df3 <- df3 %>%
  rename(
    rn = ...1
    ,tile_10per = ...2
    ,tile_25per = ...3
    ,tile_50per = ...4
    ,tile_75per = ...5
    ,tile_90per = ...6
  )

result3 <- early_time_stats %>%
  left_join(
    df3
    ,by = "rn"
  ) %>%
  mutate(
    within_80per = case_when(
      fg3Per_all >= tile_10per & fg3Per_all <= tile_90per ~ 1
      ,TRUE ~ 0
    )
    ,within_50per = case_when(
      fg3Per_all >= tile_25per & fg3Per_all <= tile_75per ~ 1
      ,TRUE ~ 0
    )
  )

# write_csv(result3, "./02_output/train_test_result_player_league.csv")

# * appendix --------------------------------------------------------------

# The percentage which final 3P% stats between x% credible interval
result3 %>%
  summarise(cnt_within_80 = sum(within_80per), cnt_within_50 = sum(within_50per))%>%
  mutate(rate_80 = cnt_within_80 / nrow(result3), rate_50 = cnt_within_50 / nrow(result3))


# credible interval by 3PA
pattern3_plot <- result3 %>%
  mutate(
    width_80perTile = tile_90per - tile_10per
    ,width_50perTile = tile_75per - tile_25per
  ) %>%
  pivot_longer(
    cols = c("width_80perTile", "width_50perTile")
    ,names_to = "interval_type"
    ,values_to = "interval_num"
  ) %>%
  ggplot(aes(x = fg3a_early, y = interval_num, color = interval_type))+
  geom_point()+
  scale_y_continuous(label = percent)+
  labs(y = "Interval_width", title = "Scatter plot of F3GA and credible interval by pattern3")+
  theme_classic()
pattern3_plot
# ggsave(plot = pattern3_plot, file = "./02_output/pattern3_plot_credibleIntervalWidth.png")


推測の幅の傾向出す
精度の判定の仕方と幅がどの程度なのかの2つの問題があること記載(前者はAUC-ROCとも違うんだよな)
予測の傾向に違いがあるかチェック(成功率が極端な選手を予測できてるか、平均に近い選手を予測できているか。前者に関しては前年のスタッチも併記して見れるようにしたい。グラフにするなら50%tileを予測できてるか。表で見るなら成功率順にソートして、それに対して信用区間に入ってるかを0,1で入れて色分けする)


近似で問題ないことのドキュメント探す
中心極限定理で正規分布に近似できて、それをnで割ることで比率の分布に変換可能。本当にn数打つとも限らないので、緩めにするために標準偏差に2をかける





選手ごとの昨シーズンの成功率使うときは、N数を昨シーズンのアテンプト数の半分にする
昨シーズンの3P%を母数とする
その上でアテンプト数によってシーズン通しての3P%は一定程度ぶれる
なので事前分布としては昨シーズンの3P%母数でn本打ったときの3P%の分布になる
でも今シーズン何本打ちそうかは分からないので、一番上の行の処理をする
(選手の個人差を盛り込みたいand事前分布でガチガチに絞る必要もなさそうなので、ラフな処理にする)