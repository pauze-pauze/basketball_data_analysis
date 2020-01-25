library(tidyverse)



# 数値データ準備 -----------------------------------------------------------------

data_byArea <- read_csv("../01_data/midrange_area.csv")  # エリアごとのシュート本数と成功数データ(過去の分析で利用したデータを再利用)

data_byArea <- data_byArea %>% #対象シーズンの絞り込み
  filter(
    season == "2018-2019"
    ,type == "Regular"
  )

midrange_percent <- sum(data_byArea$MID_RANGE_FGM)/sum(data_byArea$MID_RANGE_FGA)
print(midrange_percent)

F3GA_percent <- sum(data_byArea$C3_FGM,data_byArea$AB3_FGM)/sum(data_byArea$C3_FGA,data_byArea$AB3_FGA)
print(F3GA_percent)

data_clutch_3point <- read_csv("../01_data/clutch_3point.csv") #接戦時の3P%算出、及びその結果を元に、接戦時のmidrangeのFG%を仮定

F3GA_percent_clutch <- sum(data_clutch_3point$`3PM`)/sum(data_clutch_3point$`3PA`)
print(F3GA_percent_clutch)

midrange_percent_clutch <- midrange_percent*(F3GA_percent_clutch/F3GA_percent)
print(midrange_percent_clutch)

# シミュレーション1 ----------------------------------------------------------------

set.seed(1031) #乱数固定

N = 10000 #シミュレーション回数
freq = 5 #シュート本数

df_2point <- data.frame(sim_2point = rep(0, length = N)) #格納用のデータフレーム作成
x <- c(2, 0) #得点

for (i in 1:N){
  a <-  sum(sample(x, size = freq, replace = TRUE, prob = c(midrange_percent_clutch, 1 - midrange_percent_clutch))) #1シミュレーションごとの得点算出
  df_2point[i,] = a
  i = i + 1
}

df_3point <- data.frame(sim_3point = rep(0, length = N)) #格納用のデータフレーム作成
y <- c(3, 0) #得点

for (i in 1:N){
  a <-  sum(sample(y, size = freq, replace = TRUE, prob = c(F3GA_percent_clutch, 1 - F3GA_percent_clutch))) #1シミュレーションごとの得点算出
  df_3point[i,] = a
  i = i + 1
}

df <- df_2point %>%
  bind_cols(df_3point) %>%
  mutate(
    over3point_flag = if_else(
      sim_2point >= sim_3point
      ,1
      ,0
    )
  )
sum(df$over3point_flag)/nrow(df)


# シミュレーション2 ---------------------------------------------------------------


set.seed(1031) #乱数固定

N = 10000 #シミュレーション回数
freq = 5 #シュート本数

df_2point <- data.frame(sim_2point = rep(0, length = N)) #格納用のデータフレーム作成
x <- c(2, 0) #得点

for (i in 1:N){
  a <-  sum(sample(x, size = freq, replace = TRUE, prob = c(midrange_percent, 1 - midrange_percent))) #1シミュレーションごとの得点算出
  df_2point[i,] = a
  i = i + 1
}

df_3point <- data.frame(sim_3point = rep(0, length = N)) #格納用のデータフレーム作成
y <- c(3, 0) #得点

for (i in 1:N){
  a <-  sum(sample(y, size = freq, replace = TRUE, prob = c(F3GA_percent_clutch, 1 - F3GA_percent_clutch))) #1シミュレーションごとの得点算出
  df_3point[i,] = a
  i = i + 1
}

df <- df_2point %>%
  bind_cols(df_3point) %>%
  mutate(
    over3point_flag = if_else(
      sim_2point >= sim_3point
      ,1
      ,0
    )
  )
sum(df$over3point_flag)/nrow(df)


# シミュレーション3 ---------------------------------------------------------------

set.seed(1031) #乱数固定

N = 10000 #シミュレーション回数
freq = 90 #シュート本数

df_2point <- data.frame(sim_2point = rep(0, length = N)) #格納用のデータフレーム作成
x <- c(2, 0) #得点

for (i in 1:N){
  a <-  sum(sample(x, size = freq, replace = TRUE, prob = c(midrange_percent_clutch, 1 - midrange_percent_clutch))) #1シミュレーションごとの得点算出
  df_2point[i,] = a
  i = i + 1
}

df_3point <- data.frame(sim_3point = rep(0, length = N)) #格納用のデータフレーム作成
y <- c(3, 0) #得点

for (i in 1:N){
  a <-  sum(sample(y, size = freq, replace = TRUE, prob = c(F3GA_percent_clutch, 1 - F3GA_percent_clutch))) #1シミュレーションごとの得点算出
  df_3point[i,] = a
  i = i + 1
}

df <- df_2point %>%
  bind_cols(df_3point) %>%
  mutate(
    over3point_flag = if_else(
      sim_2point >= sim_3point
      ,1
      ,0
    )
  )
sum(df$over3point_flag)/nrow(df)
