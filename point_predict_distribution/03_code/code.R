
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


# load data ---------------------------------------------------------------

source("../B_data.R")

data_base <- game_summary %>%
  inner_join(
    game %>%
      filter(Season == "2020-21", EventId == 2)
    , by = "ScheduleKey"
  ) %>%
  filter(OT1 == 0) #延長除外。予測値の分布考える際に邪魔になるので
# 本当はOTに得点のないチームもあるかもなのでそれ踏まえた処理したいけど面倒なのでこれにする


# 検証用data_base %>%
#   group_by(ScheduleKey) %>%
#   summarise(
#     cnt = n()
#   ) %>%
#   arrange(desc(cnt))



f3ga <- sum(data_base$F3GA)
f2ga <- sum(data_base$F2GA)
fta <- sum(data_base$FTA)
f3g_per <- sum(data_base$F3GM) / sum(data_base$F3GA)
f2g_per <- sum(data_base$F2GM) / sum(data_base$F2GA)
ft_per <- sum(data_base$FTM) / sum(data_base$FTA)
game_team_cnt <- nrow(data_base)
atmpt_num <- (f3ga + f2ga + fta) / game_team_cnt #40分間の平均アテンプト回数
#本当は延長考慮したいけど悩ましい。分析の対象データから延長を除外すること検討 or もしくはしっかり延長の時間含めて40分間の平均アテンプト出す実際の予測とのずれは延長として捉える。前者の方がいいかも
exp_num <- 500 #指数分布のnの値
pts_dist <- NULL #予測された得点の分布


for (i in 1:game_team_cnt){
  set.seed(i)
  n <- 0
  atmpt_pred <- 0 # 予想されるアテンプト回数の格納用
  pts_vec <- NULL
  atmpt_df <- as_tibble( # これも事前にnestしたdfを作っておいて、このfor文内で処理しないようにしておく。ついでにnest作るときに高速に作れるようにしたい
    rownames_to_column(
      as.data.frame(
        t(rmultinom(n = exp_num, size = 1, prob = c(f3ga, f2ga, fta)))
      )
    )
    ,"ID"
  ) %>%
    mutate(
      rowname = as.numeric(rowname)
      ,f3ga = V1
      ,f2ga = V2
      ,fta = V3
    ) %>%
    select(
      -c(V1, V2, V3)
    ) %>% # 後で縦持ちにして成功率や最終的な成功フラグといい感じのjoinしたほうがいいかも。しなくてもいいかも
    pivot_longer(
      -rowname
      ,names_to = "pattern"
      ,values_to = "flag"
    ) %>%
    filter(
      flag == 1
    )
  exp_vec <- rexp(n = exp_num, rate = atmpt_num)
  
  
  for (j in 1:exp_num){
    n <- n + exp_vec[j]
    atmpt_pred <- atmpt_pred + 1 # これbreakの後に置けば、後で1引く処理いらないのかも。試す
    # print(n)
    # print(atmpt_pred)
    if(n > 1) break
  }
  atmpt_pred <- atmpt_pred - 1
  # print(atmpt_pred)
  
  
  for (k in 1:atmpt_pred) { #ここmapとかapplyでベクトル処理できそうなので試したい
    atmpt_pts <- atmpt_df %>%
      filter(rowname == k) %>%
      mutate(
        pts = case_when(
          pattern == "f3ga" ~ 3
          ,pattern == "f2ga" ~ 2
          ,pattern == "fta" ~ 1
          ,TRUE ~ 0
        )
      ) %>%
      select(pts) %>%
      as.numeric()
    atmpt_per <- atmpt_df %>%
      filter(rowname == k) %>%
      mutate(
        per = case_when(
          pattern == "f3ga" ~ f3g_per
          ,pattern == "f2ga" ~ f2g_per
          ,pattern == "fta" ~ ft_per
          ,TRUE ~ 0
        )
      ) %>%
      select(per) %>%
      as.numeric()
    pts_vec <- append(pts_vec, atmpt_pts * rbinom(n = 1, size = 1, prob = atmpt_per))
  }
  pts <- sum(pts_vec)
  pts_dist <- append(pts_dist, pts)
}

pts_dist_tibble <- as_tibble(pts_dist) %>%
  mutate(
    type = "simulation"
  ) %>%
  rename(PTS = value)


data_base %>%
  select(PTS) %>%
  mutate(
    type = "actual"
  ) %>%
  bind_rows(pts_dist_tibble) %>%
  ggplot(aes(x = PTS, fill = type))+
  geom_histogram(position = "dodge", bins = 10)+
  theme_classic()






















k <- 1
atmpt_pts <- atmpt_df %>%
  filter(rowname == k) %>%
  mutate(
    pts = case_when(
      pattern == "f3ga" ~ 3
      ,pattern == "f2ga" ~ 2
      ,pattern == "fta" ~ 1
      ,TRUE ~ 0
    )
  ) %>%
  select(pts) %>%
  as.numeric()


5200/60


#下記値を最初から足し合わせて、1を超える1個前までのデータをアテンプト回数とする
sum(rexp(
  n = 86 # 最大のアテンプト回数。余裕を持った設定にする(1000)
  , rate = 86　# 40分間の平均のアテンプト回数。この
))

# パラメータの意味合い把握するやつ
set.seed(1)
sum(rexp(
  n = 86 # 最大のアテンプト回数。余裕を持った設定にする(1000)
  , rate = 86　# 40分間の平均のアテンプト回数。この
))
set.seed(1)
sum(rexp(
  n = 86 # 最大のアテンプト回数。余裕を持った設定にする(1000)
  , rate = 86 / 40　# 1分間の平均のアテンプト回数。この
)) / 40

f3ga <- 1382
f2ga <- 1500
fta <- 1449

as_tibble(rownames_to_column(data.frame(t(rmultinom(n = 100, size = 1, prob = c(f3ga, f2ga, fta))))), "ID")

as_tibble(
  rownames_to_column(
    as.data.frame(
      t(rmultinom(n = 100, size = 1, prob = c(f3ga, f2ga, fta)))
    )
  )
  ,"ID"
) %>%
  mutate(
    rowname = as.numeric(rowname)
    ,f3ga = V1
    ,f2ga = V2
    ,fta = V3
  ) %>%
  select(
    -c(V1, V2, V3)
  ) %>% # 後で縦持ちにして成功率や最終的な成功フラグといい感じのjoinしたほうがいいかも。しなくてもいいかも
  pivot_longer(
    -rowname
    ,names_to = "pattern"
    ,values_to = "flag"
  ) %>%
  filter(flag == 1)





rbinom(n = 1, size = 1, prob = f3g_per)



