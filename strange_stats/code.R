
# パッケージの読み込み --------------------------------------------------------------

library(tidyverse)
library(bleaguer)

# データの読み込み ----------------------------------------------------------------

source("../B_data.R")

summary(game)

#boxscoreに試合のカテゴリとシーズンを反映させる用のテーブル作成
game_cat_join <- game %>%
  select(
    ScheduleKey
    ,EventId
    ,Season
  )

# 出場時間の確認 -----------------------------------------------------------------


boxscore %>% #出場時間のしきい値を設定するための分布確認
  left_join(
    game_cat_join
    ,by = "ScheduleKey"
  ) %>%
  filter(
    EventId %in% c(2, 7) #B1とB2のレギュラーシーズンの試合に限定
    ,Season != "2019-20" #2019-20が途中までの数字しか入っていないので除外
  ) %>%
  group_by(
    PlayerId
  ) %>%
  summarise(
    min_all = sum(MIN)
  ) %>%
  ggplot(aes(x = min_all))+
  geom_histogram()
# 3シーズンの合計出場時間が1800分を超えている選手を対象にしよう

#選手と関連するデータの抽出用のテーブル作成
data <- boxscore %>%
  left_join(
    game_cat_join
    ,by = "ScheduleKey"
  ) %>%
  filter(
    EventId %in% c(2, 7) #B1とB2のレギュラーシーズンの試合に限定
    ,Season != "2019-20" #2019-20が途中までの数字しか入っていないので除外
  ) %>%
  group_by(
    PlayerId
    ,Player
  ) %>%
  mutate(
    min_all = sum(MIN) #選手ごとの総出場時間でfilterするための列作成
  ) %>%
  ungroup(
    
  ) %>%
  filter(
    min_all >= 1800
  )


# 数値出してく ------------------------------------------------------------------

# 該当選手の確認
data %>%
  distinct(
    Player
    ,min_all
  ) %>%
  view()


# └ファール数とSTL・BLK比較 --------------------------------------------------------

data %>%
  group_by(
    Player
  ) %>%
  summarise(
    F_all = sum(F)
    ,STL_all = sum(ST)
    ,BLK_all = sum(BS)
    ,min_all = mean(min_all)
  ) %>%
  filter(
#    STL_all + BLK_all > F_all
    STL_all > F_all
#    BLK_all > F_all
  ) %>%
  view()


# └2PAより3PAの方が多い選手 --------------------------------------------------------

data %>%
  group_by(
    Player
  ) %>%
  summarise(
    F2GA_all = sum(FGA) - sum(F3GA)
    ,F3GA_all = sum(F3GA)
  ) %>%
  filter(
    F2GA_all < F3GA_all
  )

# └2PMより3PMの方が多い選手 --------------------------------------------------------

data %>%
  group_by(
    Player
  ) %>%
  summarise(
    F2GM_all = sum(FGM) - sum(F3GM)
    ,F3GM_all = sum(F3GM)
  ) %>%
  filter(
    F2GM_all < F3GM_all
  )

# └3FG%が2FG%を超えている選手 ------------------------------------------------------

data %>%
  group_by(
    Player
  ) %>%
  summarise(
    F2GA_all = sum(FGA) - sum(F3GA)
    ,F3GA_all = sum(F3GA)
    ,F2GM_all = sum(FGM) - sum(F3GM)
    ,F3GM_all = sum(F3GM)
    ,F2Gper = (sum(FGM) - sum(F3GM)) / (sum(FGA) - sum(F3GA))
    ,F3Gper = sum(F3GM) / sum(F3GA)
    ,min_all = mean(min_all)
  ) %>%
  filter(
    F2Gper < F3Gper
  ) %>%
  view()


# └FGMよりASの方が多い選手 ----------------------------------------------------------

data %>%
  group_by(
    Player
  ) %>%
  summarise(
    FGM_all  = sum(FGM)
    ,AS_all = sum(AS)
  ) %>%
  filter(
    AS_all > FGM_all
  ) %>%
  view()


# DREBよりOREBが少ない選手 --------------------------------------------------------
data %>%
  group_by(
    Player
  ) %>%
  summarise(
    DREB_all  = sum(DR)
    ,OREB_all = sum(OR)
  ) %>%
  filter(
    OREB_all > DREB_all
  )

