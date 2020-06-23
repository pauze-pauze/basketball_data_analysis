
# 必要なパッケージ読み込み ------------------------------------------------------------

library(tidyverse)
library(ballr)




# 利用するデータの作成 --------------------------------------------------------------

#データ取得_36分換算データ
base <- NBAPerGameStatisticsPer36Min(season = 2010) %>%
  mutate(season = 2010)
for (i in 2011:2020){
  bind <- NBAPerGameStatisticsPer36Min(season = i) %>%
    mutate(season = i)
  base <- base %>%
    bind_rows(bind)
}

#データ取得_advanced_statistics
base2 <- NBAPerGameAdvStatistics(season = 2010) %>%
  mutate(season = 2010)

for (i in 2011:2020){
  bind <- NBAPerGameAdvStatistics(season = i) %>%
    mutate(season = i)
  base2 <- base2 %>%
    bind_rows(bind)
}
#同一シーズンに複数チームでプレーした選手のスタッツをTOTだけにするフラグ作成
tot_flag <- base %>%
  group_by(
    player
    ,season
  ) %>%
  summarise(
    tot_flag = case_when(
      n() >= 2 ~ 1 #同一シーズンで複数チームに所属しているフラグ
      ,TRUE ~ 0
    )
  )

data <- base %>%
  inner_join(
    base2
    ,by = c(
      "player"
      ,"link"
      ,"season"
      ,"pos"
      ,"age"
      ,"tm"
      ,"g"
      ,"mp"
    )
  ) %>%
  left_join(
    tot_flag
    ,by = c(
      "player"
      ,"season"
    )
  ) %>%
  filter(
    xor(tm != "TOT", tot_flag == 1) # 同一シーズンで複数チームに所属していた選手をシーズン全体のスタッツだけ残す
  )
