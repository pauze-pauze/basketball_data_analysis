
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)

# load data ---------------------------------------------------------------

dist_team <- read_csv("./01_data/distance_data_team.csv") # load each team's distance data
dist_player <- read_csv("./01_data/distance_data_player.csv") # load each player's distance data
pass_team <- read_csv("./01_data/pass_data_team.csv") # load each team's pass data


# check data --------------------------------------------------------------

skim(dist_team)
skim(dist_player)
skim(pass_team)

# manipulate data
dist_player$DIST_FEET <- as.numeric(dist_player$DIST_FEET)

# check the number of teams
dist_team %>%
  group_by(
    season
    ,season_type
  ) %>%
  summarise(
    cnt = n()
  )

pass_team %>%
  group_by(
    season
    ,season_type
  ) %>%
  summarise(
    cnt = n()
  )


# EDA ---------------------------------------------------------------------
###
#MINは選手5人分の合計であることに注意。単純にMINで割ると選手一人あたりになるので、5を掛ける必要あり
###

# └Distribution of distance -----------------------------------------------

dist_team %>%
  filter(
    season_type == "regular"
  ) %>%
  mutate(
    dist_feet_per_48min = 5 * 48 * DIST_FEET / MIN
  ) %>%
  ggplot(aes(x = dist_feet_per_48min))+
  geom_histogram()+
  labs(x = "チームごとの平均走行距離(48分換算)", y = "該当チーム数", title = "チームごとの平均走行距離のシーズン別分布", caption = "レギュラーシーズンのデータを利用")+
  facet_wrap(~season, ncol = 1)+
  theme_bw()
  #ggsave(file = "./02_output/02_img/distribution_of_dist_by_team.png", dpi = 100, width = 12.8, height = 7.2)

dist_player %>%
  filter(
    season_type == "regular"
    ,MIN >= 500
  ) %>%
  mutate(
    dist_feet_per_48min = 48 * DIST_FEET / MIN
  ) %>%
  ggplot(aes(x = dist_feet_per_48min))+
  geom_histogram()+
  labs(x = "選手ごとの平均走行距離(48分換算)", y = "該当選手数", title = "チームごとの平均走行距離のシーズン別分布", caption = "レギュラーシーズンのデータを利用。また、500分以上出場の選手が対象")+
  facet_wrap(~season, ncol = 1)+
  theme_bw()


# └Scatter of dist and pass -----------------------------------------------

dist_team %>%
  left_join(
    pass_team
    ,by = c("TEAM", "season", "season_type")
  ) %>%
  filter(
    season_type == "regular"
  ) %>%
  mutate(
    dist_feet_per_48min = 5 * 48 * DIST_FEET / MIN.x
    ,pass_made_per_48min = 5 * 48 * PASSES_MADE / MIN.x
    ,team_season = str_c(TEAM, season, sep = "_")
  ) %>%
  ggplot(aes(x = dist_feet_per_48min, y = pass_made_per_48min, label = team_season))+
  geom_point()+
  #geom_text_repel()+
  labs(x = "チームごとの平均走行距離(48分換算)", y = "チームごとの平均パス成功回数(48分換算)", title = "チームごとの平均走行距離と平均パス成功回数の散布図", caption = "レギュラーシーズンのデータを利用。また、500分以上出場の選手が対象")+
  theme_bw()
# 勝率で一部のチームを選択したり、ポイントの形変えたりで見やすくする。https://id.fnshr.info/2017/03/19/ggrepel/ など見よう


# └distribtuion of distance by team ---------------------------------------

p1 <- dist_team %>%
  filter(
    season_type == "regular"
    ,season == "2016-17"
  ) %>%
  mutate(
    dist_miles_per_48min = 5 * 48 * DIST_MILES / MIN
  ) %>%
  ggplot(aes(x = reorder(TEAM, dist_miles_per_48min), y = dist_miles_per_48min))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "チームごとの平均走行距離(48分換算)", x = "チーム名", title = "2016-17シーズンのチームごと平均走行距離")+
  theme_bw()
p2 <- dist_team %>%
  filter(
    season_type == "regular"
    ,season == "2017-18"
  ) %>%
  mutate(
    dist_miles_per_48min = 5 * 48 * DIST_MILES / MIN
  ) %>%
  ggplot(aes(x = reorder(TEAM, dist_miles_per_48min), y = dist_miles_per_48min))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "チームごとの平均走行距離(48分換算)", x = "チーム名", title = "2017-18シーズンのチームごと平均走行距離")+
  theme_bw()
p3 <- dist_team %>%
  filter(
    season_type == "regular"
    ,season == "2018-19"
  ) %>%
  mutate(
    dist_miles_per_48min = 5 * 48 * DIST_MILES / MIN
  ) %>%
  ggplot(aes(x = reorder(TEAM, dist_miles_per_48min), y = dist_miles_per_48min))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "チームごとの平均走行距離(48分換算)", x = "チーム名", title = "2018-19シーズンのチームごと平均走行距離")+
  theme_bw()
p4 <- dist_team %>%
  filter(
    season_type == "regular"
    ,season == "2019-20"
  ) %>%
  mutate(
    dist_miles_per_48min = 5 * 48 * DIST_MILES / MIN
  ) %>%
  ggplot(aes(x = reorder(TEAM, dist_miles_per_48min), y = dist_miles_per_48min))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "チームごとの平均走行距離(48分換算)", x = "チーム名", title = "2019-20シーズンのチームごと平均走行距離")+
  theme_bw()
p1 + p2 + p3 + p4 + labs(caption = "レギュラーシーズンのデータ。走行距離の単位はマイル") +
  #ggsave(file = "./02_output/02_img/dist_by_team_season.png", dpi = 100, width = 12.8, height = 7.2)

# これで出せるデータも面白いけど情報過多になるのでやめる
{# p1 <- dist_team %>%
#   filter(
#     season_type == "regular"
#     ,season == "2016-17"
#   ) %>%
#   mutate(
#     dist_miles_per_48min_off = 5 * 48 * DIST_MILES_OFF / MIN
#     ,dist_miles_per_48min_def = 5 * 48 * DIST_MILES_DEF / MIN
#     ,dist_miles_per_48min = 5 * 48 * DIST_MILES / MIN
#   ) %>%
#   select(
#     TEAM
#     ,dist_miles_per_48min_off
#     ,dist_miles_per_48min_def
#   ) %>%
#   pivot_longer(
#     col = -TEAM
#     ,names_to = "off_def"
#     ,values_to = "dist"
#   ) %>%
#   ggplot(aes(x = reorder(TEAM, dist), y = dist, fill = off_def))+
#   geom_col()+
#   coord_flip()+
#   labs(y = "チームごとの平均走行距離(48分換算)", x = "チーム名", title = "2016-17シーズンのチームごと平均走行距離")+
#   theme_bw()
# p1
}


# └ off and def dist by team ----------------------------------------------

dist_team %>%
  filter(
    season_type == "regular"
  ) %>%
  mutate(
    dist_miles_off_per_48min = 5 * 48 * DIST_MILES_OFF / MIN
    ,dist_miles_def_per_48min = 5 * 48 * DIST_MILES_DEF / MIN
  ) %>%
  ggplot(aes(x = dist_miles_off_per_48min, y = dist_miles_def_per_48min, shape = season))+
  geom_point()+
  theme_bw()
# シーズンごとの分布にしたほうが良いかも。全部まとめると層別と違う傾向が出てしまう