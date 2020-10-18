
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)

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
# Distribution of distance

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

# Scatter of dist and pass
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
  geom_text_repel()+
  labs(x = "チームごとの平均走行距離(48分換算)", y = "チームごとの平均パス成功回数(48分換算)", title = "チームごとの平均走行距離と平均パス成功回数の散布図", caption = "レギュラーシーズンのデータを利用。また、500分以上出場の選手が対象")+
  theme_bw()
# 勝率で一部のチームを選択したり、ポイントの形変えたりで見やすくする。https://id.fnshr.info/2017/03/19/ggrepel/ など見よう