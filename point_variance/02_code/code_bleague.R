
# パッケージの読み込み --------------------------------------------------------------

library(tidyverse)
library(bleaguer)
library(lubridate)

# データの読み込み ----------------------------------------------------------------

source("../B_data.R")

summary(game)

#Join game to game_summary in order to set boxscore with exact date
raw <- game_summary %>%
  left_join(game, by = "ScheduleKey") %>%
  filter(EventId %in% c(2, 7)) # B1とB2のRSの試合

#Make game time(considering OT)
raw <- raw %>%
  mutate(
         game_time = case_when(
           OT4 > 0 ~ 60,
           OT3 > 0 ~ 55,
           OT2 > 0 ~ 50,
           OT1 > 0 ~ 45,
           TRUE ~ 40
         ) 
  )
# 2018-19シーズンまでにしよう


raw %>%
  ggplot(aes(x = Season, y = PTS))+
  geom_violin()+
  facet_wrap(~ EventId)

raw %>%
  ggplot(aes(x = Season, y = OR))+
  geom_violin()+
  facet_wrap(~ EventId)
