library(nbastatR)
library(tidyverse)


t <- game_logs(
  seasons = c(1995:2021),
  league = "NBA",
  result_types = "team",
  season_types = "Playoffs"
)

tt <- t %>%
  group_by(yearSeason) %>%
  summarise(
    game_cnt = n() / 2
  ) %>%
  ungroup() %>%
  ggplot(aes(x = yearSeason, y = game_cnt)) +
  geom_line() +
  geom_point(size = 1) +
  labs(x = "シーズン", y = "総試合数", title = "NBAプレイオフの試合数推移") +
  theme_classic()

ggsave(tt, file = "NBA_playoff_game_cnt_trend_Japanese.png", width = 6.4, height = 4.8)

ttt <- t %>%
  group_by(yearSeason) %>%
  summarise(
    game_cnt = n() / 2
  ) %>%
  ungroup() %>%
  ggplot(aes(x = yearSeason, y = game_cnt)) +
  geom_line() +
  geom_point(size = 1) +
  labs(x = "Season", y = "game_count", title = "NBA Playoff game count trend") +
  theme_classic()

ggsave(ttt, file = "NBA_playoff_game_cnt_trend_English.png", width = 6.4, height = 4.8)

  
  