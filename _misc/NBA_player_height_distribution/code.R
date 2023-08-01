pacman::p_load(tidyverse, hoopR)

t <- nba_leaguedashplayerbiostats(league_id = '00', season = year_to_season(most_recent_nba_season() - 1))
tt <- t$LeagueDashPlayerBioStats %>%
  select(PLAYER_NAME, PLAYER_HEIGHT, PLAYER_HEIGHT_INCHES, PLAYER_WEIGHT)
p <- tt %>%
  mutate(
    transform_height_1 = str_sub(PLAYER_HEIGHT, end = 1)
    ,transform_height_2 = str_sub(PLAYER_HEIGHT, start = 3, end = 4)
    ,transform_height_2 = if_else(
      transform_height_2 %in% c("10", "11", "12")
      ,transform_height_2
      ,str_pad(transform_height_2, 2, side = "left", pad = "0")
    )
    ,player_height = str_c(transform_height_1, transform_height_2, sep = "-")
    ,flag = case_when(
      player_height %in% c("5-11", "6-00","6-01", "6-11", "7-00", "7-01") ~ "1"
      ,TRUE ~ "0"
    )
  ) %>%
  group_by(player_height, flag) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = player_height, y = cnt, fill = as.factor(flag)))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c( "grey","black"))+
  labs(
    x = "Player Height"
    ,y = "count"
    ,title = "Distribution of Player Heights"
    ,subtitle = "2022-23 season data"
  )+
  theme_classic()+
  theme(legend.position = 'none')
p
ggsave(plot = p, file = "NBA_player_height_distribution.png")
