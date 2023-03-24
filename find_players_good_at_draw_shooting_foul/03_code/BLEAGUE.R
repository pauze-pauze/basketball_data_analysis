
# import library ----------------------------------------------------------

pacman::p_load(tidyverse, bleaguer, scales, psych)

# read data ---------------------------------------------------------------

source("../B_data.R")


# create data frame to analyse --------------------------------------------

df <- boxscore %>%
  inner_join(
    game %>% filter(EventId == 2, Season == "2021-22") %>% select(ScheduleKey) # B1 2021-22 regular season stats
    ,by = "ScheduleKey"
  ) %>%
  group_by(Player) %>%
  summarise(
    MIN = sum(MIN)
    ,FGA = sum(FGA)
    ,FD = sum(FD) # personal foul drawn
    ,FTA = sum(FTA)
  )

cor(df %>% select(-Player))
pairs.panels(df %>% select(-Player))

df %>%
  pivot_longer(
    cols = c(MIN, FGA, FD)
  ) %>%
  mutate(
    name = case_when(
      name == "MIN" ~ "出場時間(分)"
      ,name == "FGA" ~ "シュートアテンプト"
      ,name == "FD" ~ "被ファウル数" 
      ,TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = value, y = FTA))+
  geom_point()+
  facet_wrap(~ name, scales = "free", ncol = 1)+
  labs(
    x = ""
    , y = "フリースローアテンプト"
    , title = "B1の2021-22レギュラーシーズンのフリースローアテンプトと各スタッツの散布図"
    , caption = "横軸の値は各グラフのタイトルの数値"
  )+
  theme_bw()+
  ggsave("./02_output/simple_scatter_plot_BLEAGUE.png", width = 6, height = 8)


df %>%
  pivot_longer(
    cols = c(MIN, FGA, FD)
  ) %>%
  mutate(
    name = case_when(
      name == "FD" ~ "PFD" 
      ,TRUE ~ name
    )
  ) %>%
  ggplot(aes(x = value, y = FTA))+
  geom_point()+
  facet_wrap(~ name, scales = "free", ncol = 1)+
  labs(
    x = ""
    , y = "FTA"
    , title = "B.LEAGUE 2021-22Regular Season's scatter plot of FTA and other stats"
    , caption = "x-axis means each gragh title's value"
  )+
  theme_bw()+
  ggsave("./02_output/simple_scatter_plot_BLEAGUE_in_English.png", width = 8, height = 10)

