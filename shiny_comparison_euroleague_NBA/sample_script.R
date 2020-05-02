# sample script

data <- readRDS("S1_Data.rds")

library(tidyverse)
library(data.table)
library(lubridate)
# スタッツ比較 ------------------------------------------------------------------

data %>%
  filter(Season == "2015-16") %>% #シーズン選択
  group_by(League) %>%
  summarise(
    FGA = mean(Tot.P2A) + mean(Tot.P3A)
    ,FGM = mean(Tot.P2M) + mean(Tot.P3M)
    ,P2A = mean(Tot.P2A)
    ,P2M = mean(Tot.P2M)
    ,P3A = mean(Tot.P3A)
    ,P3M = mean(Tot.P3M)
    ,FTA = mean(Tot.FTA)
    ,FTM = mean(Tot.FTM)
    ,TR = mean(Tot.TRB)
    ,OR = mean(Tot.ORB)
    ,DR = mean(Tot.DRB)
    ,AS = mean(Tot.AST)
    ,TO = mean(Tot.TOV)
    ,BLK = mean(Tot.BLK)
  ) %>%
  pivot_longer( # NBAのデータのみ40分換算にするために転置
    -League
    ,names_to = "category"
    ,values_to = "stats"
  ) %>%
  mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
  pivot_wider( # %の数値を作成
    names_from = category
    ,values_from = stats
  )%>%
  mutate(
    FG_per = 100 * FGM / FGA
    ,P2_per = 100 * P2M / P2A
    ,P3_per = 100 * P3M / P3A
    ,FT_per = 100 * FTM / FTA
  ) %>%
  select( #流れ的に見やすいように並び順を入れ替え
    FGA, FGM, FG_per, P2A, P2M, P2_per, P3A, P3M, P3_per, FTA, FTM, FT_per, everything()
  ) %>%
  pivot_longer(
    -League
    ,names_to = "category"
    ,values_to = "stats"
  ) %>%
  pivot_wider(
    names_from = "League"
    ,values_from = "stats"
  )



# ヒストグラム作成 ----------------------------------------------------------------
#密度関数的なサムシング
data %>%
  filter(Season == "2015-16") %>% #シーズン選択
  pivot_longer(
    col = -c(1:7)
    ,names_to = "category"
    ,values_to = "stats"
    ) %>%
  mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
  pivot_wider(
    names_from = "category"
    ,values_from = "stats"
  ) %>%
  ggplot(aes(x = Tot.P2A, fill = League))+
  geom_histogram(stat = "density", position = "identity", alpha = 0.5)+ #bins調整したいけどできない。そういうもんか
  theme_bw()

#実数のやーつ
data %>%
  filter(Season == "2015-16") %>% #シーズン選択
  pivot_longer(
    col = -c(1:7)
    ,names_to = "category"
    ,values_to = "stats"
  ) %>%
  mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
  pivot_wider(
    names_from = "category"
    ,values_from = "stats"
  ) %>%
  ggplot(aes(x = Tot.P2A, fill = League))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 50)+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()


# 散布図作成 ---------------------------------------------------------------------
data %>%
  filter(Season == "2015-16") %>% #シーズン選択
  pivot_longer(
    col = -c(1:7)
    ,names_to = "category"
    ,values_to = "stats"
  ) %>%
  mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
  pivot_wider(
    names_from = "category"
    ,values_from = "stats"
  ) %>%
  ggplot(aes(x = Tot.P2A, y = Tot.P2M, color = League))+
  geom_point()+ 
  theme_bw()
# 面白い図にならない



# 時系列推移作成 ----------------------------------------------------------------
data %>%
#  mutate(Season = as.integer(str_sub(Season, start = 1, end = 4))) %>%
  group_by(League, Season) %>%
  summarise(
    FGA = mean(Tot.P2A) + mean(Tot.P3A)
    ,FGM = mean(Tot.P2M) + mean(Tot.P3M)
    ,P2A = mean(Tot.P2A)
    ,P2M = mean(Tot.P2M)
    ,P3A = mean(Tot.P3A)
    ,P3M = mean(Tot.P3M)
    ,FTA = mean(Tot.FTA)
    ,FTM = mean(Tot.FTM)
    ,TR = mean(Tot.TRB)
    ,OR = mean(Tot.ORB)
    ,DR = mean(Tot.DRB)
    ,AS = mean(Tot.AST)
    ,TO = mean(Tot.TOV)
    ,BLK = mean(Tot.BLK)
  ) %>%
  pivot_longer( # NBAのデータのみ40分換算にするために転置
    -c(League, Season)
    ,names_to = "category"
    ,values_to = "stats"
  ) %>%
  mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
  pivot_wider( # %の数値を作成
    names_from = category
    ,values_from = stats
  )%>%
  mutate(
    FG_per = 100 * FGM / FGA
    ,P2_per = 100 * P2M / P2A
    ,P3_per = 100 * P3M / P3A
    ,FT_per = 100 * FTM / FTA
  ) %>%
  select( #流れ的に見やすいように並び順を入れ替え
    FGA, FGM, FG_per, P2A, P2M, P2_per, P3A, P3M, P3_per, FTA, FTM, FT_per, everything()
  ) %>%
  ggplot(aes(x = Season, y = P3_per,group = League,  color = League))+
  geom_line(size = 1)+ 
  geom_point(size = 2)+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  ggsave(width = 7, height = 7)
