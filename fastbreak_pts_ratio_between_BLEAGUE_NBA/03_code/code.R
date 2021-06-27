
# import libraries --------------------------------------------------------

library(tidyverse)
library(scales)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)



# B.LEAGUE data -----------------------------------------------------------

# _load data ---------------------------------------------------------------

source("../B_data.R")

skim(game_summary)
skim(game)
skim(boxscore)
skim(event)
skim(team)


# _manipulate data ---------------------------------------------------------

# pick up season and division

division_param = 2 # B1のレギュラーシーズンに絞る


# make base data
data_base_bleague <- game_summary %>%
  left_join(
    select(game, c(ScheduleKey, Season, EventId))
    ,by = "ScheduleKey"
  ) %>%
  left_join(
    select(team, c(TeamId, Season, NameLong))
    ,by = c("TeamId", "Season")
  )%>%
  filter(
    EventId == division_param
  ) 

# summarise data
output_bleague <- data_base_bleague %>%
  group_by(
    Season
    ,NameLong
  ) %>%
  summarise(
    pts = sum(PTS)
    ,pts_fastbreak = sum(PtsFastBreak)
    ,fast_break_ratio = pts_fastbreak / pts
  ) %>%
  ungroup() %>%
  arrange(desc(fast_break_ratio))

write.csv(output_bleague, "./02_output/bleague_data.csv", fileEncoding = "CP932", row.names = FALSE)





# NBA_data ----------------------------------------------------------------


# _loda data --------------------------------------------------------------

library(nbastatR)

?teams_players_stats

t <- teams_players_stats(
  seasons = c(2017:2021)
  ,types = "team"
  ,tables = "general"
  ,measures = "Scoring"
  ,modes = "Totals"
)
tt <- unnest(t, cols = dataTable)

output_NBA <- tt %>%
  select(
    slugSeason
    ,nameTeam
    ,pctPTSasFB
  ) %>%
  mutate(
    fast_break_ratio = pctPTSasFB * 4 / 3 # 定義の違いを調整するための仮定
  ) %>%
  select(
    slugSeason
    ,nameTeam
    ,fast_break_ratio
  ) %>%
  rename(
    Season = slugSeason
    ,Team = nameTeam
  ) %>%
  mutate(
    league = "NBA"
  )


# ヒストグラムで分布の比較と順位のアウトプット ------------------------------------------------------------


hist_base <- output_bleague %>%
  select(
    Season
    ,NameLong
    ,fast_break_ratio
  ) %>%
  rename(
    Team = NameLong
  ) %>%
  mutate(
    league = "B.LEAGUE"
  ) %>%
  rbind(output_NBA)
  
hist_base %>%
  group_by(league) %>%
  summarise(cnt = n())
#In Japanese
p <- ggplot(data = hist_base, aes(x = fast_break_ratio))+
  geom_histogram(aes(color = league, fill = league), alpha = 0.5) +
  labs(x = "得点に占めるファストブレイクの割合"
       ,y = "該当チーム数"
       ,title = "B.LEAGUEとNBAのファストブレイク構成比の比較"
       ,subtitle = "  2016-17~2020-21のB1とNBAのレギュラーシーズンが対象"
       ,caption = "NBAとFIBAでのファストブレイクの定義の違いがあり、NBAの公式スタッツ記載のファストブレイク割合に4/3をかけて補正しています"
  ) +
  scale_x_continuous(labels = percent) +
  theme_classic()
plot(p)
ggsave(plot = p, filename = "./02_output/fast_break_raio_histgram.png", height = 4.5, width = 8, dpi = 100)
# In English
p_eng <- ggplot(data = hist_base, aes(x = fast_break_ratio))+
  geom_histogram(aes(color = league, fill = league), alpha = 0.5) +
  labs(x = "The ratio of fastbreak points in total points"
       ,y = "team count"
       ,title = "Compare ratio of fastbreak points in total point between NBA and B.LEAGUE"
       ,subtitle = "  2016-17~2020-21's regular season in B1 and NBA is subject to this gragh"
       ,caption = "The definition of fastbreak is diffrent between NBA and FIBA、I multiply fastbreak ratio of NBA and 4/3 to adjust."
  ) +
  scale_x_continuous(labels = percent) +
  theme_classic()
plot(p_eng)
ggsave(plot = p_eng, filename = "./02_output/fast_break_raio_histgram_eng.png", height = 4.5, width = 8, dpi = 100)


# 比率の降順のアウトプット
hist_base %>%
  arrange(
    desc(fast_break_ratio)
  ) %>%
  write.csv("./02_output/all_data.csv", fileEncoding = "CP932", row.names = FALSE)















