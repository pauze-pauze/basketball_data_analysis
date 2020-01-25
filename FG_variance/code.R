library(tidyverse)
library(bleaguer)
library(openxlsx)

team <- b.teams
event <- b.events
game <- b.games
game_summary <- b.games.summary
boxscore <- b.games.boxscore


# 2018-19のレギュラーシーズンの特定の選手のboxscoreのみ抽出 ------------------------------------------

#gameのschedulekeyとeventIDを利用してboxscoreのfilter
raw <- boxscore %>%
  left_join(game, key = "ScheduleKey") %>%
  filter(Season == "2018-19", EventId == 2)

#シーズンFGAの上位15選手のFGA、FGM
FGA_season <- raw %>%
  group_by(Player) %>%
  summarise(FGA_season = sum(FGA), FGM_season = sum(FGM)) %>%
  mutate(`FG%` = FGM_season/FGA_season) %>%
  arrange(desc(FGA_season)) %>%
  head(15)


#シーズン3FGAの上位15選手の3FGA、3FGM
F3GA_season <- raw %>%
  group_by(Player) %>%
  summarise(F3GA_season = sum(F3GA), F3GM_season = sum(F3GM)) %>%
  mutate(`F3G%` = F3GM_season/F3GA_season) %>%
  arrange(desc(F3GA_season)) %>%
  head(15) %>%
  write.xlsx("top15_3FGA.xlsx")

#FGA上位15選手のboxscore抽出
raw_FGA <- raw %>%
  inner_join(FGA_season, by = "Player")

#F3GM上位15選手のboxscore抽出
raw_F3GA <- raw %>%
  inner_join(F3GA_season, by = "Player")

# 可視化試すところ ----------------------------------------------------------------


#試合ごとのFGAの分布可視化
ggplot(aes(x = FGA), data = raw_FGA) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5)

##試合ごとのFGMの分布可視化
ggplot(aes(x = FGM), data = raw_FGA) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5)


#試合ごとの3FGAの分布可視化
ggplot(aes(x = F3GA), data = raw_F3GA) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5)

#試合ごとの3FGMの分布可視化
ggplot(aes(x = F3GM), data = raw_F3GA) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5)


# 出場時間によるfilterと40分換算の数値作成 ----------------------------------------------------
#出場時間によるfilterと40分換算の数値
raw_FGA_over20min <- raw_FGA %>%
  filter(MIN >= 20) %>%
  mutate(FGA_40min = FGA*40/MIN, FGM_40min = FGM*40/MIN, `FG%_perGame` = round(100*FGM/FGA, 2))

raw_F3GA_over20min <- raw_F3GA %>%
  filter(MIN >= 20) %>%
  mutate(F3GA_40min = F3GA*40/MIN, F3GM_40min = F3GM*40/MIN,  `F3G%_perGame` = round(100*F3GM/F3GA, 2))

#試合ごとのFGAの分布可視化
ggplot(aes(x = FGA), data = raw_FGA_over20min) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "試合ごとのFGA(20分以上出場の試合対象)",x = "FGA", y = "試合数") +
  ggsave(file = "FGA_over20min.png")

#試合ごとの3FGAの分布可視化
ggplot(aes(x = F3GA), data = raw_F3GA_over20min) +
  geom_bar(width = 0.8) +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "試合ごとの3FGA(20分以上出場の試合対象)",x = "3FGA", y = "試合数") +
  ggsave(file = "3FGA_over20min.png")


#40分換算のFGAで平均と分散作成
raw_FGA_over20min %>%
  group_by(Player) %>%
  summarise(season_FGA_mean = mean(FGA_40min), 
            season_FGA_std = sqrt(var(FGA_40min)), 
            season_FGA_std_adj = season_FGA_std*20/season_FGA_mean, #20は良さげな係数,
            season_FGM_mean = mean(FGM_40min), 
            season_FGM_std = sqrt(var(FGM_40min)), 
            season_FGM_std_adj = season_FGM_std*10/season_FGM_mean #10は良さげな係数
            ) %>% 
  mutate(season_FGA_std_rank = min_rank(season_FGA_std_adj),
         season_FGM_std_rank = min_rank(season_FGM_std_adj)) %>%
  arrange(season_FGA_std_adj) %>%
  write.xlsx("FGA_FGM_std.xlsx")

#40分換算の3FGAで平均と分散作成
raw_F3GA_over20min %>%
  group_by(Player) %>%
  summarise(season_F3GA_mean = mean(F3GA_40min), 
            season_F3GA_std = sqrt(var(F3GA_40min)), 
            season_F3GA_std_adj = season_F3GA_std*10/season_F3GA_mean,
            season_F3GM_mean = mean(F3GM_40min), 
            season_F3GM_std = sqrt(var(F3GM_40min)), 
            season_F3GM_std_adj = season_F3GM_std*3/season_F3GM_mean,
            `season_F3G%` = season_F3GM_mean/season_F3GA_mean) %>%
  mutate(season_F3GA_std_rank = min_rank(season_F3GA_std_adj),
         season_F3GM_std_rank = min_rank(season_F3GM_std_adj),
         `season_F3G%_rank` = min_rank(desc(`season_F3G%`))) %>%
  arrange(season_F3GA_std_adj) %>%
  write.xlsx("3FGA_3FGM_std.xlsx")


# 40分換算での分布可視化 ------------------------------------------------------------


#40分換算のFGA分布
raw_FGA_over20min %>%
  ggplot(aes(x = FGA_40min)) +
  geom_histogram(binwidth = 1, colour = "darkgreen", fill = "skyblue") +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "FGAの分布(試合ごとの出場時間を40分に変換)", x = "FGA_40分換算", y = "試合数") +
  theme_bw() +
  ggsave(file = "FGA_convert40min.png")

#40分換算でのFGM分布
raw_FGA_over20min %>%
  ggplot(aes(x = FGM_40min)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "green") +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "FGMの分布(試合ごとの出場時間を40分に変換)", x = "FGM_40分換算", y = "試合数")+
  theme_bw() +
  ggsave(file = "FGM_convert40min.png")

#40分換算での3FGA分布
raw_F3GA_over20min %>%
  ggplot(aes(x = F3GA_40min)) +
  geom_histogram(binwidth = 1, colour = "darkgreen", fill = "skyblue") +
  scale_y_continuous(breaks = seq(1, 15, 2), limits = c(0,13)) +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "3FGAの分布(試合ごとの出場時間を40分に変換)", x = "3FGA_40分換算", y = "試合数")+
  theme_bw() +
  ggsave(file = "3FGA_convert40min.png")

#40分換算での3FGM分布
raw_F3GA_over20min %>%
  ggplot(aes(x = F3GM_40min)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "green") +
  facet_wrap(~ Player, ncol = 5) +
  labs(title = "3FGMの分布(試合ごとの出場時間を40分に変換)", x = "3FGM_40分換算", y = "試合数")+
  theme_bw() +
  ggsave(file = "3FGM_convert40min.png")

# FGAとFG%で散布図作って、FGMのバラツキを検討 ----------------------------------------------
#FG
raw_FGA_over20min %>%
  ggplot(aes(x = FGA, y = `FG%_perGame`))+
  geom_point(colour = "skyblue")+
  facet_wrap(~Player, ncol = 5)+
  theme_bw()

#3FG
raw_F3GA_over20min %>%
  ggplot(aes(x = F3GA, y = `F3G%_perGame`))+
  geom_point(colour = "purple")+
  facet_wrap(~Player, ncol = 5)+
  theme_bw()

#FG
raw_FGA_over20min %>%
  ggplot(aes(x = FGA, y = FGM))+
  geom_point(colour = "skyblue")+
  facet_wrap(~Player, ncol = 5)+
  geom_abline(data = FGA_season ,aes(slope = `FG%`, intercept = 0), linetype = "dashed")+ #facetごとに独自のgeom_abline実装する作図
  theme_bw() +
  ggsave(file = "FGA_and_FGM_bunpu.png")


#3FG
raw_F3GA_over20min %>%
  ggplot(aes(x = F3GA, y = F3GM))+
  geom_point(colour = "purple")+
  facet_wrap(~Player, ncol = 5)+
  geom_abline(data = F3GA_season ,aes(slope = `F3G%`, intercept = 0), linetype = "dashed")+ #facetごとに独自のgeom_abline実装する作図
  theme_bw() +
  ggsave(file = "3FGA_and_3FGM_bunpu.png")

dMean <- mtcars %>%
  group_by(gear) %>%
  summarise(MN = mean(cyl))
ggplot(mtcars) +
  geom_point(aes(mpg, cyl)) +
  geom_hline(data = dMean, aes(yintercept = MN)) +
  facet_wrap(~ gear)
