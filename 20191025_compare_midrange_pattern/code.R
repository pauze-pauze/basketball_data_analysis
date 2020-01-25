library(tidyverse)
library(openxlsx)

area <- read_csv("midrange_area.csv")
clutch <- read_csv("midrange_clutch_portion.csv")


# comparison_clutch_normal ------------------------------------------------

#mean of %PTS_2PT_MR

clutch %>% #FTの増加分調整しないやつ
  group_by(season, type) %>%
  summarise(mean_MR = mean(`%PTS_2PT_MR`), 
            mean_2PT = mean(`%PTS_2PT`),
            mean_3PT = mean(`%PTS_3PT`),
            mean_FT = mean(`%PTS_FT`)
            ) %>%
  write.xlsx("clutch_FT_notAdjusted.xlsx")

clutch %>% #FTの増加分調整するやつ
  group_by(season, type) %>%
  summarise(mean_MR = mean(`%PTS_2PT_MR`*100/(100-`%PTS_FT`)), 
            mean_2PT = mean(`%PTS_2PT`*100/(100-`%PTS_FT`)),
            mean_3PT = mean(`%PTS_3PT`*100/(100-`%PTS_FT`)),
            mean_FT = mean(`%PTS_FT`)) %>%
  write.xlsx("clutch_FT_Adjusted.xlsx")


#シーズンごとの%PTS_2PT_MRの分布のヒストグラム
clutch %>% #FTの増加分調整するやつ
  mutate(mean_MR = `%PTS_2PT_MR`*100/(100-`%PTS_FT`)) %>%
  ggplot(aes(x = mean_MR, fill = type))+
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7)+
  scale_y_continuous(breaks = seq(0, 9, 1))+
  scale_fill_hue(name = "type", labels = c(all = "試合全体", clutch = "クラッチ"))+
  facet_wrap(~ season)+
  labs(x = "総得点に占めるmidrangeの得点の割合(%)", y = "該当チーム数", title = "試合全体とクラッチ時のmidrangeの得点割合")+
  theme_bw()+
  ggsave("perPTS_2PT_MR_clutch.png")


# change of shotarea in time series ---------------------------------------

#season
area %>%
  group_by(season) %>%
  summarise(`midrange_FGA%` = sum(MID_RANGE_FGA)/sum(RA_FGA + NON_RA_FGA + MID_RANGE_FGA + C3_FGA + AB3_FGA),
            `midrange_FG%` = sum(MID_RANGE_FGM)/sum(MID_RANGE_FGA))

#season and type
area %>%
  group_by(season,type) %>%
  summarise(`midrange_FGA%` = sum(MID_RANGE_FGA)/sum(RA_FGA + NON_RA_FGA + MID_RANGE_FGA + C3_FGA + AB3_FGA),
            `midrange_FG%` = sum(MID_RANGE_FGM)/sum(MID_RANGE_FGA))

#playoffに出場したチームのみ
area %>%
  left_join(area, by = "TEAM") %>%
  filter(type.y == "Playoff",
         season.x == season.y) %>%
  group_by(season.x, type.x) %>%
  summarise(`midrange_FGA%` = sum(MID_RANGE_FGA.x)/sum(RA_FGA.x + NON_RA_FGA.x + MID_RANGE_FGA.x + C3_FGA.x + AB3_FGA.x),
            `midrange_FG%` = sum(MID_RANGE_FGM.x)/sum(MID_RANGE_FGA.x)) %>%
  rename(シーズン = season.x, タイプ = type.x, FGAに占めるmidrange試投数の割合 = `midrange_FGA%`, `midrangeのFG%` = `midrange_FG%`) %>%
  write.xlsx("shotarea_midrange_season_type_onlyplayoffteam.xlsx")

