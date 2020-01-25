library(tidyverse)
library(openxlsx)

men <- read_csv("worldcup_stats_men.csv", locale = locale(encoding = "cp932"))
women <- read_csv("worldcup_stats_women.csv", locale = locale(encoding = "cp932"))


# 合計のみ集計 ------------------------------------------------------------------

men_raw <- men %>%
  filter(TEAM == "Japan", 選手名 == "合計") %>%
  group_by("合計") %>%
  summarise(F3PM = sum(`3PM`),
            F3PA = sum(`3PA`),
            F2PM = sum(`2PM`),
            F2PA = sum(`2PA`),
            FTM = sum(FTM),
            FTA = sum(FTA),
            OREB = sum(OREB),
            DREB = sum(DREB),
            REB = sum(REB),
            TO = sum(TO),
            AS = sum(AS),
            ST = sum(ST),
            PTS = sum(PTS)) %>%
  mutate(FGA = F3PA + F2PA, men_or_women = "Men")

women_raw <- women %>%
  filter(TEAM == "Japan", 選手名 == "合計") %>%
  group_by("合計") %>%
  summarise(F3PM = sum(`3PM`),
            F3PA = sum(`3PA`),
            F2PM = sum(`2PM`),
            F2PA = sum(`2PA`),
            FTM = sum(FTM),
            FTA = sum(FTA),
            OREB = sum(OREB),
            DREB = sum(DREB),
            REB = sum(REB),
            TO = sum(TO),
            AS = sum(AS),
            ST = sum(ST),
            PTS = sum(PTS)) %>%
  mutate(FGA = F3PA + F2PA, men_or_women = "Women")

# FGA内の3PAの割合 -------------------------------------------------------------

men_raw<- men_raw %>%
  mutate(ratio_3PA = F3PA/FGA)

women_raw<- women_raw %>%
  mutate(ratio_3PA = F3PA/FGA)


# 得点の内訳 -------------------------------------------------------------------
men_raw <- men_raw %>%
  mutate(ratio_point_3PM = 3*F3PM/PTS,
         ratio_point_2PM = 2*F2PM/PTS,
         ratio_point_FTM = FTM/PTS)

women_raw <- women_raw %>%
  mutate(ratio_point_3PM = 3*F3PM/PTS,
         ratio_point_2PM = 2*F2PM/PTS,
         ratio_point_FTM = FTM/PTS)

forGragh <- men_raw %>%
  bind_rows(women_raw)


forGragh %>%
  select(men_or_women, ratio_point_3PM,ratio_point_2PM, ratio_point_FTM) %>%
  rename(`3Pによる得点の割合` = ratio_point_3PM, `2Pによる得点の割合` = ratio_point_2PM, `FTによる得点の割合` = ratio_point_FTM) %>%
  gather(key = type, value = ratio, -men_or_women) %>%
  ggplot(aes(x = as.factor(men_or_women), y = ratio, label = ratio)) +
  geom_bar(stat = "identity", aes(fill = type)) +
  geom_text(aes(label = paste(round(ratio*100,1),"%",sep = ""), group = type), position = position_stack(vjust = 0.5), color = "white", family = "serif") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "代表チーム", y = "割合", title = "総得点に占める3P、2P、FTそれぞれの得点の割合") +
  ggsave("PTS_ratio.png")

# OREB%とDREB%(ファイル書き出しからの手作業で対応) -------------------------------------------------------------------
#
men %>%
  filter(選手名 == "合計") %>%
  mutate(we_opp = if_else(TEAM == "Japan", "we", "opp")) %>%
  group_by(we_opp) %>%
  summarise(OREB = sum(OREB),
            DREB = sum(DREB)) %>%
  gather(key = key, value = value, -we_opp) %>%
  write.xlsx("men_REB.xlsx")

women %>%
  filter(選手名 == "合計") %>%
  mutate(we_opp = if_else(TEAM == "Japan", "we", "opp")) %>%
  group_by(we_opp) %>%
  summarise(OREB = sum(OREB),
            DREB = sum(DREB)) %>%
  gather(key = key, value = value, -we_opp) %>%
  write.xlsx("women_REB.xlsx")

# FGMに占めるASの割合 ------------------------------------------------------------

men_raw <- men_raw %>%
  mutate(ratio_AS_FGM = AS/(F3PM + F2PM))

women_raw <- women_raw %>%
  mutate(ratio_AS_FGM = AS/(F3PM + F2PM))


# EFG% --------------------------------------------------------------------

men_raw <- men_raw %>%
  mutate(`EFG%` = (F2PM + 1.5*F3PM)/FGA,
         `FG%` = (F2PM + F3PM)/FGA)

women_raw <- women_raw %>%
  mutate(`EFG%` = (F2PM + 1.5*F3PM)/FGA,
         `FG%` = (F2PM + F3PM)/FGA)


# FT% ---------------------------------------------------------------------
men_raw <-men_raw %>%
  mutate(`FT%` = FTM/FTA)

women_raw <-women_raw %>%
  mutate(`FT%` = FTM/FTA)


# 出力 ----------------------------------------------------------------------

men_raw %>%
  write.xlsx("men_stats.xlsx")

women_raw %>%
  write.xlsx("women_stats.xlsx")
