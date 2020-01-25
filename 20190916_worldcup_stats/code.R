
# loading packege ---------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(openxlsx)
# loading data ------------------------------------------------------------

team_REB <- read_csv("stats_teams_REB.csv")
team_Shoot <- read_csv("stats_teams_Shoot.csv")
team_BLK_PF <- read_csv("stats_teams_BLK_PF.csv")
team_AS_TO <- read_csv("stats_teams_AS_TO.csv")
team_STL_TO <- read_csv("stats_teams_STL_TO.csv")
boxscore <- read_csv("worldcup_boxscore_raw.csv")
team_comparison <- read_csv("worldcup_team_comparison.csv")

#joining mulitple dataframe(summarised data)
raw <- list(team_Shoot, team_AS_TO, team_BLK_PF, team_REB, team_STL_TO) %>% 
  reduce(left_join, by = "Team")

toJoin <- boxscore %>%
  left_join(team_comparison, by = c("Timing", "Name_TeamA", "Name_TeamB"))

#including boxscore, fast break, and points in the paint data
raw_full <- toJoin %>%
  left_join(raw, by = c("Name_TeamA" = "Team")) %>%
  rename(Team = Name_TeamA)
  

# ratio of FGA and TO -----------------------------------------------------

raw %>%
  mutate("FGA/TO" = (`2PA` + `3PA`)/TO.x) %>%
  select("Team", "FGA/TO") %>%
  arrange(`FGA/TO`) %>%
  view()


# percentage of AST in FGM ------------------------------------------------

raw %>%
  mutate("AST/FGM" = round(AST/(`2PM` + `3PM`), 3)) %>%
  select("Team", "AST/FGM") %>%
  arrange(desc(`AST/FGM`)) %>%
  view() %>%
  write.xlsx("AST_per.xlsx")



# ratio of points among 2P, 3P, FT ----------------------------------------

desc_2P <- raw %>%
  mutate(ratio_2P = 100*`2PM`*2/PTS,
         ratio_3P = 100*`3PM`*3/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  arrange(ratio_2P) %>%　#2P構成比が多い順に並べ替え
  .$Team #ベクトル化？

raw %>%
  mutate(ratio_2P = 100*`2PM`*2/PTS,
         ratio_3P = 100*`3PM`*3/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  select(Team, ratio_2P, ratio_3P, ratio_FT) %>%
  gather(key = shoot, value = ratio, -Team) %>%
  ggplot(aes(x = Team, y = ratio, fill = shoot))+
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = desc_2P)+
  scale_fill_brewer(name = "シュートの種類", labels = c(ratio_2P = "2P", ratio_3P = "3P", ratio_FT = "FT"))+
  labs(x = "国名", y = "構成比(%)", title = "国別の2P,3P,FTの得点構成比")+
  coord_flip()+
  theme_bw()+
  ggsave("points_ratio_team.png", dpi = 300, width = 12, height = 10)


# ratio of points among "in the paint", other 2P, 3P, FT ------------------
#perimeterの割合でソート
perimeter_ascend <- raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            PitP = sum(PitP_TeamA),
            `2PM` = mean(`2PM`*2),
            `3PM` = mean(`3PM`*3),
            FTM = mean(FTM)) %>%
  mutate(ratio_PitP = 100*PitP/PTS,
         ratio_perimeter = 100*(`2PM`-PitP)/PTS,
         ratio_3P = 100*`3PM`/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  arrange(ratio_perimeter) %>%
  .$Team

#3Pの割合でソート
ascend_3P <- raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            PitP = sum(PitP_TeamA),
            `2PM` = mean(`2PM`*2),
            `3PM` = mean(`3PM`*3),
            FTM = mean(FTM)) %>%
  mutate(ratio_PitP = 100*PitP/PTS,
         ratio_perimeter = 100*(`2PM`-PitP)/PTS,
         ratio_3P = 100*`3PM`/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  arrange(ratio_3P) %>%
  .$Team

#PitPの割合でソート
raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            PitP = sum(PitP_TeamA),
            `2PM` = mean(`2PM`*2),
            `3PM` = mean(`3PM`*3),
            FTM = mean(FTM)) %>%
  mutate(ratio_PitP = 100*PitP/PTS,
         ratio_perimeter = 100*(`2PM`-PitP)/PTS,
         ratio_3P = 100*`3PM`/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  arrange(ratio_PitP) %>%
  .$Team

raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            PitP = sum(PitP_TeamA),
            `2PM` = mean(`2PM`*2),
            `3PM` = mean(`3PM`*3),
            FTM = mean(FTM)) %>%
  mutate(ratio_PitP = 100*PitP/PTS,
         ratio_perimeter = 100*(`2PM`-PitP)/PTS,
         ratio_3P = 100*`3PM`/PTS,
         ratio_FT = 100*FTM/PTS) %>%  
  select(Team, ratio_PitP, ratio_perimeter, ratio_3P, ratio_FT) %>%
  gather(key = shoot, value = ratio, -Team) %>%
  mutate(shoot = factor(shoot, levels = c("ratio_3P", "ratio_FT","ratio_PitP", "ratio_perimeter"))) %>% #積み上げ順を指定
  ggplot(aes(x = Team, y = ratio, fill = shoot))+
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = perimeter_ascend)+
  scale_fill_brewer(name = "シュートの種類", labels = c(ratio_PitP = "ペイントエリア内のシュート", ratio_perimeter = "ペリメーターのシュート", ratio_3P = "3P", ratio_FT = "FT"))+
  labs(x = "国名", y = "構成比(%)", title = "国別のペイントエリア、ペリメーター,3P,FTの得点構成比")+
  coord_flip()+
  theme_bw()+
  ggsave("points_ratio_team_addPitP.png", dpi = 300, width = 12, height = 10)


# per Game and ratio of fast break points ---------------------------------

#per game
order_FB_perGame <- raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            GP = mean(GP),
            FB = sum(FB_TeamA)) %>%
  mutate(FB_perGame = FB/GP,
         ratio_of_FB = 100*FB/PTS) %>%
  arrange(FB_perGame) %>%
  .$Team

raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            GP = mean(GP),
            FB = sum(FB_TeamA)) %>%
  mutate(FB_perGame = FB/GP,
         ratio_of_FB = 100*FB/PTS) %>%
  ggplot(aes(x = Team, y = FB_perGame))+
  geom_bar(stat = "identity", fill="lightblue")+
  scale_x_discrete(limits = order_FB_perGame)+
  labs(x = "国名", y = "ファストブレイクによる平均得点(per Game)", title = "国別のファストブレイクによる平均得点")+
  coord_flip()+
  theme_bw()+
  ggsave("FB_perGame.png")

#ratio
order_ratio_of_FB <- raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            GP = mean(GP),
            FB = sum(FB_TeamA)) %>%
  mutate(FB_perGame = FB/GP,
         ratio_of_FB = 100*FB/PTS) %>%
  arrange(ratio_of_FB) %>%
  .$Team

raw_full %>%
  group_by(Team) %>%
  summarise(PTS = mean(PTS),
            GP = mean(GP),
            FB = sum(FB_TeamA)) %>%
  mutate(FB_perGame = FB/GP,
         ratio_of_FB = 100*FB/PTS) %>%
  ggplot(aes(x = Team, y = ratio_of_FB))+
  geom_bar(stat = "identity", fill="green")+
  scale_x_discrete(limits = order_ratio_of_FB)+
  labs(x = "国名", y = "総得点に占めるファストブレイクの得点の割合(%)", title = "国別の総得点に占めるファストブレイクの得点の割合")+
  coord_flip()+
  theme_bw()+
  ggsave("ratio_of_FB.png")


# percentage of blocked shots in FGA --------------------------------------

order_ratio_of_blked <- raw_full %>%
  group_by(Team) %>%
  summarise(blked = sum(Blk_TeamB),
            FGA = sum(Fga_teamA)) %>%
  mutate(ratio_of_blked = 100*blked/FGA) %>%
  arrange(ratio_of_blked) %>%
  .$Team

raw_full %>%
  group_by(Team) %>%
  summarise(blked = sum(Blk_TeamB),
            FGA = sum(Fga_teamA)) %>%
  mutate(ratio_of_blked = 100*blked/FGA) %>%
  ggplot(aes(x = Team, y = ratio_of_blked))+
  geom_bar(stat = "identity", fill="green")+
  scale_x_discrete(limits = order_ratio_of_blked)+
  labs(x = "国名", y = "FGAに占める被ブロック数の割合(%)", title = "国別のFGAに占める被ブロック数の割合")+
  coord_flip()+
  theme_bw()+
  ggsave("ratio_of_blked.png")
  


# OREB%, DREB% ------------------------------------------------------------

raw_full %>%
  group_by(Team) %>%
  summarise(DREB_us = sum(Dreb_TeamA),
            OREB_us = sum(Oreb_TeamA),
            DREB_opp = sum(Dreb_TeamB),
            OREB_opp = sum(Oreb_TeamB)) %>%
  mutate(DREB_per = 100*DREB_us/(DREB_us + OREB_opp),
         OREB_per = 100*OREB_us/(DREB_opp + OREB_us)) %>%
  ggplot(aes(x = DREB_per, y = OREB_per, label = Team))+
  geom_point()+
  geom_label_repel()+
  labs(x = "DREB%", y = "OREB%", title = "国別のDREB%及びOREB%")+
  theme_bw()+
  ggsave("relation_DREBper_OREBper.png")


# difference of eFG%, 3FG%, FB which japan's oppnent ----------------------
japan_opp <- factor(c("Turkey", "Czech Republic", "USA", "New Zealand", "Montenegro"), levels = c("Turkey", "Czech Republic", "USA", "New Zealand", "Montenegro"))

raw_full %>%
  mutate(japan_or_not = if_else(Name_TeamB == "Japan", "Japan", "Other")) %>%
  group_by(Team,japan_or_not) %>%
  summarise(F2GA = sum(F2ga_teamA),
            F2GM = sum(F2gm_TeamA),
            F3GA = sum(F3ga_teamA),
            F3GM = sum(F3gm_TeamA),
            FB = sum(FB_TeamA),
            GP = n()) %>%
  filter(Team  %in% japan_opp) %>%
  mutate(`eFG%` = (F2GM + 1.5*F3GM)/(F2GA + F3GA),
         FB_perGame = FB/GP,
         `3FG%` = F3GM/F3GA,
         `3FGA` = F3GA/GP) %>%
  write.xlsx("japan_opp_Stats.xlsx")

raw_full %>%
  mutate(japan_or_not = if_else(Name_TeamB == "Japan", "vs 日本", "vs 他国")) %>%
  group_by(Team,japan_or_not) %>%
  summarise(F2GA = sum(F2ga_teamA),
            F2GM = sum(F2gm_TeamA),
            F3GA = sum(F3ga_teamA),
            F3GM = sum(F3gm_TeamA),
            FB = sum(FB_TeamA),
            PitP = sum(PitP_TeamA),
            PTS = sum(Pts_TeamA),
            FTM = sum(Ftm_TeamA),
            GP = n()) %>%
  ungroup() %>%
  filter(Team  %in% japan_opp) %>%
  mutate(ratio_PitP = 100*PitP/PTS,
         ratio_perimeter = 100*(F2GM*2-PitP)/PTS,
         ratio_3P = 100*F3GM*3/PTS,
         ratio_FT = 100*FTM/PTS) %>%
  select(Team, ratio_PitP, ratio_perimeter, ratio_3P, ratio_FT, japan_or_not) %>%
  gather(key = shoot, value = ratio, -Team, -japan_or_not) %>%
  mutate(Team = factor(Team, levels = c("Turkey", "Czech Republic", "USA", "New Zealand", "Montenegro"))) %>%
  mutate(shoot = factor(shoot, levels = c("ratio_3P", "ratio_FT", "ratio_perimeter","ratio_PitP"))) %>% #積み上げ順を指定
  ggplot(aes(x = japan_or_not, y = ratio, fill = shoot))+
  geom_bar(stat = "identity")+
  #  scale_x_discrete(limits = perimeter_ascend)+
  scale_fill_brewer(name = "シュートの種類", labels = c(ratio_PitP = "ペイントエリア内のシュート", ratio_perimeter = "ペリメーターのシュート", ratio_3P = "3P", ratio_FT = "FT"))+
  labs(x = "国名", y = "構成比(%)", title = "対戦相手別のペイントエリア、ペリメーター,3P,FTの得点構成比")+
  coord_flip()+
  facet_wrap(~ Team, ncol = 1)+
  theme_bw()+
  ggsave("japan_opp_ratio_of_pts.png")
