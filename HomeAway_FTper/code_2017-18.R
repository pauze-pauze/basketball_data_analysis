library(bleaguer)
library(tidyverse)
library(openxlsx)
B_team <- b.teams
B_player <- b.games.boxscore
B_team_stat <- b.games.summary
B_schedule <- b.events
B_games <- b.games %>%
  mutate_if(is.factor, funs(as.character))

# データ結合_B1 -------------------------------------------------------------------

#2018-19のB1RSのみ抽出
B_games_2017 <- B_games %>%
  filter(Season == "2017-18", EventId == 2)

#playerデータと結合
raw <- B_player %>%
  left_join(B_games_2017, by = "ScheduleKey") %>%
  filter(is.na(Season) == FALSE) %>% #異なる時期の選手スタッツ削除
  mutate(Home_Away = if_else(TeamId == HomeTeamId, "Home", "Away")) %>% #home,awayどちらのスタッツかのフラグ
  left_join(B_team, by = c("Season", "TeamId")) #チーム名結合

# データ作成_B1_home_away -------------------------------------------------------------------

#teamのhome_awayのFT%比較
team_home_away <- raw %>%
  group_by(NameShort, Home_Away) %>%
  summarise(FTA = sum(FTA), FTM = sum(FTM), "FT%" = round(FTM/FTA, 4)) %>%
  select(NameShort, `FT%`, Home_Away) %>% #spread時に邪魔になる列を削除
  spread(key = Home_Away, value ="FT%") %>%
  mutate(diff = Away - Home) %>% #homeに比べてawayのFT%がどれだけ低いかの数値
  select(NameShort, Home, everything())  %>% #列の並べ替え
  rename(チーム = NameShort ,"FT%の差" = diff) %>%
  write.xlsx("team_home_away_2017-18.xlsx")

#playerのhome_awayのFT%比較
player_home_away <- raw %>%
  group_by(Player, Home_Away) %>%
  summarise(FTA = sum(FTA), FTM = sum(FTM), "FT%" = round(FTM/FTA, 4)) %>%
  spread(key = Home_Away, value = FTA) %>% #home_awayでのFT本数の形を変える
  mutate_all(funs(ifelse(is.na(.),0,.)))%>% #NAを0に置換
  filter(Home >= 80 | Away >= 80) %>% #homeかawayでFTA50本以上
  gather(key = Home_Away, value = FTA, -Player, -FTM, - 'FT%') %>% 
  filter(FTA != 0)  %>%
  group_by(Player) %>% #重複行抽出
  filter(n() > 1) %>% #重複行抽出
  select(Player, `FT%`, Home_Away) %>% #spread時に邪魔になる列を削除
  spread(key = Home_Away, value ="FT%") %>%
  mutate(diff = Away - Home) %>% #homeに比べてawayのFT%がどれだけ低いかの数値
  select(Player, Home, everything()) %>% #列の並べ替え
  rename("FT%の差" = diff) %>%
  write.xlsx("player_home_away_2017-18.xlsx")

# データ作成_B1_arena ----------------------------------------------------------

#arenaごとのawayチームFT%
arena_FTper_B1 <- raw %>%
  filter(Home_Away == "Away") %>%
  group_by()

#選手ごとのシーズンFT%
player_FTper_B1 <- raw %>%
  group_by(Player) %>%
  summarise(FTM = sum(FTM), FTA = sum(FTA)) %>%
  mutate(`FT%` = FTM/FTA)

#選手のawayアリーナごとの総FTA
player_FTA_arena_B1 <- raw %>%
  filter(Home_Away == "Away") %>% #away時のFTAのみ抽出するため
  group_by(Player, Arena) %>%
  summarise(FTA_player = sum(FTA))

#アリーナごとのawayチーム総FTA
arena_FTA_B1 <- raw %>%
  filter(Home_Away == "Away") %>% #away時のFTAのみ抽出するため
  group_by(Arena) %>%
  summarise(FTA_arena = sum(FTA), FTM_arena = sum(FTM), `FT%_arena` = FTM_arena/FTA_arena)

#アリーナのawayチーム総FTAに占める選手のFTAの割合
player_FTA_ratio_arena_B1 <- player_FTA_arena_B1 %>%
  left_join(arena_FTA_B1, by = "Arena") %>%
  mutate(FTA_ratio = FTA_player/FTA_arena)

#想定FT%算出と実際のFT%と都合
SIM_real_FTper_B1 <- player_FTA_ratio_arena_B1 %>%
  left_join(player_FTper_B1, by = "Player")%>%
  filter(is.nan(`FT%`) == FALSE) %>% #FTA0本でNaNのFT%がNaNになる選手を除外
  mutate(`FT%_sim` = FTA_ratio*`FT%`)%>%
  group_by(Arena) %>%
  summarise(`FT%_sim` = sum(`FT%_sim`)) %>%
  left_join(arena_FTA_B1, by = "Arena") %>%
  mutate(diff = `FT%_sim` - `FT%_arena`)

#arenaとチーム名結合_トッケイは無視の方向で
arena_team <- raw %>%
  filter(Home_Away == "Home") %>%
  group_by(Arena, NameShort) %>%
  summarise(n = n())

arena_team %>%
  left_join(SIM_real_FTper_B1, by = "Arena") %>%
  write.xlsx("arena_FTper_2017-18.xlsx")

