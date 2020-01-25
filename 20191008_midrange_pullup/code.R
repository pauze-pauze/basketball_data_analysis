#ベースのFGAなどはpullupのデータであることに注意


# load package and data ---------------------------------------------------

library(openxlsx)
library(tidyverse)

pullup <- read_csv("player_pullup.csv")
shotarea <- read_csv("player_shotarea.csv")
clock_late <- read_csv(("shotclock_late.csv"))
clock_verylate <- read_csv("shotclock_verylate.csv")
boxscore <- read_csv("team_boxscore.csv")

boarder_pullup_2PA = 170 #minimum pullup 2PA

# joining player data -----------------------------------------------------


player_raw <- pullup %>%
  inner_join(shotarea, by = ("PLAYER")) %>%
  mutate(`3PM` = as.numeric(`3PM`),
         `3PA` = as.numeric(`3PA`),
         `3P%` = `3PM`/`3PA`) %>%
  mutate_at(vars(ends_with("GM")),
            funs(as.numeric)) %>%
  mutate_at(vars(ends_with("GA")),
            funs(as.numeric)) %>%
  mutate_at(vars(ends_with("G%")),
            funs(as.numeric)) %>%
  mutate(`NON-RA_EFG%_player` = `NON-RA_FGM`/`NON-RA_FGA`,
         `MR_EFG%_player` = MR_FGM/MR_FGA,
         pullup_2PA = FGA - `3PA`,
         pullup_2PM = FGM - `3PM`,
         `pullup_2P%` = pullup_2PM/pullup_2PA) %>%
  filter(FGA - `3PA` >= boarder_pullup_2PA)


# compare EFG% between player and team ------------------------------------


player_boxscore <- player_raw %>%
  left_join(boxscore, by = c("TEAM.x" = "TEAM")) %>% #joining team EFG% base data
  mutate(`EFG%_team` = (FGM.y + 0.5*`3PM.y`)/FGA.y) %>% #mutating team's EFG%
  mutate(`compare_NON-RA` = if_else(`NON-RA_EFG%_player` > `EFG%_team`, 1, 0, missing = 0),
         compare_MR = if_else(`MR_EFG%_player` > `EFG%_team`, 1, 0, missing = 0),
         compare_pullup_2P = if_else(`pullup_2P%` > `EFG%_team`, 1, 0, missing = 0))

#writing excel
player_boxscore %>%
  select(PLAYER, TEAM.x, `NON-RA_FGA`, MR_FGA, `NON-RA_EFG%_player`, `MR_EFG%_player`,`pullup_2P%`,  `EFG%_team`, `compare_NON-RA`, compare_MR, compare_pullup_2P) %>%
  rename("プレイヤー名" = PLAYER, "チーム名" = TEAM.x, "制限区域外のFGA" = `NON-RA_FGA`, "ミッドレンジのFGA" = MR_FGA,
         "プレイヤーの制限区域外EFG%" = `NON-RA_EFG%_player`,
         "プレイヤーのミッドレンジEFG%" = `MR_EFG%_player`,"プレイヤーのpullupのEFG%" = `pullup_2P%` , "チームのEFG%" = `EFG%_team`,
         "プレイヤーの制限区域外EFG%とチームのEFG%比較" = `compare_NON-RA`, "プレイヤーのミッドレンジEFG%とチームのEFG%比較" = compare_MR,
         "プレイヤーのプルアップ2PのEFG%とチームのEFG%比較" = compare_pullup_2P) %>%
  write.xlsx("player_boxscore.xlsx")

# compare EFG% between player and clock -- late -----------------------------------

player_clock_late <- player_raw %>%
  left_join(clock_late, by = c("TEAM.x" = "TEAM")) %>%
  mutate(`EFG%_team_late` = `EFG%.y`/ 100) %>% #convert EFG% already calculated into point
  mutate(`compare_NON-RA` = if_else(`NON-RA_EFG%_player` > `EFG%_team_late`, 1, 0, missing = 0),
         compare_MR = if_else(`MR_EFG%_player` > `EFG%_team_late`, 1, 0, missing = 0),
         compare_pullup_2P = if_else(`pullup_2P%` > `EFG%_team_late`, 1, 0, missing = 0))

#writing excel
player_clock_late %>%
  select(PLAYER, TEAM.x, `NON-RA_FGA`, MR_FGA, `NON-RA_EFG%_player`, `MR_EFG%_player`, `pullup_2P%` , `EFG%_team_late`, `compare_NON-RA`, compare_MR, compare_pullup_2P) %>%
  rename("プレイヤー名" = PLAYER, "チーム名" = TEAM.x, "制限区域外のFGA" = `NON-RA_FGA`, "ミッドレンジのFGA" = MR_FGA,
         "プレイヤーの制限区域外EFG%" = `NON-RA_EFG%_player`,
         "プレイヤーのミッドレンジEFG%" = `MR_EFG%_player`, "プレイヤーのpullupのEFG%" = `pullup_2P%` ,"チームのEFG%_late" = `EFG%_team_late`,
         "プレイヤーの制限区域外EFG%とチームのEFG%比較" = `compare_NON-RA`, "プレイヤーのミッドレンジEFG%とチームのEFG%比較" = compare_MR,
         "プレイヤーのプルアップ2PのEFG%とチームのEFG%比較" = compare_pullup_2P) %>%
  write.xlsx("player_clocklate.xlsx")

# compare EFG% between player and clock -- very late -----------------------------------

player_clock_verylate <- player_raw %>%
  left_join(clock_verylate, by = c("TEAM.x" = "TEAM")) %>%
  mutate(`EFG%_team_verylate` = `EFG%.y`/ 100) %>% #convert EFG% already calculated into point
  mutate(`compare_NON-RA` = if_else(`NON-RA_EFG%_player` > `EFG%_team_verylate`, 1, 0, missing = 0),
         compare_MR = if_else(`MR_EFG%_player` > `EFG%_team_verylate`, 1, 0, missing = 0),
         compare_pullup_2P = if_else(`pullup_2P%` > `EFG%_team_verylate`, 1, 0, missing = 0))

#writing excel
player_clock_verylate %>%
  select(PLAYER, TEAM.x, `NON-RA_FGA`, MR_FGA, `NON-RA_EFG%_player`, `MR_EFG%_player`,`pullup_2P%` , `EFG%_team_verylate`, `compare_NON-RA`, compare_MR, compare_pullup_2P) %>%
  rename("プレイヤー名" = PLAYER, "チーム名" = TEAM.x, "制限区域外のFGA" = `NON-RA_FGA`, "ミッドレンジのFGA" = MR_FGA,
         "プレイヤーの制限区域外EFG%" = `NON-RA_EFG%_player`,
         "プレイヤーのミッドレンジEFG%" = `MR_EFG%_player`,"プレイヤーのpullupのEFG%" = `pullup_2P%` , "チームのEFG%_verylate" = `EFG%_team_verylate`,
         "プレイヤーの制限区域外EFG%とチームのEFG%比較" = `compare_NON-RA`, "プレイヤーのミッドレンジEFG%とチームのEFG%比較" = compare_MR,
         "プレイヤーのプルアップ2PのEFG%とチームのEFG%比較" = compare_pullup_2P) %>%
  write.xlsx("player_clockverylate.xlsx")
