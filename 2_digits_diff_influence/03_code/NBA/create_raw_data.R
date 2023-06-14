library(hoopR)
library(tidyverse)


nba_pbp <- load_nba_pbp(seasons = 2023) %>%
  select(-wallclock) %>%
  rbind(load_nba_pbp(seasons = 2022))%>%
  rbind(load_nba_pbp(seasons = 2021))%>%
  rbind(load_nba_pbp(seasons = 2020))
quarter_log <- nba_pbp %>%
  filter(str_detect(type_text, pattern = "End Period")) %>%
  select(period = period_number, hs = home_score, vs = away_score, g_id = game_id, season)

# 
# # Get team_id
# team_id_df <- hoopR::nba_teams %>% select(TeamID)
# nrow(team_id_df)
# # Get game_id by using team_id
# gid_list <- list()
# 
# for (i in 1:nrow(team_id_df)) {
#   team_id <- team_id_df[i, 1] %>% as.character
#   game_id <- nba_teamgamelog(season = "2022-23", team_id = team_id)$TeamGameLog$Game_ID
#   gid_list <- c(gid_list, game_id)
# }
# for (i in 1:nrow(team_id_df)) {
#   team_id <- team_id_df[i, 1] %>% as.character
#   game_id <- nba_teamgamelog(season = "2021-22", team_id = team_id)$TeamGameLog$Game_ID
#   gid_list <- c(gid_list, game_id)
# }
# for (i in 1:nrow(team_id_df)) {
#   team_id <- team_id_df[i, 1] %>% as.character
#   game_id <- nba_teamgamelog(season = "2020-21", team_id = team_id)$TeamGameLog$Game_ID
#   gid_list <- c(gid_list, game_id)
# }
# gid_list <- unique(gid_list)
# 
# 
# # Get Play-by-play data by using game id
# 
# quarter_log <- data.frame()
# for (i in 1:length(gid_list)){
#   g_log <- hoopR::nba_data_pbp(game_id = gid_list[i])
#   if (nrow(g_log) > 0){ # Some game_id can't use in nba_data_pbp function.
#     g_log <- g_log  %>%
#       cbind(tibble(g_id = gid_list[i])) %>%
#       filter(str_detect(de, pattern = "End Period")) %>%
#       select(period, hs, vs, g_id)
#     quarter_log <- quarter_log %>% rbind(g_log)
#   } else {
#     quarter_log <- quarter_log
#   }
# }


saveRDS(quarter_log, "./01_data/quarter_log.rds")
