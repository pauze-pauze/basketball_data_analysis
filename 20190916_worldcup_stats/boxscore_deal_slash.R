library(tidyverse)

test <- read_csv("worldcup_boxscore.csv")

test <- test %>%
  tidyr::separate(col = Fg_TeamA, into = c("Fgm_TeamA", "Fga_teamA"), sep = "/") %>%
  tidyr::separate(col = F2g_TeamA, into = c("F2gm_TeamA", "F2ga_teamA"), sep = "/") %>%
  tidyr::separate(col = F3g_TeamA, into = c("F3gm_TeamA", "F3ga_teamA"), sep = "/") %>%
  tidyr::separate(col = Ft_TeamA, into = c("Ftm_TeamA", "Fta_teamA"), sep = "/") %>%
  tidyr::separate(col = Fg_TeamB, into = c("Fgm_TeamB", "Fga_teamB"), sep = "/") %>%
  tidyr::separate(col = F2g_TeamB, into = c("F2gm_TeamB", "F2ga_teamB"), sep = "/") %>%
  tidyr::separate(col = F3g_TeamB, into = c("F3gm_TeamB", "F3ga_teamB"), sep = "/") %>%
  tidyr::separate(col = Ft_TeamB, into = c("Ftm_TeamB", "Fta_teamB"), sep = "/") 

write.csv(test, "worldcup_boxscore_raw.csv")  
  
