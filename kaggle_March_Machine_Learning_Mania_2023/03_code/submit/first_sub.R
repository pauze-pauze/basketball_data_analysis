# import library and read data --------------------------------------------

source("./03_code/source/read_library_data.R")


# create sample ratings ---------------------------------------------------
set.seed(1031)
Mratings <- runif(nrow(MTeams), 0, 1)
Mteams_ratings <- MTeams %>%
  cbind(Mratings) %>%
  select(TeamID, ratings = Mratings)

Wratings <- runif(nrow(WTeams), 0, 1)
Wteams_ratings <- WTeams %>%
  cbind(Wratings) %>%
  select(TeamID, ratings = Wratings)


# create sample submission file -------------------------------------------
## join each teams' rating
sample_sub_df <- SampleSubmission2023 %>%
  mutate(
    team_id_left = str_sub(ID, start = 6, end = 9) %>% as.numeric()
    ,team_id_right = str_sub(ID, start = 11, end = 14) %>% as.numeric()
  ) %>%
  left_join(
    rbind(Mteams_ratings, Wteams_ratings)
    ,by = c("team_id_left" = "TeamID")
  ) %>%
  rename(rating_left = ratings) %>%
  left_join(
    rbind(Mteams_ratings, Wteams_ratings)
    ,by = c("team_id_right" = "TeamID")
  ) %>%
  rename(rating_right = ratings)
## calculate win percentage
###何かしらの形で出すけど、今回はガチで適当。
sample_sub_df %>%
  mutate(
    win_percent = 0.5 + (rating_left - rating_right) / 2 # 0 ~ 1に収めるための適当な処理
  ) %>%
  select(ID, Pred = win_percent) %>%
  write_csv("./02_output/first_sub.csv")
  
