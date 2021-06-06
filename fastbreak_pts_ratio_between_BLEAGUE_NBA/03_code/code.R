
# import libraries --------------------------------------------------------

library(tidyverse)
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

season_param = c("2017-18", "2018-19", "2020-21") # コロナで中断したシーズンを除外
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
    #Season %in% season_param
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

output_bleague %>%
  ggplot(aes(x = fast_break_ratio)) +
  geom_histogram()

write.csv()