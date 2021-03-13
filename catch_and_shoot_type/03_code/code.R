
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


# load data ---------------------------------------------------------------

raw_3P <- read_csv("./01_Data/raw_data_3P.csv")
raw_c_and_s <- read_csv("./01_Data/raw_data_C_and_s.csv")
raw_dist <- read_csv("./01_Data/raw_data_dist.csv")

skim(raw_3P)
skim(raw_c_and_s)
skim(raw_dist)


# manipulate data ---------------------------------------------------------

### It is seemed that some game don't have Distance data. So I use join function properly
# pick up needed columns

df_3P <- raw_3P %>%
  select(
    PLAYER
    ,GP # to check that all the games have tracking data.
    ,MIN
    ,`3PA`
    ,`3PM`
    ,season
  ) %>%
  rename(
    P3A_all = `3PA`
    ,P3M_all = `3PM`
  )
df_c_and_s <- raw_c_and_s %>%
  select(
    PLAYER
    ,GP 
    ,MIN
    ,`3PA`
    ,`3PM`
    ,Season
  ) %>%
  rename(
    P3A_c_and_s = `3PA`
    ,P3M_c_and_s = `3PM`
    ,season = Season
  ) %>%
  mutate(
    P3A_c_and_s = if_else(is.na(as.numeric(P3A_c_and_s)) == TRUE, 0, as.numeric(P3A_c_and_s)) 
    ,P3M_c_and_s = if_else(is.na(as.numeric(P3M_c_and_s)) == TRUE, 0, as.numeric(P3M_c_and_s)) 
  )
df_dist <- raw_dist %>%
  select(
    PLAYER
    ,`AVG SPEED OFF`
    ,Season
  ) %>%
  rename(
    avg_speed_off = `AVG SPEED OFF`
    ,season = Season
  )

# join the table and make base table

df_base <- df_3P %>%
  inner_join(
    df_c_and_s
    ,by = c("PLAYER", "season", "GP", "MIN")
  ) %>%
  inner_join(
    df_dist
    ,by = c("PLAYER", "season")
  ) %>%
  mutate(
    c_and_s_rate = P3A_c_and_s / P3A_all
  ) %>%
  mutate(
    c_and_s_rate = if_else(is.nan(c_and_s_rate) == TRUE, 0, c_and_s_rate)
  )


# analyze data ------------------------------------------------------------

# See data distribution and set threshold
hist(df_base$MIN)
min_th <- quantile(df_base$MIN, 0.5)

hist(df_base$P3A_c_and_s)
P3A_c_and_s_th <- quantile(df_base$P3A_c_and_s, 0.7)

hist(df_base$c_and_s_rate)
c_and_s_rate_th_base <- df_base %>%
  filter(P3A_c_and_s >= P3A_c_and_s_th) %>%
  select(c_and_s_rate)
hist(c_and_s_rate_th_base$c_and_s_rate)
c_and_s_rate_th <- quantile(c_and_s_rate_th_base$c_and_s_rate, 0.7)


# See ranking
df_base %>%
  filter(
    (MIN >= min_th 
     & P3A_c_and_s >= P3A_c_and_s_th
     & c_and_s_rate >= c_and_s_rate_th)
    | (PLAYER == "Yuta Watanabe" & season == "2020-21")
  ) %>%
  arrange(desc(avg_speed_off)) %>%
  write_csv("./02_output/dist_rank.csv")
