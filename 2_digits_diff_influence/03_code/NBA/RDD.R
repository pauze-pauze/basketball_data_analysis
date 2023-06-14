
# import library ----------------------------------------------------------
pacman::p_load(tidyverse, scales, broom, rdd)

# load data ---------------------------------------------------------------

df <- readRDS("./01_data/quarter_log.rds") 
df <- df %>%
  mutate(pts_diff = hs - vs)

# 2Q ----------------------------------------------------------------------
period_param <- 2

# manipulate data ---------------------------------------------------------
## extract 2Q and final results by each game.
df_result <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == period_param) %>%
      select(pts_diff_tq = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_tq < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_tq = if_else(pts_diff_tq < 0, pts_diff_tq, -pts_diff_tq)
  )



# RDD analysis ----------------------------------------------
## win_flag形式ではうまく動かなかった。non parametric RDDの性質？やむなしで最終的な得点差への影響とした
df_log <- df_result %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  filter(pts_diff_tq >= -20) # 気分

rdd_result <- RDestimate(
  data = df_log 
  ,formula = pts_diff_last ~ pts_diff_tq
  ,cutpoint = -10
)
summary(rdd_result)
plot(rdd_result)

# 3Q ----------------------------------------------------------------------
period_param <- 3

# manipulate data ---------------------------------------------------------
## extract 2Q and final results by each game.
df_result <- df %>%
  inner_join(
    df %>%
      group_by(g_id) %>%
      summarise(period = max(period), .groups = "drop") # take into account OTs
    ,by = c("g_id", "period")
  ) %>%
  select(period, g_id, pts_diff_last = pts_diff) %>%
  left_join(
    df %>%
      filter(period == period_param) %>%
      select(pts_diff_tq = pts_diff, g_id)
    ,by = "g_id"
  ) %>%
  mutate(
    pts_diff_last = if_else(pts_diff_tq < 0, pts_diff_last, -pts_diff_last) # don't change the order of these mutate area.
    ,pts_diff_tq = if_else(pts_diff_tq < 0, pts_diff_tq, -pts_diff_tq)
  )


# RDD analysis ----------------------------------------------
## win_flag形式ではうまく動かなかった。non parametric RDDの性質？やむなしで最終的な得点差への影響とした
df_log <- df_result %>%
  mutate(win_flag = if_else(pts_diff_last > 0, 1, 0)) %>%
  filter(pts_diff_tq >= -20) # 気分

rdd_result <- RDestimate(
  data = df_log 
  ,formula = pts_diff_last ~ pts_diff_tq
  ,cutpoint = -10
)
summary(rdd_result)
plot(rdd_result)

































