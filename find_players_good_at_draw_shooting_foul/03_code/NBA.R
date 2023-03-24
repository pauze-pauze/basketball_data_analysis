
# import library and read data --------------------------------------------
pacman::p_load(tidyverse, scales, psych, tidymodels, gt, ggrepel)

boxscore <- read_csv("./01_data/NBA_stats_data_find_good_at_drawing_shooting_fouls_boxscore.csv")
pfd <- read_csv("./01_data/NBA_stats_data_find_good_at_drawing_shooting_fouls_PFD.csv")


# check players' record ---------------------------------------------------
boxscore %>%
  group_by(Player) %>%
  summarise(cnt = n()) %>%
  filter(cnt >= 2)
# There are no players who have multiple records.

# create data frame to analyse --------------------------------------------

df <- boxscore %>%
  select(Player, Min, FGA, FTA) %>%
  left_join(
    pfd %>% select(Player, PFD)
    ,by = "Player"
  ) %>%
  relocate(FTA, .after = PFD)

# create sample data frame png
# df %>% 
#   head() %>%
#   gt() %>%
#   cols_label(
#     Min = md("*Min*")
#     ,FGA = md("*FGA*")
#     ,PFD = md("*PFD*")
#     ,FTA = md("*FTA*")
#   ) %>%
#   tab_options(
#     table_body.hlines.width = 0
#     ,column_labels.border.top.width = 2 
#     ,column_labels.border.top.color = "black"
#     ,column_labels.border.bottom.width = 2
#     ,column_labels.border.bottom.color = "black"
#     ,table_body.border.bottom.color = "black"
#   ) %>%
#   tab_header(title = md("Sample data frame")) %>%
#   tab_style(
#     style = cell_text(align = "left")
#     ,locations = cells_title("title")
#   ) %>%
#   tab_options(
#     table.border.top.width = 0
#     ,column_labels.border.top.width = 3
#   ) %>%
#   gtsave("./02_output/sample_data_frame.png")

# png("./02_output/relations_between_variables.png")
# pairs.panels(df %>% select(-Player))
# dev.off

pairs.panels(df %>% select(-Player))
# 
# df %>%
#   pivot_longer(
#     cols = c(Min, FGA, PFD)
#   ) %>%
#   ggplot(aes(x = value, y = FTA))+
#   geom_point()+
#   facet_wrap(~ name, scales = "free", ncol = 1)+
#   labs(
#     x = ""
#     , y = "FTA"
#     , title = " NBA 2021-22 Regular Season's scatter plot of FTA and other stats"
#     , caption = "x-axis means each gragh title's value"
#   )+
#   theme_bw()+
#   ggsave("./02_output/simple_scatter_plot_NBA.png", width = 6, height = 8)


# create predict model ----------------------------------------------------
seed = 1031
set.seed(seed)

folds <- df %>%
  vfold_cv(v = 5, strata = FTA)
folds

# define recipe
fta_recipe_base <- df %>%
  recipe(formula = FTA ~ .) %>%
  update_role(Player, new_role = "id variable") %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors())
fta_recipe_yj <- fta_recipe_base %>%
  step_YeoJohnson(Min, FGA)

# define model
lm_spec <- 
  linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

xgb_spec <- 
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")
# This time, I skip hyper parameter tuning.

fta_models <- 
  workflow_set(
    preproc = list(
      base = fta_recipe_base
      ,yj = fta_recipe_yj
    )
    ,models = list(
      lm = lm_spec
      ,xgb = xgb_spec
    )
    ,cross = TRUE
  )
set.seed(seed)
fta_models <- fta_models %>%
  workflow_map(
    resamples = folds
    ,metrics = metric_set(mae)
    ,verbose = FALSE
  )
fta_models
fta_models %>%
  rank_results(
    rank_metric = "mae"
    ,select_best = TRUE
  ) %>%
  select(rank, mean, model, wflow_id, .config)

# base recipe and lm is the best model

# predict fta and analyse the diffrence between real fta and predicted fta --------

fta_predict_wflow <- 
  workflow() %>%
  add_recipe(recipe = fta_recipe_base) %>%
  add_model(spec = lm_spec)

fta_fit <- fta_predict_wflow %>%
  fit(data = df)

df_predict <- fta_fit %>%
  augment(new_data = df) %>%
  rename(FTA_pred = .pred) %>%
  filter(FTA >= 10) %>%
  mutate(real_pred_ratio = FTA / FTA_pred)

# create table image
# df_predict %>%
#   arrange(desc(FTA)) %>%
#   head(n = 30) %>%
#   arrange(desc(real_pred_ratio)) %>%
#   gt() %>%
#   cols_label(
#     Min = md("*Min*")
#     ,FGA = md("*FGA*")
#     ,PFD = md("*PFD*")
#     ,FTA = md("*FTA*")
#     ,FTA_pred = md("*FTA_pred*")
#     ,real_pred_ratio = md("*real_pred_ratio*")
#   ) %>%
#   fmt_percent(
#     columns = real_pred_ratio
#     ,decimals = 1
#   ) %>%
#   fmt_number(
#     columns = FTA_pred
#     ,decimals = 1
#   ) %>%
#   tab_options(
#     table_body.hlines.width = 0
#     ,column_labels.border.top.width = 2
#     ,column_labels.border.top.color = "black"
#     ,column_labels.border.bottom.width = 2
#     ,column_labels.border.bottom.color = "black"
#     ,table_body.border.bottom.color = "black"
#   ) %>%
#   tab_header(title = md("Top 30 FTA players' data")) %>%
#   tab_style(
#     style = cell_text(align = "left")
#     ,locations = cells_title("title")
#   ) %>%
#   tab_options(
#     table.border.top.width = 0
#     ,column_labels.border.top.width = 3
#   ) %>%
#   gtsave("./02_output/fta_top30_players_table.png")

top_bottom_5_ratio_players <- df_predict %>%
  filter(FTA >= 100) %>%
  arrange(desc(real_pred_ratio)) %>%
  head(n = 5) %>%
  rbind(
    df_predict %>%
      filter(FTA >= 100) %>%
      arrange(real_pred_ratio) %>%
      head(n = 5)
  ) %>%
  rbind(
    df_predict %>%
      arrange(desc(FTA)) %>%
      head(n = 10)
  )
top_bottom_5_ratio_players %>%
  gt() %>%
  cols_label(
    Min = md("*Min*")
    ,FGA = md("*FGA*")
    ,PFD = md("*PFD*")
    ,FTA = md("*FTA*")
    ,FTA_pred = md("*FTA_pred*")
    ,real_pred_ratio = md("*real_pred_ratio*")
  ) %>%
  fmt_percent(
    columns = real_pred_ratio
    ,decimals = 1
  ) %>%
  fmt_number(
    columns = FTA_pred
    ,decimals = 1
  ) %>%
  tab_options(
    table_body.hlines.width = 0
    ,column_labels.border.top.width = 2
    ,column_labels.border.top.color = "black"
    ,column_labels.border.bottom.width = 2
    ,column_labels.border.bottom.color = "black"
    ,table_body.border.bottom.color = "black"
  ) %>%
  tab_header(title = md("Top and bottom 5 real_pred_ratio and top 10 fta players")) %>%
  tab_style(
    style = cell_text(align = "left")
    ,locations = cells_title("title")
  ) %>%
  tab_options(
    table.border.top.width = 0
    ,column_labels.border.top.width = 3
  ) %>%
  gtsave("./02_output/Top_and_bottom_5_real_pred_ratio_and_top_10_fta_players.png")

df_predict_gragh <- df_predict %>%
  mutate(plotname = if_else(Player %in% top_bottom_5_ratio_players$Player, Player, ""))

df_predict_gragh %>%
  ggplot(aes(x = FTA_pred, y = FTA))+
  geom_point()+
  geom_abline(
    color = "red"
    ,alpha = 0.5
    ,linetype = 2
  )+
  geom_label_repel(aes(label = plotname), size = 6, max.overlaps = 50)+
  theme_bw()+
  ggsave("./02_output/real_pred_fta.png", width = 20, height = 15)
