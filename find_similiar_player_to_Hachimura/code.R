
# 必要なパッケージ読み込み ------------------------------------------------------------

library(tidyverse)
library(ballr)
library(cluster)
library(skimr)
library(ggrepel)
library(factoextra)

# 利用するデータの作成 --------------------------------------------------------------
### 元データ作成(20200723時点のデータ取得)
# #データ取得_36分換算データ
# base_season = 1998
# 
# base <- NBAPerGameStatisticsPer36Min(season = base_season) %>%
#   mutate(season = base_season)
# for (i in base_season + 1:2020){
#   bind <- NBAPerGameStatisticsPer36Min(season = i) %>%
#     mutate(season = i)
#   base <- base %>%
#     bind_rows(bind)
# }
# 
# #データ取得_advanced_statistics
# base2 <- NBAPerGameAdvStatistics(season = base_season) %>%
#   mutate(season = base_season)
# 
# for (i in base_season + 1:2020){
#   bind <- NBAPerGameAdvStatistics(season = i) %>%
#     mutate(season = i)
#   base2 <- base2 %>%
#     bind_rows(bind)
# }
# #同一シーズンに複数チームでプレーした選手のスタッツをTOTだけにするフラグ作成
# tot_flag <- base %>%
#   group_by(
#     player
#     ,season
#   ) %>%
#   summarise(
#     tot_flag = case_when(
#       n() >= 2 ~ 1 #同一シーズンで複数チームに所属しているフラグ
#       ,TRUE ~ 0
#     )
#   )
# 
# data_base <- base %>%
#   inner_join(
#     base2
#     ,by = c(
#       "player"
#       ,"link"
#       ,"season"
#       ,"pos"
#       ,"age"
#       ,"tm"
#       ,"g"
#       ,"mp"
#     )
#   ) %>%
#   left_join(
#     tot_flag
#     ,by = c(
#       "player"
#       ,"season"
#     )
#   ) %>%
#   filter(
#     xor(tm != "TOT", tot_flag == 1) # 同一シーズンで複数チームに所属していた選手をシーズン全体のスタッツだけ残す
#   ) %>%
#   select(
#     -c(
#       rk.x
#       ,tm
#       ,link
#       ,rk.y
#       ,x
#       ,x_2
#     )
#   ) %>%
#   mutate(
#     mpg = mp/g
#     ,a2to = ast / tov
#   )
# 
# 
# write_csv(data_base, "data_base_20200723.csv")

# データ読み込み
data_base <- read.csv("data_base_20200723.csv")

# 2020シーズンにプレイした選手とそのデビューシーズンの抽出
player_20 <- data_base %>%
  filter(
    season == 2020
  ) %>%
  select(
    player
    ,season
  ) %>%
  distinct()

player_first_season <- data_base %>%
  group_by(
    player
  ) %>%
  summarise(
    season = min(season)
  ) %>%
  inner_join(
    player_20
    ,by = "player"
  ) %>%
  mutate(
    season = season.x
  ) %>%
  select(
    player
    ,season
  ) 
player_list <- player_20 %>%
  bind_rows(player_first_season) %>%
  distinct()

data <- data_base %>%
  inner_join(
    player_list
    ,by = c("player", "season")
  )
# 出場時間の分布を見て、下限を決める(八村が残るように)
data %>% # 2020シーズンの分布
  filter(
    season == 2020
  ) %>%
  ggplot(aes(x = mp))+
  geom_histogram(binwidth = 100)+
  labs(x = "出場時間(分)", y = "人数", title = "2020シーズンの出場時間分布")+
  theme_classic()

data_base %>%
  filter(
    season != 2020
  ) %>%
  ggplot(aes(x = mp))+
  geom_histogram(binwidth = 100)+
  labs(x = "出場時間(分)", y = "人数", title = "1999~2019シーズンの出場時間分布")+
  theme_classic()

data %>% # 八村の出場時間を確認
  filter(
    player == "Rui Hachimura"
  ) %>%
  select(
    mp
  )
#2020シーズンに一定以上プレイした選手と八村のリスト作成
data_mp_20 <- data %>%
  filter(
    season == 2020
  ) %>%
  filter(
    mp >= 1750 #ヤニス入れたいがゆえの時間設定
    | player == "Rui Hachimura"
    | player == "Kawhi Leonard" #特例
  ) %>%
  select(
    player
  )
data_mp_filt <- data %>%
  inner_join(
    data_mp_20
    ,by = "player"
  ) %>%
  na.omit() %>%
  mutate(
    player = str_c(player, as.character(season), sep = "_")
  ) 
  
#
#2020シーズンは途中までしか試合がないから、MPやG、GSを変数に加えると外的要因が混じりこむので、基本的には割合ベースの数値にする
#
# 正規化

data_main <- data_mp_filt %>%
  select(
    -c(
      g
      ,gs
      ,mp
      ,pos
      ,age
      ,season
    )
  ) %>%
  mutate(
    x3ppercent = if_else(is.na(x3ppercent) == TRUE, 0, x3ppercent)
  ) %>%
  mutate_at(vars(-c(player)), funs(scale)) 

rownames(data_main) <- data_main$player

skim(data_main)


# 距離の計算 -------------------------------------------------------------------
player_dist <- data_main %>%
  select(
    -player
  ) %>%
  dist() %>%
  cmdscale() %>%
  data.frame() %>%
  bind_cols(data_main[1]) %>%
  mutate(
    season_20_flag = case_when(
      str_sub(player, start = -4, end = -1) == "2020" ~ "2020"
      ,TRUE ~ "デビュー"
    )
  )
hachi_x1 <- player_dist %>%
  filter(
    player == "Rui Hachimura_2020"
  ) %>%
  select(
    X1
  ) %>%
  as.numeric()
hachi_x2 <- player_dist %>%
  filter(
    player == "Rui Hachimura_2020"
  ) %>%
  select(
    X2
  ) %>%
  as.numeric()
dist_rank <-  player_dist %>%
  mutate(
    X1_dist = X1 - hachi_x1
    ,X2_dist = X2 - hachi_x2
    ,dist = sqrt(X1_dist^2 + X2_dist^2)
  ) %>%
  arrange(
    dist
  )

top10_similiar_player <- dist_rank %>%
  filter(
    season_20_flag == "デビュー"
    | player == "Rui Hachimura_2020"
  ) %>%
  select(
    player
  ) %>%
  head(11)

data_mp_filt %>%
  inner_join(
    top10_similiar_player
    ,by = "player"
  ) %>%
  view()
# 階層的クラスタリング --------------------------------------------------------------

dist_hclust <- data_main %>%
  select(
    -player
  ) %>%
  dist()
data_hclust <- hclust(dist_hclust)
plot(data_hclust)
fviz_dend(
  data_hclust
  #,k = 8
  ,cex = 0.4
  ,horiz = TRUE
  ,rect = TRUE
  ,labelsize = 1
)

# k-means -----------------------------------------------------------------

res_dist <- data_main %>%
  select(
    -player
  ) %>%
  get_dist(
    stand = TRUE
    ,method = "euclidean"
  )
data_main %>%
  select(
    -player
  ) %>%
  fviz_nbclust(kmeans, method = "wss")

data_main %>%
  select(
    -player
  ) %>%
  fviz_nbclust(kmeans, method = "silhouette")

data_main %>%
  select(
    -player
  ) %>%
  fviz_nbclust(kmeans, method = "gap_stat")

km_res <- data_main %>%
  select(
    -player
  ) %>%
  kmeans(10, nstart = 100)


data_main_kmeans <- data_main %>%
  select(
    -player
  )

fviz_cluster(km_res, data_main_kmeans, ellipse = TRUE, ellipse.alpha= 0.1,
             palette = "jco",repel = TRUE, ggtheme = theme_classic(), 
             main= FALSE, xlab= FALSE, ylab = FALSE,
             labelsize= 8,lwd=2
)


# 多次元尺度法 ------------------------------------------------------------------

res_dist <- data_main %>%
  select(-player) %>%
  dist()
data_cmd <- cmdscale(res_dist)
plot(data_cmd)
data_lab <- data_main$player
text(data_cmd, labels = data_lab)

# 主成分分析 -------------------------------------------------------------------

res_cov <- data_main %>%
  select(
    -player
  ) %>%
  cov()

round(res_cov, 2)

eig <- eigen(res_cov)$vectors

eigenv2 <- eig[,(1:2)]
statsMatrix <- data_main %>%
  select(
    -player
  ) %>%
  data.matrix(rownames.force = NA)
PrincipalComponents2 <- statsMatrix %*% eigenv2

statsDF <- data.frame(PrincipalComponents2) %>%
  bind_cols(data_main[1]) %>%
  mutate(
    season_20_flag = case_when(
      str_sub(player, start = -4, end = -1) == "2020" ~ "2020"
      ,TRUE ~ "デビュー"
    )
  )

colors2=c("black","gray50")

ggplot(statsDF, aes(statsDF$X1, -statsDF$X2, fill = season_20_flag, color = season_20_flag))+ #x2をマイナスにしたのはk-meansと向きを合わせるため
  geom_point()+
  geom_text_repel(aes(label = player), size = 3)+
  scale_color_manual(values = colors2)+
  theme_classic()


#距離の近い選手をリスト化
hachi_x1 <- statsDF %>%
  filter(
    player == "Rui Hachimura_2020"
  ) %>%
  select(
    X1
  ) %>%
  as.numeric()
hachi_x2 <- statsDF %>%
  filter(
    player == "Rui Hachimura_2020"
  ) %>%
  select(
    X2
  ) %>%
  as.numeric()
dist_rank <-  statsDF %>%
  mutate(
    X1_dist = X1 - hachi_x1
    ,X2_dist = X2 - hachi_x2
    ,dist = sqrt(X1_dist^2 + X2_dist^2)
  ) %>%
  arrange(
    dist
  )


