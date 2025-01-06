# import library and data -------------------------------------------------

pacman::p_load("tidyverse", "showtext", "ggpubr", "gridExtra", "ggimage", "grid")

r_team <- read_csv("./01_data/r_team.csv") # チームの移動距離データ

# ロゴデータのimport ------------------------------------------------------------

logo <- data.frame(
  long_name = c(
    "Atlanta Hawks"
    ,"Boston Celtics"
    ,"Brooklyn Nets"
    ,"Charlotte Hornets"
    ,"Chicago Bulls"
    ,"Cleveland Cavaliers"
    ,"Dallas Mavericks"
    ,"Denver Nuggets"
    ,"Detroit Pistons"
    ,"Golden State Warriors"
    ,"Houston Rockets"
    ,"Indiana Pacers"
    ,"LA Clippers"
    ,"Los Angeles Lakers"
    ,"Memphis Grizzlies"
    ,"Miami Heat"
    ,"Milwaukee Bucks"
    ,"Minnesota Timberwolves"
    ,"New Orleans Pelicans"
    ,"New York Knicks"
    ,"Oklahoma City Thunder"
    ,"Orlando Magic"
    ,"Philadelphia 76ers"
    ,"Phoenix Suns"
    ,"Portland Trail Blazers"
    ,"Sacramento Kings"
    ,"San Antonio Spurs"
    ,"Toronto Raptors"
    ,"Utah Jazz"
    ,"Washington Wizards"
  )
  ,image = c("../_logo/ATL.png" 
            ,"../_logo/BOS.png" 
            ,"../_logo/BKN.png" 
            ,"../_logo/CHA.png" 
            ,"../_logo/CHI.png" 
            ,"../_logo/CLE.png" 
            ,"../_logo/DAL.png" 
            ,"../_logo/DEN.png" 
            ,"../_logo/DET.png" 
            ,"../_logo/GSW.png" 
            ,"../_logo/HOU.png" 
            ,"../_logo/IND.png" 
            ,"../_logo/LAC.png" 
            ,"../_logo/LAL.png" 
            ,"../_logo/MEM.png" 
            ,"../_logo/MIA.png" 
            ,"../_logo/MIL.png" 
            ,"../_logo/MIN.png" 
            ,"../_logo/NOP.png" 
            ,"../_logo/NYK.png" 
            ,"../_logo/OKC.png" 
            ,"../_logo/ORL.png" 
            ,"../_logo/PHI.png" 
            ,"../_logo/PHX.png" 
            ,"../_logo/POR.png" 
            ,"../_logo/SAC.png" 
            ,"../_logo/SAS.png" 
            ,"../_logo/TOR.png" 
            ,"../_logo/UTA.png" 
            ,"../_logo/WAS.png" 
  )
) %>%
  mutate(
    short_name = str_extract(image, "(?<=_logo/)[A-Z]+")
  )



# データの加工 ------------------------------------------------------------------

df <- r_team %>%
  filter(
    season %in% c("2024-25", "2023-24")
  ) %>%
  select(
    season, Team, Avg_Speed
  ) %>%
  left_join(
    logo
    ,by = c("Team" = "long_name")
  )
arrow_df <- df %>%
  pivot_wider(names_from = season, values_from = c(Avg_Speed, image))%>%
  rename(
    last_season = "Avg_Speed_2023-24"
    ,this_season = "Avg_Speed_2024-25"
    ,image_last_season = "image_2023-24"
    ,image_this_season = "image_2024-25"
  ) %>%
  mutate(
    yoy = this_season / last_season
    ,line_type = ifelse(short_name == "SAS", "dashed", "solid")  # Team Bだけ破線
    ,line_size = ifelse(short_name == "SAS", 1.5, 0.2)               # Team Bだけ線を太くする
    ,logo_size = ifelse(short_name == "SAS", 0.1, 0.02)          # Team Bだけロゴを大きくする
    # ,color = ifelse(short_name == "SAS", "red", "black")
  )
# 矢印とロゴを表示するグラフ
g <- ggplot(arrow_df) +
  geom_segment(
    aes(x = 1, xend = 2, y = last_season, yend = this_season,color = factor(short_name == "SAS"), linetype = line_type, size = line_size),  # linetypeとsizeを指定
    arrow = arrow(length = unit(0.1, "cm"))
  ) +
  geom_image(aes(x = 1, y = last_season, image = image_last_season, size = logo_size)) +
  geom_image(aes(x = 2, y = this_season, image = image_this_season, size = logo_size)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("2023-24", "2024-25"), limits = c(0.9, 2.1)) +
  scale_size_identity()+
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black") # "SAS"は赤、それ以外は黒
  )+
  guides(linetype = "none", color = "none") +  # linetypeとcolorの凡例を非表示
  labs(title = "Team Avg Speed Over Seasons",
       x = "Season",
       y = "Avg Speed") +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 15)
    , axis.text.y = element_text(size = 15)
    , plot.title = element_text(size = 20)
    ,panel.grid.minor.x = element_blank()
    ,axis.title.x = element_text(size = 15)
    ,axis.title.y = element_text(size = 15)
  )
g
ggsave(file=  "./02_output/English/average_speed_team_sas.png", plot = g, , dpi = 100, width = 12, height = 18)
