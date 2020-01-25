library(tidyverse)
library(openxlsx)
library(scales)

raw <- read_csv("NBA_OTdata.csv")


# count OT times ----------------------------------------------------------

OT_times_3season <-  raw %>%
  group_by(type) %>%
  summarise(GP = sum(GP)/2) #to distinct OT times



# compare  -------------------------------------------------------------

compare_data <- raw %>%
  group_by(type) %>%
  summarise(`FG%` = sum(FGM)/sum(FGA),
            `3FG%` = sum(`3PM`)/sum(`3PA`),
            `FT%` = sum(FTM)/sum(FTA),
            points_per_min = sum(PTS)/sum(MIN),
            FGA_per_min = sum(FGA)/sum(MIN),
            `3PA_per_min` = sum(`3PA`)/sum(MIN),
            AS_per_min = sum(AST)/sum(MIN),
            TO_per_min = sum(TOV)/sum(MIN),
            AS_ratio_FGM = sum(AST)/sum(FGM),
            `2PA` = sum(FGA) - sum(`3PA`), #to make composition ratio of 2PA, 3PA, FTA
            `3PA` = sum(`3PA`),
            FTA = sum(FTA),
            `2P_point` = 2*(sum(FGM) - sum(`3PM`)),#to make composition ratio of 2PM, 3PM, FTM
            `3P_point` = 3*sum(`3PM`),
            FT_point = sum(FTM)
            )
#エクセル出力
compare_data %>%
  write.xlsx("compare_data.xlsx")

# composition ratio -------------------------------------------------------

compare_data %>%
  select(type, 
         `2PA`, 
         `3PA`, 
         FTA 
         ) %>%
  gather(key = attempt_made, value = volume, -type) %>%
  ggplot(aes(x = type, y = volume, fill = attempt_made))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_brewer(palette = "RdYlGn")+
  scale_y_continuous(labels = percent)+
  labs(x = "Game Type", y = "割合", title = "試合全体とオーバータイムの試投数構成比",fill = "種類")+
  theme_bw()+
  ggsave("composition_attempt.png")
  
compare_data %>%
  select(type, 
         `2P_point`,
         `3P_point`,
         FT_point
  ) %>%
  gather(key = attempt_made, value = volume, -type) %>%
  ggplot(aes(x = type, y = volume, fill = attempt_made))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_brewer(palette = "RdYlGn")+
  scale_y_continuous(labels = percent)+
  labs(x = "Game Type", y = "割合", title = "試合全体とオーバータイムの得点構成比",fill = "種類")+
  theme_bw()+
  ggsave("composition_point.png")


