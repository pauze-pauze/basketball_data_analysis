
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


# load data ---------------------------------------------------------------

df <- read_csv("./01_Data/NBA_player_height_weight_data.csv")
skim(df)




# manipulate data ---------------------------------------------------------

# transform height and weight into cm and kg
ft_to_cm <- 30.48
inch_to_cm <- 2.54
lbs_to_kg <- 0.453592
df <- df %>%
  mutate(
    height_cm = height_1 * ft_to_cm + height_2 * inch_to_cm
    ,weight_kg = weight * lbs_to_kg
  )

df_plot <- df %>%
   mutate(plot_name = as.character(player))
selected_player <- "Yuta Watanabe"
df_plot <- df_plot %>%
  mutate(plot_name = if_else(plot_name %in% selected_player, plot_name, ""))
df_plot %>%
  ggplot(aes(x = height_cm, y = weight_kg, label = plot_name))+
  geom_point()+
  geom_text(size = 4)+
  theme_bw()


