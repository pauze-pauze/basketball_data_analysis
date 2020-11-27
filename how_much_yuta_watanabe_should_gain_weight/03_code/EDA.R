
# import libraries --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)


# load data ---------------------------------------------------------------

df <- read_csv("./01_Data/NBA_player_height_weight_data.csv")
skim(df)
