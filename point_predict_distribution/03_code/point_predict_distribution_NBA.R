library(nbastatR)
library(future)
plan(multiprocess) 
t <- game_logs(seasons = 2021)

# 選手単位のデータになっているのでgame単位にする必要あり