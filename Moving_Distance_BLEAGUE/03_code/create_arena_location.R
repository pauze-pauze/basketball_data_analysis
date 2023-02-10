library(ggmap)
library(tidyverse)
library(bleaguer)


# 過去の試合を行ったアリーナの名称を抽出
arena <- b.games %>%
  distinct(Arena)%>%
  mutate(Arena = as.character(Arena)) # factor型だと後が面倒に思えたので。いらない処理かも。


# Google Map APIのハッシュ値読み込む
register_google(key = "hoge") # 実行時はハッシュ値入力


# アリーナごとの緯度と経度を取得

arena_locate <- arena %>%
  mutate(
    lat = NA
    ,lng = NA
  )

for (i in 1:nrow(arena)) {
  t <- geocode(as.character(arena_locate[i, 1]) , output = "all")
  t_lat <- t$results[[1]]$geometry$viewport$northeast$lat # 緯度。北緯と南緯で表し方違うかも
  t_lng <- t$results[[1]]$geometry$viewport$northeast$lng # 経度
  arena_locate[i, 2] <- if_else(is.null(t_lat), 0, t_lat) # 0のやつはデータ取れなかったやつ
  arena_locate[i, 3] <- if_else(is.null(t_lng), 0, t_lng) # 0のやつはデータ取れなかったやつ
}

## データ取れなかったアリーナ取得
arena_locate %>%
  filter(lat == 0)
# 1 横浜文化体育館           0     0 35.4407307,139.6341344 https://www.google.co.jp/maps/place/%E6%97%A7%E6%A8%AA%E6%B5%9C%E6%96%87%E5%8C%96%E4%BD%93%E8%82%B2%E9%A4%A8%E8%B7%A1/@35.4407307,139.6341344,17z/data=!4m10!1m2!2m1!1z5qiq5rWc5paH5YyW5L2T6IKy6aSo!3m6!1s0x60185df0c1886967:0x7a3e38fface47d6a!8m2!3d35.4413437!4d139.6364986!15sChXmqKrmtZzmlofljJbkvZPogrLppKiSARNoaXN0b3JpY2FsX2xhbmRtYXJr4AEA!16s%2Fg%2F11k4xxl3rm?hl=ja
# 2 彦根市民体育センター     0     0 35.2468899,136.244827 https://www.google.com/maps/place/%E3%83%97%E3%83%AD%E3%82%B7%E3%83%BC%E3%83%89%E3%82%A2%E3%83%AA%E3%83%BC%E3%83%8AHIKONE(%E5%BD%A6%E6%A0%B9%E5%B8%82%E3%82%B9%E3%83%9D%E3%83%BC%E3%83%84%E3%83%BB%E6%96%87%E5%8C%96%E4%BA%A4%E6%B5%81%E3%82%BB%E3%83%B3%E3%82%BF%E3%83%BC)/@35.2468899,136.244827,15z/data=!4m5!3m4!1s0x0:0xd06e6f196d3cded9!8m2!3d35.2468899!4d136.244827
# 3 DIADORAアリーナ          0     0 35.3579172,136.799501 https://www.google.com/maps/place/%E3%81%84%E3%81%A1%E3%81%84%E4%BF%A1%E9%87%91%E3%82%A2%E3%83%AA%E3%83%BC%E3%83%8A/@35.3579172,136.799501,15z/data=!4m5!3m4!1s0x0:0xf6f781d1392bbecb!8m2!3d35.3579172!4d136.799501

arena_locate_final <- arena_locate
arena_locate_final[arena_locate_final$Arena == "横浜文化体育館",2] <- 35.4407307
arena_locate_final[arena_locate_final$Arena == "横浜文化体育館",3] <- 139.6341344
arena_locate_final[arena_locate_final$Arena == "彦根市民体育センター",2] <- 35.2468899
arena_locate_final[arena_locate_final$Arena == "彦根市民体育センター",3] <- 136.244827
arena_locate_final[arena_locate_final$Arena == "DIADORAアリーナ",2] <- 35.3579172
arena_locate_final[arena_locate_final$Arena == "DIADORAアリーナ",3] <- 136.799501

write.csv(arena_locate_final, "./01_data/arena_location.csv")