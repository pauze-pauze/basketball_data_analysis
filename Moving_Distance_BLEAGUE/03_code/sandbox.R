# install.packages("ggmap")
# install.packages("geosphere")
# devtools::install_github("rintaromasuda/bleaguer")
library(ggmap)
library(geosphere)
library(tidyverse)
library(bleaguer)
options(digits = 10)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 


# register_google(key = "AIzaSyDhkj88Yz1B6Gd_08NqplHaBe0zl4-xzyQ") # Github上げる時の羽残さないようにする

map <- get_googlemap("waco texas", zoom = 12)
ggmap(map)

b.games

t1 <- geocode("ブレックスアリーナ宇都宮", output = "all")
t1_lat <- t1$results[[1]]$geometry$viewport$northeast$lat # 緯度。北緯と南緯で表し方違うかも
t1_lng <- t1$results[[1]]$geometry$viewport$northeast$lng # 経度。同上

t2 <- geocode("川崎市とどろきアリーナ", output = "all")
t2_lat <- t2$results[[1]]$geometry$viewport$northeast$lat # 緯度。北緯と南緯で表し方違うかも
t2_lng <- t2$results[[1]]$geometry$viewport$northeast$lng # 経度。同上

t1_point <- c(t1_lng, t1_lat)
t2_point <- c(t2_lng, t2_lat)

distGeo(t1_point, t2_point)
