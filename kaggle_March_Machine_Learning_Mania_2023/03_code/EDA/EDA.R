# import library and read data --------------------------------------------

source("./03_code/source/read_library_data.R")
install.packages("DataExplorer", dependencies = TRUE)
library(DataExplorer)
# take a first look -------------------------------------------------------

dfSummary(Cities) %>% view()
create_report(MRegularSeasonCompactResults) #これは出力時のファイル名とかディレクトリの位置を指定できるようにしたほうが良さそう。ついでにfor文で変数名とファイル名を一致させつつダッシュボード作れると楽そう
