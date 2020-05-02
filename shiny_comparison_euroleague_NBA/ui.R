#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

shinyUI(
  navbarPage("NBAとユーロリーグのスタッツ比較"
             ,theme = shinytheme("paper")
             ,tabPanel("本サイトの概要"
                       ,h3("本サイトの概要")
                       ,br()
                       ,p(h6("本サイトはユーロリーグとNBAのスタッツをインタラクティブに比較するためのサイトです")
                          ,h6(strong("「スタッツ比較」"),"というタブでは、シーズンごとのBoxscoreをユーロリーグとNBAで比較できるようになっています")
                          ,h6(strong("「スタッツのヒストグラム」"),"及び",strong("「スタッツの時系列推移」"),"については、見たいスタッツを選択してリーグごとにグラフ化することができます")
                          ,h6("本サイトの作成者の情報やソースコードについては",strong("「その他」"),"タブをご参照ください")
                          ,helpText("※数値の正確性や妥当性については保証しません。")
                          ,br()
                          ,br()
                          ,"データは下記論文内のものを利用しています")
                       ,a(href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0223524", "Trends in NBA and Euroleague basketball: Analysis and comparison of statistical data from 2000 to 2017")
                       ,br()
                       ,br()
                       ,p(h6("本サイトで利用している略称の意味は下記です")
                          ,h6("FGA : FGの試投数")
                          ,h6("FGM : FGの成功数")
                          ,h6("FG_per/FG% : FGの成功率")
                          ,h6("P2A : 2点シュートの試投数")
                          ,h6("P2M : 2点シュートの成功数")
                          ,h6("P2_per/P2% : 2点シュートの成功率")
                          ,h6("P3A : 3点シュートの試投数")
                          ,h6("P3M : 3点シュートの成功数")
                          ,h6("P3_per/P3% : 3点シュートの成功率")
                          ,h6("FTA : フリースローの試投数")
                          ,h6("FTM : フリースローの成功数")
                          ,h6("FT_per/FT% : フリースローの成功率")
                          ,h6("TRB : 総リバウンド数")
                          ,h6("ORB : オフェンスリバウンド数")
                          ,h6("DRB : ディフェンスリバウンド数")
                          ,h6("AST : アシスト数")
                          ,h6("TOV : ターンオーバー数")
                          ,h6("BLK : ブロック数")
                          )
                       )
             ,tabPanel("スタッツ比較"
                       ,h3("スタッツ比較")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_comparison_all"
                             ,label = h5(strong("シーズンを選択してください"))
                             ,choices = list(
                               "2000-01" = "2000-01"
                               ,"2001-02" = "2001-02"
                               ,"2002-03" = "2002-03"
                               ,"2003-04" = "2003-04"
                               ,"2004-05" = "2004-05"
                               ,"2005-06" = "2005-06"
                               ,"2006-07" = "2006-07"
                               ,"2007-08" = "2007-08"
                               ,"2008-09" = "2008-09"
                               ,"2009-10" = "2009-10"
                               ,"2010-11" = "2010-11"
                               ,"2011-12" = "2011-12"
                               ,"2012-13" = "2012-13"
                               ,"2013-14" = "2013-14"
                               ,"2014-15" = "2014-15"
                               ,"2015-16" = "2015-16"
                               ,"2016-17" = "2016-17"
                             )
                             #,selected = 
                             #,multiple = 
                           )
                           ,actionButton("do_table", "このシーズンを見る")
                         )
                         ,mainPanel(
                           tableOutput("stats_comparison_all_table")
                           ,p("※NBAのスタッツは40分換算")
                           ,p("※元データの都合で延長の考慮はしていません。")
                           ,p("※スタッツの定義はNBAとEuroleagueで異なる点があるので、予めご了承ください。") 
                         ) 
                        )
                       )
             ,tabPanel("スタッツのヒストグラム"
                       ,h3("スタッツのヒストグラム")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_hist_season"
                             ,label = h5(strong("シーズンを選択してください"))
                             ,choices = list(
                               "2000-01" = "2000-01"
                               ,"2001-02" = "2001-02"
                               ,"2002-03" = "2002-03"
                               ,"2003-04" = "2003-04"
                               ,"2004-05" = "2004-05"
                               ,"2005-06" = "2005-06"
                               ,"2006-07" = "2006-07"
                               ,"2007-08" = "2007-08"
                               ,"2008-09" = "2008-09"
                               ,"2009-10" = "2009-10"
                               ,"2010-11" = "2010-11"
                               ,"2011-12" = "2011-12"
                               ,"2012-13" = "2012-13"
                               ,"2013-14" = "2013-14"
                               ,"2014-15" = "2014-15"
                               ,"2015-16" = "2015-16"
                               ,"2016-17" = "2016-17"
                             )
                          )
                          ,selectInput(
                            inputId = "stats_hist_category"
                            ,label = h5(strong("見たいスタッツを選択してください"))
                            ,choices = list(
                              "FGA" = "Tot.FGA"
                              ,"FGM" = "Tot.FGM"
                              ,"P2A" = "Tot.P2A"
                              ,"P2M" = "Tot.P2M"
                              ,"P3A" = "Tot.P3A"
                              ,"P3M" = "Tot.P3M"
                              ,"FTA" = "Tot.FTA"
                              ,"FTM" = "Tot.FTM"
                              ,"TRB" = "Tot.TRB"
                              ,"ORB" = "Tot.ORB"
                              ,"DRB" = "Tot.DRB"
                              ,"AST" = "Tot.AST"
                              ,"TOV" = "Tot.TOV"
                              ,"BLK" = "Tot.BLK"
                            )
                            ,selected = "FGA"
                          )
                          ,sliderInput(
                            "bins"
                            ,h5(strong("ビンの数を選択してください"))
                            ,min = 1
                            ,max = 100
                            ,value = 30
                          )
                          ,actionButton("do_hist_cnt", "上記の条件で出力する")
                         )
                         ,mainPanel(
                           tabsetPanel(
                             type = "tabs"
                             ,tabPanel(
                               "件数の分布"
                               ,plotOutput("stats_hist_cnt", width = 800, height = 500)
                             )
                             ,tabPanel(
                               "件数の密度分布"
                               ,plotOutput("stats_hist_ratio", width = 800, height = 500)
                             )
                           )
                           ,p("※NBAのスタッツは40分換算")
                           ,p("※元データの都合で延長の考慮はしていません。")
                           ,p("※スタッツの定義はNBAとEuroleagueで異なる点があるので、予めご了承ください。") 
                           ,p("※「ビン」は出力された棒の数を表しています。数が多いほど棒の幅が小さくなります。また、件数の密度分布の表示には影響しません")
                         )
                       ))
             ,tabPanel("スタッツの時系列推移"
                       ,h3("スタッツの時系列推移")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_line_category"
                             ,label = h5(strong("見たいスタッツを選択してください"))
                             ,choices = list(
                               "FGA" = "FGA"
                               ,"FGM" = "FGM"
                               ,"FG%" = "FG_per"
                               ,"P2A" = "P2A"
                               ,"P2M" = "P2M"
                               ,"P2%" = "P_per"
                               ,"P3A" = "P3A"
                               ,"P3M" = "P3M"
                               ,"P3%" = "P3_per"
                               ,"FTA" = "FTA"
                               ,"FTM" = "FTM"
                               ,"FT%" = "FT_per"
                               ,"TR" = "TR"
                               ,"OR" = "OR"
                               ,"DR" = "DR"
                               ,"AS" = "AS"
                               ,"TOV" = "TOV"
                               ,"BLK" = "BLK"
                             )
                             ,selected = "FGA"
                           )
                           ,actionButton("do_line", "上記のスタッツを出力する")
                         )
                         ,mainPanel(
                           plotOutput("stats_line", width = 800, height = 500)
                           ,p("※NBAのスタッツは40分換算")
                           ,p("※元データの都合で延長の考慮はしていません。")
                           ,p("※スタッツの定義はNBAとEuroleagueで異なる点があるので、予めご了承ください。") 
                         )
                       ))
             ,navbarMenu("その他"
                         ,tabPanel("自己紹介"
                                   ,h3("自己紹介")
                                   ,br()
                                   ,h6("「バスケのデータ分析」というアカウント名で活動しています")
                                   ,h6("BリーグやNBAのデータを利用したスタッツ分析や可視化、その他バスケのデータ分析に関する文献の紹介などをしています。")
                                   ,h6("分析テーマや、こういう仕事一緒にやってみませんかなどなど募集してます！")
                                   ,h6("ご連絡などは下記アカウントまでお願いいいたします！")
                                   ,a(href = "https://twitter.com/b__s__k__t", "Twitterアカウント")
                                   ,br()
                                   ,a(href = "https://note.com/b__s__k__t", "ウェブサイト(note)")
                                   )
                         ,tabPanel("ソースコード"
                                   ,h6("本サイトのソースコードは下記です")
                                   ,a(href = "https://github.com/pauze-pauze/basketball_data_analysis/tree/master/shiny_comparison_euroleague_NBA", "ソースコードへのリンク"))
                         )
             )
)