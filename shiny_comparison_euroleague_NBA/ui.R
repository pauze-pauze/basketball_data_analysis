#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
  navbarPage("NBAとユーロリーグのスタッツ比較"
             ,tabPanel("本サイトの概要"
                       ,h1("本サイトの概要")
                       ,p("本サイトはユーロリーグとNBAのスタッツをインタラクティブに比較するためのサイトです"
                          ,br()
                          ,"「スタッツ比較」というタブでは、シーズンごとのBoxscoreをEuroleagueとNBAで比較できるようになっています"
                          ,br()
                          ,"「スタッツのヒストグラム」及び「スタッツの散布図」については、見たいスタッツを選択してグラフ化することができます"
                          ,br()
                          ,"本サイトの作成者の情報やソースコードについては「その他」タブをご参照ください"
                          ,"※数値の正確性や妥当性については保証しません。"
                          ,"データは下記論文内のものを利用しています")
                       ,a(href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0223524", "Trends in NBA and Euroleague basketball: Analysis and comparison of statistical data from 2000 to 2017"))
             ,tabPanel("スタッツ比較"
                       ,h1("スタッツ比較")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_comparison_all"
                             ,label = h3("シーズンを選択してください")
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
                           ,actionButton("do", "このシーズンを見る")
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
                       ,h1("スタッツのヒストグラム"))
             ,tabPanel("スタッツの散布図"
                       ,h1("スタッツの散布図"))
             ,navbarMenu("その他"
                         ,tabPanel("自己紹介"
                                   ,h1("自己紹介")
                                   ,p("「バスケのデータ分析」というアカウント名で活動しています"
                                      ,br()
                                      ,"BリーグやNBAのデータを利用したスタッツ分析や可視化、その他バスケのデータ分析に関する文献の紹介などをしています。"
                                      ,br()
                                      ,"分析テーマや、こういう仕事一緒にやってみませんかなどなど募集してます！"
                                      ,br()
                                      ,"ご連絡などは下記アカウントまでお願いいいたします！")
                                   ,a(href = "https://twitter.com/b__s__k__t", "Twitterアカウント")
                                   ,br()
                                   ,a(href = "https://note.com/b__s__k__t", "ウェブサイト(note)")
                                   )
                         ,tabPanel("ソースコード"
                                   ,p("本サイトのソースコードは下記です")
                                   ,a(href = "https://github.com/pauze-pauze/basketball_data_analysis/tree/master/shiny_comparison_euroleague_NBA", "ソースコードへのリンク"))
                         )
             )
)