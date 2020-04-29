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
                       ,h1("本サイトの概要"))
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
                         ) 
                        )
                       )
             ,tabPanel("スタッツのヒストグラム"
                       ,h1("スタッツのヒストグラム"))
             ,tabPanel("スタッツの散布図"
                       ,h1("スタッツの散布図"))
             ,navbarMenu("その他"
                         ,tabPanel("自己紹介"
                                   ,h1("自己紹介"))
                         ,tabPanel("ソースコード"
                                   ,p("本サイトのソースコードは下記です")
                                   ,a(href = "https://github.com/pauze-pauze/basketball_data_analysis/tree/master/shiny_comparison_euroleague_NBA", "ソースコードへのリンク"))
                         )
             )
)