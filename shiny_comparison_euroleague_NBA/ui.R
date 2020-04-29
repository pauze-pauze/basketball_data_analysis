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
                       ,h1("スタッツ比較"))
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