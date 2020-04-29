#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(data.table)
options(digits = 1)

data <- readRDS("S1_Data.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #スタッツ比較のテーブル作成
  data_for_stats_comparison_all <- eventReactive(input$do,{
    data %>%
    filter(Season == input$stats_comparison_all) %>%
    group_by(League) %>%
    summarise(
      FGA = mean(Tot.P2A) + mean(Tot.P3A)
      ,FGM = mean(Tot.P2M) + mean(Tot.P3M)
      ,P2A = mean(Tot.P2A)
      ,P2M = mean(Tot.P2M)
      ,P3A = mean(Tot.P3A)
      ,P3M = mean(Tot.P3M)
      ,FTA = mean(Tot.FTA)
      ,FTM = mean(Tot.FTM)
      ,TR = mean(Tot.TRB)
      ,OR = mean(Tot.ORB)
      ,DR = mean(Tot.DRB)
      ,AS = mean(Tot.AST)
      ,TO = mean(Tot.TOV)
      ,BLK = mean(Tot.BLK)
    ) %>%
    pivot_longer( # NBAのデータのみ40分換算にするために転置
      -League
      ,names_to = "category"
      ,values_to = "stats"
    ) %>%
    mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
    pivot_wider( # %の数値を作成
      names_from = category
      ,values_from = stats
    )%>%
    mutate(
      FG_per = 100 * FGM / FGA
      ,P2_per = 100 * P2M / P2A
      ,P3_per = 100 * P3M / P3A
      ,FT_per = 100 * FTM / FTA
    ) %>%
    select( #流れ的に見やすいように並び順を入れ替え
      FGA, FGM, FG_per, P2A, P2M, P2_per, P3A, P3M, P3_per, FTA, FTM, FT_per, everything()
    ) %>%
    pivot_longer(
      -League
      ,names_to = "category"
      ,values_to = "stats"
    ) %>%
    pivot_wider(
      names_from = "League"
      ,values_from = "stats"
    ) %>%
    mutate(
      Euroleague = format(Euroleague, nsmall = 1)
      ,NBA = format(NBA, nsmall =  1)
    )
  })
  
  output$stats_comparison_all_table <- renderTable({
    data_for_stats_comparison_all()
  })
  
  
  
})
