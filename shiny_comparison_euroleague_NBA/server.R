#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("library.R")
options(digits = 1)

data <- readRDS("S1_Data.rds")
  


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #スタッツ比較のテーブル作成
  data_for_stats_comparison_all <- eventReactive(input$do_table,{
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
  #ヒストグラムの件数ver出力
  data_for_stats_hist_cnt <- eventReactive(input$do_hist_cnt,{
    data %>%
      filter(Season == input$stats_hist_season) %>% #シーズン選択
      mutate(
        Tot.FGA = Tot.P2A + Tot.P3A
        ,Tot.FGM = Tot.P2M + Tot.P3M
      ) %>%
      pivot_longer(
        col = -c(1:7)
        ,names_to = "category"
        ,values_to = "stats"
      ) %>%
      mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
      pivot_wider(
        names_from = "category"
        ,values_from = "stats"
      ) %>%
      ggplot(aes_(x = as.name(input$stats_hist_category), fill = as.name("League")))+
      geom_histogram(position = "identity", alpha = 0.5, bins = input$bins + 1)+
      labs(x = str_sub(input$stats_hist_category, start = 5), y = "試合数")+
      scale_fill_brewer(palette = "Set1")+
      theme_classic()
  })
  output$stats_hist_cnt <- renderPlot({
    data_for_stats_hist_cnt()
  })
  
  #ヒストグラムの構成比ver出力
  data_for_stats_hist_ratio <- eventReactive(input$do_hist_cnt,{
    data %>%
      filter(Season == input$stats_hist_season) %>% #シーズン選択
      mutate(
        Tot.FGA = Tot.P2A + Tot.P3A
        ,Tot.FGM = Tot.P2M + Tot.P3M
      ) %>%
      pivot_longer(
        col = -c(1:7)
        ,names_to = "category"
        ,values_to = "stats"
      ) %>%
      mutate(stats = if_else(League == "NBA", stats * 40 / 48, stats)) %>%
      pivot_wider(
        names_from = "category"
        ,values_from = "stats"
      ) %>%
      ggplot(aes_(x = as.name(input$stats_hist_category), fill = as.name("League")))+
      geom_histogram(stat = "density", position = "identity", alpha = 0.5)+
      labs(x = str_sub(input$stats_hist_category, start = 5), y = "構成比")+
      scale_fill_brewer(palette = "Set1")+
      theme_classic()
  })
  output$stats_hist_ratio <- renderPlot({
    data_for_stats_hist_ratio()
  })
  
  #スタッツの時系列比較
  data_for_stats_line <- eventReactive(input$do_line,{ 
    data %>%
    group_by(League, Season) %>%
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
      ,TOV = mean(Tot.TOV)
      ,BLK = mean(Tot.BLK)
    ) %>%
    pivot_longer( # NBAのデータのみ40分換算にするために転置
      -c(League, Season)
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
    ggplot(aes_(x = as.name("Season"), y = as.name(input$stats_line_category), group = as.name("League"),  color = as.name("League")))+
    geom_line(size = 1)+ 
    geom_point(size = 2)+
    scale_color_brewer(palette = "Set1")+
    theme_classic()
  })
  
  output$stats_line <- renderPlot({
    data_for_stats_line()
  })
  
})
