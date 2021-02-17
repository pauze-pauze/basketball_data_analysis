#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("library.R")

shinyUI(
  navbarPage("Comparison of stats between NBA and Euroleague"
             ,theme = shinythemes::shinytheme("paper")
             ,tabPanel("About"
                       ,h3("About")
                       ,br()
                       ,p(h6("This web application is for interactive comparison of Euroleague and NBA stats")
                          
                          ,h6("In the ", strong('"Compare Season"'), " tab, you can compare boxscore both NBA and Euroleague by season.")
                          ,h6("In the ", strong('"Compare Histogram"')," and ",strong('"Compare Time series transition"'), " tab, you can choose stats and make gragh.")
                          ,h6("If you are interested in author of this web application or source code, please check ", strong('"Author and Code"'), " tab")
                          ,helpText("※It is not ensured about the accuracy and validity")
                          ,br()
                          ,br()
                          ,"The data is from this paper")
                       ,a(href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0223524", "Trends in NBA and Euroleague basketball: Analysis and comparison of statistical data from 2000 to 2017")
                       ,br()
                       ,br()
                       ,p(h6("What the abbreviations in this web application mean is here.")
                          ,h6("FGA : Field Goals Attempted")
                          ,h6("FGM : Field Goals Made")
                          ,h6("FG_per/FG% : Field Goal Percentage")
                          ,h6("P2A : 2 Point Field Goals Attempted")
                          ,h6("P2M : 2 Point Field Goals Made")
                          ,h6("P2_per/P2% :  2 Point Field Goal Percentage")
                          ,h6("P3A : 3 Point Field Goals Attempted")
                          ,h6("P3M : 3 Point Field Goals Made")
                          ,h6("P3_per/P3% :  3 Point Field Goal Percentage")
                          ,h6("FTA : Free Throws Attempted")
                          ,h6("FTM : Free Throws Made")
                          ,h6("FT_per/FT% : Free Throw Percentage")
                          ,h6("TRB : Total Rebounds")
                          ,h6("ORB : Offensive Rebounds")
                          ,h6("DRB : Defensive Rebounds")
                          ,h6("AST : Assists")
                          ,h6("TOV : Turnovers")
                          ,h6("BLK : Blocks")
                          )
                       )
             ,tabPanel("Compare Season"
                       ,h3("Compare season stats")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_comparison_all"
                             ,label = h5(strong("Choose season"))
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
                           ,actionButton("do_table", "See this season")
                         )
                         ,mainPanel(
                           tableOutput("stats_comparison_all_table")
                           ,p("※NBA stats is converted into per 40 minutes.")
                           ,p("※Overtime is not considered.")
                           ,p("※The definition of stats between NBA and Euroleague is are partially different.") 
                         ) 
                        )
                       )
             ,tabPanel("Compare Histogram"
                       ,h3("Compare Histogram")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_hist_season"
                             ,label = h5(strong("Choose season"))
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
                            ,label = h5(strong("Choose stats"))
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
                            ,h5(strong("Select the number of bins."))
                            ,min = 1
                            ,max = 100
                            ,value = 30
                          )
                          ,actionButton("do_hist_cnt", "Output under the above condtions.")
                         )
                         ,mainPanel(
                           tabsetPanel(
                             type = "tabs"
                             ,tabPanel(
                               "count distributuion"
                               ,plotOutput("stats_hist_cnt", width = 800, height = 500)
                             )
                             ,tabPanel(
                               "Probability density distribution"
                               ,plotOutput("stats_hist_ratio", width = 800, height = 500)
                             )
                           )
                           ,p("※NBA stats is converted into per 40 minutes.")
                           ,p("※Overtime is not considered.")
                           ,p("※The definition of stats between NBA and Euroleague is are partially different.") 
                         )
                       ))
             ,tabPanel("Compare Time series transition"
                       ,h3("Compare Time series transition")
                       ,sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "stats_line_category"
                             ,label = h5(strong("Choose stats"))
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
                           ,actionButton("do_line", "Output the stats")
                         )
                         ,mainPanel(
                           plotOutput("stats_line", width = 800, height = 500)
                           ,p("※NBA stats is converted into per 40 minutes.")
                           ,p("※Overtime is not considered.")
                           ,p("※The definition of stats between NBA and Euroleague is are partially different.") 
                         )
                       ))
             ,navbarMenu("Author and Code"
                         ,tabPanel("Self introduction"
                                   ,h3("Self introduction")
                                   ,br()
                                   ,h6("I,m H.YAMA, a basketball data analyst in Japan.")
                                   ,h6("I am fond of basketball data analytics and would like to write articles about basketball data analysis (NBA, B.LEAGUE-Japanese professinal basketball league- etc). ")
                                   ,h6("There are my accounts below.")
                                   ,a(href = "https://twitter.com/HYAMA_1160", "Twitter account")
                                   ,br()
                                   ,a(href = "https://yamahisa.medium.com/", "Medium")
                                   )
                         ,tabPanel("Source code"
                                   ,h6("The source code of this web application is here")
                                   ,a(href = "https://github.com/pauze-pauze/basketball_data_analysis/tree/master/shiny_comparison_euroleague_NBA", "Link"))
                         )
             )
)