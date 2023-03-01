
# import library ----------------------------------------------------------

pacman::p_load(tidyverse, lubridate, summarytools, DataExplorer)


# read data ---------------------------------------------------------------
# QlURL https://qiita.com/yasyas/items/9a717aed3b02b4c4fa38
assign_data <- function(path, type = "csv"){
  names <- list.files(path = path,
                      full.names = F,
                      pattern = paste0("\\.", type, "$")) %>% 
    gsub(paste0(".",type), "",.)
  paths <- list.files(path = path,
                      full.names = T,
                      pattern = paste0(".", type, "$"))
  for(i in 1:length(names)){
    assign(names[i], read_csv(paths[i]), envir = .GlobalEnv)
  }
}
assign_data("./01_data/")