
# -------------------------------------------------------------------------

# You don't need to run this script manually. It's called by `replication-script.R`

# -------------------------------------------------------------------------






library(readr)
library(tidyverse)
library(readxl)


read_retriever_xlsx <- function(file){
  
  dt <- read_xlsx(file, skip = 2)
  
  
  dt <- dt %>% 
    filter(!is.na(Søk), 
           Søk != "Totalt") %>%   # Fjerner en rad med bare NA
    select(-last_col())
  
  
  dt <- dt %>% 
    pivot_longer(names_to = "month",
                 values_to = "articles",
                 cols = -Søk) %>% 
    rename(term = Søk) %>%
    mutate(term = str_remove(term, pattern = '"$'),
           term = str_remove(term, pattern = '\"')) %>% 
    mutate(month = lubridate::ymd(parse_date_time(month, "%m/%y"))) %>% 
    filter(!is.na(articles))
  
  dt
  
}
