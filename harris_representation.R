library(tidyverse)

in.dir <- "/projects/DESMOND/data_nb/Renee/Eviction Tracker/OVERHAUL/CLEANED DATA/CURRENT/"

harris_2020_2021 <- readRDS(paste0(in.dir, "harris_2020_2021.rds"))

harris_test <- harris_2020_2021 %>% 
  group_by(xcasenum) %>% 
  mutate(defendant_represented = if_else(any(!is.na(`Defendant Atty Name`)), 1, 0),
         plaintiff_represented = if_else(any(!is.na(`Plaintiff Atty Name`)), 1, 0)) %>% 
  ungroup()


harris_rep <- harris_test %>% 
  filter(include == T) %>%  
  group_by(xfileyear, xfilemonth) %>% 
  summarize(def_represented = sum(defendant_represented == 1), 
            plaintiff_represented = sum(plaintiff_represented == 1), 
            n_total = n(), 
            pct_def_represented = round(100 * def_represented/n_total, 2), 
            pct_plaintiff_represented = round(100 * plaintiff_represented/n_total, 2))

Test
