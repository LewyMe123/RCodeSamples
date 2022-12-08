library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(janitor)
library(data.table)

setwd("C:/Users/Nick.Lewis/OneDrive - Inter Miami CF/Nordbord/Raw Trials")

directory <- getwd()
files_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)

teamNord <- files_list %>%
        map_df(.x = ., .f = function(.x){
                directory <- getwd()
                Player_Nordic <- read_csv(.x, skip = 4, col_select = c(1,2,3))
                Player_Vitals <- read_csv(.x, n_max = 1,col_select = c(1,2))
                
                Player_Nordic <- clean_names(Player_Nordic)
                
                
                
                Nordic_Round <- Player_Nordic %>%
                mutate(seconds = round(as.numeric(seconds), 2)) %>%
                group_by(seconds) %>%
                summarize_at(vars(left_force:right_force), .funs = mean) %>%
                mutate(left_force = round(as.numeric(left_force), 2),
                       right_force = round(as.numeric(right_force), 2),
                       avg_force = round(rowMeans(.[,2:3]), 2),
                       effort = ifelse(avg_force >= 100, "Effort", "Rest"))
        
                
                Nordic_Full <- bind_cols(Player_Vitals, Nordic_Round) %>%
                        mutate(Date = mdy(Date))
                
                Nordic_Full_Round <- Nordic_Full %>%
                        mutate(TrialIndex = (rleid(Name, effort)/2)) %>%
                        filter(effort == "Effort") %>%
                        group_by(Date, TrialIndex) %>%
                        mutate(duration = seconds - min(seconds),
                               durLength = max(duration))
#                        mutate(zeroedTime = ifelse((shift(TrialIndex, n=1L, type=c("lag")))!= TrialIndex, seconds, 0),
#                               cumalativeTime = aggregate(seconds, by = list(as.factor(TrialIndex)), FUN = cumsum))        

        })           
 
            
