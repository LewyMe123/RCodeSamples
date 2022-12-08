library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)

setwd("C:/Users/Nick.Lewis/OneDrive - Nick.Lewis/Jumps 2022/Daily Jumps/Raw Jump Trials")

directory <- getwd()
files_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)

clubJumps <- files_list %>%
        map_df(.x = ., .f = function(.x){
                directory <- getwd()
                Player_Jump <- read_csv(.x, skip = 9, col_select = c(1,2,3,5,6,7,8,9))
                Player_Vitals <- read_csv(.x, n_max = 1)
                
                
                Jump_Round2 <- Player_Jump %>%
                        mutate(Time = round(as.numeric(Time), 2))%>%
                        group_by(Time) %>%
                        summarize_at(vars(Left:Impulse), .funs = mean) %>%
                        mutate(Time = Time - first(Time),
                               Left = round(as.numeric(Left), 2),
                               Right = round(as.numeric(Right), 2),
                               Acceleration = round(as.numeric(Acceleration), 2),
                               Velocity = round(as.numeric(Velocity), 2),
                               Height = round(as.numeric(Height), 2),
                               Power = round(as.numeric(Power), 2),
                               Impulse = round(as.numeric(Impulse), 2)) %>%
                        pivot_longer(!c(Time, Acceleration, Velocity,
                                        Height, Power, Impulse),
                                     names_to = "Leg", values_to = "Newtons")
                
                
                FileName <- .x %>%
                        basename() %>%
                        as.character() %>%
                        data.frame()
                
                colnames(FileName)[1] <- "FileName"
                
                Vitals_Weight <- Player_Vitals %>%
                        select(2) %>%
                        slice(1) %>%
                        unlist(., use.names = FALSE)
                
                Vitals_All <- data.frame(FileName, Vitals_Weight)
                
                Player_Binded_Final <- bind_cols(Vitals_All, Jump_Round2) %>%
                        separate(col = FileName, into = c("Name", "Jump_Type", "Date", "TimeofDay", "Trial"),
                                 sep = "-") %>%
                        mutate(Date = ymd(Date),
                               Trial = as.character(gsub("\\..*", "", Trial)))
                
                
                
        })

write.table(clubJump,
            file = "C:/Users/Nick.Lewis/OneDrive - Nick.Lewis/Jumps 2022/RawJumpTrials.csv",
            append = TRUE, row.names = FALSE, col.names = FALSE,  sep = ",")

