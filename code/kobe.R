# Library
library(tidyverse) # Data wrangle
library(lubridate) # Date

# Working directory
working_dir <- "C:/Users/Public/medium_kobe/"
file_name <- "kobe.csv"
file_dir <- paste0(working_dir, file_name)
data <- read.csv(file_dir, stringsAsFactors = F)

# Pre-setting
data <- mutate(data, shot_point = as.numeric(substring(shot_type, 1, 1)), 
               shot_gain = shot_point * shot_made_flag,
               total_seconds_remaining = minutes_remaining * 60 + seconds_remaining)
data[, "game_date"] <- as.Date(data[, "game_date"] ,"%Y/%m/%d")


# Na-checking
apply(data, 2, FUN = function(x) sum(is.na(x)))

#(1)
data_1 <- 
  data %>%
  select(opponent, shot_type, shot_made_flag) %>% 
  filter(opponent == "HOU") %>% 
  group_by(shot_type) %>% 
  summarise(FGp = sum(shot_made_flag)/length(shot_made_flag),
            shoot_number = n())
#(2)
data_2 <- 
  data %>%
  select(game_id, opponent, shot_gain) %>%
  group_by(game_id, opponent) %>% 
  summarise(game_score = sum(shot_gain)) %>% 
  group_by(opponent) %>% 
  summarise(mean_score = mean(game_score)) %>% 
  arrange(mean_score)

#(3)
data_3 <- 
  data %>%
  select(game_id, opponent, playoffs, period, total_seconds_remaining, shot_gain) %>%
  filter(playoffs == 1, period == 4, total_seconds_remaining <= 3*60) %>% 
  group_by(game_id, opponent) %>% 
  summarise(total_scores = sum(shot_gain)) %>% 
  arrange(desc(total_scores))

#(4)
data_4 <-
  data %>%
  select(season, opponent, playoffs, period, total_seconds_remaining, action_type, shot_made_flag) %>%
  filter(playoffs == 1, period == 4, total_seconds_remaining <= 1*60, action_type == "Jump Shot") %>% 
  group_by(season) %>% 
  summarise(FGp = sum(shot_made_flag)/ length(shot_made_flag)) %>% 
  print.data.frame()

#(5)
continuous_detect <- function (x) {
  #check
  if (class(x)[1] == "tbl_df"){
    x <- pull(x)
  }
  ini <- 1; con_df <- c(); end <- length(x)
  while (ini != end) {
    if ( x[ini] == 0) {
      ini <- ini + 1
    }else{
      start <- ini
      while (x[ini] == 1 ) {
        ini <- ini + 1 
      }
      df <- data.frame(start_index = start, end_index = ini - 1, interval = ini - start ) 
      con_df <- rbind(con_df, df)
    }
  }
  con_df
}
data_5_intme <- 
  data %>%
  select(game_id, game_date, shot_made_flag) %>% 
  group_by(game_id, game_date) %>% 
  summarise(FGp = sum(shot_made_flag)/ n()) %>% 
  arrange(game_date) %>% 
  mutate(meet_goal = if_else(FGp < 0.33, 0, 1) )
dt <- continuous_detect(data_5_intme[,4])
date <- dt %>% arrange(desc(interval)) %>% head(3)
calender <- lapply(1:nrow(date), function(i) as.character(c(data_5_intme$game_date[date[i,1]], data_5_intme$game_date[date[i,2]])))
data_5 <- as.data.frame(cbind(do.call(rbind,calender), date$interval))
colnames(data_5) <- c("starting_date", "ending_date", "interval")
data_5
#(6)
FsurpassS_id <-
  data %>%
  select(game_date, period, shot_gain) %>% 
  mutate(half = ifelse(period <= 2, "first", "second")) %>% 
  group_by(game_date, half) %>% 
  summarise(half_scores = sum(shot_gain)) %>% 
  group_by(game_date) %>%
  summarise(gains_diff =  half_scores[1] - half_scores[2]) %>% 
  filter(gains_diff > 0)
data_6 <- 
  data %>% 
  select(game_date, period, opponent, shot_gain, shot_made_flag) %>% 
  filter(game_date %in% FsurpassS_id$game_date, period %in% 1:4) %>% 
  group_by(game_date, opponent) %>% 
  summarise(FGp = sum(shot_made_flag)/n(), 
            points = sum(shot_gain)) %>% 
  inner_join(FsurpassS_id) %>% 
  arrange(FGp)

#(7) 
continuous_time <- 3
continuous_max_intvl <- function (x) {
  ini <- 1; end <- length(x); interval <- 0;interval_list <- c()
  if ( end < 2) {
    if ( x[1] == 0){
      interval <- interval + 1
    }
    interval
  }else{
    interval_list <-  c(interval_list, interval)
    while (ini != end + 1 ) {
      if ( x[ini] == 1) {
        ini <- ini + 1
      }else{
        interval <- 0
        while ((x[ini] == 0) & (ini != end + 1)){
          interval <- interval + 1
          ini <- ini + 1
        }
        interval_list <- c(interval_list, interval)
        }
    }
    max(interval_list)
  }
}
data_7_ini <- 
  data %>% 
  select(game_date, period, minutes_remaining, shot_made_flag) %>% 
  arrange(game_date, period) %>% 
  group_split(game_date)
total_intvl <-unlist(lapply(1:length(data_7_ini), function(i) continuous_max_intvl(data_7_ini[[i]]$shot_made_flag) ))
total_date <- unlist(lapply(1:length(data_7_ini), function(i) as.character(data_7_ini[[i]]$game_date[1])))
total_date <- as.Date(total_date ,"%Y-%m-%d")
date_intvl <- data.frame(game_date = total_date, interval = total_intvl)
index <- date_intvl %>% arrange(desc(interval)) %>% head(continuous_time)
data_7 <-
  data %>% 
  select(game_date, opponent, shot_gain) %>% 
  filter(game_date %in% index$game_date) %>% 
  group_by(game_date, opponent) %>% 
  summarise(points = sum(shot_gain)) %>% 
  inner_join(index) %>% 
  arrange(desc(interval))
  