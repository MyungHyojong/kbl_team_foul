result_Home <- rbind(result_Home1, result_Home2)
result_Away <- rbind(result_Away1, result_Away2)

result_Home <- result_Home %>%
  mutate(min = sapply(strsplit(time, ":"), function(x) x[1])) %>%
  mutate(sec = sapply(strsplit(time, ":"), function(x) x[2])) %>%
  mutate(min = as.numeric(min)) %>%
  mutate(sec = as.numeric(sec)) %>%
  mutate(time_num = min + sec/60)

result_Away <- result_Away %>%
  mutate(min = sapply(strsplit(time, ":"), function(x) x[1])) %>%
  mutate(sec = sapply(strsplit(time, ":"), function(x) x[2])) %>%
  mutate(min = as.numeric(min)) %>%
  mutate(sec = as.numeric(sec)) %>%
  mutate(time_num = min + sec/60)


#데이터 작을 떄 -> 직관적 dplyr
avg_time_Home_year <- result_Home %>% group_by(year) %>% summarise(
  avg_team_foul_time = mean(time_num)
)
avg_time_Away_year <- result_Away %>% group_by(year) %>% summarise(
  avg_team_foul_time = mean(time_num)
)


#데이터 클 때 -> data.table
result_Home <- as.data.table(result_Home)
result_Home[,.(avg_time = round(mean(time_num),2)),keyby = 'year']
