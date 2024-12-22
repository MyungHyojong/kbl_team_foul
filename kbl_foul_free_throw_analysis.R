library(dplyr); library(stringr)

data_time_rank <- readRDS('data_time_rank.rds')
url_Home_Away_data <- readRDS('url_Home_Away_data.RDS')
kbl_broadcast <- readRDS('kbl_broadcast.rds')

url_Home_Away_data <- url_Home_Away_data %>% mutate(text2 = ifelse(is.na(text2), text1, text2)) %>%
  mutate(text1 = ifelse(is.na(text1),'H','A'))

#전체 자유투 평균
tot_foul_count <- kbl_broadcast %>%
  group_by(url, Q) %>%
  summarise(
    home_count_tot = sum(str_detect(Home, "파울자유투"), na.rm = TRUE),
    away_count_tot = sum(str_detect(Away, "파울자유투"), na.rm = TRUE)
  )

#팀파울 후 자유투 평균

kbl_broadcast$Q_sec <- kbl_broadcast$sec - (kbl_broadcast$Q-1)*600
kbl_broadcast <- kbl_broadcast %>% select(url, Q,time,Home, Away, Q_sec)
kbl_broadcast_TF_A <- merge(kbl_broadcast, url_Home_Away_data %>% filter(text1 == 'A'), by = c('url','Q'), all.x = T) %>%
  filter(Q_sec > 60*time_num)
kbl_broadcast_TF_H <- merge(kbl_broadcast, url_Home_Away_data %>% filter(text1 == 'H'), by = c('url','Q'), all.x = T) %>%
  filter(Q_sec > 60*time_num)

TF_A_foul_count <- kbl_broadcast_TF_A%>% group_by(url, Q) %>% summarise(TF_away_count_tot = sum(str_detect(Away, "파울자유투"), na.rm = TRUE))
TF_H_foul_count <- kbl_broadcast_TF_H%>% group_by(url, Q) %>% summarise(TF_home_count_tot = sum(str_detect(Home, "파울자유투"), na.rm = TRUE))


# 통합
foul_count <- merge(tot_foul_count, TF_A_foul_count, by = c('url','Q'), all.x = T)
foul_count <- merge(foul_count, TF_H_foul_count, by = c('url','Q'), all.x = T)
foul_count <- merge(foul_count, url_Home_Away_data %>% select(url, Q, time_num), by = c('url','Q'), all.x = T)
foul_count[is.na(foul_count)] <- 0

foul_count$tot_FT <- foul_count$home_count_tot + foul_count$away_count_tot
foul_count$tot_FT_after_TF <- foul_count$TF_away_count_tot + foul_count$TF_home_count_tot

#1쿼터 당 자유투 개수 기댓값
mean(foul_count$tot_FT)
#1쿼터 당 팀파울 이후 자유투 개수 기댓값
mean(foul_count$tot_FT_after_TF/foul_count$time_num * 10)

head(foul_count)





