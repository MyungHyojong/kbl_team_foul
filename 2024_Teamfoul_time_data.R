library(readr); library(data.table); library(ggplot2)
data <- read_csv("2024_broadcast_data.csv")

names(data) <- c('Q', 'time', 'Home','Away', 'score1')
data$score1 <- 0
data$score2 <- 0
data$sec <- 0

urls <- data$Q[grep('http', data$Q)]
url_pos <- grep('http', data$Q)

data <- data %>% filter(grepl(':',time)) %>% filter(Q != 'Q5')

data$sec <- sapply(data$time, function(x){
  min <- as.numeric(strsplit(x, ':')[[1]][1])
  sec <- as.numeric(strsplit(x, ':')[[1]][2])
  time <- 600 - (min*60 + sec)
})

data$Q <- as.numeric(substr(data$Q, 2,2))
data$sec <- data$sec + 600*(data$Q-1)

data <- data %>%
  mutate(
    # 이전 row의 Q가 4이고 현재 row의 Q가 1이면 새 경기 시작(change=1)
    change = if_else(lag(Q) == 4 & Q == 1, 1, 0),
    # 첫 번째 행은 무조건 새 경기 시작으로 처리해야 하므로 NA -> 1 교체
    change = replace_na(change, 1)
  ) %>%
  # 누적합(cumsum)으로 game_id 생성
  mutate(game_id = cumsum(change)) %>%
  # game_id를 인덱스로 사용해 urls 벡터에서 url 할당
  mutate(url = urls[game_id])



data <- as.data.table(data)
data <- data[order(url,sec),]
data$score1 = 0
data$score2 = 0
#data <- data%>% distinct()
setDT(data)

# 그룹별 점수 계산
data[, `:=`(
  score1 = cumsum(
    grepl('2점슛성공|덩크슛성공', Home) * 2 +
      grepl('3점슛성공', Home) * 3 +
      grepl('자유투성공', Home)
  ),
  score2 = cumsum(
    grepl('2점슛성공|덩크슛성공', Away) * 2 +
      grepl('3점슛성공', Away) * 3 +
      grepl('자유투성공', Away)
  )
), by = url]

team_data <- read_csv("url_home_away_2024.csv")
names(team_data) <- c('url','home_team','away_team')
kbl_broad_cast_2024 <- merge(data, team_data, by = 'url')

home_5th <- kbl_broad_cast_2024 %>%
  filter(str_detect(Home, "팀파울")) %>%
  group_by(url, Q) %>%
  arrange(sec, .by_group = TRUE) %>%
  summarise(
    sec_home_5th = ifelse(n() >= 5, sec[5], 0),
    .groups = "drop"
  )

away_5th <- kbl_broad_cast_2024 %>%
  filter(str_detect(Away, "팀파울")) %>%
  group_by(url, Q) %>%
  arrange(sec, .by_group = TRUE) %>%
  summarise(
    sec_away_5th = ifelse(n() >= 5, sec[5], 0),
    .groups = "drop"
  )


home_5th[is.na(home_5th)] <- 0
away_5th[is.na(away_5th)] <- 0

home_5th$game_time_H <- ifelse(home_5th$sec_home_5th == 0, 600, home_5th$sec_home_5th - 600*(home_5th$Q-1))
away_5th$game_time_A <- ifelse(away_5th$sec_away_5th == 0, 600, away_5th$sec_away_5th - 600*(away_5th$Q-1))

full_time <- merge(home_5th, away_5th, by= c('url','Q'), all.x= T, all.y = T)
full_time <- merge(full_time, team_data, by= 'url')

Home_result <- full_time %>% group_by(home_team) %>% summarise(
  home_time_appearance = n_distinct(url)*4,
  my_foul_H = mean(game_time_H, na.rm = T),
  your_foul_H = mean(game_time_A, na.rm = T)
)

Away_result <- full_time %>% group_by(away_team) %>% summarise(
  away_time_appearance = n_distinct(url)*4,
  my_foul_A = mean(game_time_A, na.rm = T),
  your_foul_A = mean(game_time_H, na.rm = T)
)

names(Home_result)[1] = 'team'
names(Away_result)[1] = 'team'

Result <- merge(Home_result, Away_result, by = 'team') %>%
  mutate(foul_time = (home_time_appearance*my_foul_H + away_time_appearance *my_foul_A)/(home_time_appearance +away_time_appearance)/60) %>% 
  mutate(fouled_time = (home_time_appearance*your_foul_H + away_time_appearance *your_foul_A)/(home_time_appearance +away_time_appearance)/60)%>%
  select(team, foul_time , fouled_time)
Result <- Result[order(Result$foul_time),]
rank <- c(10, 3,4,8,5,9,6,2,7,1)
Result <- cbind(Result, rank)

library(ggplot2)

ggplot(Result, aes(x = rank)) +
  # foul_time 라인 & 점
  geom_line(aes(y = foul_time, color = "foul_time"), size = 1.2) +
  geom_point(aes(y = foul_time, color = "foul_time"), size = 3) +
  
  # fouled_time 라인 & 점
  geom_line(aes(y = fouled_time, color = "fouled_time"), size = 1.2) +
  geom_point(aes(y = fouled_time, color = "fouled_time"), size = 3) +
  
  # 색상 수동 지정
  scale_color_manual(values = c("foul_time" = "blue", "fouled_time" = "red")) +
  
  # x축: rank(1~10)와 팀 이름 추가
  scale_x_continuous(
    breaks = Result$rank,          # x축 눈금 위치: rank 값
    labels = Result$team           # x축 라벨: 팀 이름
  ) +
  
  # y축 범위 및 눈금
  scale_y_continuous(limits = c(6.5, 10.5), breaks = seq(6.5, 10.5, 0.5)) +
  
  # 제목, 라벨
  labs(
    title = "2024-25 kbl rank and Team foul time",
    x = "Rank",
    y = "Time",
    color = "Type"
  ) +
  
  # 테마 설정
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line    = element_line(size = 1.2, color = "black"),
    axis.ticks   = element_line(size = 1.2, color = "black"),
    axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),  # x축 라벨 기울이기
    axis.text.y  = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
