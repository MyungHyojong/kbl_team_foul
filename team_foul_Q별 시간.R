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

result_time <- rbind(result_Home , result_Away)

#데이터 작을 떄 -> 직관적 dplyr
avg_time <- result_time %>% group_by(Q,year) %>% summarise(
  avg_team_foul_time = mean(time_num)
) %>% mutate(Q = as.factor(Q))


# ggplot2를 활용한 그래프 생성
ggplot(avg_time, aes(x = year, y = avg_team_foul_time, color = Q)) +
  geom_line(size = 1) +                      # 라인 그래프
  geom_point(size = 2) +                     # 데이터 포인트
  labs(
    title = "Average Time of Entering Team Foul Trouble of each Quarter by Year", 
    x = "Year", 
    y = "Time", 
    color = "Quarter"
  ) +
  theme_minimal() +                          # 간결한 테마
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # 제목 설정
    axis.title = element_text(size = 12),    # 축 라벨 크기
    legend.title = element_text(size = 12),  # 범례 제목 크기
    panel.grid = element_blank()             # 배경 격자 제거
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2023, 1)              # x축: 연도 1년 단위 표시
  ) +
  scale_y_continuous(
    limits = c(7, 10),                       # y축: 5에서 10까지 설정
    breaks = seq(7, 10, 1)                   # y축: 1 단위로 표시
  ) +
  theme(
    panel.border = element_blank(),          # 기본 테두리 제거
    axis.line = element_line(color = "black", size = 0.8) # xy축 선 추가
  )
