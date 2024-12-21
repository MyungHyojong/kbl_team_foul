library(dplyr)

# Self-join으로 상대팀 데이터 병합
opponent_data <- result_all %>%
  rename(opponent_team = team, opponent_time_num = time_num, opponent_Home_Away = `Home/Away`) %>%
  select(url, opponent_team, opponent_time_num, opponent_Home_Away)

# 원본 데이터와 병합하여 상대팀 데이터 추가
result_with_opponent <- result_all %>%
  inner_join(opponent_data, by = "url") %>%
  filter(`Home/Away` != opponent_Home_Away)  # Home/Away가 서로 다른 경우만 선택

# 연도별 각 팀의 경기별 상대팀 팀파울 진입 시간의 평균 계산
opponent_avg_time_per_year <- result_with_opponent %>%
  group_by(team, year) %>%  # 팀, 연도, URL 기준으로 그룹화
  summarise(avg_opponent_time = mean(opponent_time_num, na.rm = TRUE), .groups = 'drop')  # 평균 계산

# 전체 외부 병합
data_opponent_time_rank<- merge(
  opponent_avg_time_per_year, 
  rank, 
  by = c("team", "year"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

data_time_rank <- data_time_rank %>%
  arrange(year, order)  # year 순으로 정렬, 같은 year에서는 order 순으로 정렬

# ggplot2 패키지 로드
library(ggplot2)

# 박스 그래프 생성
ggplot(data_opponent_time_rank, aes(x = as.factor(order), y = avg_opponent_time)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +  # 박스 그래프
  labs(
    title = "Boxplot of Time by Rank (Grouped by Year)",
    x = "Rank",
    y = "Average Time"
  ) +
  theme_minimal() +  # 깔끔한 테마
  theme(
    panel.grid = element_blank(),       # 격자선 제거
    axis.line = element_line(size = 1), # 축 진하게
  )
