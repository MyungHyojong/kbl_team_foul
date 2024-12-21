# 데이터 나누기 설정
chunk_size <- 5583  # 한 조각에 포함될 행의 개수

# url과 text1만 선택한 데이터
Home_team <- team_data[, c("url", "text1")]

# url과 text2만 선택한 데이터
Away_team <- team_data[, c("url", "text2")]

# 각 데이터 나누기
Home_team_split <- split(Home_team, ceiling(seq_len(nrow(Home_team)) / chunk_size))
Away_team_split <- split(Away_team, ceiling(seq_len(nrow(Away_team)) / chunk_size))

# 리스트를 데이터 프레임으로 병합
Home_team <- do.call(rbind, Home_team_split)
Away_team <- do.call(rbind, Away_team_split)

# 열 이름 변경
colnames(Home_team) <- c("url", "text1")
colnames(Away_team) <- c("url", "text2")


# 필요한 패키지 로드
library(dplyr)

# 예제 데이터
result_Away
Away_team

# 전체 외부 병합
result_all_Away <- merge(
  result_Away, 
  Away_team, 
  by = c("url"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

# 예제 데이터
result_Home
Home_team

# 전체 외부 병합
result_all_Home <- merge(
  result_Home, 
  Home_team, 
  by = c("url"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

result_all <- merge(
  result_all_Away, 
  result_all_Home, 
  by = c("url", "Q", "time", "year", "min", "sec", "time_num"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
  all.y = TRUE
)


# text2의 NA를 'Home'으로 대체
result_all$text2[is.na(result_all$text2)] <- "Home"

# text1의 NA를 'Away'로 대체
result_all$text1[is.na(result_all$text1)] <- "Away"

# text2 열에 "Home"이 있는 경우 text1 열과 자리를 바꿈
swap_rows <- result_all$text2 == "Home"  # text2 열이 "Home"인 행 찾기
temp <- result_all$text1[swap_rows]      # text1 값을 임시 변수에 저장
result_all$text1[swap_rows] <- result_all$text2[swap_rows]  # text1에 text2 값을 할당
result_all$text2[swap_rows] <- temp      # text2에 임시 변수 값을 할당

# text2 열 이름을 "team"으로 변경
colnames(result_all)[colnames(result_all) == "text2"] <- "team"

# text1 열 이름을 "Home/Away"로 변경
colnames(result_all)[colnames(result_all) == "text1"] <- "Home/Away"

# 연간 평균 팀파울 진입 시간 계산
average_team_time <- result_all %>%
  group_by(team, year) %>%              # 팀과 연도를 기준으로 그룹화
  summarise(avg_time = mean(time_num, na.rm = TRUE)) %>%  # 평균 팀파울 시간 계산
  arrange(team, year)                   # 팀과 연도로 정렬

# rank 데이터에서 첫 3열만 선택
#rank <- rank[, 1:3]

# 열 이름 변경
colnames(rank) <- c("year", "order", "team")

# 예제 데이터
average_team_time
rank

# 전체 외부 병합
data_time_rank<- merge(
  average_team_time, 
  rank, 
  by = c("team", "year"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

data_time_rank <- data_time_rank %>%
   arrange(year, order)  # year 순으로 정렬, 같은 year에서는 order 순으로 정렬

# ggplot2 패키지 로드
library(ggplot2)

# 박스 그래프 생성
ggplot(data_time_rank, aes(x = as.factor(order), y = avg_time)) +
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