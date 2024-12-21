rank_other_stat <- read_ods("크블 득점 역순위.ods")
colnames(rank_other_stat) <- c("year", "order", "team", "G", "W", "L", "PCT", "PTS", "2PM", "2PA", "2P%", "3PM", "3PA", "3P%", "FGM", "FGA", "FG%", "FTM", "FTA", "FT%", "OREB", "DREB", "REB", "AST", "STL", "BLK", "GD", "DK", "DKA", "TO", "PF", "PP", "PPA", "PP%", "+/-")

# 열 인덱스를 사용하여 1열과 2열의 순서 바꾸기
average_team_time <- average_team_time[, c(2, 1, 3:ncol(average_team_time))]  # 2열을 첫 번째로, 1열을 두 번째로 이동


# 예제 데이터
average_team_time
rank_other_stat

# 전체 외부 병합
data_time_rank_other_stat<- merge(
  average_team_time, 
  rank_other_stat, 
  by = c("team", "year"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

library(dplyr)

# 데이터프레임에서 4번째 열 삭제
data_time_rank_other_stat <- data_time_rank_other_stat %>% select(-4)

# 필요한 패키지 설치 및 로드
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")
if (!requireNamespace("rvg", quietly = TRUE)) install.packages("rvg")

library(ggplot2)
library(officer)
library(rvg)

# 변수 목록 생성 (4열부터 35열까지)
variables <- colnames(data_time_rank_other_stat)[4:35]

# PowerPoint 생성
ppt <- read_pptx()

# 각 변수에 대해 그래프 생성 및 추가
for (var in variables) {
  # 변수 이름이 숫자로 시작하거나 특수 문자가 포함된 경우 백틱 처리
  safe_var <- ifelse(grepl("^[^a-zA-Z]|\\W", var), paste0("`", var, "`"), var)
  
  # 상관계수 계산
  correlation <- cor(data_time_rank_other_stat[[var]], data_time_rank_other_stat$avg_time, 
                     use = "complete.obs", method = "pearson")
  
  # ggplot으로 산점도 및 추세선 생성
  plot <- ggplot(data_time_rank_other_stat, aes_string(x = safe_var, y = "avg_time")) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", col = "red") +
    ggtitle(paste0(var, " vs avg_time\nCorrelation: ", round(correlation, 2))) +
    xlab(var) +
    ylab("avg_time") +
    theme_minimal()
  
  # PowerPoint에 추가
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- ph_with(ppt, value = dml(ggobj = plot), location = ph_location_type(type = "body"))
}

# PowerPoint 저장
print(ppt, target = "avg_time_correlations.pptx")


#피팀파울 시간

# 예제 데이터
data_opponent_time_rank
rank_other_stat

# 전체 외부 병합
data_opponent_time_rank_other_stat<- merge(
  data_opponent_time_rank1, 
  rank_other_stat, 
  by = c("team", "year"),  # 여러 열 이름을 벡터로 전달
  all.x = TRUE,  # 왼쪽 외부 병합
)

# 데이터프레임에서 4번째 열 삭제
data_opponent_time_rank_other_stat <- data_opponent_time_rank_other_stat %>% select(-4)

library(dplyr)



# 필요한 패키지 설치 및 로드
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")
if (!requireNamespace("rvg", quietly = TRUE)) install.packages("rvg")

library(ggplot2)
library(officer)
library(rvg)

# 변수 목록 생성 (4열부터 35열까지)
variables <- colnames(data_opponent_time_rank_other_stat)[4:35]

# 데이터 타입 확인 및 변환
data_opponent_time_rank_other_stat <- data_opponent_time_rank_other_stat %>%
  mutate(across(all_of(variables), as.numeric)) %>% # 4~35 열을 숫자형으로 변환
  mutate(avg_opponent_time = as.numeric(avg_opponent_time)) # avg_opponen_time도 숫자형으로 변환

# NA 제거를 위한 함수 정의
clean_data <- function(x, y) {
  valid <- !is.na(x) & !is.na(y)
  list(x = x[valid], y = y[valid])
}

# PowerPoint 생성
ppt <- read_pptx()

# 각 변수에 대해 그래프 생성 및 추가
for (var in variables) {
  # 데이터 클리닝
  cleaned <- clean_data(data_opponent_time_rank_other_stat[[var]], 
                        data_opponent_time_rank_other_stat$avg_opponent_time)
  
  # 상관계수 계산
  correlation <- cor(cleaned$x, cleaned$y, method = "pearson")
  
  # ggplot으로 산점도 및 추세선 생성
  plot <- ggplot(data.frame(x = cleaned$x, y = cleaned$y), aes(x = x, y = y)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", col = "red") +
    ggtitle(paste0(var, " vs avg_opponent_time\nCorrelation: ", round(correlation, 2))) +
    xlab(var) +
    ylab("avg_opponent_time") +
    theme_minimal()
  
  # PowerPoint에 추가
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- ph_with(ppt, value = dml(ggobj = plot), location = ph_location_type(type = "body"))
}

# PowerPoint 저장
print(ppt, target = "avg_opponent_time_correlations.pptx")
