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
