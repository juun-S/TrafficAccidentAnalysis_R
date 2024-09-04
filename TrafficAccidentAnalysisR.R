# 필요한 라이브러리 설정
library(tidyverse)
library(ggplot2)
library(showtext)

# 구글 폰트 추가 및 설정
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Gamja Flower", "Gamja Flower")
showtext_auto()

# 데이터 로드
data <- read.csv("~/datascience_basic_R/reports/도로교통공단_요일별 시간대별 교통사고 통계_20221231.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
data
str(data)
# 요일 순서 지정
data$요일 <- factor(data$요일, levels = c("월", "화", "수", "목", "금", "토", "일"))

# 시간대를 숫자로 변환
data$시간대 <- as.numeric(gsub("시-.*", "", data$시간대))

# 요일별 평일/주말 열 추가
data <- data %>%
  mutate(평일주말 = ifelse(요일 %in% c("토", "일"), "주말", "평일"))

# 사고건수대비 사망자, 중상자, 경상자, 부상신고자수 비율
data_combined <- data %>%
  group_by(요일, 시간대, 평일주말) %>%
  summarise(사망자수 = sum(사망자수), 중상자수 = sum(중상자수), 경상자수 = sum(경상자수), 사고건수 = sum(사고건수), 부상신고자수= sum(부상신고자수)) %>%
  mutate(합계비율 = (사망자수 + 중상자수 + 경상자수 + 부상신고자수) / 사고건수 * 100)

# 교통사고 사망자수 비율 계산
data_dead_ratio <- data %>%
  group_by(요일, 시간대, 평일주말) %>%
  summarise(사고건수 = sum(사고건수), 사망자수 = sum(사망자수)) %>%
  mutate(사망자수비율 = 사망자수 / 사고건수 * 100)

# 그래프시각화 - 교통사고 건수
plot1 <- data %>%
  ggplot(aes(x = 시간대, y = 사고건수, color = 요일)) +
  geom_point() +
  geom_line(aes(group = 요일), size = 1) +
  labs(title = "시간대별 교통사고 수",
       x = "시간대",
       y = "교통사고 건수",
       color = "요일") +
  scale_x_continuous(breaks = seq(0, max(data$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue"))

# 그래프 시각화 - 요일별 합계비율
plot2 <- data_combined %>%
  ggplot(aes(x = 시간대, y = 합계비율, color = 요일)) +
  geom_point(alpha = 1) +
  geom_line(aes(group = 요일), size = 1) +
  labs(title = "요일별시간대별 인피자수 비율",
       x = "시간대",
       y = "합계비율 (%)",
       color = "요일") +
  scale_x_continuous(breaks = seq(0, max(data_combined$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue")) 

plot2
# 그래프 시각화 - 요일별 사망자수 비율
plot3 <- data_dead_ratio %>%
  ggplot(aes(x = 시간대, y = 사망자수비율, color = 요일)) +
  geom_point(alpha = 1) +
  geom_line(aes(group = 요일), size = 1) +
  labs(title = "요일별 시간대별 사망자수 비율",
       x = "시간대",
       y = "사망자수 비율 (%)",
       color = "요일") +
  scale_x_continuous(breaks = seq(0, max(data_dead_ratio$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue"))
plot3

# 주말과 평일 별 교통사고 건수
weekday_accidents <- data %>%
  filter(평일주말 %in% "평일") %>%
  group_by(시간대) %>%
  summarise(평일_사고건수 = sum(사고건수)/5)

weekend_accidents <- data %>%
  filter(평일주말 %in% "주말") %>%
  group_by(시간대) %>%
  summarise(주말_사고건수 = sum(사고건수)/2)


# 그래프 시각화 - 주말/평일 교통사고 건수
plot4 <- data %>%
  ggplot(aes(x = 시간대, y = 사고건수, color = 평일주말)) +
  geom_point(alpha = 0.2) +
  geom_line(data = weekday_accidents, aes(x = 시간대, y = 평일_사고건수), color = "darkblue", size = 2) +
  geom_line(data = weekend_accidents, aes(x = 시간대, y = 주말_사고건수), color = "purple", size = 2) +
  labs(title = "주말/평일 시간대별 교통사고 건수",
       x = "시간대",
       y = "교통사고 건수",
       color = "주말/평일") +
  scale_x_continuous(breaks = seq(0, max(data$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue")) +
  scale_color_manual(name = "주말/평일", values = c("평일" = "darkblue", "주말" = "purple"),
                     guide = guide_legend(override.aes = list(alpha = 1)))

plot4
 
#평일과 주말 별 사고건수대비 인피자수, 사망자수비율
weekday_avg_combined <- data_combined %>%
  filter(평일주말 %in% "평일") %>%
  group_by(시간대) %>%
  summarise(avg_weekday_combined = mean(합계비율, na.rm = TRUE))

weekend_avg_combined <- data_combined %>%
  filter(평일주말 %in% "주말") %>%
  group_by(시간대) %>%
  summarise(avg_weekend_combined = mean(합계비율, na.rm = TRUE))

weekday_avg_ratio <- data_dead_ratio %>%
  filter(평일주말 %in% "평일") %>%
  group_by(시간대) %>%
  summarise(avg_weekday_ratio = mean(사망자수비율, na.rm = TRUE))

weekend_avg_ratio <- data_dead_ratio %>%
  filter(평일주말 %in% "주말") %>%
  group_by(시간대) %>%
  summarise(avg_weekend_ratio = mean(사망자수비율, na.rm = TRUE))

# 그래프 시각화 평일/주말 합계비율
plot5 <- data_combined %>%
  ggplot(aes(x = 시간대, y = 합계비율, color = 평일주말)) +
  geom_point(alpha = 0.2) +
  geom_line(data = weekday_avg_combined, aes(x = 시간대, y = avg_weekday_combined), color = "darkblue", size = 2) +
  geom_line(data = weekend_avg_combined, aes(x = 시간대, y = avg_weekend_combined), color = "purple", size = 2) +
  labs(title = "주말/평일 시간대별 인피자수 비율",
       x = "시간대",
       y = "합계비율 (%)",
       color = "주말/평일") +
  scale_x_continuous(breaks = seq(0, max(data_combined$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue")) +
  scale_color_manual(name = "주말/평일", values = c("평일" = "darkblue", "주말" = "purple"),
                     guide = guide_legend(override.aes = list(alpha = 1)))

# 그래프 시각화 - 주말/평일 사망자수 비율
plot6 <- data_dead_ratio %>%
  ggplot(aes(x = 시간대, y = 사망자수비율, color = 평일주말)) +
  geom_point(alpha = 0.35) +
  geom_line(data = weekday_avg_ratio, aes(x = 시간대, y = avg_weekday_ratio), color = "darkblue", size = 2) +
  geom_line(data = weekend_avg_ratio, aes(x = 시간대, y = avg_weekend_ratio), color = "purple", size = 2) +
  labs(title = "주말/평일 시간대별 사망자수 비율",
       x = "시간대",
       y = "사망자수 비율 (%)",
       color = "주말/평일") +
  scale_x_continuous(breaks = seq(0, max(data_dead_ratio$시간대), by = 2)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(color = "red", face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue")) +
  scale_color_manual(name = "주말/평일", values = c("평일" = "darkblue", "주말" = "purple"),
                     guide = guide_legend(override.aes = list(alpha = 1)))


# 그래프 나열
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)


