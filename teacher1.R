if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(ggrepel)) {
  install.packages('ggrepel')
  library(ggrepel)
}
library(RColorBrewer)


##################   데이터 import
data <- read.csv('d:/R/data/general analysis1.csv', header = T, stringsAsFactors = T)


#################    데이터 확인
summary(data)
nrow(data)
str(data)

data$year <- as.factor(substr(data$year, 1, 4))

data <- data %>% filter(existance != '폐(원)교')
table(data$existance)

data$kind <- factor(data$kind, levels = c('유치원', '초등학교', '중학교', '고등학교', '특수학교', '고등공민학교', '고등기술학교', '각종학교'), ordered = T)
table(data$kind)

data$province <- factor(data$province, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)
table(data$province)

data$estkind <- factor(data$estkind, levels = c('국립', '공립', '사립'), ordered = T)
table(data$estkind)


data$scale <- factor(data$scale, levels = c('특별/광역시', '중소도시', '읍지역', '면지역', '도서벽지'), ordered = T)
table(data$scale)


###############    교사대비 직원 비율 필드 생성
data <- data %>% mutate(teacher.per.staff = teacher/staff)
summary(data$teacher.per.staff)
data[, c('teacher', 'staff', 'teacher.per.staff')]


###############    전체 교원수
data %>% 
  group_by(year) %>% summarise(sum = sum(teacher, na.rm = T)) %>%
  ggplot(aes(x = year, y = sum)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), show.legend = FALSE) +
  labs(title = '전체 교원수', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue")) + 
  ggsave("전체교사수(staff).jpg", dpi = 300) 


###############    전체 직원수
data %>% 
  group_by(year) %>% summarise(sum = sum(staff, na.rm = T)) %>% filter(sum != 0) %>%
  ggplot(aes(x = year, y = sum)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), show.legend = FALSE) +
  labs(title = '전체 직원수', x = '연도', y = '직원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue")) + 
  ggsave("전체직원수(staff).jpg", dpi = 300) 



###############    학교급별 직원수

data %>% 
  group_by(year, kind) %>% summarise(sum = sum(staff, na.rm = T)) %>% filter(sum != 0) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  labs(title = '학교급별 직원수', x = '연도', y = '직원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 


###############    지역별 직원수

data %>% 
  group_by(year, province, kind) %>% summarise(sum = sum(staff, na.rm = T)) %>% filter(sum != 0) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
#  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  labs(title = '학교급별 직원수', x = '연도', y = '직원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~province) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 

###############    직원 1인당 교원수

data %>% 
  group_by(year) %>% summarise(mean = mean(staff, na.rm = T)) %>% filter(mean != 0) %>%
  ggplot(aes(x = year, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE) +
  labs(title = '직원 1인당 교원수', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12))

data %>% 
  group_by(year, kind) %>% summarise(mean = mean(staff, na.rm = T)) %>% filter(mean != 0) %>%
  ggplot(aes(x = year, y = mean, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE) +
  labs(title = '직원 1인당 교원수', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12))



###############    학교별 직원 1인당 교원수 

data %>% 
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_boxplot() +
#  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', aes(color = 'blue'), show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=-1, aes( label=paste0('중간값', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', aes(color = 'red'), show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=1, aes( label=paste0('평균값', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black', 
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black', 
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '연도별 학교당 직원1인당 교원수 분포', x = '연도', y = '직원수', color = '통계값') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
#  facet_wrap(~province) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"),  
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 


###############    학교별 직원 1인당 교원수 

data %>% filter(kind %in% c('유치원', '초등학교', '중학교', '고등학교')) %>%
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_boxplot() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', aes(color = 'blue'), show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=-1, aes( label=paste0('중간값', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', aes(color = 'red'), show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=1, aes( label=paste0('평균값', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black', 
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black', 
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '연도별 학교당 직원1인당 교원수 분포', x = '연도', y = '직원수', color = '통계값') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~kind) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 
