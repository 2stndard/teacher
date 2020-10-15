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

data <- data %>% filter(kind %in% c('유치원', '초등학교', '중학교', '고등학교', '특수학교')) 


###############    교사대비 직원 비율 필드 생성
data <- data %>% mutate(teacher.per.staff = teacher/staff)
summary(data$teacher.per.staff)
data[, c('teacher', 'staff', 'teacher.per.staff')]

###############    전체 교원수 및 직원수
data %>% 
  group_by(year) %>% summarise(sum.teacher = sum(teacher, na.rm = T), 
                               sum.staff = sum(staff, na.rm = T)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = sum.teacher, group = 1)) + 
  geom_point(aes(y = sum.teacher)) +  
  geom_text_repel(aes(y = sum.teacher, label = scales::number_format(big.mark = ',')(sum.teacher)), show.legend = FALSE) +
  geom_line(aes(y = sum.staff, group = 1)) + 
  geom_point(aes(y = sum.staff)) +  
  geom_text_repel(aes(y = sum.staff, label = scales::number_format(big.mark = ',')(sum.staff)), show.legend = FALSE) + 
  theme_bw() +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  labs(title = '전체 교원수 및 직원수', x = '연도', y = '교원수 및 직원수') +
  theme(plot.title=element_text(size=20, color="blue"))
  

data %>% 
  group_by(year) %>% summarise(sum.teacher = sum(teacher, na.rm = T), 
                               sum.staff = sum(staff, na.rm = T)) %>%
  write.table('clipboard', sep = '\t')


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

data %>% 
  group_by(year, kind) %>% summarise(sum = sum(staff, na.rm = T)) %>% filter(sum != 0) %>%
  spread(kind, sum) %>% 
  write.table('clipboard', sep = '\t')


###############    지역별 직원수

data %>% 
  group_by(year, province, kind) %>% summarise(sum = sum(staff, na.rm = T)) %>% 
  filter(sum != 0) %>%
  filter(kind %in% c('유치원', '초등학교', '중학교', '고등학교', '특수학교')) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  labs(title = '시도별 직원수', x = NULL, y = '직원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~province) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("지역급별 직원수.jpg", dpi = 300) 

data %>% 
  group_by(kind, year, province) %>% summarise(sum = sum(staff, na.rm = T)) %>% 
  spread(province, sum) %>% 
  write.table('clipboard', sep = '\t')

data %>% 
  group_by(year, province) %>% summarise(sum = sum(staff, na.rm = T)) %>% 
  filter(sum != 0) %>%
#  filter(kind %in% c('유치원', '초등학교', '중학교', '고등학교', '특수학교')) %>%
  ggplot(aes(x = year, y = sum, group = province)) +
  geom_line() +
  geom_point() +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  labs(title = '시도별 직원수', x = NULL, y = '직원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~province) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("지역급별 직원수.jpg", dpi = 300) 

data %>% 
  group_by(year, province) %>% summarise(sum = sum(staff, na.rm = T)) %>% 
  spread(province, sum) %>% 
  write.table('clipboard', sep = '\t')


###############    학교당 평균 직원수

data %>% 
  group_by(year) %>% summarise(mean = mean(staff, na.rm = T)) %>% filter(mean != 0) %>%
  ggplot(aes(x = year, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE) +
  labs(title = '학교당 평균 직원수', x = '연도', y = '직원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교당 평균 직원수.jpg", dpi = 300) 

data %>% 
  group_by(year, kind) %>% summarise(mean = mean(staff, na.rm = T)) %>% filter(mean != 0, kind %in% c('유치원', '초등학교', '중학교', '고등학교')) %>% 
  ggplot(aes(x = year, y = mean, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE) +
  labs(title = '학교당 평균 직원수', x = '연도', y = '직원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교당 평균 직원수(학교급별).jpg", dpi = 300) 


###############    직원 1인당 교원수

data %>% filter(round(teacher.per.staff, 1) != Inf) %>%
  group_by(year) %>% summarise(mean = mean(round(teacher.per.staff, 1), na.rm = T)) %>%
  ggplot(aes(x = year, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.001)(mean)), show.legend = FALSE) +
  labs(title = '직원 1인당 교원수', subtitle = '직원 1인당 교원수 = 교원 / 교직원', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("직원 1인당 교원수.jpg", dpi = 300) 


###############    직원 1인당 교원수(학교급별)

data %>% filter(round(teacher.per.staff, 1) != Inf) %>%
  group_by(year, kind) %>% summarise(mean = mean(round(teacher.per.staff, 1), na.rm = T)) %>%
  ggplot(aes(x = year, y = mean, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.01)(mean)), show.legend = FALSE) +
  labs(title = '직원 1인당 교원수', subtitle = '직원 1인당 교원수 = 교원 / 교직원', x = '연도', y = '교원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("직원 1인당 교원수(학교급별).jpg", dpi = 300) 


###############    직원 1인당 교원수(학교급별)

data %>% filter(round(teacher.per.staff, 1) != Inf) %>%
  group_by(year, kind, estkind) %>% summarise(mean = mean(round(teacher.per.staff, 1), na.rm = T)) %>%
  ggplot(aes(x = year, y = mean, group = estkind, color = estkind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE, size = 4) +
  labs(title = '직원 1인당 교원수', subtitle = '직원 1인당 교원수 = 교원 / 교직원', x = '연도', y = '교원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  facet_grid(~kind) +
  ggsave("직원 1인당 교원수(학교급별).jpg", dpi = 300) 



###############    직원 1인당 교원수(학교급별)

data %>% filter(round(teacher.per.staff, 1) != Inf) %>%
  group_by(year, kind, scale) %>% summarise(mean = mean(round(teacher.per.staff, 1), na.rm = T)) %>%
  ggplot(aes(x = year, y = mean, group = scale, color = scale)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 0.1)(mean)), show.legend = FALSE, size = 4) +
  labs(title = '직원 1인당 교원수', subtitle = '직원 1인당 교원수 = 교원 / 교직원', x = '연도', y = '교원수', color = '지역규모') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  facet_grid(~kind) +
  ggsave("직원 1인당 교원수(학교급별).jpg", dpi = 300) 



###############    학교별 직원 1인당 교원수 분포
data %>% filter(staff < 30) %>%
  ggplot(aes(x = staff, y = teacher)) +
  geom_jitter() + 
  geom_smooth(aes(color = kind), method = 'lm')

data %>% filter(staff >= 30, staff <= 40) %>%
  ggplot(aes(x = staff, y = teacher)) +
  geom_jitter() + 
  geom_smooth(aes(color = kind), method = 'lm')

###############    학교별 직원 1인당 교원수 

data %>% filter(round(teacher.per.staff, 1) != Inf) %>%
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_violin() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', color = 'blue', show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=1, aes( label=paste0('중간값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', color = 'red', show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=-0.5, aes( label=paste0('평균값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black', 
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black', 
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '직원대비 교원 비율별 학교 분포', subtitle = '직원 교원 비율 = 교원 / 교직원', x = '연도', y = '비율') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  #  facet_wrap(~province) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"),  
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 


###############    직원대비 교원 비율별 학교 분포

data %>% 
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_violin() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', color = 'blue', show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', size = 3,
               vjust=1, aes( label=paste0('중간값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', color = 'red', show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red',  size = 3,
               vjust=-0.5, aes( label=paste0('평균값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black',  size = 3,
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black',  size = 3,
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '직원대비 교원 비율별 학교 분포', subtitle = '직원 교원 비율 = 교원 / 교직원', x = '연도', y = '비율') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~kind) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 600) 



###############    직원대비 교원 비율별 학교 분포

data %>% 
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_violin() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', color = 'blue', show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=1, aes( label=paste0('중간값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', color = 'red', show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=-0.5, aes( label=paste0('평균값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black', 
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black', 
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '직원대비 교원 비율별 학교 분포', subtitle = '직원 교원 비율 = 교원 / 교직원', x = '연도', y = '비율') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_wrap(~estkind) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 

###############    직원대비 교원 비율별 학교 분포

data %>% 
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_violin() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', color = 'blue', show.legend = F) +
#  stat_summary(geom = 'text', fun = 'median', colour='blue', 
#               vjust=1, aes( label=paste0('중간값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', color = 'red', show.legend = F) +
#  stat_summary(geom = 'text', fun = 'mean', colour='red', 
#               vjust=-0.5, aes( label=paste0('평균값 = ', round(..y.., digits=1)))) +
#  stat_summary(geom = 'text', fun = 'max', colour='black', 
#               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
#  stat_summary(geom = 'text', fun = 'min', colour='black', 
#               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '직원대비 교원 비율별 학교 분포', subtitle = '직원 교원 비율 = 교원 / 교직원', x = '연도', y = '비율') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_grid(estkind~kind) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 


###############    직원대비 교원 비율별 학교 분포

data %>% filter(kind %in% c('유치원', '초등학교', '중학교', '고등학교')) %>% 
  ggplot(aes(x = year, y = teacher.per.staff)) +
  geom_violin() +
  #  geom_jitter(alpha = 0.1) +
  #  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), show.legend = FALSE) +
  stat_summary(geom = 'point', fun = 'median', color = 'blue', show.legend = F) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=1, aes( label=paste0('중간값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'point', fun = 'mean', color = 'red', show.legend = F) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=-0.5, aes( label=paste0('평균값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'max', colour='black', 
               vjust=1, aes( label=paste0('최대값 = ', round(..y.., digits=1)))) +
  stat_summary(geom = 'text', fun = 'min', colour='black', 
               vjust=1, aes( label=paste0('최소값 = ', round(..y.., digits=1)))) +
  labs(title = '직원대비 교원 비율별 학교 분포', subtitle = '직원 교원 비율 = 교원 / 교직원', x = '연도', y = '비율') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  facet_grid(scale~kind) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 직원수.jpg", dpi = 300) 

glimpse(data)


###############    정규직원 대비 비정규직원
data %>% 
  group_by(year) %>% 
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  summarise(regular.sum = sum(staff.regular),
            irregular.sum = sum(staff.temporal)) %>% 
  mutate(정규직원 = regular.sum / (regular.sum + irregular.sum), 
         비정규직원 = irregular.sum /(regular.sum + irregular.sum)) %>%
  gather(div, value, 2:5) %>%
  filter(div %in% c('정규직원', '비정규직원')) %>% 
  ggplot(aes(x = year, y = value, fill = div, label = scales::percent_format(accuracy = 0.1)(value))) + 
  geom_col(stat = 'identity', position = 'stack') + 
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원 구성비', x = '연도', y = '백분률', fill = '교원 직위', subtitle = '정규직원 : 정규직/(사립유)일반직 비정규직원 : 공무직·무기계약직/(사립유)기타직') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
#  facet_grid(province ~ estkind) +
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  ggsave("정규 비정규 구성비.jpg", dpi = 300) 


###############    정규직원 대비 비정규직원(시도별)
data %>%
  filter(year == '2020') %>%
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  group_by(province) %>% 
  summarise(regular.sum = sum(staff.regular),
            irregular.sum = sum(staff.temporal)) %>% 
  mutate(정규직원 = regular.sum / (regular.sum + irregular.sum), 
             비정규직원 = irregular.sum /(regular.sum + irregular.sum)) %>%
  gather(div, value, 2:5) %>%
  filter(div %in% c('정규직원', '비정규직원')) %>% 
  ggplot(aes(x = province, y = value, fill = div, label = scales::percent_format(accuracy = 0.1)(value))) + 
  geom_col(stat = 'identity', position = 'stack') + 
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원 구성비(20)', x = '연도', y = '백분률', fill = '교원 직위', subtitle = '정규직원 : 정규직 비정규직원 : 공무직·무기계약직') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  #  facet_grid(province ~ estkind) +
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  geom_hline(yintercept = 0.268, color = 'red') +
  ggsave("정규 비정규 구성비(시도별).jpg", dpi = 300) 

###############    정규직원 대비 비정규직원(시도별)
data %>%
  filter(year == '2020') %>%
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  group_by(province, kind) %>% 
  summarise(regular.sum = sum(staff.regular),
            irregular.sum = sum(staff.temporal)) %>% 
  mutate(정규직원 = regular.sum / (regular.sum + irregular.sum), 
             비정규직원 = irregular.sum /(regular.sum + irregular.sum)) %>%
  gather(div, value, 3:6) %>%
  filter(div %in% c('정규직원', '비정규직원')) %>% 
  ggplot(aes(x = province, y = value, fill = div, label = scales::percent_format(accuracy = 0.1)(value))) + 
  geom_col(stat = 'identity', position = 'stack') + 
#  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원 구성비(20)', x = '연도', y = '백분률', fill = '교원 직위', subtitle = '정규직원 : 정규직 비정규직원 : 공무직·무기계약직') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  facet_wrap(~ kind) +
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom', 
        axis.text.x=element_text(angle=90, hjust=1, vjust = 0)) + 
  geom_hline(yintercept = 0.268, color = 'red') +
  ggsave("정규 비정규 구성비.jpg", dpi = 300) 

###############    정규직원 대비 비정규직원
data %>% 
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  group_by(year) %>% 
  summarise(정규직원 = sum(staff.regular),
            비정규직원 = sum(staff.temporal)) %>% 
  gather(div, value, 2:3) %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = div, color = div)) +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(value)), size = 4) +
#  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원수', x = '연도', y = '직원수', color = '교원 직위', subtitle = '정규직원 : 정규직/(사립유)일반직 비정규직원 : 공무직·무기계약직/(사립유)기타직') +
  scale_y_continuous(label = scales::number_format(accuracy = 1, big.mark = ',')) + 
  #  facet_grid(province ~ estkind) +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  ggsave("정규 비정규 직원수.jpg", dpi = 300) 




###############    정규직원 대비 비정규직원(학교급)
data %>% 
  group_by(year, kind) %>% 
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  summarise(정규직원 = sum(staff.regular),
                비정규직원 = sum(staff.temporal)) %>% 
  gather(div, value, 3:4) %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = div, color = div)) +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(value)), size = 4) +
  #  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원수', x = '연도', y = '직원수', color = '교원 직위', subtitle = '정규직원 : 정규직/(사립유)일반직 비정규직원 : 공무직·무기계약직/(사립유)기타직') +
  scale_y_continuous(label = scales::number_format(accuracy = 1, big.mark = ',')) + 
  #  facet_grid(province ~ estkind) +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  facet_grid(~kind) + 
  ggsave("정규 비정규 직원수(학교급).jpg", dpi = 300) 


###############    정규직원 대비 비정규직원(지역급)
data %>% 
  group_by(year, province) %>% 
  filter(!(estkind == '사립' && kind == '유치원')) %>%
  summarise(정규직원 = sum(staff.regular),
            비정규직원 = sum(staff.temporal)) %>% 
  gather(div, value, 3:4) %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = div, color = div)) +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(value)), size = 2) +
  #  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 직원수', x = '연도', y = '직원수', color = '교원 직위', subtitle = '정규직원 : 정규직/(사립유)일반직 비정규직원 : 공무직·무기계약직/(사립유)기타직') +
  scale_y_continuous(label = scales::number_format(accuracy = 1, big.mark = ',')) + 
  #  facet_grid(province ~ estkind) +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  facet_wrap(~ province) + 
  ggsave("정규 비정규 직원수(지역규모).jpg", dpi = 300) 
