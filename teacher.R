if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

getwd()

##################   데이터 import
teacher <- read.csv('d:/R/data/teacher.csv', header = T, stringsAsFactors = T)

#################    데이터 확인
summary(teacher)
nrow(teacher)
str(teacher)

################    연도 변환
teacher$year <- as.factor(substr(teacher$year, 1, 4))
teacher <- teacher %>% filter(existance != '폐(원)교', personnel.teacher != 0)

summary(teacher$existance)


###############    기간제 비율 필드 생성
teacher <- teacher %>% mutate(temp.teacher.rate = (temp.total)/personnel.teacher)
summary(teacher$temp.teacher.rate)


###############     긴형태로 데이터 프레임 변환
long.teacher <- gather(teacher, div, value, 15:48)
summary(long.teacher)

##############  시간강사 그래프
teacher %>% ggplot(aes(x = time.total))+ geom_density(aes(color = year), alpha = 1) + 
  geom_vline(xintercept = density(teacher$time.total)$x[which.max(density(teacher$time.total)$y)], color = 'red') + 
  scale_x_log10() + facet_grid(~kind)

teacher %>% filter(kind == '고등학교') %>% table(year, time.total)

teacher %>% filter(kind == '중학교', teacher.time.total != 0) %>% count(year, teacher.time.total)

##############  기간제 비율
teacher %>% ggplot(aes(x = temp.teacher.rate)) + 
  geom_density(aes(linetype = year, color = year), alpha = 1) + 
  scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red'))  + 
  facet_grid(~kind)

teacher %>% filter(kind == '중학교') %>% ggplot(aes(x = temp.teacher.rate)) + 
  geom_density(aes(linetype = year, color = year), alpha = 1) + 
  scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red'))  + 
  facet_grid(~kind)


teacher %>% filter(kind == '고등학교', time.total != 0) %>% count(year, temp.teacher.rate) %>% as.data.frame()


##############  기간제 비율 평균
if(!require(ggrepel)) {
  install.packages('ggrepel')
  library(ggrepel)
}

teacher %>% group_by(year, kind) %>% summarise(mean = mean(temp.teacher.rate, na.rm = T)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean)))
  

##############  순회학급
teacher %>% group_by(year, kind) %>% summarise(mean = sum(class.circuit)/sum(class)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean)))


##############  기간제 교사수
teacher %>% group_by(year) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)))


teacher %>% group_by(year, kind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)))

##############  휴직 교사수
teacher %>% group_by(year) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)))


teacher %>% group_by(year, kind) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + facet_wrap(~kind)


##############  휴직 교사수 + 기간제 교사수
teacher %>% group_by(year) %>%  summarise(sum1 = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total), 
                                          sum2 = sum(temp.total + time.total)) %>% 
  ggplot(aes(x = year)) + geom_line(aes(y = sum1, group = 1, color = 'red')) + 
  geom_line(aes(y = sum2, group = 1, color = 'blue')) + 
  geom_text_repel(aes(y = sum1, label = scales::number_format(big.mark = ',')(sum1))) + 
  geom_text_repel(aes(y = sum2, label = scales::number_format(big.mark = ',')(sum2))) + 
  geom_point(aes(y = sum1)) + 
  geom_point(aes(y = sum2)) + 
  scale_color_discrete(name="교원구분", labels=c('비정규교원', '휴직교원')) + 
  labs(x = '연도', y = '교원수')




##############  정규직 교사 대비 기간제 교사수 
teacher %>% group_by(year) %>% summarise(temp.sum = sum(temp.total + time.total), 
                                         teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
                               mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

teacher %>% group_by(year, kind) %>% summarise(temp.sum = sum(temp.total + time.total), 
                                         teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_wrap(~kind)


teacher %>% group_by(year, kind, province) %>% summarise(temp.sum = sum(temp.total + time.total), 
                                               teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_grid(kind~province)

teacher %>% group_by(year, kind, scale) %>% summarise(temp.sum = sum(temp.total + time.total), 
                                                         teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_grid(kind~scale)

teacher %>% group_by(year, kind, estkind) %>% summarise(temp.sum = sum(temp.total + time.total), 
                                                      teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_grid(kind~estkind)

teacher %>% filter(kind == '고등학교') %>%group_by(year, province, estkind) %>% 
  summarise(temp.sum = sum(temp.total + time.total), teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '고등학교 정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_grid(estkind~province)

teacher %>% filter(kind == '중학교') %>%group_by(year, province, estkind) %>% 
  summarise(temp.sum = sum(temp.total + time.total), teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '중학교 정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + facet_grid(estkind~province)
