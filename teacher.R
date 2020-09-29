if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(ggrepel)) {
  install.packages('ggrepel')
  library(ggrepel)
}

getwd()

##################   데이터 import
teacher <- read.csv('c:/R/data/teacher.csv', header = T, stringsAsFactors = T)

#################    데이터 확인
summary(teacher)
nrow(teacher)
str(teacher)

################    연도 변환
teacher$year <- as.factor(substr(teacher$year, 1, 4))
teacher <- teacher %>% filter(existance != '폐(원)교', personnel.teacher != 0)
teacher$kind <- factor(teacher$kind, levels = c('초등학교', '중학교', '고등학교'), ordered = T)
teacher$province <- factor(teacher$province, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)
teacher$estkind <- factor(teacher$estkind, levels = c('국립', '공립', '사립'), ordered = T)
summary(teacher$province)


###############    기간제 비율 필드 생성
teacher <- teacher %>% mutate(temp.per.personnel.rate = (temp.total)/personnel.teacher)
summary(teacher$temp.per.personnel.rate)


###############     긴형태로 데이터 프레임 변환
long.teacher <- gather(teacher, div, value, 15:48)
summary(long.teacher)

##############  시간강사 그래프
teacher %>% ggplot(aes(x = time.total))+ geom_density(aes(color = year), alpha = 1) + 
  geom_vline(xintercept = density(teacher$time.total)$x[which.max(density(teacher$time.total)$y)], color = 'red') + 
  scale_x_log10() + facet_grid(~kind)

label.data <- teacher %>% distinct(year)

teacher %>% filter(kind == '중학교', time.total != 0) %>% count(year, time.total) %>% 
  ggplot(aes(x = time.total, y = n)) + geom_line(aes(color = year)) +
  labs(x = '시간강사수', y = '학교수', title = '시간강사수별 학교수') +
  scale_x_log10() +
  scale_color_discrete(name = '연도')

##############  기간제 확률밀도함수, 히스토그램 - 별로
teacher %>% ggplot(aes(x = temp.per.personnel.rate)) + 
  geom_histogram(aes(fill = year), alpha = 0.3) + 
  scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red'))  + 
  facet_grid(~kind) +
  scale_x_log10() +
  scale_color_discrete(name = '연도')

teacher %>% filter(kind == '중학교') %>% ggplot(aes(x = temp.per.personnel.rate)) + 
  geom_density(aes(linetype = year, color = year), alpha = 1) + 
  scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red'))  + 
  facet_grid(~kind)


teacher %>% filter(kind == '고등학교', time.total != 0) %>% count(year, temp.per.personnel.rate) %>% as.data.frame()


##############  기간제 비율 평균
teacher %>% group_by(year, kind) %>% summarise(mean = mean(temp.per.personnel.rate, na.rm = T)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean))) +
  scale_color_discrete(name = '학교급') +
  labs(x = '연도', y = '정원대비 비정규교사 비율', title = '정원대비 비정규 교사 비율') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1))
  

##############  순회학급
teacher %>% group_by(year, kind) %>% summarise(mean = sum(class.circuit)/sum(class)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean))) +
  labs(x = '연도', y = '순회학급 비율', title = '전체 학급수 대비 순회학급 비율') +
  scale_y_continuous(label = scales::percent_format(accuracy = 0.01)) +
  scale_color_discrete(name = '학교급')


##############  비정규 교사수(기간제 + 시간강사)
teacher %>% group_by(year) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수')
  
teacher %>% group_by(year, kind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급')
  
teacher %>% group_by(year, province) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum))) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급') +
  facet_wrap(~province, scales = 'free_y')


teacher %>% group_by(year, province) %>% summarise(sum = sum(temp.total + time.total)) %>% spread(province, sum) %>% ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:17) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value))) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률') +
  geom_hline(yintercept = 0, color= 'red')

teacher %>% group_by(year, scale) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = scale, color = scale)) + 
  geom_point(aes(color = scale)) + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) +
  labs(x = '연도', y = '교사수', title = '학교지역별 비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green', 'purple', 'orange'), name = '학교지역')

teacher %>% group_by(year, estkind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = estkind, color = estkind)) + 
  geom_point(aes(color = estkind)) + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum))) +
  labs(x = '연도', y = '교사수', title = '설립별 비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green', 'purple', 'orange'), name = '설립')


##############  휴직 교사수
teacher %>% group_by(year) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) +
  labs(x = '연도', y = '교사수', title = '휴직 교사수') + 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))
  


teacher %>% group_by(year, kind) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
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
  labs(x = '연도', y = '비율', title = '정규교원 대비 비정규교원 비율') +
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
