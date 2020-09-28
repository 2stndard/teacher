getwd()

##################   데이터 import
teacher <- read.csv('c:/R/data/teachers.csv', header = T, stringsAsFactors = T)

#################    데이터 확인
summary(teacher)
nrow(teacher)
str(teacher)

################    연도 변환
teacher$year <- as.factor(substr(teacher$year, 1, 4))
teacher <- teacher %>% filter(existance != '폐(원)교', personnel.teacher != 0)
factor(teacher$existance)

###############    합계 필드 생성
teacher <- teacher %>% mutate(teacher.time.total = teacher.time.male + teacher.time.female)
str(teacher)

###############    기간제 비율 필드 생성
teacher <- teacher %>% mutate(teacher.temp.rate = (teacher.temp.male + teacher.temp.female)/personnel.teacher)
summary(teacher$teacher.temp.rate)


###############     긴형태로 데이터 프레임 변환
if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

long.teacher <- gather(teacher, div, value, 15:37)

##############  시간강사 그래프
teacher %>% filter(kind == '고등학교') %>% ggplot(aes(x = teacher.time.total))+ geom_density(aes(color = year), alpha = 1) + geom_vline(xintercept = density(teacher$teacher.time.total)$x[which.max(density(teacher$teacher.time.total)$y)], color = 'red') + scale_x_log10() + facet_grid(~kind)

teacher %>% filter(kind == '고등학교', teacher.time.total != 0) %>% count(year, teacher.time.total)

teacher %>% filter(kind == '중학교', teacher.time.total != 0) %>% count(year, teacher.time.total)

##############  기간제 비율
teacher %>% filter(kind == '고등학교') %>% ggplot(aes(x = teacher.temp.rate))+ geom_density(aes(linetype = year, color = year), alpha = 1) + scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red'))

teacher %>% filter(kind == '중학교') %>% ggplot(aes(x = teacher.temp.rate))+ geom_density(aes(linetype = year, color = year), alpha = 1) + scale_color_manual(values = c('black', 'orange', 'blue', 'dark green', 'red')) + scale_x_log10()

teacher %>% filter(kind == '고등학교', teacher.time.total != 0) %>% count(year, teacher.temp.rate)

teacher %>% filter(kind == '고등학교') %>% group_by(year) %>% summarise(mean(teacher.temp.rate, na.rm = T))
summary(teacher$teacher.temp.rate)

teacher %>% filter(kind == '중학교') %>% group_by(year) %>% summarise(mean(teacher.temp.rate, na.rm = T))

teacher %>% filter(kind == '초등학교') %>% group_by(year) %>% summarise(mean(teacher.temp.rate, na.rm = T))


ggplot(teacher, aes(x = total.teacher))+ geom_density(aes(color = year), alpha = 0.3) + geom_vline(xintercept = density(teacher$total.teacher)$x[which.max(density(teacher$total.teacher)$y)], color = 'red') + geom_vline(xintercept = 38, color = 'red')


ggplot(teacher, aes(x = teacher.time.total))+ geom_density() + facet_grid(kind~year) + scale_x_log10()

summary(teacher$teacher.time.total)

ggplot(teacher, aes(x = total.teacher))+ geom_density() + coord_trans(x="log10", xlim=c(-100,100))

?coord_trans
ggplot(teacher, aes(x = total.teacher))+ geom_histogram(binwidth = 1) + geom_vline(xintercept = 10, color = 'red') + geom_vline(xintercept = 38, color = 'red') 

ggplot(count(teacher, total.teacher), aes(x = total.teacher, y = n)) + geom_line() + geom_point(size = 0.8) + geom_vline(xintercept = 10, color = 'red') + geom_vline(xintercept = 38, color = 'red') + labs(x = '교사수', y = '학교수') + facet_wrap(~year)
density(teacher$total.teacher)$x[which.max(density(teacher$total.teacher)$y)]
summary(teacher$total.teacher)
count(teacher, total.teacher)
