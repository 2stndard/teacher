if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(ggrepel)) {
  install.packages('ggrepel')
  library(ggrepel)
}
library(RColorBrewer)

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
teacher$kind <- factor(teacher$kind, levels = c('초등학교', '중학교', '고등학교'), ordered = T)
teacher$province <- factor(teacher$province, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)
teacher$estkind <- factor(teacher$estkind, levels = c('국립', '공립', '사립'), ordered = T)
teacher$scale <- factor(teacher$scale, levels = c('특별/광역시', '시', '읍지역', '면지역', '특수지역'), ordered = T)
summary(teacher$scale)

###############    기간제 비율 필드 생성
teacher <- teacher %>% mutate(temp.per.personnel.rate = (temp.total+time.total)/personnel.teacher)
summary(teacher$temp.per.personnel.rate)


###############     긴형태로 데이터 프레임 변환
long.teacher <- gather(teacher, div, value, 15:48)
summary(long.teacher)



#############################  전체 학교수
teacher %>% 
  group_by(year, kind) %>% summarise(count = n()) %>%
  ggplot(aes(x = year, y = count, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(accuracy = 1, big.mark = ',')(count)), show.legend = FALSE) +
  labs(title = '학교급별 학교수', x = '연도', y = '학교수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 학교수.jpg", dpi = 300) 

#############################  전체 학생수
teacher %>% 
  group_by(year, kind) %>% summarise(sum = sum(student)) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(accuracy = 1, big.mark = ',')(sum)), show.legend = FALSE) +
  labs(title = '학교급별 학생수', x = '연도', y = '학교수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 학생수.jpg", dpi = 300) 



###############    전체 교원수
teacher %>% 
  group_by(year) %>% summarise(sum = sum(total)) %>%
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
  ggsave("전체교사수.jpg", dpi = 300) 


###############    학교급별 교원수

teacher %>% 
  group_by(year, kind) %>% summarise(sum = sum(total)) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), show.legend = FALSE) +
  labs(title = '학교급별 교원수', x = '연도', y = '교원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교급별 교원수.jpg", dpi = 300) 



###############    학교 지역별 교원수

teacher %>% 
  group_by(year, scale) %>% summarise(sum = sum(total)) %>%
  ggplot(aes(x = year, y = sum, group = scale, color = scale)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), show.legend = FALSE) +
  labs(title = '학교지역별 교원수', x = '연도', y = '교원수', color = '학교지역') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("학교지역별 교원수.jpg", dpi = 300) 


#############################  교원수별 학교수

teacher %>%
  ggplot(aes(x = year, y = total)) +
  geom_violin()+
  geom_jitter(alpha = 0.01) + 
  stat_summary(geom = 'point', fun = 'median', aes(color = 'blue'), show.legend = T) +
  stat_summary(geom = 'line', fun = 'median', color = 'blue', aes(group = 1)) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=-2, aes( label=round(..y.., digits=1))) +
  stat_summary(geom = 'point', fun = 'mean', aes(color = 'red'), show.legend = T) +
  stat_summary(geom = 'line', fun = 'mean', color = 'red', aes(group = 1)) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=2, aes( label=round(..y.., digits=1))) +
  facet_grid(~kind) + 
  scale_color_manual('', values = c('blue','red'), labels = c('중간값', '평균값')) +
  labs(title = '교원수별 학교수', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  ggsave("교원수별 학교수.jpg", dpi = 300)


############### 교원 1인당 학생수
teacher %>% 
  group_by(year) %>% 
  summarise(sum = sum(total), sum.std = sum(student)) %>% 
  mutate(rate = round(sum.std / sum, 1)) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_point() +
  geom_line(aes(group = 1, color = '전체')) +
  geom_text_repel(aes(label = scales::number_format(accuracy = 0.1)(round(sum.std / sum, 1))), show.legend = FALSE) +
  geom_line(data = teacher %>% 
              group_by(year, kind) %>% 
              summarise(sum = sum(total), sum.std = sum(student)) %>% 
              mutate(rate = round(sum.std / sum, 1)), 
            aes(x = year, y = rate, color = kind, group = kind)) +
  geom_point(data = teacher %>% 
               group_by(year, kind) %>% 
               summarise(sum = sum(total), sum.std = sum(student)) %>% 
               mutate(rate = round(sum.std / sum, 1)), 
             aes(x = year, y = rate, color = kind)) +
  geom_text_repel(data = teacher %>% 
                    group_by(year, kind) %>% 
                    summarise(sum = sum(total), sum.std = sum(student)) %>% 
                    mutate(rate = round(sum.std / sum, 1)),
                  aes(label = scales::number_format(accuracy = 0.1)(rate)), show.legend = FALSE) +
  labs(title = '교사당 학생수', x = '연도', y = '학생수', color = '학교급') + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1')+
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12)) + 
  scale_color_discrete(limits=c('전체', '초등학교', '중학교', '고등학교')) + 
  ggsave("교사당 학생수.jpg", dpi = 300) 


############### 지역별 학교급별 교원수

teacher %>% 
  group_by(year, province, kind) %>% summarise(sum = sum(total)) %>%
  ggplot(aes(x = year, y = sum, group = kind, color = kind)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), show.legend = FALSE, size = 1.5) +
  labs(title = '지역별 학교급별 교원수', x = '연도', y = '교원수', color = '학교급') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  facet_wrap(~province, scales = 'free_y') +
  ggsave("지역별 학교급별 교원수.jpg", dpi = 300)


#############################  교원수별 학교수

teacher %>%
  ggplot(aes(x = year, y = total)) +
  geom_violin()+
  geom_jitter(alpha = 0.01) + 
  stat_summary(geom = 'point', fun = 'median', aes(color = 'blue'), show.legend = T) +
  stat_summary(geom = 'line', fun = 'median', color = 'blue', aes(group = 1)) +
  stat_summary(geom = 'text', fun = 'median', colour='blue', 
               vjust=-1, aes( label=round(..y.., digits=1))) +
  stat_summary(geom = 'point', fun = 'mean', aes(color = 'red'), show.legend = T) +
  stat_summary(geom = 'line', fun = 'mean', color = 'red', aes(group = 1)) +
  stat_summary(geom = 'text', fun = 'mean', colour='red', 
               vjust=1, aes( label=round(..y.., digits=1))) +
  facet_grid(kind ~ scale) + 
  scale_color_manual('', values = c('blue','red'), labels = c('중간값', '평균값')) +
  labs(title = '지역별 학교급별 학교당 교원수', x = '연도', y = '교원수') +
  scale_y_continuous(label = scales::number_format(big.mark = ',')) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12))+ 
  ggsave("지역별 학교급별 학교당 교원수.jpg", dpi = 300)



##############  비정규 교사수(기간제 + 시간강사)
teacher %>% group_by(year) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교원수', title = '비정규 교원수', subtitle = '기간제 및 시간강사') + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12))+ 
  ggsave("비정규 교원수.jpg", dpi = 300)


############################  학교급별 비정규 교원수
teacher %>% group_by(year, kind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급') + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')+ 
  ggsave("학교급별 비정규 교원수.jpg", dpi = 300)

############################# 학교지역별 비정규 교원수
teacher %>% group_by(year, scale, estkind, kind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = scale, color = scale)) + 
  geom_point(aes(color = scale)) + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum)), size = 4) +
  labs(x = '연도', y = '교사수', title = '학교지역별 설립별 학교급별 비정규 교원수') +
  scale_color_manual(values = c('red', 'blue', 'dark green', 'purple', 'orange'), name = '학교지역') + 
  theme_bw() +
  facet_grid(kind~estkind) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')+ 
  ggsave("학교지역별 설립별 학교급별 비정규 교원수.jpg", dpi = 300)


#############################  지역별 비정규 교원수
teacher %>% 
  group_by(year, province) %>% 
  filter(kind == '초등학교') %>%
  summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), size = 2) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수(초등)') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급') +
  facet_wrap(~province, scales = 'free_y') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.position = 'bottom')+ 
  ggsave("지역별 비정규 교원수 초등.jpg", dpi = 300)


teacher %>% 
  group_by(year, province) %>% 
  filter(kind == '중학교') %>%
  summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), size = 2) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수(중학교)') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급') +
  facet_wrap(~province, scales = 'free_y') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.position = 'bottom')+ 
  ggsave("지역별 비정규 교원수 중중.jpg", dpi = 300)


teacher %>% 
  group_by(year, province) %>% 
  filter(kind == '고등학교') %>%
  summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum)), size = 2) + 
  scale_y_continuous(label = scales::number_format(big.mark = ',')) +
  labs(x = '연도', y = '교사수', title = '비정규 교사수(고등학교)') +
  scale_color_manual(values = c('red', 'blue', 'dark green'), name = '학교급') +
  facet_wrap(~province, scales = 'free_y') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.position = 'bottom')+ 
  ggsave("지역별 비정규 교원수 고등.jpg", dpi = 300)


##############  정규, 비정규별 교사 구성 
teacher %>% 
  group_by(year) %>% 
  summarise(regular.sum = sum(president.total + vicepresident.total + teacher.total),
            irregular.sum = sum(temp.total + time.total)) %>% 
  mutate(정규교사 = regular.sum / (regular.sum + irregular.sum), 
             비정규교사 = irregular.sum /(regular.sum + irregular.sum)) %>%
  gather(div, value, 2:5) %>%
  filter(div %in% c('정규교사', '비정규교사')) %>% 
  ggplot(aes(x = year, y = value, fill = div, label = scales::percent_format(accuracy = 0.1)(value))) + 
  geom_col(stat = 'identity', position = 'stack') + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '정규 비정규 구성비', x = '연도', y = '백분률', fill = '교원 직위', subtitle = '정규교사 : 교장, 교감, 교원 비정규교사 : 기간제, 시간강사') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +
  ggsave("정규 비정규 구성비.jpg", dpi = 600)


##############  직위별 교사 구성(전체)
long.teacher %>% filter(div %in% c('president.total', 'vicepresident.total', 'teacher.total', 'temp.total', 'time.total')) %>%
  group_by(year, div) %>%
  summarise(sum = sum(value)) %>% group_by(year) %>% mutate(year.sum = sum/sum(sum)) -> composition.teacher


composition.teacher$div <- factor(composition.teacher$div, levels = c('president.total', 'vicepresident.total', 'teacher.total', 'temp.total', 'time.total'), labels = c('교장', '교감', '교사', '기간제교사', '시간강사'), ordered = T)

composition.teacher %>% 
  group_by(year) %>% 
  mutate(year.sum = sum/sum(sum)) %>% 
  ggplot(aes(x = year, y = year.sum, fill = div, label = scales::percent_format(accuracy = 0.1)(year.sum))) + 
  geom_col(position = 'stack', stat = 'identity') + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '직위별 교사 구성비', x = '연도', y = '백분률', fill = '교원 직위') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') + 
  ggsave("직위별 교사 구성비.jpg", dpi = 300)


##############  정규, 비정규별 교사 구성 (세부)
teacher %>% 
  group_by(year, kind, estkind) %>% 
  summarise(regular.sum = sum(president.total + vicepresident.total + teacher.total),
            irregular.sum = sum(temp.total + time.total)) %>% 
  mutate(정규교사 = regular.sum / (regular.sum + irregular.sum), 
             비정규교사 = irregular.sum /(regular.sum + irregular.sum)) %>%
  gather(div, value, 4:7) %>%
  filter(div %in% c('정규교사', '비정규교사')) %>% 
  ggplot(aes(x = year, y = value, fill = div, label = scales::percent_format(accuracy = 0.1)(value))) + 
  geom_col(stat = 'identity', position = 'stack') + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '설립별 학교급별 정규 비정규 교사 구성비', x = '연도', y = '백분률', fill = '', subtitle = '정규교사 : 교장, 교감, 교원 비정규교사 : 기간제, 시간강사') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  theme_classic() +
  facet_grid(estkind~kind) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.position = 'bottom') + 
  ggsave("설립별 학교급별 정규 비정규 교사 구성비.jpg", dpi = 300) 



##############  직위별 교사 구성(구분)
long.teacher %>% filter(div %in% c('president.total', 'vicepresident.total', 'teacher.total', 'temp.total', 'time.total')) %>%
  group_by(year, kind, estkind, div) %>%
  summarise(sum = sum(value)) %>% group_by(year, kind, estkind) %>% 
  mutate(year.sum = sum/sum(sum)) -> composition.teacher


composition.teacher$div <- factor(composition.teacher$div, levels = c('president.total', 'vicepresident.total', 'teacher.total', 'temp.total', 'time.total'), labels = c('교장', '교감', '교사', '기간제교사', '시간강사'), ordered = T)

composition.teacher %>% 
  group_by(year, kind, estkind) %>% 
  mutate(year.sum = sum/sum(sum)) %>% 
  ggplot(aes(x = year, y = year.sum, fill = div, label = scales::percent_format(accuracy = 0.1)(year.sum))) + 
  geom_col(position = 'stack', stat = 'identity') + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = '직위별 설립별 교사 구성비', x = '연도', y = '백분률', fill = '교원 직위') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  facet_wrap(estkind~kind) +
  ggsave("직위별 설립별 교사 구성비.jpg", dpi = 600)


##############  기간제 비율 평균
teacher %>% 
  group_by(year, kind) %>% 
  summarise(mean = mean(temp.per.personnel.rate, na.rm = T)) %>%
  mutate(Label = ifelse(year == 2020, levels(kind), NA)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point(aes(color = kind)) + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean))) +
  scale_color_discrete(name = '학교급') +
  labs(x = '연도', y = '백분율', title = '정원대비 비정규 교사 비율', subtitle = '비율 = (기간제교사+시간강사) / (교원정원) * 100') +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  theme(legend.title = element_blank()) +   # 범례의 타이틀을 지움
  geom_text(aes(label = Label), hjust = 1, vjust = 1) +
  ggsave("정원대비비정규교사비율.jpg", dpi = 300) 


##############  비정규 증감률(전체)
teacher %>% 
  group_by(year, province) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(province, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:17) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 2) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) +  
  ggsave("전년대비 비정규 교사증감률.jpg", dpi = 600)


##############  비정규 증감률(초등)
teacher %>% 
  filter(kind == '초등학교') %>%
  group_by(year, province) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(province, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:17) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 3) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(초등)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) +  
  ggsave("전년대비 비정규 교사증감률 초등.jpg", dpi = 600)


##############  비정규 증감률(중학교)
teacher %>% 
  filter(kind == '중학교') %>%
  group_by(year, province) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(province, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:17) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 3) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(중)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) +  
  ggsave("전년대비 비정규 교사증감률 중학교.jpg", dpi = 600)


##############  비정규 증감률(고등학교)
teacher %>% 
  filter(kind == '고등학교') %>%
  group_by(year, province) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(province, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:17) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 3) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(고등)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) +  
  ggsave("전년대비 비정규 교사증감률 고등학교.jpg", dpi = 600)


##############  비정규 증감률(학교급, 설립)
teacher %>% 
  filter(kind == '초등학교') %>%
  group_by(year, estkind) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(estkind, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:3) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('국립', '공립', '사립'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 4) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(초등)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) -> p1

teacher %>% 
  filter(kind == '중학교') %>%
  group_by(year, estkind) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(estkind, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:3) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('국립', '공립', '사립'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 4) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(중)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) -> p2


teacher %>% 
  filter(kind == '고등학교') %>%
  group_by(year, estkind) %>% 
  summarise(sum = sum(temp.total + time.total)) %>% 
  spread(estkind, sum) %>% 
  ts(frequency = 1, start = 2016) -> temp.ts


((apply(temp.ts, 2, diff)/apply(temp.ts, 2, lag)[-1,]))[,-1] %>% as.data.frame() %>%
  cbind(year = c(2017, 2018, 2019, 2020)) %>%  gather(div, value, 1:3) -> temp.ts.trans

temp.ts.trans$div <- factor(temp.ts.trans$div, levels = c('국립', '공립', '사립'), ordered = T)


temp.ts.trans %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~div, scales = 'free_y') + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(value)), size = 4) + 
  scale_y_continuous(label = scales::percent_format()) +
  labs(x = '연도', y = '증감률', title = '전년대비 비정규 교사증감률(고등)') +
  geom_hline(yintercept = 0, color= 'red') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue")) -> p3

library(gridExtra)

grid.arrange(p1,p2,p3, nrow=3, ncol=1)


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
  labs(x = '연도', y = '교원수', title = '휴직교원수 및 비정규교원수 추이') + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  ggsave("휴직 기간제 교사수.jpg", dpi = 600)



teacher %>% group_by(year, kind) %>%  summarise(sum1 = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total), 
                                                sum2 = sum(temp.total + time.total)) %>% 
  ggplot(aes(x = year)) + geom_line(aes(y = sum1, group = 1, color = 'red')) + 
  geom_line(aes(y = sum2, group = 1, color = 'blue')) + 
  geom_text_repel(aes(y = sum1, label = scales::number_format(big.mark = ',')(sum1))) + 
  geom_text_repel(aes(y = sum2, label = scales::number_format(big.mark = ',')(sum2))) + 
  geom_point(aes(y = sum1)) + 
  geom_point(aes(y = sum2)) + 
  scale_color_discrete(name="교원구분", labels=c('비정규교원', '휴직교원')) + 
  labs(x = '연도', y = '교원수') + 
  theme_bw() +
  facet_wrap(~kind) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  ggsave("학교급별 휴직 기간제 교사수.jpg", dpi = 600)


##############  정규직 교사 대비 기간제 교사수 
teacher %>% 
  group_by(year) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate))) + 
  labs(x = '연도', y = '비율', title = '정규교원 대비 비정규교원 비율', subtitle = '휴직자 제외') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  theme_bw() +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  ggsave("학교급별 휴직 기간제 교사수.jpg", dpi = 600)



##############  정규교원 대비 비정규교원 비율
teacher %>% group_by(year, kind, estkind, scale) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + 
  geom_line(aes(color = kind, group = kind)) + 
  geom_point(aes(color = kind)) + 
  facet_grid(scale ~ estkind) +
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate)), size = 3) + 
  labs(x = '연도', y = '비율', title = '정규교원 대비 비정규교원 비율', color = '학교급') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')

  
##############  학교급별 설립별 기간제 교사비율

teacher %>% group_by(year, kind, estkind) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + 
  geom_line(aes(group = 1)) + 
  geom_point() + 
  facet_grid(kind ~ estkind) +
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate)), size = 3) + 
  labs(x = '연도', y = '비율', title = '학교급별 설립별 기간제 교사비율', color = '학교급') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1') +
  scale_fill_brewer(type = 'div', palette = 'Set1') +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom') +  
  ggsave("학교급별 설립별 기간제 교사비율.jpg", dpi = 600)


##############  학교급별 시도별 정규교원 대비 비정규교원 비율
teacher %>% 
  group_by(year, kind, province) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  theme_bw() +
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate)), size = 3) + 
  labs(x = '연도', y = '비율', title = '정규교원 대비 비정규교원 비율') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  facet_grid(province ~ kind) + 
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')
  

##############  설립별 시도별 정규교원 대비 비정규교원 비율(고등학교)

teacher %>% 
  filter(kind == '고등학교') %>%
  group_by(year, province, estkind) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  theme_bw() +
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate)), size = 3) + 
  labs(x = '연도', y = '비율', title = '고등학교 정규교원 대비 비정규교원 비율(고등학교)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  facet_grid(province ~ estkind) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')



##############  설립별 시도별 정규교원 대비 비정규교원 비율(중학교)

teacher %>% 
  filter(kind == '중학교') %>%
  group_by(year, province, estkind) %>% 
  summarise(temp.sum = sum(temp.total + time.total), 
            teacher.sum = sum(president.total + vicepresident.total + teacher.total)) %>%
  mutate(rate = temp.sum / teacher.sum) %>%
  ggplot(aes(x = year, y = rate)) + geom_line(aes(group = 1), color = 'red') + 
  geom_point() + 
  theme_bw() +
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.1)(rate)), size = 3) + 
  labs(x = '연도', y = '비율', title = '고등학교 정규교원 대비 비정규교원 비율(중학교)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  facet_grid(province ~ estkind) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom')


####################################################################################################


################################################################
foo <- as.data.frame(ggplot_build(p)$data[[1]])
View(foo)

foo %>% filter()

foo[which(foo$count == max(foo$count)),]$y
distinct(foo, x)
p + annotate('text', x = foo$x, y = foo$y, group = foo$group, aes(label = foo$y))
###############################################################



 


teacher %>% 
  group_by(year) %>% summarise(sum = sum(total), sum.std = sum(student)) %>%
  mutate(rate = round(sum.std / sum, 1)) -> data.teacher

(ylim.prim <- range(data.teacher$sum))
(ylim.sec <- range(data.teacher$rate))

(b <- diff(ylim.prim) / diff(ylim.sec))
(a <- b * (ylim.prim[1] - ylim.sec[1]))


dual_plot <- function(data, x, y_left, y_right) {
  x <- ensym(x)
  y_left <- ensym(y_left)
  y_right <- ensym(y_right)
  
  # Introducing ranges
  left_range <- range(data %>% pull(!!y_left))
  right_range <- range(data %>% pull(!!y_right))
  
  data %>%
    select(!!x, !!y_left, !!y_right) %>%
    # Transform
    mutate(!!y_right := scales::rescale(!!y_right, to=left_range)) %>%
    gather(k, v, -!!x) %>%
    ggplot() +
    geom_line(aes(!!x, v, colour = k)) +
    # Change secondary axis scaling and label
    scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(., to=right_range),
                                           name = rlang::as_string(y_right))) +
    labs(y = rlang::as_string(y_left),
         color = "Series")
}

data.teacher %>% dual_plot(year, sum, rate)

data.teacher$rate

data.teacher %>%
  ggplot(aes(x = year, y = sum)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_line(aes(y = a + rate * b, group = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~ (. - a)/b ))



  geom_line(aes(y = rate, group = 1))



  


  teacher %>% 
    group_by(year) %>% summarise(sum = sum(total), sum.std = sum(student)) %>%
    mutate(round(sum.std / sum, 1)*b *4.20 + 245000)
  
    
scale_y_continuous(label = scales::number_format(big.mark = ','), sec.axis = sec_axis(~ (. - a)/b))
  



##############  시간강사 그래프 - 별로
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







  

##############  순회학급
teacher %>% group_by(year, kind) %>% summarise(mean = sum(class.circuit)/sum(class)) %>%
  ggplot(aes(x = year, y = mean)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::percent_format(accuracy = 0.01)(mean))) +
  labs(x = '연도', y = '순회학급 비율', title = '전체 학급수 대비 순회학급 비율') +
  scale_y_continuous(label = scales::percent_format(accuracy = 0.01)) +
  scale_color_discrete(name = '학교급')









teacher %>% group_by(year, estkind) %>% summarise(sum = sum(temp.total + time.total)) %>%
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = estkind, color = estkind)) + 
  geom_point(aes(color = estkind)) + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',', accuracy = 1)(sum))) +
  labs(x = '연도', y = '교사수', title = '설립별 비정규 교사수') +
  scale_color_manual(values = c('red', 'blue', 'dark green', 'purple', 'orange'), name = '설립') + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"))



##############  휴직 교사수
teacher %>% group_by(year) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) +
  labs(x = '연도', y = '교사수', title = '휴직 교사수') + 
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"))

  


teacher %>% group_by(year, kind) %>%  summarise(sum = sum(leave.president.total + leave.vicepresident.total + leave.teacher.total)) %>% 
  ggplot(aes(x = year, y = sum)) + geom_line(aes(group = kind, color = kind)) + 
  geom_point() + 
  geom_text_repel(aes(label = scales::number_format(big.mark = ',')(sum))) + facet_wrap(~kind) + 
  theme_classic() +
  theme(plot.title=element_text(size=20, color="blue"))







