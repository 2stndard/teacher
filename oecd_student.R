if(!require(plotly)) {
  install.packages('plotly')
  library(plotly)
}
oecd.student.origin <- read.csv('d:/R/data/oecd.student.csv', header = T, stringsAsFactors = T, dec = '.')

# oecd.student.origin <- read.table('clipboard', sep = '\t', header = T, dec = '.')

oecd.student <- oecd.student.origin %>% filter(Flag.Codes != 'M')

oecd.student %>% 
  filter(SUBJECT %in% c('PRY')) %>% 
  spread(TIME, Value) %>% 
  write.table('clipboard', sep = '\t', dec = '.')

oecd.student %>% 
  filter(SUBJECT %in% c('PRY_NTRY')) %>% 
  spread(TIME, Value) %>% 
  write.table('clipboard', sep = '\t', dec = '.')


glimpse(oecd.student)
str(oecd)
count(oecd, SUBJECT)

oecd.student.kor <- oecd.student %>% 
  filter(LOCATION == 'KOR') %>%
  filter(SUBJECT == 'PRY_NTRY')
  

ggplotly(oecd.student %>% 
  filter(SUBJECT == 'PRY_NTRY') %>%
  ggplot(aes(x = TIME, y = Value)) +
  geom_line(aes(group = LOCATION, color = LOCATION)) + 
  geom_line(data = oecd.student.kor, mapping = aes(group = LOCATION), color = 'blue', size = 1) +
  theme_bw() 
) 
  
