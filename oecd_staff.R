oecd.staff.origin <- read.csv('d:/R/data/oecd.teachingstaff.csv', header = T, stringsAsFactors = T, dec = '.')

# oecd.student.origin <- read.table('clipboard', sep = '\t', header = T, dec = '.')

str(oecd.staff.origin)
oecd.staff <- oecd.staff.origin %>% filter(Flag.Codes != 'M')


str(oecd.staff)
tail(oecd.staff)

oecd.staff %>% filter(TIME == '2018') %>% group_by(LOCATION, SUBJECT) %>% summarise(sum = sum(Value)) %>%
  as.data.frame() -> oecd.staff.sum

ggplotly(
oecd.staff.sum %>%
  ggplot(aes(x = LOCATION, y = sum)) + 
  geom_point(aes(shape = SUBJECT, color = SUBJECT)) +
  theme(plot.title=element_text(size=20, color="blue"), 
        legend.text=element_text(size=12), 
        legend.position = 'bottom', 
        axis.text.x=element_text(angle=90, hjust=1, vjust = 0))
)


  
