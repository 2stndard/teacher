#########   oecd spending
oecd.spending.origin <- read.csv('d:/R/data/oecd.spending.csv', header = T, stringsAsFactors = T, dec = '.')

oecd.spending <- oecd.spending.origin %>% filter(Flag.Codes != 'M')
