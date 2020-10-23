if (!require(kormaps2014)) {
  install.packages('kormaps2014')
  library(kormaps2014)
}

if (!require(sf)) {
  install.packages('sf')
  library(sf)
}
library(plotly)

if (!require(maptools)) {
  install.packages('maptools')
  library(maptools)
}

if (!require(ggmap)) {
  install.packages('ggmap')
  library(ggmap)
}

if (!require(numbers)) {
  install.packages('numbers')
  library(numbers)
}

library(tidyverse)

province.name <- c('11', '26', '27', '28', '29', '30', '31', '36', '41', '42', '43', '44', '45', '46', '47', '48', '50')

shapes <- data.frame()

read.shp <- function(x) {
  return(fortify(readShapePoly(paste0("D:/R/data/map/", x, "000/TL_SCCO_CTPRVN.shp"))) %>%  mutate(region = x))
}

shapes <- map(province.name, read.shp)

shapes.t <- data.frame()

for(i in 1:17) {
  shapes.t <- rbind(shapes.t, shapes[[i]])
}

shapes.t %>%  
  filter(group %in% c('0.1'))  %>% #filter(region == '46') %>%
  plot_ly(x = ~long, y = ~lat, split = ~paste0(region, group), color = I('white'), showlegend = F) %>%  
  add_polygons(line = list(width = 1, color = toRGB('red'))) %>%
  layout(xaxis = list(scaleanchor = 'y', showgrid = F, showticklabels = F, title = NA),
         yaxis = list(showgrid = F, showticklabels = F, title = NA)
        )

lapply(shapes, View)


dir <- ('D:/R/data/map/location/')

file.list <- list.files(dir, pattern = 'entrc_')

data <- data.frame()

for(file in file.list) {
  print(file)
  temp <- read.delim(paste0(dir, file), sep = '|', header = F)
  data <- rbind(data, temp)
}

d <- data %>% filter(is.na(V17) == F, is.na(V18) == F, V14 %in% '교육및복지시설')

d <- data %>% filter(is.na(V17) == F, is.na(V18) == F, grepl('대학교', V12), V14 %in% '교육및복지시설')

dim(d)
dim(data)



shapes.t %>%  
  filter(group %in% c('0.1'))  %>% #filter(region == '46') %>%
  plot_ly(x = ~long, y = ~lat, color = I('white'), showlegend = F) %>%  
  add_polygons(split = ~paste0(region, group), line = list(width = 1, color = toRGB('red'))) %>%
  layout(xaxis = list(scaleanchor = 'y', showgrid = F, showticklabels = F, title = NA),
         yaxis = list(showgrid = F, showticklabels = F, title = NA)
  ) %>%
  add_data(data = d) %>%
  add_markers(x = ~V17, y = ~V18, marker = list(size = 3, color = toRGB('blue'), opacity = 1), 
              hoverinfo = 'text', 
              text = ~V12)
