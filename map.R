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

if (!require(Math)) {
  install.packages('Math', repos = 'https://cloud.r-project.org/')
  library(Math)
}
library(tidyverse)

korea_map_shp = readShapePoly("D:/R/git/teacher/map/CTPRVN.shp")
korea_map = fortify(korea_map_shp)

korea_map %>% 
  filter(group %in% c('0.1', '1.1','2.1','3.1','4.1','5.1','6.1','7.1','8.1','9.1','10.1','11.1','12.1','13.1','14.1','15.1','16.1')) %>%  
  plot_ly(x = ~long, y = ~lat, split = ~group, color = I('white'), showlegend = F) %>%  
  add_polygons(line = list(width = 0.4, color = toRGB('black'))) %>%
  layout(xaxis = list(scaleanchor = 'y', showgrid = F, showticklabels = F, title = NA),
         yaxis = list(showgrid = F, showticklabels = F, title = NA)
         )
         
         
korea_map %>% mutate(group1 = as.numeric(as.character(korea_map$group)) - as.numeric(id)) %>%  
  filter(group %in% c('1.1')) %>% head
