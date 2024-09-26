library(dplyr)
library(leaflet)

df1 <- read.csv("C:/Users/chikazhet/haurakimap.csv")
colnames(df1)[1] <- "Farm"
df1$region <- factor(df1$region)
#df1$region <- LETTERS[df1$region]

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'black'),
  domain = df1$region
)

## OR automatically generate color palettes
pal <- colorFactor(
  palette = 'Dark2',
  domain = df1$region
)

# Create leaflet map
leaflet(df1) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             color = ~pal(region), 
             radius = 2000)%>% 
  addLegend("topright",  pal = pal,labels =df1$Farm,  values = df1$region, title="Selected Hauraki catchment case study farms")
