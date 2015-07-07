# A basic leaflet map
library(leaflet)
library(maps)
library(rgdal)

m <- leaflet() %>%
        + addTiles %>%
        + addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m

#map_FamBwPvP_df <- map_df[,c(2,3,4,12,13)]
my_map <- map("state", region="new york", fill=TRUE, boundary=FALSE, plot=FALSE)
map_data <- map_df[,c(2,3,12)]

cm <- leaflet() %>%
        + addTiles() %>%
        + addPolygons(data=map_data, fillColor = topo.colors(10, alpha=NULL), stroke=FALSE)
cm

leaflet(height="300px") %>% addPolygons(data=nyc_shapes)
