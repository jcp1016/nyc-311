##---------------------------------------------------------------------------------
## This script links 2013 3-year ACS data with 2013 NYC 311 data and  
## generates an interactive choropleth map.
## Author:  Janet Prumachuk
##---------------------------------------------------------------------------------
rm(list=ls())
require(rgdal)
require(rgeos)
require(maptools)
require(dplyr)
require(sp)
require(leaflet)

## Read and clean poverty data
setwd("~/Columbia/nyc-311/DATA")
poverty <- read.csv("nyc_poverty_2013.csv", header=TRUE, stringsAsFactors=FALSE)
for (i in 1:ncol(poverty))
        names(poverty)[i] <- poverty[1,i]
xvals <- which(poverty[2,] == "(X)")
poverty_data <- poverty[-1,-xvals]
poverty_data <- poverty_data[,c(2,4,5)]
poverty_data$puma <- as.factor(poverty_data$PUMA_ID)
poverty_data[,3] <- as.numeric(poverty_data[,3])
poverty_data$FamPvCat <- cut(poverty_data$FamBwPvP, 
                             breaks=c(0,10,20,30,40,50), 
                             labels=c( "(0-10%)", "[10-20%)","[20-30%)", "[30-40%)", "[40-50%)" ), 
                             include.lowest=TRUE)

## Get map shapes for New York census tracts
setwd("~/Columbia/nyc-311/DATA/OGPDownload-4")
census_tracts <- readOGR(dsn=".","Columbia_nyct2010ids")
census_tracts <- census_tracts[substring(census_tracts$geoid, 1, 2) == "36",]

## Transform to EPS 4326 - WSG84 (required)
nyc_shapes <- spTransform(census_tracts, CRS("+init=epsg:4326"))

## Join map with poverty data
nyc_shapes@data <- data.frame(nyc_shapes@data, poverty_data[match(nyc_shapes@data$puma, poverty_data$puma),])

# Unite the shapes at puma level
pumas <- unionSpatialPolygons(nyc_shapes, nyc_shapes@data$puma)
nyc_shapes_data <- nyc_shapes@data
puma_data <- unique(nyc_shapes_data[,c(9,17,18,20)])
puma_data <- arrange(puma_data, puma)
row.names(puma_data) <- sapply(slot(pumas, "polygons"), function(x) slot(x, "ID"))
puma_df <- SpatialPolygonsDataFrame(pumas, data=puma_data)

# Format the map popups
m_pop <- paste0("<strong>", 
                puma_df@data$CD_Name,
                "</strong><br />",
                puma_df@data$FamBwPvP,
                "% ",
                "earned below poverty level.")

pal <- colorFactor("Purples", NULL, n=5)

m <- leaflet() %>%
        #addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data=puma_df,
                fillColor = ~pal(puma_df@data$FamPvCat),
                fillOpacity = 1,
                weight = 0.5,
                color = "black",
                stroke = TRUE,
                #dashArray = "3",
                popup = m_pop) %>%
        addControl(html="<b>Percentage of Households Earning Below Poverty Level</b> <br />Click on a neighborhood",
                   position = "topright") %>%
        addLegend("topright", 
              pal = pal, 
              values = puma_df@data$FamPvCat,
              #title = "Percentage of Households Below Poverty Level",
              opacity = 1) %>%
        setView(-73.860161, 40.759741, zoom = 10)

m