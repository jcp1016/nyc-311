## This script reads a dataset that was extracted from the 2013 ACS and generates static choropleth maps.

libs <- c("dplyr","ggmap","ggplot2","ggthemes","maps","maptools","RColorBrewer","rgdal","rgeos",
          "sp","scales","plyr","reshape2", "ggplot2")
x <- sapply(libs,function(x)if(!require(x,character.only = T)) install.packages(x))
rm(x,libs)

## Read and clean poverty data
setwd("~/Columbia/nyc-311/DATA")
poverty <- read.csv("nyc_poverty_2013.csv", header=TRUE, stringsAsFactors=FALSE)
for (i in 1:ncol(poverty))
        names(poverty)[i] <- poverty[1,i]
xvals <- which(poverty[2,] == "(X)")
poverty_data <- poverty[-1,-xvals]
poverty_data$PUMA_ID <- as.factor(poverty_data$PUMA_ID)
for (i in 5:42)
        poverty_data[,i] <- as.numeric(poverty_data[,i])

## Get map shapes for census tracts
setwd("~/Columbia/nyc-311/DATA/OGPDownload-4")
census_tracts <- readOGR(dsn=".","Columbia_nyct2010ids")
nyc_shapes <- spTransform(census_tracts, CRS("+proj=longlat + datum=WGS84"))

## Merge map shapes with poverty data
nycmap_df <- fortify(nyc_shapes)
map_data  <- data.frame(id=rownames(nyc_shapes@data),
                        PUMA_ID=nyc_shapes@data$puma,
                        GEO_ID=nyc_shapes@data$geoid )
map_data  <- merge(map_data, poverty_data, by="PUMA_ID")
map_df    <- merge(nycmap_df, map_data, by="id")


## Make plots
qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=id, fill=FamBwPvP),
                     data=map_df, alpha=.9) +
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of People\nLiving Below\nPoverty Level", colours=brewer.pal(4,"GnBu"), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
#ggsave("map1.png", dpi=72, width=10.02, height=7.725)

qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=FCU18BwPvP),
                     data=map_df, alpha=.9) +
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of Children (U18)\nLiving Below\nPoverty Level", colours=c("white", brewer.pal(5,"YlOrRd")), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
##ggsave("map2.png", dpi=72, width=10.02, height=7.725)

qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=P65plBwPvP),
                     data=map_df, alpha=.9) +
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of Elderly (O65)\nLiving Below\nPoverty Level", colours=c("white", brewer.pal(5,"YlOrRd")), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
##ggsave("map3.png", dpi=72, width=10.02, height=7.725)

## New code for leaflet
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap") 
install.packages(x) # warning: this may take a number of minutes 
lapply(x, library, character.only = TRUE) # load the required packages

map_df2 <- map_df[,c(1:13)]
## Write data to GeoJSON
setwd("~/Columbia/nyc-311/DATA")
fn <- "data_geojson"
if (file.exists(fn)) {
    file.remove(fn)
}
#writeOGR(nyc_shapes, fn, layer="", driver="GeoJSON")
#m_data <- leafletR::toGeoJSON(data=nyc_shapes, name="data")
m_data <- leafletR::toGeoJSON(data=map_df2, name="data")
#m_data <- system.file(package="leafletR", "files", "data.geojson")

#labels <- c( "(0-10%)", "[10-20%)","[20-30%)", "[30-40%)", "[40-50%)", "[50-60%)" )
require("RColorBrewer")
m_colors <- brewer.pal(6, "Blues")
m_style <-  leafletR::styleGrad(prop="FamBwPvP", breaks=c(0,10,20,30,40,50,60), 
                                closure="right",
                                out=0,
                                style.par="col",
                                style.val=m_colors,
                                lwd=2,
                                leg="Percentage of families earning \nbelow poverty level", 
                                fill.alpha=0.8)

#map_popup <- c("CD_Name", "FamBwPvP")
m_pop <- paste0("<strong>Community: </strong>", 
                map_data$CD_Name,
                "<br><strong>Percentage of families earning below poverty level: </strong>", 
                map_data$FamBwPvP)

map <- leafletR::leaflet(data=m_data, title="index", base.map="osm", 
                         incl.data=TRUE, style=m_style, popup=c("CD_Name", "FamBwPvP", "FamPvCat"),
                         overwrite=TRUE, center=c(40.7471983, -73.9983273), zoom=11)