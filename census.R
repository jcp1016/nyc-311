require("dplyr")
require("ggmap")
require("ggplot2")
require("ggthemes")
require("ggvis")
#require("gpclib")
#require("graphics")
#require("grDevices")
#require("grid")
#require("gridExtra")
require("lattice")
#require("lubridate")
require("maps")
require("maptools")
require("RColorBrewer")
require("rgdal")
require("rgeos")
#require("shiny")
require("sp")

## Read and clean poverty data
setwd("~/Columbia/BlogPostProject/DATA")
poverty <- read.csv("nyc_poverty_2013.csv", header=TRUE, stringsAsFactors=FALSE)
for (i in 1:ncol(poverty)) { names(poverty)[i] <- poverty[1,i] }
xvals <- which(poverty[2,] == "(X)")
poverty_data <- poverty[-1,-xvals]
poverty_data$PUMA_ID <- as.factor(poverty_data$PUMA_ID)
for (i in 5:42) {poverty_data[,i] <- as.numeric(poverty_data[,i])}

## Get map
setwd("~/Columbia/BlogPostProject/DATA/OGPDownload-4")
census_tracts <- readOGR(dsn=".","Columbia_nyct2010ids")
nyc_shapes <- spTransform(census_tracts, CRS("+proj=longlat + datum=WGS84"))
 
## Merge map with poverty data  
nycmap_df <- fortify(nyc_shapes)
map_data  <- data.frame(id=rownames(nyc_shapes@data), 
                        PUMA_ID=nyc_shapes@data$puma, 
                        GEO_ID=nyc_shapes@data$geoid )
map_data  <- merge(map_data, poverty_data, by="PUMA_ID")
map_df    <- merge(nycmap_df, map_data, by="id")

## Make plots
setwd("~/Columbia/BlogPostProject")
qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=id, fill=FamBwPvP), 
                     data=map_df, alpha=.9) +
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of People\nLiving Below\nPoverty Level", colours=c("white", brewer.pal(5,"YlOrRd")), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
ggsave("map1.png", dpi=72, width=10.02, height=7.725)

qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=FCU18BwPvP), 
                     data=map_df, alpha=.9) + 
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of Children (U18)\nLiving Below\nPoverty Level", colours=c("white", brewer.pal(5,"YlOrRd")), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
ggsave("map2.png", dpi=72, width=10.02, height=7.725)

qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=P65plBwPvP), 
                     data=map_df, alpha=.9) + 
        ggtitle("Poverty in New York City") +
        scale_fill_gradientn("% of Elderly (O65)\nLiving Below\nPoverty Level", colours=c("white", brewer.pal(5,"YlOrRd")), na.value="grey20", guide="colourbar") +
        theme(plot.title = element_text(size=16, face="bold"))
ggsave("map3.png", dpi=72, width=10.02, height=7.725)