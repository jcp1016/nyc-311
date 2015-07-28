##---------------------------------------------------------------------------------
## This script links 2013 3-year ACS data with 2013 NYC 311 data and
## generates geoJSON file for use in an interactive choropleth map in javascript
##
##---------------------------------------------------------------------------------
#rm(list=ls())
require(rgdal)
require(rgeos)
require(maptools)
require(dplyr)
require(sp)
require(leaflet)

## Read and clean poverty data
setwd("./DATA")
poverty <- read.csv("nyc_poverty_2013.csv", header=TRUE, stringsAsFactors=FALSE)
for (i in 1:ncol(poverty))
    names(poverty)[i] <- poverty[1,i]
xvals <- which(poverty[2,] == "(X)")
poverty_data <- poverty[-1,-xvals]
poverty_data <- poverty_data[,c("PUMA_ID", "CD_Name", "PBwPvP", "FamBwPvP", "PU18BwPvP", "PU5cBwPvP", "P65plBwPvP")]

poverty_data$puma <- as.factor(poverty_data$PUMA_ID)
for (i in 3:7) {
        poverty_data[,i] <- as.numeric(poverty_data[,i])
        poverty_data[,i] <- round(poverty_data[,i], digits=0)
}
poverty_data$PvCat <- cut(poverty_data$PBwPvP,
                             breaks=c(0,10,20,30,40,50,60),
                             include.lowest=TRUE, right=FALSE)

poverty_data$FamPvCat <- cut(poverty_data$FamBwPvP,
                             breaks=c(0,10,20,30,40,50,60),
                             include.lowest=TRUE, right=FALSE)

poverty_data$U18PvCat <- cut(poverty_data$PU18BwPvP,
                             breaks=c(0,10,20,30,40,50,60),
                             include.lowest=TRUE, right=FALSE)

poverty_data$U5PvCat <- cut(poverty_data$PU5cBwPvP,
                             breaks=c(0,10,20,30,40,50,60),
                             include.lowest=TRUE, right=FALSE)

poverty_data$O65PvCat <- cut(poverty_data$P65plBwPvP,
                            breaks=c(0,10,20,30,40,50,60),
                            include.lowest=TRUE, right=FALSE)

## Read and clean 311 data
nyc311_df <- read.csv("~/DATA/311_Service_Requests_from_2010_to_Present.csv", stringsAsFactors=FALSE)
names(nyc311_df)[6] <- "ComplaintType"
names(nyc311_df)[9] <- "Zipcode"

## Attach PUMA IDs to 311 data based on a zip code to PUMA mapping table
require("qdapTools")
setwd("~/Columbia/nyc-311/DATA")
zipcode_to_puma <- read.csv("nyc_zcta10_to_puma10.csv", stringsAsFactors=FALSE)
zipcode_to_puma$zcta10 <- as.character(zipcode_to_puma$zcta10)
nyc311_df$PUMA_ID <- lookup(nyc311_df$Zipcode, zipcode_to_puma[,c(1,4)])
nyc311_df$PUMA_ID <- as.factor(nyc311_df$PUMA_ID)

## Clean up complaint type
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "APPLIANCE"] <- "Appliance"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "GENERAL CONSTRUCTION"] <- "Construction"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "CONSTRUCTION"] <- "Construction"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "Derelict Vehicles"] <- "Derelict Vehicle"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "ELECTRIC"] <- "Electric"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "GENERAL"] <- "General"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "HEATING"] <- "Heating"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "NONCONST"] <- "Non construction"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "PAINT - PLASTER"] <- "Paint/plaster"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "PAINT/PLASTER"] <- "Paint/plaster"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "PLUMBING"] <- "Plumbing"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "STRUCTURAL"] <- "Structural"
nyc311_df$ComplaintType[substr(nyc311_df$ComplaintType,1,5)  == "Noise"] <- "Noise"
nyc311_df$ComplaintType[substr(nyc311_df$ComplaintType,1,11) == "Street Sign"] <- "Street Sign Damaged/Missing"
nyc311_df$ComplaintType[substr(nyc311_df$ComplaintType,1,10) == "Fire Alarm"] <- "Fire Alarm Addn/Modif/Insp"
nyc311_df$ComplaintType[substr(nyc311_df$ComplaintType,1,12) == "Highway Sign"] <- "Highway Sign Damaged/Missing"
nyc311_df$ComplaintType[substr(nyc311_df$ComplaintType,1,20) == "General Construction"] <- "Construction"

## Aggregate by complaint type
by_cat <- group_by( nyc311_df, PUMA_ID, ComplaintType )
nyc311_cat <- summarize( by_cat, n=n() )

## Merge aggregated complaints with poverty stats by PUMA ID
data <- merge(nyc311_cat, poverty_data, by="PUMA_ID")
names(data)[6] <- "PvCategory"
names(data)[3] <- "NumCalls"

## Include totals by PUMA
by_puma <- group_by( nyc311_df, PUMA_ID)
ptotal <- summarize( by_puma, n=n() )
names(ptotal)[2] <- "TotalCalls"
data <- merge(data, ptotal, by="PUMA_ID")
data$CallPct <- round((data$NumCalls / data$TotalCalls) * 100, digits=0)
data <- arrange(data, PUMA_ID, desc(CallPct))

## Keep top n call categories by PUMA and combine with poverty data
n <- 10
require(data.table)
d <- data.table(data, key="CallPct")
top_calls <- d[, tail(.SD, n), by=PUMA_ID]
top_calls <- arrange(top_calls, PUMA_ID, desc(CallPct))
tc <- top_calls[,.(PUMA_ID, ComplaintType, CallPct)]
p <- as.vector(unique(tc$PUMA_ID))
complaint_data <- data.frame(stringsAsFactors=FALSE)
for (i in 1:length(p)) {
    puma_rows  <- data.frame(filter(tc, PUMA_ID == p[i]), stringsAsFactors=FALSE)
    tmp_df <- data.frame(p[i], puma_rows[c(1:n), c(2,3)], stringsAsFactors=FALSE)
    names(tmp_df) <- c("PUMA_ID", paste("ComplaintType",i), paste("CallPct",i))
    complaint_data <- cbind(complaint_data, tmp_df)
    rm(tmp_df)
}
complaint_data <- unique(complaint_data)
all_data <- suppressWarnings(left_join(poverty_data, complaint_data, by="PUMA_ID"))
rm(d, top_calls, tc, p, complaint_data)

## Get map shapes for New York City census tracts
setwd("./OGPDownload-4")
census_tracts <- readOGR(dsn=".","Columbia_nyct2010ids")
census_tracts <- census_tracts[substring(census_tracts$geoid, 1, 2) == "36",]

## Transform to EPS 4326 - WSG84 (required)
nyc_shapes <- spTransform(census_tracts, CRS("+init=epsg:4326"))

## Join map with data
nyc_shapes@data <- data.frame(nyc_shapes@data, all_data[match(nyc_shapes@data$puma, all_data$puma),])

# Unite shapes at puma level
pumas <- unionSpatialPolygons(nyc_shapes, nyc_shapes@data$puma)
nyc_shapes_data <- nyc_shapes@data
puma_data <- unique(nyc_shapes_data[,c(9,17,18,20:30)])
puma_data <- arrange(puma_data, puma)
row.names(puma_data) <- sapply(slot(pumas, "polygons"), function(x) slot(x, "ID"))
puma_df <- SpatialPolygonsDataFrame(pumas, data=puma_data)

# Write data to a geoJSON file for a JavaScript version of the map
setwd("~/Columbia/nyc-311/www/scripts")
jsfile = "pumas.js"
if (file.exists(jsfile)) {
    file.remove(jsfile)
}
writeOGR(puma_df, jsfile, layer="", driver="GeoJSON")

# Insert variable declaration at beginning of geoJSON file
jsfile = "var.txt"
if (file.exists(jsfile)) {
    file.remove(jsfile)
}
fileConn <- file(jsfile)
writeLines("var pumaData = ", fileConn)
close(fileConn)

system("cat var.txt pumas.js > polygons.js")
file.remove("pumas.js")
file.remove("var.txt")
setwd("../../")

## Now index.html can be viewed in any browser
browseURL("www/index.html")
