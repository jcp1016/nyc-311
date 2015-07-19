
nyc311_df <- read.csv("~/DATA/311_Service_Requests_from_2010_to_Present.csv", stringsAsFactors=FALSE)
names(nyc311_df)[6] <- "ComplaintType"
names(nyc311_df)[9] <- "Zipcode"

## Add categories for poverty data
poverty_data$FamPvCategory  <- cut(poverty_data$FamBwPvP,   breaks=c(0,10,20,30,40,50,60), labels=c( "[0-10%)", "[10-20%)","[20-30%)", "[30-40%)", "[40-50%)", "[50-60%)" ), include.lowest=TRUE)
poverty_data$ChldPvCategory <- cut(poverty_data$FCU18BwPvP, breaks=c(0,10,20,30,40,50,60), labels=c( "[0-10%)", "[10-20%)","[20-30%)", "[30-40%)", "[40-50%)", "[50-60%)" ), include_lowest=TRUE)
poverty_data$EldPvCategory  <- cut(poverty_data$P65plBwPvP, breaks=c(0,10,20,30,40,50,60), labels=c( "[0-10%)", "[10-20%)","[20-30%)", "[30-40%)", "[40-50%)", "[50-60%)" ), include_lowest=TRUE)
poverty_all_df  <- poverty_data[,c(1,2,4,43)]
poverty_chld_df <- poverty_data[,c(1,2,4,44)]
poverty_eld_df  <- poverty_data[,c(1,2,4,45)]
setwd("~/Columbia/nyc-311")

## Attach PUMA IDs to 311 data based on a lookup
require("qdapTools")
setwd("~/Columbia/nyc-311/DATA")
zipcode_to_puma <- read.csv("nyc_zcta10_to_puma10.csv", stringsAsFactors=FALSE)
zipcode_to_puma$zcta10 <- as.character(zipcode_to_puma$zcta10)
nyc311_df$PUMA_ID <- lookup(nyc311_df$Zipcode, zipcode_to_puma[,c(1,4)])
nyc311_df$PUMA_ID <- as.factor(nyc311_df$PUMA_ID)

## Clean up complaint type
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "APPLIANCE"] <- "Appliance"
nyc311_df$ComplaintType[nyc311_df$ComplaintType == "GENERAL CONSTRUCTION"] <- "Construction"
nyc311_df$ComplaintType[nyc311_df$CnmplaintType == "CONSTRUCTION"] <- "Construction"
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

## Prep for plotting
require("reshape")
require("gplots")
require("RColorBrewer")
require("ggplot2")
require("ggthemes")
require("dplyr")

## Aggregate by complaint type
by_cat <- group_by( nyc311_df, PUMA_ID, ComplaintType )
nyc311_cat <- summarize( by_cat, n=n() )

## Merge aggregated complaints with poverty stats by PUMA ID
data <- merge(nyc311_cat, poverty_data, by="PUMA_ID")
names(data)[6] <- "PvCategory"

## Build heatmap by poverty level
by_pvcat <- group_by( data, PvCategory, ComplaintType )
s1 <- summarize( by_pvcat, n=sum(n) )
t1 <- melt(s1)
t2 <- cast(t1, ComplaintType ~ PvCategory, sum, margins=TRUE)
names(t2)[ncol(t2)] <- "Total"
t3 <- filter(t2, Total > 16000)
t4 <- t3[order(desc(t3$Total)),]
t4 <- t4[-1,]
row.names(t4) <- t4$ComplaintType
t4 <- t4[,-1]
t4 <- t4[,-ncol(t4)]
data_matrix <- data.matrix(scale(t4))
setwd("~/Columbia/nyc-311")
pdf("heatmap_lvl.pdf", width=10, height=12)
heatmap.2(data_matrix,
          Rowv=NA, Colv=NA,
          dendrogram='none',
          scale='none',
          col=brewer.pal(9,"Blues"),
          main="What A Million Calls to 311 \nReveal About New York",
          key=FALSE,
          keysize=1,
          srtCol=0,
          trace='none',
          offsetRow = 0.5,
          offsetCol = 0.5,
          margins=c(5,12),
          xlab="\nPoverty level based on community district of caller",
          ylab=NULL,
          colsep,
          rowsep,
          sepcolor='white',
          sepwidth=c(0.1,0.1))

dev.off()

data$LongCDName <- paste0(data$PUMA_ID, data$DCP_Boro, data$CD_Name)
## Build heatmap by Borough and community district
## by_dc <- group_by( data, CD_Name, ComplaintType )
by_dc <- group_by( data, LongCDName, ComplaintType )
s1 <- summarize( by_dc, n=sum(n) )
t1 <- melt(s1)
#t2 <- cast(t1, ComplaintType ~ CD_Name, sum, margins=TRUE)
t2 <- cast(t1, ComplaintType ~ LongCDName, sum, margins=TRUE)
names(t2)[ncol(t2)] <- "Total"
t3 <- filter(t2, Total > 5000)
t4 <- t3[order(desc(t3$Total)),]
t4 <- t4[-1,]
row.names(t4) <- t4$ComplaintType
t4 <- t4[,-1]
t4 <- t4[,-ncol(t4)]
data_matrix <- data.matrix(scale(t4))
setwd("~/Columbia/nyc-311")
pdf("heatmap_cd_by_boro.pdf", width=10, height=14)
heatmap.2(data_matrix,
          Rowv=NA, Colv=NA,
          dendrogram='none',
          scale='none',
          col=brewer.pal(9,"Blues"),
          #main="311 Calls by Community",
          key=FALSE,
          keysize=1,
          trace='none',
          offsetRow = 0.5,
          offsetCol = 0.5,
          margins=c(20,12),
          #margins=c(12,5),
          xlab=NULL,
          ylab=NULL)
dev.off()

## To remake the Wired chart, limit results to the top 22 complaint types
setwd("~/Columbia/nyc-311")
by_ct <- group_by( data, ComplaintType )
ct1   <- summarize( by_ct, n=n() )
top_complaints <- filter( ct1, n > 17000 )
top_22 <- inner_join(ct1, top_complaints, by="ComplaintType")
names(top_22)[2] <- "n"
top_22[,3] <- NULL
top_22 <- top_22[order(desc(top_22$n)),]

ggplot(data=top_22, aes(x=ComplaintType, y=n, fill=ComplaintType, width=2)) +
        geom_bar(width=0.8, stat="identity", position=position_dodge(width=0.40)) +
        coord_flip() +
        guides(fill=FALSE) +
        scale_fill_hue() +
        theme_bw() +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(limits=rev(top_22$ComplaintType))
#ggsave("barchart.png", dpi=72, width=10.02, height=7.725)
ggsave("barchart.png")

require(googleVis)
bar <- gvisBarChart(data=top_22, xvar=ComplaintType, yvar=n, options=list(bars='vertical'))
plot(bar)
