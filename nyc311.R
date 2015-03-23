require("qdapTools")

setwd("~/Columbia/BlogPostProject/DATA")
zipcode_to_puma <- read.csv("nyc_zcta10_to_puma10.csv", stringsAsFactors=FALSE)
zipcode_to_puma$zcta10 <- as.character(zipcode_to_puma$zcta10)
nyc311_df$PUMA_ID <- lookup(nyc311_df$Zipcode, zipcode_to_puma[,c(1,4)])
nyc311_df$PUMA_ID <- as.factor(nyc311_df$PUMA_ID)

by_puma_cat <- group_by( nyc311_df, PUMA_ID, `Complaint.Type`, Descriptor )
nyc311_aggr <- summarize( by_puma_cat, PUMA_ID, n=n() )
#data_by_day   <- na.omit(d2)

by_cat <- group_by( nyc311_aggr, PUMA_ID, `Complaint.Type` )
nyc311_cat <- summarize( by_cat, n=sum(n) )

by_cat <- group_by( nyc311_cat, `Complaint.Type` )
nyc311_rank <- summarize( by_cat, n=sum(n) )

s1 <- nyc311_aggr %>%
        select(PUMA_ID, n, `Complaint.Type`, Descriptor) %>%
        arrange(PUMA_ID, desc(n))

s2 <- nyc311_cat %>%
        select( PUMA_ID, n, `Complaint.Type` ) %>%
        arrange( PUMA_ID, desc(n) )

s3 <- nyc311_rank %>%
        arrange( desc(n) )

#Create categories for poverty data
poverty_data$FamPvCategory <- cut(poverty_data$FamBwPvP, breaks=c(0,10,20,30,40,50), labels=c("0-10%", "10-20%","20-30%", "30-40%", "40-50%" ))
poverty_data$ChldPvCategory <- cut(poverty_data$FCU18BwPvP, breaks=c(0,10,20,30,40,50), labels=c("0-10%", "10-20%","20-30%", "30-40%", "40-50%" ))
poverty_data$EldPvCategory <- cut(poverty_data$P65plBwPvP, breaks=c(0,10,20,30,40,50), labels=c("0-10%", "10-20%","20-30%", "30-40%", "40-50%" ))
