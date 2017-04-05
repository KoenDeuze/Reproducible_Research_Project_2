library(ggplot2) # enhanced grahics
library(ggthemes) # advanced themese
library(reshape2)
library(lattice)

# download the zip file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="./repdata-data-StormData.csv.bz2", method="curl")


# read the csv from the zipfile
stormdata <- read.csv('./repdata-data-StormData.csv.bz2', header=T)

# examine which event (based on EVTYPE in the dataset) are the most harmful to population health. We will make a difference between the number of injuries en the number of fatalities
injuries_per_event <- aggregate(stormdata$INJURIES, by=list(stormdata$EVTYPE), FUN=sum)
names(injuries_per_event) <- c("event","injuries")
fatalities_per_event <- aggregate(stormdata$FATALITIES, by=list(stormdata$EVTYPE), FUN=sum)
names(fatalities_per_event) <- c("event","fatalities")

injuries_and_fatalities <- merge(x = fatalities_per_event, y = injuries_per_event, by = "event", all = TRUE)
injuries_and_fatalities$total <- injuries_and_fatalities$injuries + injuries_and_fatalities$fatalities

top10_injuries_and_fatalities <- head(injuries_and_fatalities[order(injuries_and_fatalities$total,decreasing = T),],10)
top10_injuries_and_fatalities <- subset(top10_injuries_and_fatalities, select = c(event,injuries,fatalities) )

top10_injuries_and_fatalities <- melt(top10_injuries_and_fatalities, id.vars = 'event')
names(top10_injuries_and_fatalities) <- c("Event", "fatality_injury", "Count")

ggplot(top10_injuries_and_fatalities, aes(x = Event, y = Count, fill = fatality_injury)) +
        geom_bar(stat = "identity") +    
        labs(fill="") +
        labs(x="", y="number of injuries and fatalities") + 
        ggtitle("Weather-related Fatality & Injury Counts")+
        coord_flip()