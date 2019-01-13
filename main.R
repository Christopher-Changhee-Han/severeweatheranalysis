# Download file
linkurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(linkurl, "stormdata.csv")

# Load the data

storm <- read.csv("stormdata.csv", stringsAsFactors = FALSE)

# Data preprocessing

library(dplyr)
library(lubridate)
head(storm)

# Make new data frame with the relevant variables.
new.storm <- select(storm, 
                    BGN_DATE,
                    EVTYPE,
                    FATALITIES,
                    INJURIES,
                    PROPDMG,
                    PROPDMGEXP,
                    CROPDMG,
                    CROPDMGEXP)

names(new.storm) <- tolower(names(new.storm))
names(new.storm) <- sub("_", ".", names(new.storm))

# Subset with only the rows containing data from January 1996
# This is when they started collecting all event types

new.storm$bgn.date <- mdy_hms(new.storm$bgn.date)
after1996 <- filter(new.storm, 
                    bgn.date >= "1996-01-01", 
                    fatalities > 0,
                    injuries > 0,
                    propdmg > 0,
                    cropdmg > 0 
                   )

# pre-process the variable event type
after1996$evtype <- tolower(after1996$evtype)
after1996$evtype <- gsub(" ", "", after1996$evtype)
after1996$evtype <- gsub("-", "", after1996$evtype)
after1996$evtype <- sub("^(hurricane).*", "hurricane", after1996$evtype)
after1996$evtype <- sub("(.*flood)$", "flood", after1996$evtype)
after1996$evtype <- sub("(.*wind)$", "thunderstormwind", after1996$evtype)

# Most dangerous to human health

population.health <- after1996 %>%
        group_by(evtype) %>%
        summarise(
                fatalities = sum(fatalities),
                injuries = sum(injuries)
        )
library(ggplot)
library(reshape2)
population.health.long<-melt(population.health, id.vars = "evtype")

ggplot(data = population.health.long,
        aes(x = evtype, y = value, fill = variable)) + 
        geom_bar(stat = 'identity', position = 'dodge') + 
        labs(title = "Total Fatalities and Injuries from 1996-2011", 
             x = "Event Type")

# Greatest economic consequences

# Create new variables prop.value, crop.value that takes into account the exponents

exponent.key <- c(K = 1000, M = 1000000, B = 1000000000)
econ.dmg <- after1996 %>%
        group_by(evtype) %>%
        summarise(
                property = sum(propdmg * exponent.key[propdmgexp]),
                crop = sum(cropdmg * exponent.key[cropdmgexp])
        )

econ.dmg.long <- melt(econ.dmg, id.vars = "evtype")

ggplot(data = econ.dmg.long,
       aes(x = evtype, y = value, fill = variable)) + 
        geom_bar(stat = 'identity', position = 'dodge') + 
        labs(title = "Total Property and Crop Damage from 1996-2011", 
             x = "Event Type", fill = "Type of Damage")

