# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Select "best" data from various conflicting values

# ---- Setup ----
library(rgdal)
library(geojsonio)
#library(jsonlite)
#library(spatialEco)

setwd("C:/Dev/Projects/CookEastPlantHandHarvest/R/cleaningHY2014")

hy2014 <- read.csv("Input/HY2014GB_aggregate_all_data_171122.csv")
hy2014$ShoudKeep <- NULL
georef <- geojson_read("Input/CookEast_GeoreferencePoints_171127.json",
                       what = "sp")
# Merge the data based on col and row2 because John Morse said he focused on
# row and columns, not ID2 values (this is important because ID2 values are not
# consistent with row/column values)
df <- merge(hy2014, georef,
           by.x = c("Col", "Row2"),
           by.y = c("Column", "Row2"),
           all.x = TRUE)

# Remove missing data
dfc <- df[!is.na(df$ID2.y),]

# Create dataframe with "good" values
dfg <- subset(dfc, FALSE)

for (id in 1:max(dfc$ID2.y, na.rm = TRUE)) {
    reps <- dfc[dfc$ID2.y == id,]

    # Keep if is only value
    if (nrow(reps) == 1) {
        dfg <- rbind(dfg, reps)
        next
    }

    # Keep if NIR data, next iteration if found
    rep.withProtein <- reps[which(!is.na(reps$protein)),]
    if (nrow(rep.withProtein) == 1) {
        dfg <- rbind(dfg, rep.withProtein)
        next
    }
    else if (nrow(rep.withProtein) > 1) {
        # If all rows are duplicates, then keep first row
        if (nrow(rep.withProtein[duplicated(rep.withProtein), ]) == 
            (nrow(rep.withProtein) / 2)) {
              dfg <- rbind(dfg, rep.withProtein[1,])
              next
        }
        else if (length(unique(rep.withProtein$TotalGrain.g.))==1) {
            # If grain mass are all the same, just take first row
            dfg <- rbind(dfg, rep.withProtein[1,])
            next
        }
        else {
            stop(paste("id:", id, "- something wrong with protein"))
        }
    }

    # Keep if weighed in 2015, next iteration if so
    rep.weighed2015 <- reps[
        which(
            as.numeric(format(
                as.Date(reps$LastSaved, origin = '1900-1-1'), "%Y")) >= 2015),]
    if (nrow(rep.weighed2015) == 1) {
        dfg <- rbind(dfg, rep.weighed2015)
        next
    }
    else if (nrow(rep.weighed2015) > 1) {
        # If all rows are duplicates, then keep first row
        if (nrow(rep.weighed2015[duplicated(rep.weighed2015), ]) ==
            (nrow(rep.weighed2015) / 2)) {
            dfg <- rbind(dfg, rep.weighed2015[1,])
            next
        }
        else if (length(unique(rep.weighed2015$TotalGrain.g.)) == 1) {
            # If grain mass are all the same, just take first row
            dfg <- rbind(dfg, rep.weighed2015[1,])
            next
        }
        else {
            stop(paste("id:", id, "something wrong checking weighed in 2015"))
        }
    }

    # Keep if has grain+bag value
    rep.withGrainBagWeight <- reps[which(!is.na(reps$TareBag.g..1)),]
    if (nrow(rep.withGrainBagWeight) == 1) {
        dfg <- rbind(dfg, rep.withGrainBagWeight)
        next
    }
    else if (nrow(rep.withGrainBagWeight) > 0) {
        # If all rows are duplicates, then keep first row
        if (nrow(rep.withGrainBagWeight[duplicated(rep.withGrainBagWeight), ]) ==
            (nrow(rep.withGrainBagWeight) / 2)) {
                dfg <- rbind(dfg, rep.withGrainBagWeight[1,])
                next
        }
        else if (length(unique(rep.withGrainBagWeight$TotalGrain.g.)) == 1) {
            # If grain mass are all the same, just take first row
            dfg <- rbind(dfg, rep.withGrainBagWeight[1,])
            next
        }
        else {
            stop(paste("id:", id, "something wrong with grain+bag weight"))
        }
        
    }
}

filePath <- paste(
                  "Output/selectValueTest_",
                  format(Sys.Date(), "%y%m%d"),
                  ".csv",
                  sep = "")
write.csv(dfg, filePath)