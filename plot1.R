#######################################################
## Plot 1
#######################################################

### Begin Setup ###

## Load Required libraries
library(ggplot2)
library(data.table)

## Read in National Emissions Inventory Dataset
if (!exists("NEI.data")) {
  NEI.data <- as.data.table(readRDS("summarySCC_PM25.rds"))
}

## Read in Source Code Classification Dataset
if (!exists("SCC.data")) {
  SCC.data <- as.data.table(readRDS("Source_Classification_Code.rds"))
}

### End Setup ###

## Calculate total emissions per year
total.emissions.by.year <- with(NEI.data, aggregate(Emissions, by=list(year), sum))

## Reset the names
names(total.emissions.by.year) <- c("Year", "Emissions")

## Open the PNG file device
png(filename = "plot1.png", width = 480, height = 480, units = "px")

## Plot total emissions per year (without the axes)
plot(total.emissions.by.year,
     type = "b",
     pch = 15,
     ylim = c(0, 8e06),
     ylab = "Total Emissions (in 100,000 tons)", 
     main = "Total Emissions by Year",
     axes = FALSE)

## Render the x axis for the valid years
axis(1, at = seq(min(total.emissions.by.year$Year),max(total.emissions.by.year$Year), by = 3))

## Render the y axis in units of 100,000
axis(2, at = seq(0, 8e6, by = 1e6), labels = seq(0, 8, by = 1))

## Draw the box around the plot
box()

## Close the file device
dev.off()