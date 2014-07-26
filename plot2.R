#######################################################
## Plot 2
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
total.baltimore.emissions.by.year <- with(NEI.data[fips == "24510"], 
                                          aggregate(Emissions, by=list(year), sum))
## Reset the names
names(total.baltimore.emissions.by.year) <- c("Year", "Emissions")

## Open the PNG file device
png(filename = "plot2.png", width = 480, height = 480, units = "px")

## Plot total emissions for Baltimore per year (without the axes)
plot(total.baltimore.emissions.by.year,
     type = "b",
     pch = 15,
     ylim = c(0, 4000),
     ylab = "Total Emissions (in 1,000 tons)", 
     main = "Total Emissions by Year for Baltimore",
     axes = FALSE)

## Render the x axis for the valid years
axis(1, at = seq(
  min(total.baltimore.emissions.by.year$Year),
  max(total.baltimore.emissions.by.year$Year), 
  by = 3))

## Render the y axis in units of 100,000
axis(2, at = seq(0, 4000, by = 1000), labels = seq(0, 4, by = 1))

## Draw the box around the plot
box()

## Close the file device
dev.off()