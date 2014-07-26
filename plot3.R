#######################################################
## Plot 3
#######################################################

### Begin Setup ###

## Load Required libraries
library(ggplot2)
library(grid)
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

## Subset emissions for Baltimore
baltimore.emissions <- NEI.data[fips == "24510"]

## Open the PNG file device
png(filename = "plot3.png", width = 720, height = 640, units = "px")

## Plot the emissions (using a log transformation as an attempt to somehwat normalize
## the rather wide distribution) for Baltimore by year broken out by source and show linear 
## regression line for trend
ggplot(data = baltimore.emissions, aes(x = year, y = log(Emissions + 1))) + 
  geom_point(aes(colour = type), size = 2) +
  facet_grid(.~type) +
  theme(panel.margin = unit(0.8, "lines"),
        plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  stat_smooth(method = "loess", size = 1.0, col = "steelblue", se = FALSE) +
  stat_smooth(method = "lm", size = 0.5, col = "sienna", se = FALSE) +
  scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
  labs(x = "Year", 
       y = "ln(Emissions + 1)", 
       title = "Emissions by Year in Baltimore per Source",
       colour = "Source")

## Close the file device
dev.off()