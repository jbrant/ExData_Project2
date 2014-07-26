#######################################################
## Plot 5
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

## Merge the NEI and SCC datasets, including only the following columns:
## 1. SCC
## 2. Emissions
## 3. year
## 4. EI.Sector
baltimore.data.merged <- merge(
  x = baltimore.emissions[, c("SCC", "Emissions", "year"), with = FALSE], 
  y = SCC.data[, c("SCC", "EI.Sector"), with = FALSE], 
  by = "SCC")

## Extract emissions data for motor vehicle sources
## Note: this simply greps on "Vehicles" as I'm assuming "motor vehicles" entails
## automobiles that travel on the road and every Mobile entry that didn't include
## "Vehicle" in the name was non-road equipment, marine vessels, aircaraft, or
## locomotives.
baltimore.vehicles.data <- baltimore.data.merged[
  EI.Sector %in% baltimore.data.merged[
    grep("Vehicles", baltimore.data.merged$EI.Sector)
  ]$EI.Sector
]

## Calculate total emissions from motor vehicle sources per year
total.baltimore.vehicle.emissions <- aggregate(
  baltimore.vehicles.data$Emissions, 
  list(baltimore.vehicles.data$year), 
  sum
)

## Reset the names
names(total.baltimore.vehicle.emissions) <- c("Year", "Emissions")

## Open the PNG file device
png(filename = "plot5.png", width = 640, height = 640, units = "px")

## Plot the total emissions from motor vehicles in Baltimore for each of the four years
ggplot(total.baltimore.vehicle.emissions, aes(x = Year, y = Emissions)) +
  geom_point(color = "indianred", size = 3) +
  geom_line(color = "steelblue") +
  theme(panel.margin = unit(0.8, "lines"),
        plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
  labs(x = "Year", 
       y = "Total Emissions (in tons)", 
       title = "Total Motor Vehicle Emissions in Baltimore City")

## Close the file device
dev.off()