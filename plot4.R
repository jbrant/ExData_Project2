#######################################################
## Plot 4
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

## Merge the NEI and SCC datasets, including only the following columns:
## 1. SCC
## 2. Emissions
## 3. year
## 4. EI.Sector
data.merged <- merge(
  x = NEI.data[, c("SCC", "Emissions", "year"), with = FALSE], 
  y = SCC.data[, c("SCC", "EI.Sector"), with = FALSE], 
  by = "SCC")

## Extract emissions data for coal combustion sources
data.coal <- data.merged[EI.Sector %in% data.merged[grep("Coal", data.merged$EI.Sector)]$EI.Sector]

## Calculate total emissions from coal combustion sources per year
total.coal.emissions <- aggregate(data.coal$Emissions, list(data.coal$year), sum)

## Reset the names
names(total.coal.emissions) <- c("Year", "Emissions")

## Open the PNG file device
png(filename = "plot4.png", width = 640, height = 640, units = "px")

## Plot the total emissions from coal for each of the four years
ggplot(total.coal.emissions, aes(x = Year, y = Emissions/1e5)) +
  geom_point(color = "indianred", size = 3) +
  geom_line(color = "steelblue") +
  theme(panel.margin = unit(0.8, "lines"),
      plot.title = element_text(face = "bold", size = 16, vjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
  scale_y_continuous(limits = c(0, 6)) +
  labs(x = "Year", 
       y = "Total Emissions (in 100,000 tons)", 
       title = "Total Emissions from Coal Combustion (for U.S.)")

## Close the file device
dev.off()