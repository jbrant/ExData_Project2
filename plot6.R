#######################################################
## Plot 6
#######################################################

### Begin Setup ###

## Load Required libraries
library(ggplot2)
library(grid)
library(data.table)
library(plyr)

## Read in National Emissions Inventory Dataset
if (!exists("NEI.data")) {
  NEI.data <- as.data.table(readRDS("summarySCC_PM25.rds"))
}

## Read in Source Code Classification Dataset
if (!exists("SCC.data")) {
  SCC.data <- as.data.table(readRDS("Source_Classification_Code.rds"))
}

### End Setup ###

## Function to construct linear model equation 
linear.model.equation <- function(dataset){
  model <- lm(log(Emissions+1) ~ year, dataset);
  equation <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(model)[1], digits = 2), 
                        b = format(coef(model)[2], digits = 2), 
                        r2 = format(summary(model)$r.squared, digits = 3)))
  as.character(as.expression(equation));                 
}

## Merge the NEI and SCC datasets, including only the following columns:
## 1. SCC
## 2. Emissions
## 3. year
## 4. EI.Sector
data.merged <- merge(
  x = NEI.data[, c("SCC", "Emissions", "year", "fips"), with = FALSE], 
  y = SCC.data[, c("SCC", "EI.Sector"), with = FALSE], 
  by = "SCC")

## Extract emissions data for motor vehicle sources
## Note: this simply greps on "Vehicles" as I'm assuming "motor vehicles" entails
## automobiles that travel on the road and every Mobile entry that didn't include
## "Vehicle" in the name was non-road equipment, marine vessels, aircaraft, or
## locomotives.
vehicles.data <- data.merged[
  EI.Sector %in% data.merged[
    grep("Vehicles", data.merged$EI.Sector)
    ]$EI.Sector
  ]

## Subset data set to include only Baltimore city and Los Angeles County
baltimore.la.vehicles.data <- vehicles.data[fips == "24510"|fips == "06037"]

## Convert the county codes to location names
baltimore.la.vehicles.data$fips[
  which(baltimore.la.vehicles.data$fips == "24510")
] <- "Baltimore City"
baltimore.la.vehicles.data$fips[
  which(baltimore.la.vehicles.data$fips == "06037")
  ] <- "Los Angeles County"

## Rename the variable "fips" to "Location"
setnames(baltimore.la.vehicles.data, "fips", "Location")

## Apply linear model extraction over both locations
lm.equation <- ddply(
  as.data.frame.matrix(baltimore.la.vehicles.data),
  .(Location),
  linear.model.equation
)

## Open the PNG file device
png(filename = "plot6.png", width = 640, height = 640, units = "px")

## Plot the emissions (using a log transformation as an attempt to somehwat normalize
## the rather wide distribution) for both Baltimore city and Los Angeles County.  The
## linear model plots the normalized change over time.
ggplot(baltimore.la.vehicles.data, aes(x = year, y = log(Emissions+1))) + 
  geom_point(aes(colour = Location), size = 2) +
  facet_grid(.~Location) +
  theme(panel.margin = unit(0.8, "lines"),
        plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  stat_smooth(method = "lm", size = 0.5, col = "sienna") +
  geom_text(data = lm.equation, aes(x = 2003, y = 7, label=V1), colour = "magenta", parse = TRUE, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
  labs(x = "Year", 
       y = "ln(Emissions + 1)", 
       title = "Emissions by Year in Baltimore and L.A.",
       colour = "Location")

## Close the file device
dev.off()