library('tidyverse')
library('lubridate')
library('ggforce')
library('ggplot2')

# Set work directory.
setwd("../input/evs-one-electric-vehicle-dataset")

# Read data
electric_cardata <- read.csv('ElectricCarData_Clean.csv')
```
```{r echo=FALSE}
# Convert datatype from character to integer and inspect data
glimpse(electric_cardata)

# Which car has the fastest 0-100 acceleration?

```{r}
electric_cardata_accel <-  # Sort vehicles in ascending order by acceleration 
  electric_cardata[,1:3][order(electric_cardata$AccelSec), ]

head(electric_cardata_accel) # View first 6 rows of dataframe

electric_cardata_fastest_accel <- electric_cardata_accel[1, ] # Grab first row

# Output vehicle with the fastest 0-100 acceleration
writeLines(paste0("Fastest Acceleration: ", 
                  electric_cardata_fastest_accel[1], 
                  electric_cardata_fastest_accel[2],                   
                  "at ", 
                  electric_cardata_fastest_accel[3], 
                  " secs"))

# Which Vehicle Has the Highest Efficiency?

```{r}
electric_cardata_efficiency <- # Sort vehicles in ascending order by efficiency 
  electric_cardata[,c(1:2,6)][order(electric_cardata$Efficiency_WhKm), ]

head(electric_cardata_efficiency) # View first 6 rows of dataframe

electric_cardata_highest_efficiency <- # Grab first row
  electric_cardata_efficiency[1, ]

# Output vehicle with the highest efficiency
writeLines(paste0("High Efficiency: ", 
                  electric_cardata_highest_efficiency[1],  
                  electric_cardata_highest_efficiency[2],                    
                  "at ", 
                  electric_cardata_highest_efficiency[3], " Wh/Km"))
# Does A Difference in Power Train Effect the Range, Top Speed, and Efficiency?

```{r}
# Create dataframe with range, top speed, efficiency & power train
electric_cardata_powertrain <- electric_cardata[ , c(4:6, 9)]
```

## Power Train vs Range (km)

```{r}
# Calculate the average range for each power train
avg_ranges <- aggregate(electric_cardata_powertrain$Range_Km ~ 
                          electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_ranges <- 
  avg_ranges %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_Range_Km = `electric_cardata_powertrain$Range_Km`)
```
```{r echo = FALSE}
avg_ranges
```
```{r}
# Plot Power Train vs Range and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=Range_Km, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Range?",
       subtitle = "Power Train vs Range (km)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) +
  # Mark average range for AWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[1], y = 1.5, 
                   xend = avg_ranges$Avg_Range_Km[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average range for FWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[2], y = 2.5, 
                   xend = avg_ranges$Avg_Range_Km[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average range for RWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[3], y = 3.5, 
                   xend = avg_ranges$Avg_Range_Km[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))

## Power Train vs Top Speed (km/h)

```{r}
# Calculate the average top speed for each power train
avg_topspeed <- aggregate(electric_cardata_powertrain$TopSpeed_KmH ~ 
                          electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_topspeed <- 
  avg_topspeed %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_TopSpeed_KmH = `electric_cardata_powertrain$TopSpeed_KmH`)
```
```{r echo=FALSE}
avg_topspeed
```
```{r}
# Plot Power Train vs Top Speed and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=TopSpeed_KmH, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Top Speed?",
       subtitle = "Power Train vs Top Speed (km/h)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) +
  # Mark average top speed for AWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[1], y = 1.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average top speed for FWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[2], y = 2.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average top speed for RWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[3], y = 3.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))

## Power Train vs Efficiency (Wh/km)

```{r}
# Calculate the average efficiency for each power train
avg_efficiency <- aggregate(electric_cardata_powertrain$Efficiency_WhKm ~ 
                            electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_efficiency <- 
  avg_efficiency %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_Efficiency_WhKm = `electric_cardata_powertrain$Efficiency_WhKm`)
```
```{r echo=FALSE}
avg_efficiency
```
```{r}
# Plot Power Train vs Efficiency and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=Efficiency_WhKm, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Efficiency?",
       subtitle = "Power Train vs Efficiency (Wh/km)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) + 
  # Mark average efficiency for AWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[1], y = 1.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average efficiency for FWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[2], y = 2.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average efficiency for RWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[3], y = 3.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))
