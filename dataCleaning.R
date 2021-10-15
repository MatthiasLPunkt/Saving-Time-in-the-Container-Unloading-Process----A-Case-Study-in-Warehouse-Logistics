# Installing packages
install.packages("tidyverse")

# Loading packages
library(tidyverse)

# Check and set current working directory - YOU NEED TO CHANGE THIS PATH
setwd("C:/Users/Matthias/Desktop/Google_Data_Analytics_Capstone_Project_Container")
getwd()

# Importing and brief inspection of the data
ContainerData <- read_csv2("rawDataContainerUnloading2019v2.csv")
glimpse(ContainerData) # Number of rows and columns is correct. Data types are correct.

# Narrowing the data down to the columns of interest
core_data <- select(ContainerData, DiffTotalPaletteCompletionTimeSeconds, TeamSize, ItemWeight, ItemVolumeCubiccentimeter, ItemQuantityPerPalette, PaletteNumber)
core_data <- rename(core_data,
                    time_seconds = DiffTotalPaletteCompletionTimeSeconds,
                    team_size = TeamSize,
                    item_weight = ItemWeight,
                    item_volume = ItemVolumeCubiccentimeter,
                    palette_quantity = ItemQuantityPerPalette,
                    palette_number = PaletteNumber)
glimpse(core_data)

## Checking if values in columns "make sense"
# Are the unique values reasonable?
unique(core_data$team_size)
unique(core_data$palette_quantity)
unique(core_data$palette_number)
# Is the data range from lowest to highest value reasonable?
summary(core_data)

## Column time_seconds shows some questionable values (lowest value of five seconds to fully stack a palette is humanly impossible). Further investigation is needed.
# Save copy of the original data file
write.csv2(core_data, "core_data_v1.csv")
# Sort data by time_seconds, ascending
sorted <- arrange(core_data, time_seconds)
# Output shows that there are multiple observations in the time_seconds column that seem nonsensical
print(sorted$time_seconds)

# To gage which nonsensical observations to drop, we introduce a new column that shows the time it takes to stack only one item onto a palette
core_data_v2 <- core_data %>%
  mutate(item_time = time_seconds / palette_quantity)

# Show properties of new column
summary(core_data_v2)
sorted_2 <- arrange(core_data_v2, item_time)
print(sorted_2$item_time)

## We make the ad-hoc decision to drop all observations with item_time < 3 seconds
# Save copy of the original data file
write.csv2(core_data_v2, "core_data_v2.csv")
core_data_v3 <- filter(core_data_v2, item_time >= 3)
core_data_v3 <- rename(core_data_v3,
                       item_weight_kg = item_weight,
                       item_volume_cc = item_volume,
                       item_time_sec = item_time,
                       time_sec = time_seconds)

# Now we have 199 observations left
summary(core_data_v3)
glimpse(core_data_v3)
write.csv2(core_data_v3, "core_data_v3.csv")