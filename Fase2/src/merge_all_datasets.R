install.packages("dplyr")
library(dplyr)

# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))


#Read all the datasets "filtered_weather_Lisbon.csv", "filtered_consumption_Lisbon.csv", "filtered_census.csv"
weather <- read.csv("../mdle_data/out/filtered_weather_Lisbon.csv", sep= ",")
consumption <- read.csv("../mdle_data/out/filtered_consumption_Lisbon.csv", sep= ",")
census <- read.csv("../mdle_data/out/filtered_census.csv", sep= ",")

#Merge the dataset "census" and "consumption" by Zip Code
df <- merge(consumption, census, by = "Zip.Code")

#Merge the dataset "weather" to "df"
df <- merge(df, weather, by = "datetime")

write.csv(df, "../mdle_data/out/merged_dataset.csv", row.names = FALSE)
