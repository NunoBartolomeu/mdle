install.packages("dplyr")
library(dplyr)


# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

consumo_horario <- read.csv("../mdle_data/consumos_horario_codigo_postal.csv", sep = ";")

data <- consumo_horario %>% select(-Date.Time)

df <- data %>%
  mutate(datetime = paste(Date, "T", Hour, ":00", sep = ""))

df <- df %>%
  select(datetime, Date, Hour, Zip.Code, Active.Energy..kWh.)

write.csv(df, file="../mdle_data/out/filtered_consumption_Lisbon.csv", row.names = FALSE)
