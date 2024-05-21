install.packages("dplyr")
library(dplyr)


# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

consumo_horario <- read.csv("../mdle_data/consumos_horario_codigo_postal.csv", sep = ";")
census <- read.csv("../mdle_data/INE/Census/Census_final_data.csv", sep = ",")

census <- census %>% rename(Zip.Code = 'postal_code')

# Combinar os data frames usando merge()
merged_df <- merge(census, consumo_horario, by = "Zip.Code")

write.csv(merged_df, file="../mdle_data/consumo_codigo_postal.csv")



