# Set the working directory to the 'src' directory
#setwd("D:/Cadeiras/MDLE/mdle/project1/src")
setwd("C:/Users/pedro/Desktop/mestradoLab/mdle/project1/src")

# Read the CSV file using a relative path
data <- read.csv("../mdle_data/consumos_horario_codigo_postal.csv", sep = ";")

# Remove the 'Date' and 'Hour' columns
data <- subset(data, select = -c(Date, Hour))

# Filter the 'Zip Code' column to contain only numbers between 1000 and 1990
data <- subset(data, Zip.Code >= 1105 & Zip.Code <= 1512)

# Write the filtered data to a new CSV file
write.csv(data, file = "../mdle_data/consumos_horario_codigo_postal_lisboa.csv", row.names = FALSE)
