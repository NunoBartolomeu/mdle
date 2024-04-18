
# Set the working directory to the 'src' directory
#setwd("D:/Cadeiras/MDLE/mdle/project1/src")
setwd("C:/Users/pedro/Desktop/mestradoLab/mdle/project1/src")

# Read the CSV file using a relative path
data <- read.csv("../mdle_data/INE/FS-2021-Secção-Tot.csv", sep = ",")

head(data)

lisbon_census <- subset(data, data$NUTS2.DSG == "Área Metropolitana de Lisboa")

head(lisbon_census)

lisbon_census <- as.data.frame(lapply(lisbon_census, as.integer))

variance_lisbon_census <- apply(lisbon_census, 2, function(x) var(x, na.rm = TRUE))

relevance_lisbon_census <- data.frame(features = colnames(lisbon_census), variance = variance_lisbon_census)

valid_indices <- which(relevance_lisbon_census$variance > 0 & relevance_lisbon_census$features != "ORD")


# Remove the specified columns
filtered_relevance_lisbon_census <- lisbon_census[, valid_indices]

head(filtered_relevance_lisbon_census)
################################################################################
# Write the filtered data to a new CSV file
write.csv(filtered_relevance_lisbon_census, file="../mdle_data/INE/Censos_2021_Lisbon_filtered.csv", row.names=FALSE)






