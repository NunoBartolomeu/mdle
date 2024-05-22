install.packages("dplyr")
install.packages("sf")
install.packages("sp")
install.packages("revgeo")
install.packages("httr")
library(dplyr)
library(sf)
library(sp)
library(revgeo)
library(httr)
#Este ficheiro serve para filtrar o BGRI2021_1106.csv obtido a partir do ficheiro BGRI2021_1106.gpkg, 
#removendo atributos que não estão relaionados com o objetivo do trabalho e adicionando os códigos postais
#obtidos graças às coordenadas X e Y

# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

#####################Auxiliar Functions########################################
#Função auxiliar para obter os códigos postais tendo em conta a longitude e a latitude
get_postal_code_nominatim <- function(lon, lat) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lon=", lon, "&lat=", lat, "&addressdetails=1")
  response <- GET(url)
  content <- content(response, as = "parsed")
  if (!is.null(content$address$postcode)) {
    return(content$address$postcode)
  } else {
    return(NA)
  }
}
##################################################################################
# Read the CSV file using a relative path
bgri2021_data <- read.csv("../mdle_data/INE/BGRI2021_1106-LX/BGRI2021_1106.csv", sep = ",")

bgri2021_sf <- st_as_sf(bgri2021_data, coords = c("X", "Y"), crs = 3763)
bgri2021_sf_wgs84 <- st_transform(bgri2021_sf, crs = 4326)
# Extrair coordenadas
coordinates <- st_coordinates(bgri2021_sf_wgs84)

bgri2021_data$longitude <- coordinates[,1]
bgri2021_data$latitude <- coordinates[,2]
head(bgri2021_data)

#Obtem o Código postal tendo em conta as coordenadas
#Pode demorar algum tempo 
bgri2021_data$postal_code <- mapply(get_postal_code_nominatim, bgri2021_data$longitude, bgri2021_data$latitude)

#Armazena os dados bgri2021 com a nova coluna com os códigos postais num CSV
write.csv(bgri2021_data, file="../mdle_data/INE/BGRI2021_1106-LX/BGRI2021_with_postalCodes.csv")

bgri2021_data <- read.csv("../mdle_data/INE/BGRI2021_1106-LX/BGRI2021_with_postalCodes.csv", sep = ",")
#Retira as colunas desnecessárias
census_data <- subset(bgri2021_data, select = -c(longitude, latitude, SHAPE_Length, SHAPE_Area, NUTS3, NUTS2, NUTS1, 
                                                 SUBSECCAO, SECSSNUM21, SSNUM21, SECNUM21, DTMNFRSEC21, DTMNFR21, 
                                                 DTMN21, DT21, BGRI2021, OBJECTID, Y, X))
#Armazena os dados censeus após a remoção de colunas desnecessárias num CSV
write.csv(census_data, file="../mdle_data/INE/Census/Census_by_postalCode.csv")

#Filtra o código postal para permanecer os 4 primeiros dígitos e soma os valores nos casos em que os códigos postais sejam iguais
census_data$postal_code <- gsub("-", "", census_data$postal_code)
census_data$postal_code <- substr(census_data$postal_code, 1, 4)
aggregated_data <- census_data %>%
  group_by(postal_code) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

aggregated_data <- aggregated_data %>% slice(-n())

postal_code <- aggregated_data %>% select(postal_code)


#Seleção de atributos por variância
variance <- apply(aggregated_data, 2, function(x) var(x, na.rm = TRUE))

# Combine relevance measures for each dataset
relevance <- data.frame(features = colnames(aggregated_data), variance = variance)

print("Census Dataset:")
print(relevance)

# Remove columns with 0 variance
valid_indices <- which(relevance$variance > 0)

filtered_data <- aggregated_data[, valid_indices]

print("Census Dataset (filtered):")
print(filtered_data)

census <- filtered_data %>% rename(Zip.Code = 'postal_code')
census <- census %>% select(-c("X.1"))
#Armazena os dados finais dos census com a filtragem dos códigos postais 
write.csv(census, file="../mdle_data/out/filtered_census.csv", row.names = FALSE)






