# Ewan McHenry
# 2021-07-20

# Load libraries
library(sf)
library(tidyverse)
library(httr) # to parse data
library(jsonlite) # to convert it to dataframe from JSON

# provide api url
url_NFI_GB_2022 <- "https://services2.arcgis.com/mHXjwgl3OARRqqD4/arcgis/rest/services/National_Forest_Inventory_Woodland_GB_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# use the above api url to load the spatial data as an sf object
NFI_GB_2022 <- st_read(url_NFI_GB_2022)



# Send a GET request to the API
response <- GET(url_NFI_GB_2022)

# Parse the JSON response
data <- fromJSON(content(response, "text"))

# Check if request was successful
if (http_status(response)$status_type == "Success") {
  # Parse JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to spatial data (if applicable)
  spatial_data <- st_as_sf(data)
  
  # Now you can work with the spatial data
  print(head(spatial_data))
} else {
  # Handle error
  print("Error: Unable to fetch data from API")
}




# Define API endpoint and parameters
endpoint <- "https://api.nfi.com/spatialdata"
parameters <- list(
  dataset = "your_dataset",
  bbox = "your_bounding_box_coordinates"
)

# Make API request
response <- GET(endpoint, query = parameters)

# Check if request was successful
if (http_status(response)$status_type == "Success") {
  # Parse JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to spatial data (if applicable)
  spatial_data <- st_as_sf(data)
  
  # Now you can work with the spatial data
  print(head(spatial_data))
} else {
  # Handle error
  print("Error: Unable to fetch data from API")
}
