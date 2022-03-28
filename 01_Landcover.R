#01_Landcover
#Microcuenca >< Caudal
#Calculating Landcover per Microcuenca 
#Author: Annika Ertel, (contact: aertel@gmx.net)
#March 2022- 

#Skript to calculate NDVI & co. earth engine http://amazeone.com.br/barebra/pandora/rgeebookT1eng.pdf
#rootfolder for EarthEngine: users/annikaert/microcuenca , EE username: annikaert

#SETTING UP####
library(rgee) #EarthEngine for R 
       # When working in Rprojects you have to check if the proj is using the right python environment
       # Tools -> Global options -> python -> select python interpreter -> conda-> choose rgee env.
#ee_install() #just the first time on new computer

library(stars)
library(tmap)

library(raster) # for working with rasters 
library(rgdal)  # for working with rasters 

ee_Initialize() #Initialize once at the beginning of EACH session!!!

#Search Earth_Engine for data####
# https://developers.google.com/earth-engine/datasets/catalog
      #take the name from the dataset you want to load from the Earth Engine Snippet
      # the code below (ee.ImageCollection("blabla") is not R code. R code is: ee$ImageCollection('blabla')

#Getting the correct data (Sentinel2 data)
col<-ee$ImageCollection('COPERNICUS/S2_SR') #select satellite 
point <- ee$Geometry$Point(-70.77428, 19.07462) #input point coordinates
start <- ee$Date("2022-01-01")              #input date
end <- ee$Date("2022-03-01")                #input date
filter<-col$filterBounds(point)$filterDate(start,end) #make filter that takes color band with spec time and place
img <- filter$first()                       #use filter

#-Visible RGB ---------------------------------------------------
#Visualize
# Map RGB Visible
vPar <- list(bands = c("B4", "B3", "B2"),min = 100,max = 8000,
             gamma = c(1.9,1.7,1.7))
# Ploting map: center map,mapscale,
Map$setCenter(-70.77428, 19.07462, zoom = 10) # Location near Jarabacoa
map1<- Map$addLayer(img, vPar, "True Color Image")
map1

#-Geology and Soils ---------------------------------------------------
#MAP SWIR RGB: Geology and soils
vPar2 <- list(bands = c("B12", "B11", "B8"),min = 500,max = 5000,
              gamma = 1.6)
Map$addLayer(img, vPar2, "Geology/Soil") # Ploting map

#-NDVI---------------------------------------------------
getNDVI <- function(image) {
  return(image$normalizedDifference(c("B8", "B4")))
}
ndvi1 <- getNDVI(img)
ndviPar <- list(palette = c(
  "#cccccc", "#f46d43", "#fdae61", "#fee08b",
  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
),min=0,max=1)

#-NDWI ---------------------------------------------------
getNDWI <- function(image) {
  return(image$normalizedDifference(c("B3", "B5")))
}
ndwi <- getNDWI(img)
ndwiPar <- list(palette = c("#ffffff", "#0000ff","#0000ff"),min=-
                  0.25,max=0.75)

#-NDMI ---------------------------------------------------
getNDMI <- function(image) {
  return(image$normalizedDifference(c("B8", "B11")))
}
ndmi <- getNDMI(img)
ndmiPar <- list(palette = c(
  "#d73027", "#f46d43", "#fdae61", "#fee08b",
  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
),min=-1,max=1)
#-Interactive map---------------------------------------------------
#Creating the map with the three indexes:
# Centering the map and adding the images with the
# parameter created above

Map$setCenter(-70.77428, 19.07462, zoom = 10)
Map$addLayer(ndvi1, ndviPar, "NDVI")+Map$addLayer(ndwi, ndwiPar,
                                                  "NDWI")+ Map$addLayer(ndmi, ndmiPar, "NDMI")



# Export Raster of study area ----------

#load study area
los_dajoas_shape <- read_sf(dsn = "C:/Users/Annika/Documents/weltwärts/Plan Yaque/Proyecto_Microcuenca/02 Data/Spatial_Data/Map_Watershed_Yaque_Norte", layer= "Los_Dajoas") %>%
  sf_as_ee() #load study area an directly convert it to ee object

# Define an area of interest.
geometry <- ee$Geometry$Rectangle(
  coords = c(-70.75, 19.14, -70.66, 19.14),
  proj = "EPSG:4326",
  geodesic = FALSE
)


mask <- system.file("C:/Users/Annika/Documents/weltwärts/Plan Yaque/Proyecto_Microcuenca/02 Data/Spatial_Data/Map_Watershed_Yaque_Norte/Los_Dajoas.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()

region <- mask$geometry()$bounds()


image_rgb<- ee$Image(img)$
  select(c("B4", "B3", "B2"))$
  divide(10000)

raster_los_dajoas<-ee_as_raster(image = img,
                                region = geometry,
                                via = "drive")

plot(raster_los_dajoas$B4)

#ee_as_raster(img,region = ee$geometry$los_dajoas_shape, dsn = "los_dajoas_raster", via = "drive")

