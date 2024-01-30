library(raster)
library(rgeos)
library(rgdal)
library(stringr)
library(dplyr)
library(sf)
library(dplyr)
library(terra)

# Micah's functions
source("/Users/neidavillanueva/Desktop/cod_footprint/micahs_functions.R")

# Reading in map and border layers -----
b_shp_path<-"/Users/neidavillanueva/Desktop/Canada_and_US_Border" # project shape file location
border_poly<-readOGR(b_shp_path,"Canada_and_US_Border") #shape file we want to search
border_poly<-spTransform(border_poly, masp)  # Project to

gispath<-"Desktop/cod_footprint" # USA base layer location
crs_use<-masp # Mass state plane meters projection
usa<-readOGR(gispath,"usa") # Reading in USA shape file
usa_ne<-gUnaryUnion(spTransform(subset(usa,STATE%in%c("ME","NH","MA","RI")),crs_use)) # Parsing through states
load("/Users/neidavillanueva/Desktop/cod_footprint/dem.rdat")

#-----file path to the kml file----
kmlfile<- "/Users/neidavillanueva/Desktop/survey_0.kml"
st_layers(kmlfile)
kml_file1<- st_read(kmlfile)

crab_n <- st_zm(kml_file1[1], drop=T, what='ZM')
crab.df<- as.data.frame(crab_n)
polygon.crab<- as(crab_n, "Spatial")
st_write(crab_n, dsn= "/Users/neidavillanueva/Desktop/blue_crab/", driver= "ESRI Shapefile",'bcrab.shp')


crab2<- readOGR("/Users/neidavillanueva/Desktop/survey_0.kml")

poly_crab<- SpatialPoints(crab2@coords)
writeOGR(crab2, dsn ='/Users/neidavillanueva/Desktop/blue_crab/',driver = "ESRI Shapefile" ,layer = 'bluecrab' )

#---read in shapefile----
sp<- readOGR(dsn="/Users/neidavillanueva/Desktop/blue_crab/",'bluecrab')
# pull out coordinates from file
crab_dt <- sp@coords
# make coordinate as a data frame
crab_dt2<- data.frame(crab_dt)
# apply coordinate values 
coordinates(crab_dt2) = ~ coords.x1 + coords.x2
# set to lat long
proj4string(crab_dt2)=CRS("+init=epsg:4326")
# transform to mass state plane
crab_dt2 = spTransform(crab_dt2,(masp))
# sets margins
par(mar = c(4, 4, 1,1))
# set the extent 
xlim <- c(bbox(crab_dt2)[1], bbox(crab_dt2)[2])
#ylim <- c(bbox(crab_dt2)[3], bbox(crab_dt2)[4])

# plot blue crab sightings
plot(crab_dt2, xlim = xlim, pch = 21, bg = "deepskyblue",col = "deepskyblue3",cex = .65,  main = "Blue Crab Sightings")
maptix(ytix=40:50,xtix=-(74:66),crx=masp) 
# add US layer  
plot(usa_ne, col = "white", add = TRUE)

# dots above
plot(crab_dt2,pch = 21, bg = "deepskyblue",col = "deepskyblue3",cex = .65, add = TRUE)
# Plot the US layer with specified limits

plot(border_poly,col = 'blue', add = TRUE)

contour(abs(dem),levels=c(50,100),add=TRUE,col=adjustcolor("gray",0.5)) 






# colnames(crab_points_sp@coords)[1] <- 'long'
# colnames(crab_points_sp@coords)[2] <- 'lat'
# 
# # Create a spatial points object
# crab_points_data <- data.frame(crab_points_sp@coords)
# #crab_points_data <- spTransform(crab_points_data,'+init=epsg:26986')
# #crab_points_data$coords.x1 <- crab_points_data$coords.x1
# #crab_points_data<- crab_points_data[,c(2,1)]
# 
# 
# coordinates(crab_points_data) <- ~ long + lat
# gridded(crab_points_data) = TRUE
# #proj4string(crab_points_data) <- CRS("+proj=longlat +datum=WGS84")
# 
# # Plot the original spatial points to visualize
# spplot(crab_points_data, main = "Original Spatial Points")
# # Create a raster layer with adjusted resolution for aspect ratio
# 
# 
# plot(crab_test)
# 
# 
# 
# raster_layer <- raster(resolution = 3)
# extent(raster_layer) <- extent(crab_points_data)
# 
# # Convert spatial points to raster
# rasterize_points <- rasterize(crab_points_data , raster_layer)
# 
# # Plot the raster
# plot(rasterize_points , main="Points to Raster", col = "orange", asp = 1)
# # add US and border layers
# # plot(border_poly, add = TRUE)
# plot(usa_ne,col="gray21",add = TRUE)
# 
# 
# 
# 
# 
# 






