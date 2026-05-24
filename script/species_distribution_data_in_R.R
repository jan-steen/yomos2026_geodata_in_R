### R Script Working with species distribution data ###

# install.packages(c("terra", "sf", "exactextractr"))
library(terra)
library(exactextractr)
library(sf)

## loading the data we need

gecko_traits <- read.csv("../data/gecko_data/Madagaskar_Gecko_Traits.csv")
head(gecko_traits)

shp_gecko <- read_sf("../data/gecko_data/Madagaskar_geckos_GARD1.7.shp")
# vect("../data/gecko_data/Madagaskar_geckos_GARD1.7.shp") # is also an option
head(shp_gecko)

bio1 <- rast("../data/gecko_data/bio1_Madagaskar.tif")
plot(bio1)

## Task 1: a simple species richness map

shp.a <- shp_gecko[1,] #subset for the first species of the list
richness <- rasterize(shp.a, bio1) # use bio1 raster as a mask layer to rasterize
richness[is.na(richness)] <- 0 # set NAs to 0 for calculations

# for loop to add all species together in one richness raster
for (i in 2:nrow(shp_gecko)){
  shp.i<- shp_gecko[i,]
  rast.i<- rasterize(shp.i, bio1)
  rast.i[is.na(rast.i)] <- 0
  richness <- (rast.i + richness)
}
richness[richness == 0] <- NA # return 0 to NAs
writeRaster(richness, "../data/gecko_data/gecko_sp_richness_Madagascar.tif", overwrite=T) # save for later
plot(richness)

## Task 2: mapping a discrete trait

# activity Nocturnal
activity.noc <- subset(gecko_traits, gecko_traits$Activity == "Nocturnal")
shp.a <- subset(shp_gecko, shp_gecko$binomial %in% activity.noc[1,]) #subset for the first species
richness_noc <- rasterize(shp.a, bio1) 
richness_noc[is.na(richness_noc)] <- 0 

for (i in 2:length(activity.noc$Species)){
  shp.i<- subset(shp_gecko, shp_gecko$binomial == activity.noc$Species[i])
  rast.i<- rasterize(shp.i, bio1)
  rast.i[is.na(rast.i)] <- 0
  richness_noc<- (rast.i + richness_noc)
}

richness_noc[richness_noc == 0] <- NA
writeRaster(richness_noc, "../data/gecko_data/richness_nocturnal_madagaskar.tif", overwrite=T)
plot(richness_noc)

# activity Diurnal
activity.diu <- subset(gecko_traits, gecko_traits$Activity == "Diurnal")
shp.a <- subset(shp_gecko, shp_gecko$binomial  %in% activity.noc[1,]) #subset for the first species
richness_diu <- rasterize(shp.a, bio1) 
richness_diu[is.na(richness_diu)] <- 0 

for (i in 2:length(activity.diu$Species)){
  shp.i<- subset(shp_gecko, shp_gecko$binomial == activity.diu$Species[i])
  rast.i<- rasterize(shp.i, bio1)
  rast.i[is.na(rast.i)] <- 0
  richness_diu<- (rast.i + richness_diu)
}

richness_diu[richness_diu == 0] <- NA
writeRaster(richness_diu, "../data/gecko_data/richness_diurnal_madagaskar.tif", overwrite=T)
plot(richness_diu)

## Task 3: mapping a continuous trait

shp.a <- shp_gecko[1,]
rast.i<- rasterize(shp.a, bio1)
rast.i[is.na(rast.i)] <- 0
SVL.i <- max(gecko_traits$max_SVL[1])
richness_SVL <- classify(rast.i, cbind(1, SVL.i)) # 1s in the raster get replaced with the max SVL value

for (i in 2:nrow(shp_gecko)){
  shp.i <- shp_gecko[i,]
  rast.i<- rasterize(shp.i, bio1)
  rast.i[is.na(rast.i)] <- 0
  SVL.i <- max(gecko_traits$max_SVL[i])
  rast.i <- classify(rast.i, cbind(1, SVL.i))
  richness_SVL<- (rast.i + richness_SVL)
}

richness_SVL[richness_SVL == 0] <- NA
overall_richness <- rast("../data/gecko_data/gecko_sp_richness_Madagascar.tif")
average_SVL <- richness_SVL/overall_richness
plot(average_SVL)

## Task 4: extracting climate data

res_complete <- c()
for (i in 1:length(shp_gecko)) {
  shp.i <- shp_gecko[i,]
  clim.i <- exact_extract(bio1, shp.i, c("stdev", "median")) #extracting the standard deviation and the median of all four CHELSA variables
  res.i <- cbind(shp.i$binomial, clim.i)
  colnames(res.i) <- c("Species", "stdev_bio1", "median_bio")
  res_complete <- rbind(res_complete, res.i)
} 
head(res_complete)
write.csv(res_complete, "../data/gecko_data/CHELSA_madagaskar_gecko.csv")





