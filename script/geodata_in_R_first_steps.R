################################################################################

####     Geodata in R - YoMos Workshop 2026 by Yasemin Kukla & Jan Steen    ####

################################################################################

### load the necessary packages

#install.packages("sf")
library(sf)

#install.packages("terra")
library(terra)

# install.packages("dplyr")
library(dplyr)

#install.packages("mapview")
library(mapview)


################################################################################

#### first steps with vector data 


### read our example dataset from OpenStreetMap using st_read()

landuse <- st_read("../data/vectordata/landuse.gpkg")
roads <- st_read("../data/vectordata/roads.gpkg")
buildings <- st_read("../data/vectordata/buildings.gpkg")
pois <- st_read("../data/vectordata/pois.gpkg")


### simple features are like a dataframe with an added geometry
str(landuse)


################################################################################

### thematic queries

# We can perform simple thematic queries using the same operations we know from dplyr

bench <- pois |> filter(fclass=="bench") 

# and the plot it using the mapview package 
mapview(bench)
# or a simple plot() function
plot(bench)



################################################################################

### topological queries

# We want to know which benches are located inside the heathland for a nicer view
# than sitting in the Lister residential area

heath <- landuse |> filter(fclass=="heath")

# using st_intersects we can perform topological queries and then filter our bench data

BenchInHeath <- st_intersects(bench,heath)
BenchInHeath <- bench[lengths(BenchInHeath) > 0,]

mapview(heath, col.regions = "hotpink2")+mapview(BenchInHeath ,col.regions = "sienna")


# Aftwerwards we want to know where we can do a short break near the AWI Gästehaus

# first we look for the Gästehaus in our buildings simple feature

AWI <- buildings |> filter(name=="AWI Gästehaus")
mapview(AWI)

# And using st_is_within_distance we can search for benches in a 500 meter radius

rest <- st_is_within_distance(bench, AWI, dist = 500)

mapview(bench[lengths(rest) > 0,]) + mapview(AWI, col.regions="red")



################################################################################

### vector calculations

# we can perform simple calculations using our sf objects

# for example using st_area() we receive the area for every heath object 
# and then simply calculate the mean area of all objects

heath$area <- st_area(heath)
mean(heath$area)

# the same works for line objects using st_length()

roads$length <- st_length(roads)
mean(roads$length)

mapview(roads)




################################################################################

#### first steps with raster data















################################################################################

#### projections


# R does not project objects on the fly, when loading them into our environment,
# so we always have to check if everything is in the same coordinate system

bench

# our bench data, for example is in the WGS 84 coordinate system


# now only using placeholders, we can reproject our data in sf using st_transform()
# or in terra using project()



# Reproject Vector data

st_transform(x, st_crs(y))      ## use another sf object
st_transform(x, "EPSG:4326")    ## use an epsg code


# Reproject Raster data

project(x, y)                   ## use another object
project(x, 4326)                ## use an epsg code

# adding missing information

crs(x) <- "EPSG:4326"          






















