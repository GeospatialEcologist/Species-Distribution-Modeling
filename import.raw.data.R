
# load libraries
library(spocc)
library(rgdal)
library(foreign)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(mapview)
library(raster)
library(rgeos)


### Projections
#some coordinate reference systems
# http://spatialreference.org/ref/epsg/4269/
prj.latlong <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"  # epsg:4269

# http://spatialreference.org/ref/epsg/nad83-utm-zone-12n/
prj.utmN83z12 <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"  # epsg:26912

# http://spatialreference.org/ref/epsg/wgs-84-utm-zone-12n/
#  state of UT
prj.utmWGSz12 <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"  # epsg:32612

# https://epsg.io/42303
prj.aeaN83 <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"  # epsg:42303

# http://spatialreference.org/ref/epsg/4326/
prj.wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # epsg:4326





# import species point data - SPOCC

spp.occ <- occ(query = "Leucosticte atrata", 
               from = c("gbif", "inat", "ecoengine", "vertnet", 
                        "bison", "ala", "idigbio", "obis"), 
               limit = 50000, has_coords = T)
spp.occRAW <- data.frame(occ2df(spp.occ))  # coerce to dataframe

# examine data
dim(spp.occRAW)
names(spp.occRAW)
str(spp.occRAW)

# import species point data - Heritage

spp.hertRAW <- read.dbf("F:/UDWR/HeritageData/UNHP_SDHM_20200317/Black_Rosy_finch.dbf", 
                        as.is = FALSE)

# examine data
dim(spp.hertRAW)
names(spp.hertRAW)


########################################################################
# Cleaning up Data
# goal is dataset with these columns: Name, Date, X, Y, Source, PA, tru.PA
# clean SPOCC data


# create R dates
spp.occRAW$date.r <- as.Date(as.character(spp.occRAW$date)) #new column with dates as Rdates
spp.occRAW$PA <- 1 # adding presence
spp.occRAW$tru.PA <- "F" # False means presence only data
spp.occRAW$spp.code <- "blrf" # this is a name holder for black rosy finch
spp.occRAW$dat.source <- "SPOCC"
spp.occRAW$recordID <- paste(spp.occRAW$prov, spp.occRAW$key, sep = "")
# examine
head(spp.occRAW)

# rearrange the dataframe
# arrangment: 
names(spp.occRAW)
spp.occRAW <- spp.occRAW[c(11, 12, 7, 2:3, 10, 8:9)]
head(spp.occRAW)

# remove missing values
length(which(is.na(spp.occRAW$date.r))) # check each variable for missing
dim(spp.occRAW)
spp.occRAW <- na.omit(spp.occRAW)
dim(spp.occRAW)
which(is.na(spp.occRAW))








#######################################################################
### Not dealing with heritage right now!
#######################################################################
# clean up Heritage data

names(spp.hertRAW)
head(spp.hertRAW, 2)
# cleaning up heritage data
spp.hertRAW$date.r <- as.Date(as.character(spp.hertRAW$VISITDATE))
spp.hertRAW$PA <- 1
spp.hertRAW$tru.PA <- "F"
spp.hertRAW$spp.code <- "blrf"
spp.hertRAW$dat.source <- "Heritage"
names(spp.hertRAW)[1] <- "recordID"
spp.hertRAW1 <- spp.hertRAW[c(32, 1, 28, 4:5, 31, 29:30)]
names(spp.hertRAW1)




########################################################################


# assign geometry to dataframe - spp.occRAW

spp.paSF <- st_as_sf(spp.occRAW, coords = c("longitude", "latitude"),
                     crs = prj.latlong, remove = F)
spp.paSF

spp.paSF1 <- st_transform(spp.paSF, crs = prj.aeaN83)
spp.paSF1

spp.paSF2 <- cbind(spp.paSF1, st_coordinates(spp.paSF1))
spp.paSF2
names(spp.paSF2)

spp.paSF3 <- spp.paSF2[c(1:3, 9:10, 6:8, 11)]
head(spp.paSF3)
names(spp.paSF3)[4:5] <- c("x_aea", "y_aea")


# assign geometry to dataframe - Heritage

spp.hertSF <- st_as_sf(spp.hertRAW1, coords = c("UTMX", "UTMY"),
               crs = prj.utmN83z12, remove = F)
spp.hertSF

spp.hertSF1 <- st_transform(spp.hertSF, crs = prj.aeaN83)
spp.hertSF1

spp.hertSF2 <- cbind(spp.hertSF1, st_coordinates(spp.hertSF1))
spp.hertSF2

spp.hertSF3 <- spp.hertSF2[c(1:3, 9:10, 6:8, 11)]
head(spp.hertSF3)
names(spp.hertSF3)[4:5] <- c("x_aea", "y_aea")



# bind the spatial datasets
spp.presSF <- rbind(spp.paSF3, spp.hertSF3)
spp.presSF


########################################################################

# outfile data

spp.presDF <- st_drop_geometry(spp.presSF)
save(spp.presSF, spp.presDF, file = "F:/sdhm-2020/data/Pres/spp.pres.RData")

st_write(spp.presSF, dsn="F:/sdhm-2020/data/Pres", layer = "spp.presSF", driver = "ESRI Shapefile",
         delete_layer = T, delete_dsn = T) # output shapefile
