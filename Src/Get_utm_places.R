### Get UTM projected places

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
dat <- file.path(wd,"Data/org")
vec <- file.path(wd,"Data/Vector")

# load package
require(openxlsx)
require(stringr)
require(raster)
require(rgdal)

# load original data
org <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"WESTLICHE DATEN GESAMT.xlsx"))# I Data table preparation - cleaning up.

# check structure ##############################################################

str(org) # 'lfd' and 'GID' numerical, rest chr
str(org[1:10])

# char to numeric
org$LONG <- as.numeric(org$LONG)
org$LAT  <- as.numeric(org$LAT)


# change colnames and reorder ##################################################

colnames(org)[]

# relocate 
sub <- subset(org, select=c(4,6,7))
colnames(sub) <- c("place", "long","lat")

# transform to spatialpoints
wgs <- SpatialPointsDataFrame(sub[,2:3],sub)

# set crs wgs84
proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"

# reproject to utm32
utm <- spTransform(wgs,CRS("+init=epsg:25832"))
# check effect
plot(wgs)
plot(utm) # slightly differnet
crs(utm)
# get geometry

geo <- geom(utm)
# write UTM geometry
sub$utm_e <- geo[,2]
sub$utm_n <- geo[,3]

# order utm 
colnames(sub)
places <- subset(sub, select=c(1,4,5))
head(places)
# create output ################################################################

places_utm <- SpatialPointsDataFrame(places[,2:3],places)
crs(places_utm)
proj4string(places_utm) <- CRS("+init=epsg:25832")
crs(places_utm)
writeOGR(places_utm,file.path(vec,"places_utm.shp"),driver = "ESRI Shapefile",layer="utm") # shp with umt32
