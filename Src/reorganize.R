### Clean up and rdy up Dataset

# script to handle transformation from xlsx (original data) to csv / shp format.
# will reorganize column order and clean up specific problems

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
dat <- file.path(wd,"Data/org") # hardcoded path to original data
out <- file.path(wd,"Data/org_mod_output")

# load package
require(openxlsx)
require(stringr)
require(raster)
require(mapview)

# load original data
org <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"WESTLICHE DATEN GESAMT.xlsx"))# I Data table preparation - cleaning up.

# check structure ##############################################################

str(org) # 'lfd' and 'GID' numerical, rest chr
str(org[1:10])

# char to numeric
org$LONG <- as.numeric(org$LONG)
org$LAT  <- as.numeric(org$LAT)


################################################################################
# change colnames and reorder ##################################################

colnames(org)[]

# relocate 
org <- subset(org, select=c(2:17,145,1,18:21,22:144,146:ncol(org)))

# ID and location
colnames(org)[1:6]
colnames(org)[1:6] <- c("ID","sheet_nr","place","GID","LONG","LAT")

# social data
colnames(org)[7:13]
colnames(org)[7:13] <- c("single/double","gender","profession","age","born_at_place","standing_at_place","birthplace")
unique(org[13])

# social data 2
colnames(org)[14:17]
colnames(org)[14:17] <-c("birthplace_father","birthplace_mother","farmer_rate","drive_to_for_shopping")
unique(org[17])

# comment columns
colnames(org)[18:22] 
colnames(org)[18:22] <-c("comment_questions","comment_dubletten","comment_socialdata","comment_general","comment_special questions")

colnames(org[1:22])

# 1:7  <-------- are organization and geometry
# 8:22 <-------- social data
# 23: ncol(org) <-------- are the words and sentences



################################################################################
# transform to UTM #############################################################

# transform to spatialpoints
which(colnames(org)=="LONG")
which(colnames(org)=="LAT")
wgs <- SpatialPointsDataFrame(coords = org[,5:6],org)

# set crs wgs84
proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"

# reproject to utm32
utm <- spTransform(wgs,"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# check effect
plot(wgs)
plot(utm) # slightly differnet

# get geometry
geo <- geom(utm)
# write UTM geometry
org$utm_e <- geo[,2]
org$utm_n <- geo[,3]

# order utm 
colnames(org)
org <- subset(org, select=c(1:6,148:149,7:147))
#org <- subset(org, select=c(1:4,148:149,7:147)) # without LONG LAT
colnames(org)
head(org[,1:9])
################################################################################
# create output ################################################################

# check for correct col
which(colnames(org)=="LONG")
which(colnames(org)=="LAT")
which(colnames(org)=="utm_e")
which(colnames(org)=="utm_n")

# get points
p_utm <- SpatialPointsDataFrame(org[,7:8],org)
p_wgs <- SpatialPointsDataFrame(org[,5:6],org)

crs(p_utm)
crs(p_wgs)

# set crs
proj4string(p_utm) <- CRS("+init=epsg:25832")
proj4string(p_wgs) <- CRS("+init=epsg:4326")
#proj4string(UTM) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # full string (but causes problems in Qgis)

mapview(p_utm)
mapview(p_wgs)

# set or check output path
#out <- 
out

# write points
writeOGR(p_utm,file.path(out,"western_data_utm.shp"),driver = "ESRI Shapefile",layer="western_data_utm") # shp with umt32
writeOGR(p_wgs,file.path(out,"western_data_wgs.shp"),driver = "ESRI Shapefile",layer="western_data_wgs") # shp with umt32

# write table ! encoding problem
write.csv(org,file.path(out,"western_data.csv"),row.names = F,fileEncoding = "CP1252" )

#$$$$ Endocing Problem $$$$# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# check output


test <- read.csv(file.path(out,"western_data.csv"),sep=";")
head(test)
identical(test,org)
all.equal(test,org)

identical(test[1:20,129],org[1:16,129])
test[1:20,129]
org[1:20,129]
all.equal(test[4,129],org[4,129])

test[4,129]
org[4,129]

org

# check output shp

test2 <- as.data.frame(p_utm)
test2<-test2[,1:149]
ncol(test2)
identical(test2,org)
all.equal(test2,org)
all.equal(test2[1:20],org[1:20])
names(test2)
names(org)

### $$$ error just in names but still seems to be encoding problem