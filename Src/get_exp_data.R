# get example data

# set envi
wd <- "C:/Envimaster/DSA_plygrnd/"
dat <- file.path(wd,"Data/")
exp <- file.path(wd,"walktrough/exp_data/")
# load package
require(openxlsx)
require(mapview)
require(sp)
require(rgdal)
require(raster)

# load data
maurer <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"org/WESTLICHE DATEN GESAMT.xlsx"))

# subset data from maurer
hunde <- maurer[,c(2,6,7,32)]
colnames(hunde) <- c("lfd", "long", "lat","hunde")
str(hunde)

# convert long lat to numeric for further conversion to spatial object
hunde$long <- as.numeric(hunde$long)
hunde$lat <- as.numeric(hunde$lat)
head(hunde)

# sort tabel (count unique values)
sort(table(hunde$hunde), decreasing = T)

### classifie hunde by pattern

# ifelse method, replaces pattern by replacement, no match gets 0
hunde$type <- ifelse(grepl("nd|nt", hunde$hunde), "nd", 
                     ifelse(grepl("ng|n.g", hunde$hunde), "ng", 
                            ifelse(grepl("nn|n$", hunde$hunde), "nn", 0)))
head(hunde)

# subset only rows with class (exclude 0)
hunde <- dplyr::filter(hunde, grepl("nd|ng|nn", type))


# convert to spatial object
wgs <- SpatialPointsDataFrame(hunde[,2:3],hunde)

# set crs wgs84
proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"
mapview(wgs,zcol="type")

# project to utm32
utm <- spTransform(wgs,"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# test alternative way
utm2 <- spTransform(wgs,CRS("+init=epsg:25832"))
crs(utm)
crs(utm2) # equal in R but Qgis works better with this one
mapview(utm,zcol="type")
mapview(utm2,zcol="type")

# get geometry
geo <- geom(utm)
# write UTM geometry
utm$utm_e <- geo[,2]
utm$utm_n <- geo[,3]

geo2 <- geom(utm2)
# write UTM geometry
utm2$utm_e <- geo2[,2]
utm2$utm_n <- geo2[,3]
head(utm)
head(utm2)

utm
utm2

# check if coords are equal
identical(utm$utm_n,utm2$utm_n)
# check generall equal?
identical(utm,utm2)# no
all.equal(utm,utm2)# a comment in string

proj4string(utm)
proj4string(utm2)

# write
writeOGR(wgs,file.path(exp,"hunde_wgs.shp"),driver = "ESRI Shapefile",layer="wgs") # shp with wgs84
writeOGR(utm,file.path(exp,"hunde_utm.shp"),driver = "ESRI Shapefile",layer="utm") # shp with umt32
writeOGR(utm2,file.path(exp,"hunde_utm2.shp"),driver = "ESRI Shapefile",layer="utm") # shp with umt32
write.csv(hunde,file.path(exp,"hunde.csv")) # csv with estimated wgs84


