# get name of admin level for places

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
vec <- file.path(wd,"Data/vector_admin")

# load package
require(openxlsx)
require(stringr)
require(raster)
require(dplyr)
require(mapview)

shp <- readOGR(file.path(vec,"places_utm.shp"))
poly<- readOGR(file.path(vec,"admin_western_data.shp"))

# check proj
crs(shp)
crs(poly) 

mapview(shp)+poly

# crop points to polygons
ls <- lapply(1:length(poly), function(x){
  p<-crop(shp,poly[x,])
  # add name to points
  p$admin <- poly[x,]$name
  return(p)
})
ls

# bind list obj to one
admin <- do.call("bind",ls)
head(admin)

# get df
df <-as.data.frame(admin)
head(df)
# sort and reorder
df <-df[,c(1,4,2,3)]

mapview(admin)+poly
# write
write.csv(df,file.path(vec,"result_admin.csv"))
writeOGR(admin,file.path(vec,"result_admin.shp"),driver = "ESRI Shapefile",layer="result_admin") # shp with umt32
