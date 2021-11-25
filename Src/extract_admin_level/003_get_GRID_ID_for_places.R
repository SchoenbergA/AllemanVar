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
require(rgdal)

shp <- readOGR(file.path(vec,"places_utm.shp"))
poly<- readOGR(file.path(vec,"DWA_planquadrate_utm.shp"))

# check proj
crs(shp)
crs(poly) 

mapview(shp)+poly

# clean duplicates
which(duplicated(poly$name))
poly <- poly[-which(duplicated(poly$name)),]


# crop points to polygons
ls <- lapply(1:length(poly), function(x){
  p<-crop(shp,poly[x,])
  # add name to points
  p$DWA_grid <- poly[x,]$name
  return(p)
})
ls

# bind list obj to one
grid <- do.call("bind",ls)
class(grid)

# get df
df <-as.data.frame(grid)
head(df)

# remove "DWA-Planquadrat"
df$DWA_grid<-str_remove(df$DWA_grid, "DWA-Planquadrat")
grid$DWA_grid<-str_remove(grid$DWA_grid, "DWA-Planquadrat")
poly$name<-str_remove(poly$name, "DWA-Planquadrat")
head(poly)
head(df)
head(grid)

# sort and reorder
df <-df[,c(1,2,5,3,4)]

mapview(grid)+poly
# write csv and shp
write.csv(df,file.path(vec,"DWA_grid_western.csv"))
writeOGR(grid,file.path(vec,"DWA_grid_points_western.shp"),driver = "ESRI Shapefile",layer="DWA_grid_points_western") # shp with umt32
writeOGR(poly,file.path(vec,"DWA_grid_western.shp"),driver = "ESRI Shapefile",layer="DWA_grid_western") # shp with umt32
