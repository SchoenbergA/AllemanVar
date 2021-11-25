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

shp <- readOGR(file.path(vec,"Places_full_utm.shp"))
poly<- readOGR(file.path(vec,"Admin_gadm_merged_utm.shp"))

# check proj
crs(shp)
crs(poly) 

mapview(shp)+poly

which(poly$Full_name=="Bodensee")
poly <-poly[-4,]
poly$name <- poly$Full_name
# crop points to polygons
ls <- lapply(1:length(poly), function(x){
  p<-crop(shp,poly[x,])
  # add name to points
  p$admin <- poly[x,]$name
  return(p)
})
ls

any(ncol(ls)==1)

for (i in 1:length(ls)) {
  print(ncol(ls[[i]]))
  
}
ncol(ls[[4]])

ls[[4]]
# bind list obj to one
admin <- do.call("bind",ls)
head(admin)

# get df
df <-as.data.frame(admin)
head(df)
# sort and reorder
df <-df[,c(1,2,5,3,4)]

mapview(admin)+poly
# write
write.csv(df,file.path(vec,"result_admin_full.csv"))
writeOGR(admin,file.path(vec,"result_admin.shp"),driver = "ESRI Shapefile",layer="result_admin") # shp with umt32
