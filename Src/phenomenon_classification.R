### Phenomenon Classification

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
dat <- file.path(wd,"Data/org")

# load package

# load data
maurer <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"WESTLICHE DATEN GESAMT.xlsx"))

# subset data from maurer
hunde <- maurer[,c(2,6,7,32)]
colnames(hunde) <- c("lfd", "long", "lat","hunde")
str(hunde)

# convert long lat to numeric for further conversion to spatial object
hunde$long <- as.numeric(hunde$long)
hunde$lat <- as.numeric(hunde$lat)
head(hunde)

# sort table (count unique values)
sort(table(hunde$hunde), decreasing = T)

### classify hunde by pattern

# ifelse method, replaces pattern by replacement, no match gets 0
hunde$type <- ifelse(grepl("nd|nt", hunde$hunde), "nd", 
                     ifelse(grepl("ng|n.g", hunde$hunde), "ng", 
                            ifelse(grepl("nn|n$", hunde$hunde), "nn", 0)))
head(hunde)

# subset only rows with class (exclude 0)
hunde_red <- dplyr::filter(hunde, grepl("nd|ng|nn", type))
which(hunde$type==0)
