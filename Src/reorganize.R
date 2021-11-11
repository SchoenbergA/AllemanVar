### Clean up Dataset

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
dat <- file.path(wd,"Data/org")

# load package
require(openxlsx)
require(stringr)
require(raster)

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

# check for empty columns and missing values

### check for empty columns
ecol <- sapply(org, function (k) all(is.na(k)))

any(ecol)==T
if(any(ecol)==T){which(ecol==T)}
if(any(ecol)==T){length(which(ecol==T))} # 76 empty columns , most named X some with strings
# remove empty columns
if(any(ecol)==T){org <- org[!ecol]}


# check occurrence of entries

testCol <- function(df,n,mode="total"){
  if(mode=="threshold"){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]=="" ))
      nkempty <-length(which(df[,i]=="k.A." ))
      naempty <-length(which(is.na(org[,i])))
      if(nempty/nrow(df)>n){
        per=round(nempty/nrow(df),digits=5)
        print(paste0("column ' ",i," ' has ",per*100,"% of entries = ''     colname: ",colnames(df)[i]))
      }
      if(naempty/nrow(df)>n){
        per=round(naempty/nrow(df),digits=5)
        print(paste0("column ' ",i," ' has ",per*100,"% of entries = 'NA'   colname: ",colnames(df)[i]))
        
      }
      if(nkempty/nrow(df)>n){
        per=round(nkempty/nrow(df),digits=5)
        print(paste0("column ' ",i," ' has ",per*100,"% of entries = 'k.A.' colname: ",colnames(df)[i]))
        
      }
    }
  } # end mode
  if(mode=="total"){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]==""))
      naempty <-length(which(is.na(org[,i])))
      nkempty <-length(which(df[,i]=="k.A."))
      per=round(nempty/nrow(df),digits=5)
      print(paste0("column '",i,"' has ",per*100,"% of entries = ''     colname: ",colnames(df)[i]))
      per=round(naempty/nrow(df),digits=5)
      print(paste0("column '",i,"' has ",per*100,"% of entries = 'NA'   colname: ",colnames(df)[i]))
      per=round(nkempty/nrow(df),digits=5)
      print(paste0("column '",i,"' has ",per*100,"% of entries = 'k.A.' colname: ",colnames(df)[i]))
      
    }
  } # end mode
}

# check columns
testCol(org,0.01,"threshold")
testCol(org,0.4,"threshold")

# check spatial and organization columns
testCol(org[1:7],0.00001,"threshold")

# check for tailing white space ################################################
# check for tailling whitespace
any(grepl(" $", org$sheet_nr)==T)
which(grepl(" $", org$sheet_nr)==T)
org$sheet_nr[c(240,247,344,600)]

test <- org
any(grepl(" $",org,fixed = T))
which(grepl(" $",org[,2],fixed = F))
org[240,2]
which(grepl(" $",org,fixed = T))
any(grepl(" $", paste(test[,1],test[,
                                    3]))==T)
which(grepl(" $", paste(test[,2],test[,5]))==T)
test[,2] <-str_trim(test[,2], "right") 

cleanTail <- function(df,trim){
  for (i in 1:ncol(df)) {
    if(any(grepl(" $", df[,i])==T)){
      cat(paste0("detected ",length(which(grepl(" $", df[,i])==T))," tailling whitespaces in column ",colnames(df)[i]),sep = "\n")
    } else { cat("no tailling white spaces detected",sep = "\n")}
    if(trim==T){
      df[,i] <- str_trim(df[,i],"right")
      print("trimmed")
    }
  } 
  return(df)
}

cleanTail <- function(df,trim=F){
  if(trim==F){
    cat(paste0("detected ",length(which(grepl(" $", df[,i])==T))," tailling whitespaces in column ",colnames(df)[i]),sep = "\n")
  }
}

test2 <-org
cleanTail(test2, trim=F)
test2 <-cleanTail(test2,trim=T)

(grep(" $", c(org[,2])))
grep(" $",paste(org[1:ncol(org)]))
# trim
org <-str_trim(org, "right") 
test <- trimws(org)

# transform to UTM #############################################################

# transform to spatialpoints
wgs <- SpatialPointsDataFrame(org[,5:6],org)

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

# create output ################################################################

UTM <- SpatialPointsDataFrame(org[,7:8],org)
WGS <- SpatialPointsDataFrame(org[,5:6],org)
proj4string(UTM) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
plot(UTM)
summary(UTM)
str(UTM)
str(org)
