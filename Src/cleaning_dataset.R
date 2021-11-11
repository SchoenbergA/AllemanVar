### Clean up Dataset

# load data
# set environment paths
wd <- "C:/Envimaster/AllemanVar/" # local path to repository
dat <- file.path(wd,"Data/org")

# load package
require(openxlsx)
require(stringr)

# load data
org <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"WESTLICHE DATEN GESAMT.xlsx"))# I Data table preparation - cleaning up.




##########################################################
# handle double entries separated by ","
#test <- "1 before,after"
#gsub(".*:", "", test)
#str_replace(test, ".*,","") # would delete all before ","
#str_replace(test, ",.*","") # would delet all after ","
###################################################################

#### SETUP ####

### 1. Check dataframe for problems ####

# check structure
str(org) # all columns are characters

### 1.1 check head and tail
head(org[1:6]) # headers are far too complex
tail(org[1:6]) # data frame has empty row in 1277


### 1.2 check for empty columns
ecol <- sapply(org, function (k) all(is.na(k)))

any(ecol)==T
which(ecol==T)
length(which(ecol==T)) # 76 empty columns , most named X some with strings
# remove empty columns
org <- org[!ecol]


# check occurrence of entries

testCol <- function(df,n,mode="total"){
  if(mode=="threshold"){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]=="" ))
      nkempty <-length(which(df[,i]=="k.A." ))
      naempty <-length(which(is.na(org[,i])))
      if(nempty/nrow(df)>n){
        per=round(nempty/nrow(df),digits=5)
        print(paste0("column ' ",i," ' has ",per*100,"% of entries = '' ",colnames(df)[i]))
      }
        if(naempty/nrow(df)>n){
          per=round(naempty/nrow(df),digits=5)
          print(paste0("column ' ",i," ' has ",per*100,"% of entries = 'NA' ",colnames(df)[i]))
        
        }
      if(nkempty/nrow(df)>n){
        per=round(nkempty/nrow(df),digits=5)
        print(paste0("column ' ",i," ' has ",per*100,"% of entries = 'k.A.' ",colnames(df)[i]))
        
      }
    }
  } # end mode
  if(mode=="total"){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]==""))
      naempty <-length(which(is.na(org[,i])))
      per=round(nempty/nrow(df),digits=5)
      print(paste0("column ' ",i," ' has ",per*100,"% of entries = '' ",colnames(df)[i]))
      per=round(naempty/nrow(df),digits=5)
      print(paste0("column ' ",i," ' has ",per*100,"% of entries = 'NA' ",colnames(df)[i]))
    }
  } # end mode
}

# check columns
testCol(org,0.01,"threshold")
testCol(org,0.9,"threshold")

#unique(org[,13])

# what to do with those columns?

### 1.3 rename/sort columns

# check "words" columns
colnames(org)[] # "VI..Nach.welcher.Stadt.fährt.man.zum.Einkaufen." seems to be social data instead of "wording"

# relocate 
org <- subset(org, select=c(2:17,145,1,18:21,22:144,146:ncol(org)))

head(org)
head(org[1:20])

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
################################################################################

### 2. Check ID and location columns ####

### 2.1 ID and Fragebogen Nr

# check if each row has a unique ID
nrow(org)
length(unique(org$ID)) # matches

# check "Fragebogen"
nrow(org)
length(unique(org$sheet_nr)) # not matching

# check occurence of entries
tab <-table(org$sheet_nr)
length(which(tab>1)) # 3 entries are dublicates
which(tab>1)

which(org$sheet_nr=="Hag M 11")
which(org$sheet_nr=="Hag V 1")
which(org$sheet_nr=="Hag W 16")



### 2.3 check long lat entries

# Longitude
any(grepl(" $", org$LONG)==T)
nchar(org$LONG) # entries have different precision: 2 up to 5 digits 
unique(nchar(org$LONG))
which(nchar(org$LONG)==4) # entry 372 has only 2 digits resulting in ca 0,78 km resolution (could be up to 7,2 kilometer away from the point)
which(nchar(org$LONG)==5) # 10 entries have 3 digits resulting in ca 78 m resolution (could be up to 772 meter away from the point)

# Latitude
any(grepl(" $", org$LAT)==T)
(org$LAT)
nchar(org$LAT) # entries have different precision: 3 up to 5 digits 
unique(nchar(org$LAT))
which(nchar(org$LAT)==6) # 10 entries have 3 digits resulting in ca 100 m resolution (could be up to 900 meter away from the point)

# are the lower precision coordinates for LONG and LAT for the same points?
which(nchar(org$LONG)==5)==which(nchar(org$LAT)==6)
# no single point where both coordinates have lesser digits.

# change CRS to UTM
which(colnames(org)=="LONG")
which(colnames(org)=="LAT")

org$LONG <- as.numeric(org$LONG)
org$LAT  <- as.numeric(org$LAT)
wgs <- SpatialPointsDataFrame(org[,5:6],org)

# set crs wgs84
proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"
mapview(wgs)
# project to utm32
utm <- spTransform(wgs,"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# test alternative way
utm2 <- spTransform(wgs,CRS("+init=epsg:25832"))
crs(utm)
crs(utm2) # equal in R but Qgis works better with this one
mapview(utm)
mapview(utm2)

# check effect
plot(wgs)
plot(utm) # slightly differnet
plot(utm2) # equal to utm
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
# possible solutions:
# 1. add tailing zeros
# 2. drop at least the LONG 2 digit entry
# 3. drop all entries with digits <= 4

### 2.4 GID


# check for tailling whitespace
any(grepl(" $", org$sheet_nr)==T)
which(grepl(" $", org$sheet_nr)==T)
org$sheet_nr[c(240,247,344,600)]
# trim
org$sheet_nr <-str_trim(org$sheet_nr, "right") 

# 2.2 "ort"
any(grepl(" $", org$place)==T)
which(grepl(" $", org$place)==T)
# trim
org$place <-str_trim(org$place, "right") 


class(org$GID)
as.numeric(org$GID)
any(grepl(" $", org$GID)==T)

nrow(org)
length(unique(org$GID))

tab <- table(org$GID)
length(which(tab>1)) # entries are dublicates
which(tab>1)

### 3 Check social data ####

# 3.1 rename column
as.numeric(org$far)
class(org$farmer_rate)

unique(org$farmer_rate)

# set k.A. to NA
org$farmer_rate[org$farmer_rate== "k.A." ]<- NA
org$farmer_rate[org$farmer_rate== "k.A. " ]<- NA
### 2.1 handle character entries
unique(org$farmer_rate) # 3 entries with strings
which(org$farmer_rate=="1 bis 2% reine Arbeiter") # row 1206
which(org$farmer_rate=="fast keine Arbeiter" ) # row 1047
which(org$farmer_rate=="nur 1 Bauer") # row 718

# set to NA
org$farmer_rate[org$farmer_rate== "1 bis 2% reine Arbeiter" ]<- NA
org$farmer_rate[org$farmer_rate== "fast keine Arbeiter"  ]<- NA
org$farmer_rate[org$farmer_rate== "nur 1 Bauer" ]<- NA

### 2.2 hndle % symbols
any(grepl(" $", org$farmer_rate)==T)
which(grepl("%$", org$farmer_rate)==T)

# crop % symbol
org$farmer_rate <-str_replace(org$farmer_rate, "%","")

### 2.3 handle decimal symbol
any(grepl(",", org$farmer_rate)==T)

# replace , by .
org$farmer_rate <-str_replace(org$farmer_rate, ",",".")
unique(org$farmer_rate)[90:138]

# !!! open issues

# 1. "." vs "," is 0.7 = 70 % or 0.7% ?
# 2. ranged values (bis, -) -> using mean min max or drop entries?

### 2.4 check native
unique(org$native)

### 2.5 check M/E

any(grepl(" $", org$E_M)==T)
which(grepl(" $", org$E_M)==T)
org$E_M[375]
# trim
org$E_M <-str_trim(org$E_M, "right") 

unique(org$E_M)
which(org$E_M=="k.A.")
org[786,] # all social values missing for this entry

### 3. Check columns for social data

length(which(org$E_M=="E")) # 1054 single entries
length(which(org$E_M=="M")) # 221 double entires
# There are single and doubble entries which require differnet workflows for cleaning
# first subset for both single and double entries

single <- subset(org,subset = org$E_M=="E")
double <- subset(org,subset = org$E_M=="M")

### 4. CHeck Social data for single entries

### 4.1 gender (single)
unique(single$m.w)

# trim
single$m.w <-str_trim(single$m.w, "right") 

# set k.A. to NA
single$m.w[single$m.w== "k.A." ]<- NA
length(which(is.na(single$m.w)))

# 4.2  check age
unique(single$Alter) # 3 unique entries have issues

which(single$Alter=="14 und k.A.")
single[358,] # is single due to "E" but has double entries for social data

which(single$Alter=="")
single[355,] # is single due to "E" but has double entries for social data
# set NA
single$Alter[single$Alter==""] <- NA

which(single$Alter=="k.A.")
length(which(single$Alter=="k.A.")) # 8 k.A for Age in single
# set NA
single$Alter[single$Alter=="k.A."] <- NA

unique(single$Alter)

### 4.3 profession

# rename column
colnames(single)[9] <- "profession"

# check for tailling whitespaces
any(grepl(" $", single$profession)==T)
which(grepl(" $", single$profession)==T)
single$profession[213]
# trim
single$profession <-str_trim(single$profession, "right") 



unique(single$profession)
length(unique(single$profession)) # 170 different entries, maybe useful to make classes.





### X. Get Subset

soc <- org[,7:12]
colnames(soc) <- c("s/d","m/w","age","native_loc","peasent_p")

head(soc)
str(soc)



# convert class formates, all numbers to numeric
IDlocWest1$ID  <- as.numeric(IDlocWest1$ID)
IDlocWest1$GID <- as.numeric(IDlocWest1$GID)
IDlocWest1$LONG<- as.numeric(IDlocWest1$LONG)
IDlocWest1$LAT <- as.numeric(IDlocWest1$LAT)

write.csv(IDlocWest1,"Data/IDlocWest1.csv",row.names = F)

test <- read.csv("Data/IDlocWest1.csv")


