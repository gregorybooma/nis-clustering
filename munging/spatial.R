# processing of spatial data -- zip codes, hospital referral and service zones
if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr", "maptools", "rgdal", "rgeos", "RColorBrewer","VIM"), require, quietly = T, character.only = T)
options(digits = 15, scipen = 999999999, stringsAsFactors = F)

zippolys <- readShapePoly("./OGP913/SDE2_ESRI09USZIP5_POLY", IDvar = "ZIP", proj4string=CRS("+proj=longlat +datum=WGS84"))
summary(zippolys)
head(zippolys@data)
tail(zippolys@data)
zippolys@data$POP2009[zippolys@data$POP2009 == -99] <- NA
zippolys@data$POP09_SQMI[zippolys@data$POP09_SQMI == -99] <- NA
setpolys <- zippolys[zippolys$STATE %in% c("PA","NJ","NY","CT","RI","MA","VT","NH","ME"),]
summary(setpolys)
# make sure zip codes were assigned as row names
row.names(setpolys)[1:10]
plot(setpolys)
rm(zippolys)

# avoid the caps lock key
names(setpolys@data) <- str_to_lower(names(setpolys@data))
names(nisSet) <- str_to_lower(names(nisSet))

# Maine, Massachusetts, and New Hampshire have unorganized regions that are officially "unpopulated",
# but have seasonal and transient population. Zipcodes for these areas start "001". To ensure
# that they are included when I build hospital referral and service zones, I will assign to
# them the zipcode of the nearest neighboring valid zipcode based upon the geographic center of each.
nepolys <- setpolys[setpolys$state=="ME" | setpolys$state=="MA" | setpolys$state=="NH",]
plot(nepolys)
#ids <- sapply(setpolys@polygons[setpolys@data$state=="ME"],function(x) {x@ID})
text(coordinates(nepolys), labels = nepolys@data$zip, cex = 0.5, col = "blue")

# need to create separate point layers for valid, and invalid zipcodes, to facilitate assignment
validpt <- SpatialPointsDataFrame(coordinates(nepolys[!nepolys$zip %in% grep("^001",nepolys@data$zip,value = T),]), data = nepolys@data[!nepolys$zip %in% grep("^001",nepolys@data$zip,value = T),], proj4string = CRS("+proj=longlat +datum=WGS84"))
invalidpt <- SpatialPointsDataFrame(coordinates(nepolys[nepolys$zip %in% grep("^001",nepolys@data$zip,value = T),]), data = nepolys@data[nepolys$zip %in% grep("^001",nepolys@data$zip,value = T),], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(nepolys)
points(validpt,col="blue")
points(invalidpt,pch = 3,col="red")

distances <- gDistance(invalidpt,validpt,byid = T)
oldzips <- dimnames(distances)[[2]]
# need USE.NAMES=F to stop overwriting of names -- they are needed as-is
newzips <- names(sapply(dimnames(distances)[[2]],function(x) { which(distances[,x]==min(distances[,x],na.rm = T))}, USE.NAMES = F))
newzipdf <- data.frame(cbind(oldzips,newzips))
names(newzipdf) <- c("zip","newzip")

setpolys@data <- left_join(setpolys@data,newzipdf,"zip")
setpolys@data$zip[!is.na(setpolys@data$newzip)] <- setpolys@data$newzip[!is.na(setpolys@data$newzip)]
setpolys@data$newzip <- NULL

# check that the zips were assigned correctly
plot(setpolys)
text(coordinates(setpolys), labels = setpolys@data$zip, cex = 0.5, col = "blue")

rm(nepolys,validpt,invalidpt,distances,oldzips,newzips,newzipdf)

# it seems, through perusing the dartmouth atlas literature, hsa and hrr bounds did not change from '09 to '10
hosphsahrr <- read.csv.raw("hosp_hsa_hrr_2010.csv", colClasses = rep("character",10))
# read.csv.raw leaves quotes in the headers, so strip them
names(hosphsahrr) <- gsub("\"","",noquote(names(hosphsahrr)))
hosphsahrr <- data.frame(apply(hosphsahrr,2,str_trim))

ziphsahrr <- read.csv.raw("ziphsahrr09.csv", colClasses = rep("character",7))
names(ziphsahrr) <- gsub("\"","",noquote(names(ziphsahrr)))
ziphsahrr <- data.frame(apply(ziphsahrr,2,str_trim))

hospzones <- filter(hosphsahrr, hsastate %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT"))
zipzones <- filter(ziphsahrr, hsastate %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT"))
zipzones <- rename(zipzones,zip = zipcode09)

# see how well the zips match between files -- zips are not available for Maine in the NIS
niszips <- distinct(select(nisSet[nisSet$hospst!="ME",],hospzip,hospst),hospzip,hospst)
niszipzones <- left_join(niszips, zipzones, by = c("hospzip"="zip"))
# make sure all rows are accounted for
nrow(niszips)
length(unique(niszipzones$hospzip))
# confirm that hospital state corresponds to hsa state, but not always hrr state
which(niszipzones$hospst!=niszipzones$hsastate)
which(niszipzones$hospst!=niszipzones$hrrstate)
# hospst is redundant, so drop for joining
niszipzones$hospst <- NULL

# add hospital zones to the NIS subset...
nisSet <- left_join(nisSet,niszipzones,"hospzip")
# confirm that only Maine hospitals are missing hrrstate
countNA(nisSet$hospst)
length(which(is.na(nisSet$hrrstate) & nisSet$hospst != "ME"))
# populate hrrstate missing with ME
nisSet$hrrstate[is.na(nisSet$hrrstate)] <- "ME"

# make sure all zipcode polygons have hospital areas assigned
check <- left_join(setpolys@data,zipzones,"zip")
countNA(check$hrrnum)
check$state[is.na(check$hrrnum)]

# PA is missing a handful. Have to assign nearest...
papolys <- setpolys[setpolys$state=="PA",]
plot(papolys)
text(coordinates(papolys), labels = papolys@data$zip, cex = 0.5, col = "blue")

validpt <- SpatialPointsDataFrame(coordinates(papolys[!papolys$zip %in% check$zip[is.na(check$hrrnum)],]), data = papolys@data[!papolys$zip %in% check$zip[is.na(check$hrrnum)],], proj4string = CRS("+proj=longlat +datum=WGS84"))
invalidpt <- SpatialPointsDataFrame(coordinates(papolys[papolys$zip %in% check$zip[is.na(check$hrrnum)],]), data = papolys@data[papolys$zip %in% check$zip[is.na(check$hrrnum)],], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(papolys)
points(validpt,col="blue")
points(invalidpt,pch = 3,col="red")
rm(check)

distances <- gDistance(invalidpt,validpt,byid = T)
oldzips <- dimnames(distances)[[2]]
# need USE.NAMES=F to stop overwriting of names -- they are needed as-is
newzips <- names(sapply(dimnames(distances)[[2]],function(x) { which(distances[,x]==min(distances[,x],na.rm = T))}, USE.NAMES = F))
newzipdf <- data.frame(cbind(oldzips,newzips))
names(newzipdf) <- c("zip","newzip")
glimpse(newzipdf)

setpolys@data <- left_join(setpolys@data,newzipdf,"zip")
setpolys@data$zip[!is.na(setpolys@data$newzip)] <- setpolys@data$newzip[!is.na(setpolys@data$newzip)]
setpolys@data$newzip <- NULL
# check for nulls again
check <- left_join(setpolys@data,zipzones,"zip")
countNA(check$hrrnum)
rm(check)

setpolys@data <- select(setpolys@data,zip,po_name,state)
setpolys@data <- left_join(setpolys@data,zipzones,"zip")
# check that the zips in PA were assigned correctly
bbox(setpolys)
plot(setpolys, xlim = c(-80.5,-79.25), ylim = c(39.5,40.65))
text(coordinates(setpolys), xlim = c(-80.5,-79.25), ylim = c(39.5,40.65), labels = setpolys@data$zip, cex = 0.5, col = "blue")

# clean-up, but keep hospzones dataframe in case it is needed later
rm(papolys, validpt, invalidpt, distances, oldzips, newzips, newzipdf, hosphsahrr, ziphsahrr)

# look at hospital referral and hospital service zone boundaries to make sure all looks ok
hrrpolys <- unionSpatialPolygons(setpolys,IDs = as.character(setpolys$hrrnum))
plot(hrrpolys, col = brewer.pal(12,"Set3"))
polygonsLabel(hrrpolys,labels = names(hrrpolys), method = "centroid", doPlot = T, cex = 0.75)

# create state boundary polys to plot over hospital bounds for comparison
statepolysraw <- unionSpatialPolygons(setpolys,IDs = as.character(setpolys$state))
# clean-up some holes (gets rid of internal holes, but not those extending to outer bounds)
statepolygeom <- slot(statepolysraw, "polygons")
stateholegeom <- lapply(statepolygeom, function(x) {sapply(slot(x, "Polygons"), slot, "hole")})
statepolygeomnoholes <- lapply(1:length(statepolygeom), function(x) {slot(statepolygeom[[x]],"Polygons")[!stateholegeom[[x]]]})
ids <- row.names(statepolysraw)
statepolys <- SpatialPolygons(lapply(1:length(statepolygeomnoholes), function(x) {Polygons(statepolygeomnoholes[[x]], ID=ids[x])}), proj4string=CRS(proj4string(statepolysraw)))
# overlay state bounds on hrr polygons
plot(statepolys, col = "transparent", border = "grey", add = T)

# now a look at hsa and state bounds
hsapolys <- unionSpatialPolygons(setpolys,IDs = as.character(setpolys$hsanum))
plot(hsapolys, col = brewer.pal(12,"Set3"))
plot(statepolys, col = "transparent", border = "grey", add = T)
polygonsLabel(hsapolys,labels = names(hsapolys), method = "centroid", doPlot = T, cex = 0.45)

rm(hospzones,hrrpolys,niszips,niszipzones,zippolys,zipzones,setpolys,statepolysraw,ids,stateholegeom,statepolygeom,statepolygeomnoholes)
