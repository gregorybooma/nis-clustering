# Processing of spatial data -- zip codes, hospital referral and service zones
# 
# Still using sp objects here. Converting to sf will be more time-consuming, 
# so will need to revisit when finished with other scripts in the repo.

if (basename(getwd()) == "nis-clustering") {
    setwd("./workspace")
} else {
    stopifnot(basename(getwd()) == "workspace")
}
lapply(c("iotools", "Hmisc", "plyr", "dplyr", "stringr", "maptools", "rgdal", "rgeos", "RColorBrewer","VIM"), require, quietly = T, character.only = T)
options(digits = 15, scipen = 999, stringsAsFactors = F)

# not sure if IDvar is needed so keeping as reminder to check
#zip_polys <- readShapePoly("./OGP913/SDE2_ESRI09USZIP5_POLY", IDvar = "ZIP", proj4string=CRS("+proj=longlat +datum=WGS84"))
zip_polys <- readOGR("./OGP913", layer = "SDE2_ESRI09USZIP5_POLY")

# quick check -- and make sure zip came in as character
head(zip_polys@data)
tail(zip_polys@data)
summary(zip_polys)

# readOGR() doesn't support setting an ID var so have to do here
length(which(is.na(zip_polys$ZIP)))
zip_polys <- spChFIDs(zip_polys,zip_polys$ZIP)
# verify
for (i in 1:5) { message(slot(slot(zip_polys, "polygons")[[i]], "ID")) }
row.names(zip_polys)[1:5]

zip_polys@data <- zip_polys@data %>% mutate_at("POP2009", ~na_if(.,-99)) %>% mutate_at("POP09_SQMI", ~na_if(.,-99))

# no longer need to specify @data for this
set_polys <- zip_polys[zip_polys$STATE %in% c("PA","NJ","NY","CT","RI","MA","VT","NH","ME"),]
summary(set_polys)
# make sure zip codes were assigned as row names
plot(set_polys)
rm(zip_polys)
gc()

# avoid the caps lock key
names(set_polys) <- str_to_lower(names(set_polys))
names(nis_set) <- str_to_lower(names(nis_set))

# Maine, Massachusetts, and New Hampshire have unorganized regions that are officially "unpopulated",
# but have seasonal and transient population. Zipcodes for these areas start "001". To ensure
# that they are included when I build hospital referral and service zones, I will assign to
# them the zipcode of the nearest neighboring valid zipcode based upon the geographic center of each.
ne_polys <- set_polys[set_polys$state=="ME" | set_polys$state=="MA" | set_polys$state=="NH",]
plot(ne_polys)
#ids <- sapply(set_polys@polygons[set_polys@data$state=="ME"],function(x) {x@ID})
text(coordinates(ne_polys), labels = ne_polys@data$zip, cex = 0.5, col = "blue")

# need to create separate point layers for valid, and invalid zipcodes, to facilitate assignment
valid_pts <- SpatialPointsDataFrame(coordinates(ne_polys[!ne_polys$zip %in% grep("^001",ne_polys@data$zip,value = T),]), data = ne_polys@data[!ne_polys$zip %in% grep("^001",ne_polys@data$zip,value = T),], proj4string = CRS("+proj=longlat +datum=WGS84"))

invalid_pts <- SpatialPointsDataFrame(coordinates(ne_polys[ne_polys$zip %in% grep("^001",ne_polys@data$zip,value = T),]), data = ne_polys@data[ne_polys$zip %in% grep("^001",ne_polys@data$zip,value = T),], proj4string = CRS("+proj=longlat +datum=WGS84"))

plot(ne_polys)
points(valid_pts,col="blue")
points(invalid_pts,pch = 3,col="red")

distances <- gDistance(invalid_pts,valid_pts,byid = T)
old_zips <- dimnames(distances)[[2]]

# need USE.NAMES=F to stop overwriting of names -- they are needed as-is
new_zips <- names(sapply(dimnames(distances)[[2]],function(x) { which(distances[,x]==min(distances[,x],na.rm = T))}, USE.NAMES = F))
new_zip_df <- data.frame(cbind(old_zips,new_zips))
names(new_zip_df) <- c("zip","new_zip")

set_polys@data <- left_join(set_polys@data,new_zip_df,"zip")
set_polys@data$zip[!is.na(set_polys@data$new_zip)] <- set_polys@data$new_zip[!is.na(set_polys@data$new_zip)]
set_polys@data$new_zip <- NULL

# check that the zips were assigned correctly
plot(set_polys)
text(coordinates(set_polys), labels = set_polys@data$zip, cex = 0.5, col = "blue")

rm(ne_polys,valid_pts,invalid_pts,distances,old_zips,new_zips,new_zip_df)

# it seems, through perusing the dartmouth atlas literature, hsa and hrr bounds did not change from '09 to '10
hosp_hsahrr <- read.csv.raw("../workspace/hosp_hsa_hrr_2010.csv", colClasses = rep("character",10))
# read.csv.raw leaves quotes in the headers, so strip them
names(hosp_hsahrr) <- str_replace_all(names(hosp_hsahrr),"\"","")
hosp_hsahrr <- data.frame(apply(hosp_hsahrr,2,str_trim))

zip_hsahrr <- read.csv.raw("ziphsahrr09.csv", colClasses = rep("character",7))
names(zip_hsahrr) <- str_replace_all(names(zip_hsahrr),"\"","")
zip_hsahrr <- data.frame(apply(zip_hsahrr,2,str_trim))

hosp_zones <- filter(hosp_hsahrr, hsastate %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT"))
zip_zones <- filter(zip_hsahrr, hsastate %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT"))
zip_zones <- rename(zip_zones,zip = zipcode09)

# see how well the zips match between files -- zips are not available for Maine in the NIS
nis_zips <- nis_set %>% select(hospzip,hospst) %>% filter(hospst!="ME") %>% distinct()

nis_zip_zones <- left_join(nis_zips, zip_zones, by = c("hospzip"="zip"))
# make sure all rows are accounted for
nrow(nis_zips)
length(unique(nis_zip_zones$hospzip))
# confirm that hospital state corresponds to hsa state, but not always hrr state
setequal(nis_zip_zones$hospst,nis_zip_zones$hsastate)
setequal(nis_zip_zones$hospst,nis_zip_zones$hrrstate)
# hospst is redundant, so drop for joining
nis_zip_zones <- nis_zip_zones %>% select(-hospst)

# add hospital zones to the NIS subset...
nis_set <- left_join(nis_set,nis_zip_zones,by = "hospzip")
# confirm that only Maine hospitals are missing hrrstate
countNA(nis_set$hospst)
length(which(nis_set$hospst=="ME"))
length(which(is.na(nis_set$hrrstate) & nis_set$hospst == "ME"))
length(which(is.na(nis_set$hrrnum)))

# populate hrrstate and hsastate missing with ME
# although very NW of ME is in a NH hrr there are no facilities there
nis_set <- nis_set %>% mutate_at("hrrstate", ~coalesce(.,"ME")) %>% mutate_at("hsastate", ~coalesce(.,"ME"))

#######################################################################################
# EVERYTHING BELOW THIS POINT IS CREATION OF SPATIAL LAYERS FOR MAPPING PURPOSES ONLY.
# NO DATA BELOW IS USED IN THE CLUSTERING PROCESS.
# #####################################################################################

# make sure all zipcode polygons have hospital areas assigned
check <- left_join(set_polys@data,zip_zones,"zip")
countNA(check$hrrnum)
check$state[is.na(check$hrrnum)]

# PA is missing a handful. Have to assign nearest...
# remember subsetting sp so no "tidy" verbs here
pa_polys <- set_polys[set_polys$state=="PA",]
plot(pa_polys)
text(coordinates(pa_polys), labels = pa_polys@data$zip, cex = 0.5, col = "blue")

valid_pts <- SpatialPointsDataFrame(coordinates(pa_polys[!pa_polys$zip %in% check$zip[is.na(check$hrrnum)],]), data = pa_polys@data[!pa_polys$zip %in% check$zip[is.na(check$hrrnum)],], proj4string = CRS("+proj=longlat +datum=WGS84"))
invalid_pts <- SpatialPointsDataFrame(coordinates(pa_polys[pa_polys$zip %in% check$zip[is.na(check$hrrnum)],]), data = pa_polys@data[pa_polys$zip %in% check$zip[is.na(check$hrrnum)],], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(pa_polys)
points(valid_pts,col="blue")
points(invalid_pts,pch = 3,col="red")
rm(check)

distances <- gDistance(invalid_pts,valid_pts,byid = T)
old_zips <- dimnames(distances)[[2]]

# need USE.NAMES=F to stop overwriting of names -- they are needed as-is
new_zips <- names(sapply(dimnames(distances)[[2]],function(x) { which(distances[,x]==min(distances[,x],na.rm = T))}, USE.NAMES = F))
new_zip_df <- bind_cols(data.frame(old_zips),data.frame(new_zips))
names(new_zip_df) <- c("zip","new_zip")
glimpse(new_zip_df)

set_polys@data <- left_join(set_polys@data,new_zip_df,"zip")
set_polys@data$zip[!is.na(set_polys@data$new_zip)] <- set_polys@data$new_zip[!is.na(set_polys@data$new_zip)]
set_polys@data$new_zip <- NULL
# check for nulls again
check <- left_join(set_polys@data,zip_zones,"zip")
countNA(check$hrrnum)
rm(check)

set_polys@data <- select(set_polys@data,zip,po_name,state)
set_polys@data <- left_join(set_polys@data,zip_zones,"zip")
# check that the zips in PA were assigned correctly
bbox(set_polys)
plot(set_polys, xlim = c(-80.5,-79.25), ylim = c(39.5,40.65))
text(coordinates(set_polys), xlim = c(-80.5,-79.25), ylim = c(39.5,40.65), labels = set_polys@data$zip, cex = 0.5, col = "blue")

# clean-up, but keep hosp_zones dataframe in case it is needed later
rm(pa_polys, valid_pts, invalid_pts, distances, old_zips, new_zips, new_zip_df, hosp_hsahrr, zip_hsahrr)

# look at hospital referral and hospital service zone boundaries to make sure all looks ok
hrr_polys <- unionSpatialPolygons(set_polys,IDs = as.character(set_polys$hrrnum))
plot(hrr_polys, col = brewer.pal(12,"Set3"))
polygonsLabel(hrr_polys,labels = names(hrr_polys), method = "centroid", doPlot = T, cex = 0.75)

# create state boundary polys to plot over hospital bounds for comparison
state_polys_raw <- unionSpatialPolygons(set_polys,IDs = as.character(set_polys$state))
# clean-up some holes (gets rid of internal holes, but not those extending to outer bounds)
state_poly_geom <- slot(state_polys_raw, "polygons")
state_hole_geom <- lapply(state_poly_geom, function(x) {sapply(slot(x, "Polygons"), slot, "hole")})

state_poly_geom_noholes <- lapply(1:length(state_poly_geom), function(x) {slot(state_poly_geom[[x]],"Polygons")[!state_hole_geom[[x]]]})

ids <- row.names(state_polys_raw)

state_polys <- SpatialPolygons(lapply(1:length(state_poly_geom_noholes), function(x) {Polygons(state_poly_geom_noholes[[x]], ID=ids[x])}), proj4string=CRS(proj4string(state_polys_raw)))
# overlay state bounds on hrr polygons
plot(state_polys, col = "transparent", border = "grey", add = T)

# now a look at hsa and state bounds
hsa_polys <- unionSpatialPolygons(set_polys,IDs = as.character(set_polys$hsanum))
plot(hsa_polys, col = brewer.pal(12,"Set3"))
plot(state_polys, col = "transparent", border = "grey", add = T)
polygonsLabel(hsa_polys,labels = names(hsa_polys), method = "centroid", doPlot = T, cex = 0.45)

rm(hosp_zones,hrr_polys,nis_zips,nis_zip_zones,zip_zones,set_polys,state_polys_raw,ids,state_hole_geom,state_poly_geom,state_poly_geom_noholes)
