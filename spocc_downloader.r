

# load packages
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
require(here)
if (!requireNamespace("rinat", quietly = TRUE)) install.packages("rinat")
require(rinat)
if (!requireNamespace("rgbif", quietly = TRUE)) install.packages("rgbif")
require(rgbif)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
require(tidyverse)
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
require(sf)
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
require(lubridate)
if (!requireNamespace("arcgisbinding", quietly = TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)

arc.check_product()


# copy the blank gdb directory from the base folder to the output directory
current_folder <- here::here("_data","template","blank.gdb") 
new_folder <- here::here("_data","output","extData.gdb") # updateName,
list_of_files <- list.files(path=current_folder, full.names=TRUE) 
dir.create(new_folder)
file.copy(from=file.path(list_of_files), to=new_folder,  overwrite=TRUE, recursive=FALSE, copy.mode=TRUE)
rm(list_of_files, current_folder)



sp <- read.csv("OKO_iNat.csv", stringsAsFactors = FALSE)
sp <- sp[which(sp$Potential_OKO!=""),]

splist <- sp$GNAME

library(spocc)

a_inat <- list()
a_gbif <- list()
a_idig <- list()

for(x in 1:length(splist)){ #
  print(paste("getting data from iNaturalist for ",splist[x],".", sep="") )
  df_inat <- occ(query=splist[x], from='inat', has_coords=TRUE)
  a_inat[[x]] <- df_inat$inat$data[[1]]
  
  print(paste("getting data from GBIF for ",splist[x],".", sep="") )
  df_gbif <- occ(query=splist[x], from='gbif', has_coords=TRUE)
  a_gbif[[x]] <- df_gbif$gbif$data[[1]]
  
  print(paste("getting data from iDigBio for ",splist[x],".", sep="") )
  df_idig <- occ(query=splist[x], from='idig', has_coords=TRUE)
  a_idig[[x]] <- df_idig$idig$data[[1]]
}  

# convert to a data frame
recs_inat <- plyr::ldply(a_inat)  
recs_gbif <- plyr::ldply(a_gbif)  
recs_idig <- plyr::ldply(a_idig)

names(recs_inat)
names(recs_gbif)
names(recs_idig)

recs_inat_terse <- recs_inat[c("id","name","positional_accuracy","public_positional_accuracy", "observed_on","quality_grade","captive","geoprivacy","obscured","longitude","latitude","taxon.conservation_status.geoprivacy","geojson")]
recs_gbif_terse <- recs_gbif[c("key","species","longitude", "latitude","coordinateUncertaintyInMeters","year","issues","georeferenceProtocol","locationRemarks","footprintWKT" )]
recs_idig_terse <- recs_idig[c("recordids","name","country","datasetid","datecollected","longitude","latitude","coordinateuncertainty","flags")]

recs_inat_terse$source <- "inat"
recs_gbif_terse$source <- "gbif"
recs_idig_terse$source <- "idig"

# standardize the species name column
recs_inat_terse <- recs_inat_terse %>% 
  rename(rec_id=id, sp_name=name, coordinatencertaintyMeters=public_positional_accuracy)
recs_gbif_terse <- recs_gbif_terse %>% 
  rename(rec_id=key, sp_name=species, coordinatencertaintyMeters=coordinateUncertaintyInMeters)
recs_idig_terse <- recs_idig_terse %>% 
  rename(rec_id=recordids, sp_name=name, coordinatencertaintyMeters=coordinateuncertainty)

# change idig sp name to upcase
recs_idig_terse$sp_name <- str_replace(recs_idig_terse$sp_name, "^\\w{1}", toupper)

# update lastobs year
recs_inat_terse$year <- year(parse_date_time(recs_inat_terse$observed_on,"ymd"))
recs_gbif_terse$year <- recs_gbif_terse$year
recs_idig_terse$year <- year(parse_date_time(recs_idig_terse$datecollected,"ymd"))

# drop missing coordinates
recs_inat_terse <- recs_inat_terse[which(!is.na(recs_inat_terse$latitude)),]
recs_gbif_terse <- recs_gbif_terse[which(!is.na(recs_gbif_terse$latitude)),]
recs_idig_terse <- recs_idig_terse[which(!is.na(recs_idig_terse$latitude)),]

# fill in default uncertainty distance if not present
def_uncertainty <- 300 # meters
recs_inat_terse$coordinatencertaintyMeters <- recs_inat_terse$coordinatencertaintyMeters %>% replace_na(def_uncertainty)
recs_gbif_terse$coordinatencertaintyMeters <- recs_gbif_terse$coordinatencertaintyMeters %>% replace_na(def_uncertainty)
recs_idig_terse$coordinatencertaintyMeters <- recs_idig_terse$coordinatencertaintyMeters %>% replace_na(def_uncertainty)

#3
sortOrder <- c("source","rec_id","sp_name","longitude", "latitude","coordinatencertaintyMeters","year") #recs_inat_sf <- bamona_sf[final_fields]

recs_inat_terse <- recs_inat_terse[sortOrder]
recs_gbif_terse <- recs_gbif_terse[sortOrder]
recs_idig_terse <- recs_idig_terse[sortOrder]

recs_obs <- rbind(recs_inat_terse, recs_gbif_terse, recs_idig_terse)


# create the spatial layers
# create a spatial layer
recs_obs_sf <- st_as_sf(recs_obs, coords=c("longitude","latitude"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
recs_obs_sf <- st_transform(recs_obs_sf, crs="+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs") # reproject to the NA albers
arc.write(path=here::here("_data","output","extData.gdb","pt_obs"), recs_obs_sf, overwrite=TRUE) # write a feature class into the geodatabase
recs_obs_buffer <- st_buffer(recs_obs_sf, dist=recs_obs_sf$coordinatencertaintyMeters) # buffer
arc.write(path=here::here("_data","output","extData.gdb","buff_obs"), recs_obs_buffer, overwrite=TRUE) # write a feature class into the geodatabase
