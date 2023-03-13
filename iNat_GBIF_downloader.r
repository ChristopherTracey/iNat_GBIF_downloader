

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
if (!requireNamespace("arcgisbinding", quietly = TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)

arc.check_product()


sp <- read.csv("OKO_iNat.csv", stringsAsFactors = FALSE)
sp <- sp[which(sp$Potential_OKO!=""),]


splist <- sp$GNAME

taxa_tracker <- data.frame("GNAME"=character(), "iNat_name"=character())
a <- list()
k <- NULL
for(x in 81:length(splist)){
  #get metadata on the number of occurrences
  print(paste("getting metadata from iNaturalist for ",splist[x],".", sep="") )
  try(k <- get_inat_obs(taxon_name=splist[x],  geo=TRUE, meta=TRUE) ) # this step first queries iNat to see if there are any records present, if there are it actually downloads them.   ## bounds=c(39.7198, -80.519891, 42.26986,	-74.689516) ,
  Sys.sleep(5) # this is too throttle our requests so we don't overload their servers
  if(is.list(k)){
    print(paste("There are ", k$meta$found, " records on iNaturalist", sep=""))
    if(k$meta$found>0){
      
      if(k$meta$found>=10000){
        maxrec <- 10000
      } else {
        maxrec <-  k$meta$found
      }
      
      a[[x]] <- get_inat_obs(taxon_name=splist[x], geo=TRUE, maxresults=maxrec, meta=FALSE) #place_id=1,  # bounds=c(39.7198, -80.519891, 42.26986,	-74.689516) , 
     
     # insert the names into the taxa tracker
     taxa_tracker <- rbind(taxa_tracker, data.frame("GNAME"=splist[x], "iNat_name"=unique(a[[x]]$scientific_name))) 
      
     # save a map of the points
     ggplot(data=a[[x]], aes(x=longitude, y=latitude,colour=quality_grade)) +
       geom_polygon(data = map_data("world"),
                    aes(x=long, y=lat, group=group),
                    fill="grey95",
                    color="gray40",
                    size=0.1) +
       geom_point(size=1.5, alpha=0.95) +
       coord_fixed(xlim=c(-178.2,-49.0),#range(a[[x]]$longitude, na.rm = TRUE),
                   ylim=c(16,83.3) #range(a[[x]]$latitude, na.rm = TRUE)
                  ) +
       scale_color_manual(values=c("casual"="orange", "research"="blue", "needs_id"="darkgreen")) +
       theme_bw()
     ggsave(filename=here::here("_data","output", paste0(splist[x],".png")), last_plot())
     
      k <- NULL
    } else {}
  } else {
    print("No records found")
  }
}

# convert to a data frame
inatrecs <- plyr::ldply(a)

# remove captive cultivated records
inatrecs1 <- inatrecs[which(inatrecs$captive_cultivated!="true"),]

# remove obscured records
inatrecs1 <- inatrecs1[which(inatrecs1$geoprivacy!="obscured"),]
inatrecs1 <- inatrecs1[which(inatrecs1$taxon_geoprivacy!="open"|inatrecs1$taxon_geoprivacy!=""),]
inatrecs1 <- inatrecs1[which(inatrecs1$coordinates_obscured!="true"),]

# not research grade
inatrecs1 <- inatrecs1[which(inatrecs1$quality_grade=="research"),]

# sort just to make it cleaner
inatrecs1 <- inatrecs1[order(inatrecs1$scientific_name),]

#replace urls to all be https
inatrecs1$url <- str_replace_all(inatrecs1$url, "http://www.inaturalist.org/", "https://www.inaturalist.org/")
inatrecs1$url <- str_replace_all(inatrecs1$url, "[\r\n]" , "")

# positional accuracy
summary(inatrecs1$positional_accuracy)
inatrecs2 <- inatrecs1[which(inatrecs1$positional_accuracy<=100),]
summary(inatrecs2$positional_accuracy)
unique(inatrecs2$scientific_name)

inat_sf <- st_as_sf(inatrecs2, coords=c("longitude","latitude"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#inat_sf <- st_transform(inat_sf, crs=customalbers) # reproject to the custom albers
arc.write(path="S:/Projects/USFWS/NE_NaturesNetwork_2022/Pro/Draft/NaturesNetwork_ctracey/NaturesNetwork_ctracey.gdb/inat_records", inat_sf, overwrite=TRUE) # write a feature class into the geodatabase



###########################################
# GBIF

# gets the  keys for each species name, based on GBIF
keys <- sapply(splist, function(x) name_backbone(name=x)$speciesKey[1], USE.NAMES=FALSE)
### add some code to put out the list of species not found in GBIF
a1 <- which(sapply(keys,is.null))
missingGBIFsp <- splist[a1]
missingGBIFsp
rm(a1)
cat(length(missingGBIFsp),"of",length(splist), "species were not found in GBIF", "\n", "They are:", paste(missingGBIFsp, collapse=", "))

# gets rid of any null values generated by name_backbone in the case of unmatchable species names
keys1 <- keys[-(which(sapply(keys,is.null),arr.ind=TRUE))] #note: seems to break if there is only one item in the list... use for multiple species!
keys <- keys1
#searches for occurrences
dat <- occ_search(
  taxonKey=keys, 
  limit=10000, # modify if needed, fewer will make testing go faster
  return='data', 
  hasCoordinate=TRUE,
  #geometry='POLYGON ((-80.577111647999971 42.018959019000079, -80.583025511999949 39.690462536000041, -77.681987232999973 39.68735201800007, -75.761816590999956 39.690666106000037, -75.678308913999956 39.790810226000076, -75.53064649099997 39.815101786000071, -75.411566911999955 39.776679135000052, -75.101245089999964 39.880029385000057, -75.09383042199994 39.944216030000064, -74.690932882999959 40.133570156000076, -74.690425973999936 40.17528313400004, -74.893196517999968 40.350896889000069, -74.914505704999954 40.415842984000051, -75.012247039999977 40.448477402000037, -75.004556583999943 40.522413349000033, -75.134560399999941 40.623471625000036, -75.136516799999981 40.723392383000032, -75.002409694999983 40.867515299000047, -75.082051382999964 40.971575944000051, -74.830463730999952 41.152763058000062, -74.768212647999974 41.271891205000031, -74.640518995999969 41.358839422000074, -74.709416559999966 41.454495330000043, -74.826329023999961 41.475865789000068, -74.936988959999951 41.521739840000066, -75.018029425999941 41.617276498000081, -75.012709979999954 41.733926517000043, -75.061642930999938 41.85481505100006, -75.218658916999971 41.904656042000056, -75.336705265999967 42.017618624000079, -77.511689405999959 42.017704281000078, -79.721693517999938 42.024739989000068, -79.715980736999938 42.353623043000027, -80.577111647999971 42.018959019000079))', # simplified boundary of Pennsylvania.
  year='1970,2023'
)



dat <-  dat[dat!="no data found, try a different search"] # deletes the items from the list where no occurrences were found. doesn't work for one species



datdf <- lapply(dat, function(x) x[[3]])# selects the $data from each list element

#make the columns all match across the dataframes
#choose which columns you actually care about and will be keeping (these are ragged tables otherwise)
fields <- c('species','scientificName','datasetKey','identifier','recordedBy','key','decimalLatitude','decimalLongitude','country','basisOfRecord','coordinateUncertaintyInMeters','coordinateAccuracy','coordinatePrecision','year','month','day', 'datasetName','stateProvince','issues','references')

#not every data frame has every column--this loop adds in the "fields" columns for the dfs that are missing those columns
for (i in 1:length(datdf))    {
  if ((identical(colnames(datdf[[i]]),fields)) == FALSE) {
    nms   = fields
    df =   datdf[[i]]
    aux = colnames(df)
    aux1 = row.names(df)
    Missing = setdiff(nms, colnames(df))  
    ind = seq(1,length(Missing)) #creating indices 1-5 for loop
    for (j in ind)  {    #loop to add columns with zeros
      df = cbind(df,c(NA))
    }
    colnames(df) = c(aux,Missing)   #updates columns names
    df = df[,order(colnames(df))]  #put columns into order
    datdf[[i]] = df              #updates object from list
  } 
}
dat_df <- lapply(datdf, '[', fields) #subset out just the focal columns

gbif_df <- dplyr::bind_rows(dat_df, .id = "column_label") #turns it into one big dataframe

# remove GBIF records that we already have from the iNat download
`%notin%` <- Negate(`%in%`)
gbif_df1 <- gbif_df[gbif_df$identifier %notin% unique(inatrecs$id),] #inatrecs$url

# remove non-USA records
gbif_df1 <- gbif_df1[which(gbif_df1$country=="United States of America"),]

a <- as.data.frame(unique(gbif_df1$issues))
a1 <- unique(separate_rows(a, 'unique(gbif_df1$issues)'))

gbif_df1 <- gbif_df1[c("species", "datasetKey", "recordedBy", "basisOfRecord","coordinateUncertaintyInMeters","coordinateAccuracy","year","month","day")]




# additional metadata
lu_gbifissues <- gbif_issues()
gbif_citation("861d3d64-f762-11e1-a439-00145eb45e9a") #unique(gbif_df1$datasetKey)



gbif_sf <- st_as_sf(gbif_df1, coords=c("decimalLongitude","decimalLatitude"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
arc.write(path="S:/Projects/USFWS/NE_NaturesNetwork_2022/Pro/Draft/NaturesNetwork_ctracey/NaturesNetwork_ctracey.gdb/gbif_records", gbif_sf, overwrite=TRUE) # write a feature class into the geodatabase

##################################################
# combine the iNat and GBIF layers into one

names(gbif_sf)
names(inat_sf)




