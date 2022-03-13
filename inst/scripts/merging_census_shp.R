#This script is for merging census data and the shapefile.

#1. Loading census data which are .dta file
#--> use the readstata13 package, there is a function calledread.dta or something like that.
#2. Loading shapefile
#--> First off, if you go into the MOZmap project repo, in the branch titled ifybranch, you will see a script called MOZ_datapull_final. Line 24 shows you how to read in the shapefile. When you load it you will notice, 80000+ observations, those are the all the areas or villages (not sure which one). Perhaps you can find out by doing some a bit of exploratory work




#1. Loading census data which are .dta file
#library(haven)
library(readstata13)
library(data.table)

#Load The data
census_full <- readstata13::read.dta13("D:/AFR_Database/SSAPOV-Harmonization/Daylan/GMD/MOZ_Census/01.Input/MOZ Pop Census 2017 Full (selected vars).dta")


moz_shp <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                       layer = "BASE_COMPLETA_DE_AE_CENSO_2017")


census_full$PROVINCIA<- as.integer(census_full$PROVINCIA)

##Keep only the relevant variables from the census_full. (ie. Labour informality and identifiers)


##This function adds a leading zero for variables with 2 9similar to the variables in moz_shp

ad_zero02<- function(X){
  return(sprintf("%02d",X))
}

##Apply the funtion to each variable in census_full

new_vars<-apply(census_full[,c("PROVINCIA","DISTRITO","POSTO","LOCALIDADE",
                                   "BAIRRO")],
                MARGIN = 2,FUN = ad_zero02)

census_full$AE<-sprintf("%03d",census_full$AE)

##Since the Apply function returns a matrix, we convert it into a data frame

new_vars<- as.data.frame(new_vars)

##Now we change the name of the columns


colnames(new_vars)<- c('Province',"District","Postal", "Location","Neighbor")


##Merge our our new colums with the census.

Census_merged <- cbind(new_vars,census_full)
rm(new_vars,census_full)


##Create the AES variable to merge with the shp file.

Census_merged$AES<- paste0(Census_merged$Province,Census_merged$District,Census_merged$Postal,
                           Census_merged$Location,Census_merged$Neighbor,Census_merged$AE)

##Merge

Maputo_merged_data <- merge(moz_shp,Census_merged, by="AES")
rm(Census_merged,moz_shp)

##Check the merge

unique(length(Maputo_merged_data$AES))


