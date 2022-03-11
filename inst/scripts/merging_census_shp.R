#This script is for merging census data and the shapefile.

#1. Loading census data which are .dta file
#--> use the readstata13 package, there is a function calledread.dta or something like that.
#2. Loading shapefile
#--> First off, if you go into the MOZmap project repo, in the branch titled ifybranch, you will see a script called MOZ_datapull_final. Line 24 shows you how to read in the shapefile. When you load it you will notice, 80000+ observations, those are the all the areas or villages (not sure which one). Perhaps you can find out by doing some a bit of exploratory work

#command for merg= emerged = merge(sce_all, hhf, by = "userid")


#1. Loading census data which are .dta file
#library(haven)
library(readstata13)
library(data.table)
census_full = readstata13::read.dta13("D:/AFR_Database/SSAPOV-Harmonization/Daylan/GMD/MOZ_Census/01.Input/MOZ Pop Census 2017 Full (selected vars).dta")
##View(x)
#2. Loading shapefile

##as.data.table(census_full, TRUE)
##as.data.frame(census_full)

moz_shp <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                       layer = "BASE_COMPLETA_DE_AE_CENSO_2017")


##Change the formar of the variables and create the AES var from the moz_shp


census_full$PROVINCIA<- as.integer(census_full$PROVINCIA)

census_full$PROVINCIA<-sprintf("%02d",census_full$PROVINCIA)
census_full$DISTRITO<-sprintf("%02d",census_full$DISTRITO)
census_full$POSTO<-sprintf("%02d",census_full$POSTO)
census_full$LOCALIDADE<-sprintf("%02d",census_full$LOCALIDADE)
census_full$BAIRRO<-sprintf("%02d",census_full$BAIRRO)
census_full$AE<-sprintf("%03d",census_full$AE)

census_full$AES<- paste0(census_full$PROVINCIA,census_full$DISTRITO,
                         census_full$POSTO,census_full$LOCALIDADE,census_full$BAIRRO,
                         census_full$AE)

## Now we can try to merge the census and the shp file using the AES variable.









##Do not run the code below

##This function adds a leading zero for variables with 2 or 3 digits similar to the variables in moz_shp

ad_zero02<- function(X){
  return(sprintf("%02d",X))
}

ad_zero03<- function(X){
  return(sprintf("%03d",X))
}


##Apply the funtion to each variable in census_full


new_full<-cbind(census_full,apply(census_full[,c("P","DISTRITO","POSTO","LOCALIDADE","BAIRRO")], MARGIN = 2,
                  FUN = ad_zero02))

colnames(new_full)

new_full <-make.unique(new_full$P,sep ="_")

##Merge



##Using the census data, I replicated the variable AES from the shp file.

census_full$PROVINCIA<- sprintf("%02d",census_full$PROVINCIA)







census_full$AES <-paste0(census_full$PROVINCIA,
                        census_full$DISTRITO,
                        census_full$POSTO,
                        census_full$LOCALIDADE,
                        census_full$BAIRRO,




                        census_full$AE)
length(unique(census_full$AES))



##Contruct Variable

moz_shp$ID <- paste(moz_shp$AES,moz_shp$AC,sep="-") ##67957 unique observations
##length(unique(moz_shp$ID))



