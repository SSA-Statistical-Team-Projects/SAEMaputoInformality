#This script is for merging census data and the shapefile.

#1. Loading census data which are .dta file
#--> use the readstata13 package, there is a function calledread.dta or something like that.
#2. Loading shapefile
#--> First off, if you go into the MOZmap project repo, in the branch titled ifybranch, you will see a script called MOZ_datapull_final. Line 24 shows you how to read in the shapefile. When you load it you will notice, 80000+ observations, those are the all the areas or villages (not sure which one). Perhaps you can find out by doing some a bit of exploratory work




#1. Loading census data which are .dta file
#library(haven)
.libPaths("E:/Daylan/R")
library(sf)
library(readstata13)
library(data.table)
library(dplyr)
library(tidyverse)
library (ggplot2)
library(rgdal)
library(fst)


#Load The data


##Read in the 10% census data from the esapov.
census_10 <- readstata13::read.dta13("//esapov/esapov/MOZ/GEO/Population/moz_census_10.dta")


moz_shp <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                       layer = "BASE_COMPLETA_DE_AE_CENSO_2017")

crs_dt<- rgdal::make_EPSG()
moz_shp<- st_transform(moz_shp, crs_dt$prj4[crs_dt$code==3974])
rm(crs_dt)


# census_full$PROVINCIA<- as.integer(census_full$PROVINCIA)

##Keep only the relevant variables from the census_full. (ie. Labour informality and identifiers)


##This function adds a leading zero for variables with 2 9similar to the variables in moz_shp

ad_zero02<- function(X){
  return(sprintf("%03d",X))
}

#
# ##Apply the funtion to each variable in census_full
#
# new_vars<-apply(census_full[,c("PROVINCIA","DISTRITO","POSTO","LOCALIDADE",
#                                    "BAIRRO")],
#                 MARGIN = 2,FUN = ad_zero02)
#
# census_full$AE<-sprintf("%3d",census_full$AE)
#
# ##Since the Apply function returns a matrix, we convert it into a data frame
#
# new_vars<- as.data.frame(new_vars)
#
# ##Now we change the name of the columns
#
#
# colnames(new_vars)<- c('Province',"District","Postal", "Location","Neighbor")
#
#
# ##Merge our our new colums with the census.
#
# Census_merged <- cbind(new_vars,census_full)
# rm(new_vars,census_full)
#
#
# ##Create the AES variable to merge with the shp file.
#
# Census_merged$AES<- paste0(Census_merged$Province,Census_merged$District,Census_merged$Postal,
#                            Census_merged$Location,Census_merged$Neighbor,Census_merged$AE)
#
# ##Merge
#
# Maputo_merged_data <- merge(moz_shp,Census_merged, by="AES")
# rm(Census_merged,moz_shp)
#
#
#

##Work on the 10% census

##Unique ID creation. This ID does not match the shp file.
census_10$Cod_distrito<- as.character(census_10$Cod_distrito)
census_10$Cod_AF_Distrito <- sprintf("%014.0f",census_10$Cod_AF_Distrito)
census_10$AES<- paste0(census_10$Cod_distrito,census_10$Cod_AF_Distrito)

#Alternative ID to merge with the shp file
census_10$A2_PROVINCIA<- as.integer(census_10$A2_PROVINCIA)

new_vars<-apply(census_10[,c("A2_PROVINCIA","A3_DISTRITO","POSTO","LOCALIDADE",
                               "BAIRRO")],
                MARGIN = 2,FUN = ad_zero02)


new_vars<- as.data.frame(new_vars)

##Now we change the name of the columns


colnames(new_vars)<- c('Province',"District","Postal", "Location","Neighbor")


##Merge our our new col with the census.


census_10 <- cbind(new_vars,census_10)
rm(new_vars)

census_10$ID<- paste0(census_10$Province,census_10$District,census_10$Postal,
                      census_10$Location,census_10$Neighbor)

length(unique(census_10$ID))

##Subsetthe census
census_10<- as.data.table(census_10)
census_10<-census_10[,.(ID,TIPO_DE_TRABALHADOR,HP31,NIVEL_DE_ENSINO_FREQUENTADO,P24_NIVEL_DE_ENSINO_CONCLUIDO_1,
                        CURSO_SUPERIOR_CONCLUIDOX,CURSO_SUPERIOR_CONCLUIDO,TRABALHOU_NA_ULTIMA_SEMANA_JULHO,
                        PORQUE_NAO_TRABALHOU,TRABALHO_DOMESTICO,HORAS_TRABALHADAS_ULTIMA_SEMANA)]





##Create the same ID on the shp file
moz_shp$CodProv<- as.integer(moz_shp$CodProv)
moz_shp$CodDist<- as.integer(moz_shp$CodDist)
moz_shp$CodPost<- as.integer(moz_shp$CodPost)
moz_shp$CodLocal<- as.integer(moz_shp$CodLocal)
moz_shp$CodBairro<- as.integer(moz_shp$CodBairro)





##Know what distric code corresponds to what distric name
moz_shp<- as.data.table(moz_shp)
test<-unique(moz_shp[,c("CodDist", "Distrito")])

moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", CodDist := 24]

##Since Maputo is only a distric without a code for teh rest of sub divitions, I
##used the same system they used with different similar districs.

moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", CodPost := 99]
moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", Posto := "Nao Aplicavel"]
moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", CodBairro := 99]
moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", Bairro := "Nao Aplicavel"]
moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", CodLocal := 99]
moz_shp[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", Localidade := "Nao Aplicavel"]

test<-unique(moz_shp[,c("CodPost", "Posto", "CodDist", "Provincia", "CodBairro", "Bairro", "CodLocal", "Localidade","Distrito")]) ##we have two observations as NA


rm(test)



new_vars<-apply(moz_shp[,c("CodProv","CodDist","CodPost","CodLocal",
                             "CodBairro")],
                MARGIN = 2,FUN = ad_zero02)


new_vars<- as.data.frame(new_vars)

##Now we change the name of the columns

colnames(new_vars)<- c('Province',"District","Postal", "Location","Neighbor")


moz_shp1 <- cbind(new_vars,moz_shp)
rm(new_vars)

moz_shp$ID<- paste0(moz_shp1$Province,moz_shp1$District,moz_shp1$Postal,
                    moz_shp1$Location,moz_shp1$Neighbor)
rm(moz_shp1)

length(unique(moz_shp$ID))


####Create a shpfile at the barrio level
moz_shp<- as.data.table(moz_shp)

moz_shp<-moz_shp %>%
          group_by(ID) %>%
          summarize()

moz_shp1<- moz_shp
## Merge the data
moz_shp <- merge(census_10,moz_shp, by="ID")


moz_shp <- st_as_sf(moz_shp, agr="constant",crs=3974 )






## 1720 unique observations at the bairro level for the shp.

#1715 unique observations from the 10 census.

## 1705 unique observations on the merged shp




## None of these ones are currenlty working
write_sf(moz_shp, "//esapov/esapov/MOZ/GEO/Population/Merged_shp/borrar")

write_fst(moz_shp,"//esapov/esapov/MOZ/GEO/Population/Merged_shp/borrar")

moz<- read_fst("//esapov/esapov/MOZ/GEO/Population/Merged_shp/Moz_merged_shp_10.fst")

sf::st_write(obj = moz_shp, dsn = "//esapov/esapov/MOZ/GEO/Population/Merged_shp",
                                                          layer = "Moz_merged_shp_10",
                                                          driver = "ESRI Shapefile", append = FALSE)


