##A script to merge the grids with the cellphone tower data.
##First Load Packages

if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")

}
library(sf)
library(fst)
library(tidyverse)
library(dplyr)
library(data.table)
library(tmap)
library(leaflet)
sf_use_s2(FALSE)

##load moz grid file and the cellphone tower data

moz_grid<- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                       layer = "moz_poppoly_gridded") ##Grid with only 2 vars
moz_grid<- st_transform(moz_grid, crs= 4326)
moz_full_grid <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                      layer = "moz_poppoly_full_gridded")# Grid with all variables

moz_full_grid<-as.data.table(moz_full_grid)
moz_full_grid<- moz_full_grid[, c("poly_id","CodProv", "Provincia", "CodDist", "Distrito", "CodPost", "Posto",
                    "CodLocal", "Localidade", "CodBairro", "Bairro", "population")]#Keep only the relevant variables

bairro_dt<- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population",
                        layer = "neighborhood")##

CellTower_DT<- fread(file= "//esapov/esapov/ALL/Energy/cell_towers_2022-04-10-T000000.csv")
CellTower_DT<- as.data.frame(CellTower_DT)## Convert to data frame for the st_as_sf.


#Convert the CellTower_DT data to an sf object.
CellTower_DT<- st_as_sf(CellTower_DT, coords = c("lon","lat"), crs=4326)

#merge the grid to the cellphone tower data
CellTower_DT<- st_join(moz_grid,CellTower_DT) #merging the data

saveRDS(CellTower_DT,"//esapov/esapov/MOZ/GEO/Energy/temp/Joint_data_cell_temp.rds" )




#Convert to data table, create ID .
CellTower_DT <- as.data.table(CellTower_DT)
CellTower_DT$tower_ID <- seq_along(CellTower_DT[,1])##Create a tower ID with a value of 1



#Group by Poly ID and count the amount of towers per grid.
#CellTower_DT<-setDT(CellTower_DT[,lapply(.SD, sum), by = poly_id, .SDcols = "tower_count"])

CellTower_DT[,cellphonecount:= length(tower_ID), by=poly_id]# Creates a variable with the
#amount of towers per polygon.

##Merge the cellphone tower data with the full grid to get the geo identifiers.
##Get the mean of the cell phone count per kilometer in each neighborhood.
CellTower_DT<-  merge(CellTower_DT,moz_full_grid, by= "poly_id")
CellTower_DT<- CellTower_DT[, mean(cellphonecount), by=c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]


##Merge Bairro_dt into the CellTower_DT and name it bairro_dt
bairro_dt<- as.data.table(bairro_dt)
bairro_dt<-CellTower_DT[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]
bairro_dt<- st_as_sf(bairro_dt)



#Rename V1 to Tower_per_sqkm
bairro_dt<- as.data.frame(bairro_dt)
bairro_dt<-dplyr::rename(bairro_dt, Tower_per_sqkm=V1)# Rename the variable V1
bairro_dt<- st_as_sf(bairro_dt)

#Clean the environment
rm(CellTower_DT,moz_grid)

################################################################################
######################MERGE THE BUILDING DATA###################################
################################################################################


building_dt <- read.fst("//esapov/esapov/MOZ/GEO/BuildingFootprints/grid_bldstats.fst",
                        as.data.table=TRUE)

##Merge the building data data with the full census.
building_dt<- merge(building_dt,moz_full_grid, by="poly_id")




################################################################################
#Wouldn't erasing 0 population from the moz_shp grid risks that we are erasing
# areas from the census that do have people in it?
################################################################################

#Erase away population 0.
#temp_dt<- temp_td[!(temp_td$population==0),]


## Get the mean building data in each neighbourhood
building_dt<-setDT(building_dt[,lapply(.SD, mean),
                               by = c("CodProv", "CodDist", "CodPost", "CodLocal",
                                      "CodBairro"), .SDcols = c("poly_id" , "count",
                                       "cv_area" ,     "cv_length" ,   "density" ,
                                       "imagery_year", "mean_area","mean_length",
                                      "total_area", "total_length", "urban" ,"cell_area")])

#Merge the building data with the bairro_dt
bairro_dt<-as.data.table(bairro_dt)
bairro_dt<-building_dt[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]
bairro_dt<- st_as_sf(bairro_dt)

#Save the bairro_dt
write.fst(bairro_dt, path = "//esapov/esapov/MOZ/GEO/Population/Daylan/bairro.fst")















