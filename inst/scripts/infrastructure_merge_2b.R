###################################################################################################

##A script to merge the grids with the cellphone tower data.
##First Load Packages

if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")

}

packages <- c("sf", "fst", "tidyverse", "dplyr",
              "data.table", "tmap", "leaflet")


load_packages(packages)

sf_use_s2(FALSE)

#Load MOZ grid file and the cellphone tower data

moz_grid<- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                       layer = "moz_poppoly_gridded") ##Grid with only 2 vars

moz_grid<- st_transform(moz_grid, crs= 4326)

moz_full_grid <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                      layer = "moz_poppoly_full_gridded")# Grid with all variables

moz_full_grid<-as.data.table(moz_full_grid)
moz_full_grid<- moz_full_grid[, c("poly_id","CodProv", "Provincia", "CodDist", "Distrito",
                                  "CodPost", "Posto", "CodLocal", "Localidade", "CodBairro",
                                  "Bairro", "population")]#Keep only the relevant variables

bairro_dt<- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population",
                        layer = "neighborhood") ##read in the neighhorhood shapefile

cell_tower_dt<- fread(file= "//esapov/esapov/ALL/Energy/cell_towers_2022-04-10-T000000.csv")
cell_tower_dt<- as.data.frame(cell_tower_dt)## Convert to data frame for the st_as_sf.


#Convert the cell_tower_dt data to an sf object.
cell_tower_dt<- st_as_sf(cell_tower_dt, coords = c("lon","lat"), crs = 4326)

#merge the grid to the cellphone tower data
cell_tower_dt<- st_join(moz_grid, cell_tower_dt) #merging the data

#saveRDS(cell_tower_dt,"//esapov/esapov/MOZ/GEO/Energy/temp/Joint_data_cell_temp.rds" )

#Convert to data table, create ID .
cell_tower_dt <- as.data.table(cell_tower_dt)
cell_tower_dt$tower_ID <- seq_along(cell_tower_dt[, 1])##Create a tower ID with a value of 1



#Group by Poly ID and count the amount of towers per grid.
cell_tower_dt[,cellphonecount := length(tower_ID), by=poly_id]# Creates a variable with the
#amount of towers per polygon.

##Merge the cellphone tower data with the full grid to get the geo identifiers.
##Get the mean of the cell phone count per kilometer in each neighborhood.
cell_tower_dt<-  merge(cell_tower_dt, moz_full_grid, by = "poly_id")
cell_tower_dt<- cell_tower_dt[, mean(cellphonecount), by = c("CodProv", "CodDist", "CodPost",
                                                           "CodLocal", "CodBairro")]
##Merge Bairro_dt into the cell_tower_dt and name it bairro_dt
bairro_dt<- as.data.table(bairro_dt)
bairro_dt<- cell_tower_dt[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal",
                                           "CodBairro")]
# bairro_dt<- st_as_sf(bairro_dt, crs = 4326, agr = "constant")

# #Rename V1 to Tower_per_sqkm
# bairro_dt<- as.data.frame(bairro_dt)
bairro_dt<- dplyr::rename(bairro_dt, Tower_per_sqkm = V1)# Rename the variable V1
bairro_dt<- st_as_sf(bairro_dt, crs = 4326, agr = "constant")

#Clean the environment
rm(cell_tower_dt,moz_grid)

################################################################################
##################### MERGE THE BUILDING DATA ##################################
################################################################################


building_dt <- read.fst("//esapov/esapov/MOZ/GEO/BuildingFootprints/grid_bldstats.fst",
                        as.data.table=TRUE)

##Merge the building data data with the full census.
building_dt<- merge(building_dt, moz_full_grid, by = "poly_id")


#Erase away population 0.
building_dt<- building_dt[!(building_dt$population==0),]


## Get the mean building data in each neighbourhood
building_dt<-setDT(building_dt[,lapply(.SD, mean),
                               by = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro"),
                               .SDcols = c("poly_id", "count", "cv_area" , "cv_length" ,
                                           "density", "imagery_year", "mean_area","mean_length",
                                           "total_area", "total_length", "urban" ,"cell_area")])

#Merge the building data with the bairro_dt
bairro_dt<- as.data.table(bairro_dt)
bairro_dt<- building_dt[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]
bairro_dt<- st_as_sf(bairro_dt, crs = 4326, agr = "constant")


# write_sf(bairro_dt,
#          dsn = "inst/extdata",
#          layer = "bldstats_celltower_bairrolevel",
#          driver = "ESRI Shapefile",
#          append = FALSE)

saveRDS(bairro_dt, "inst/extdata/bldstats_celltower_bairrolevel.RDS")











