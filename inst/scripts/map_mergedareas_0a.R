
packages(c("data.table", "tidyverse"))

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

### read in the shapefile for MOZ
sf::sf_use_s2(FALSE)
shp_dt <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                      layer = "BASE_COMPLETA_DE_AE_CENSO_2017")

### we need to plot the primary, secondary and tertiary cities
##Create the same ID on the shp file
ad_zero02<- function(X){
  return(sprintf("%03d",X))
}

shp_dt$CodProv<- as.integer(shp_dt$CodProv)
shp_dt$CodDist<- as.integer(shp_dt$CodDist)
shp_dt$CodPost<- as.integer(shp_dt$CodPost)
shp_dt$CodLocal<- as.integer(shp_dt$CodLocal)
shp_dt$CodBairro<- as.integer(shp_dt$CodBairro)





##Know what distric code corresponds to what distric name
shp_dt<- as.data.table(shp_dt)

shp_dt[Distrito == "AEROPORTO INTERNACIONAL DE MAPUTO", CodDist := 24]

test<-unique(shp_dt[,c("CodDist", "Distrito")])
rm(test)

new_vars<-apply(shp_dt[,c("CodProv","CodDist","CodPost","CodLocal",
                           "CodBairro")],
                MARGIN = 2,FUN = ad_zero02)


new_vars<- as.data.frame(new_vars)

##Now we change the name of the columns

colnames(new_vars)<- c('Province',"District","Postal", "Location","Neighbor")


shp_dt1 <- cbind(new_vars,shp_dt)
rm(new_vars)

shp_dt$ID<- paste0(shp_dt1$Province,shp_dt1$District,shp_dt1$Postal,
                    shp_dt1$Location,shp_dt1$Neighbor)

shp_dt <- sf::st_as_sf(shp_dt, crs = 4326, agr = "constant")

##extract population into the shapefile
moz_raster <- raster::raster("//esapov/esapov/MOZ/GEO/Population/moz_ppp_2020_1km_Aggregated_UNadj.tiff")

shp_dt$population <- exactextractr::exact_extract(moz_raster, shp_dt)

##### create plot at the primary level
bairro_dt <- shp_dt %>%
  group_by(ID) %>%
  summarize()

tmap::tmap_mode("view")
tmap::tm_shape(bairro_dt) +
  tmap::tm_polygons()

province_dt <- shp_dt %>%
  group_by(Provincia) %>%
  summarize()
tmap::tmap_mode("view")
tmap::tm_shape(province_dt) +
  tmap::tm_polygons()

district_dt <- shp_dt %>%
  group_by(Distrito) %>%
  summarize()

tmap::tmap_mode("view")
tmap::tm_shape(district_dt) +
  tmap::tm_polygons()










