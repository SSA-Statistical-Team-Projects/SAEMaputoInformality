### a script for merging the 10% census data with the shapefile

if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")

}

packages <- c("sf", "data.table", "fst", "stringi", "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



minicensus_dt <- read_fst(path = "//esapov/esapov/MOZ/GEO/Population/tenpercent_census.fst",
                          as.data.table = TRUE)


## reading in the shapefile
shp_dt <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                      layer = "BASE_COMPLETA_DE_AE_CENSO_2017")
shp_dt <- as.data.table(shp_dt)

## perform a merge based on province, district, postal code, localidade,bairro/neighborhood

### a quick function that might be useful for string cleaning of the area names
keep_lettersonly <- function(X){

  y <- stri_trans_general(X, "Latin-ASCII") ##remove all accents
  y <- tolower(y)  ##make sure all characters are lower case
  y <- gsub(" ", "", y) ##remove all spaces
  y <- gsub("[[:punct:]]", "", y) ##remove random text

  return(y)
}

### create matching province codes from the shapefile
add_dt <- unique(shp_dt[,c("CodProv", "Provincia")])
minicensus_dt[,CodProv := as.integer(A2_PROVINCIA)]
minicensus_dt[,CodProv := sprintf("%02d", CodProv)]

add_dt[minicensus_dt, on = "CodProv"] ##test to see if the merge works and it does

### create matching district codes with shapefile
add_dt <- unique(shp_dt[,c("CodProv", "CodDist")])
minicensus_dt[,CodDist := sprintf("%02d", A3_DISTRITO)]

add_dt[minicensus_dt, on = c("CodProv", "CodDist")] ##test to see if the merge works and it does

### create matching postal codes with the shapefile
add_dt <- unique(shp_dt[,c("CodProv", "CodDist", "CodPost")])
minicensus_dt[,CodPost := sprintf("%02d", POSTO)]

add_dt[minicensus_dt, on = c("CodProv", "CodDist", "CodPost")] ##test to see if the merge works and it does

### create a matching localidade codes with the shapefile
add_dt <- unique(shp_dt[,c("CodProv", "CodDist", "CodPost", "CodLocal")])
minicensus_dt[,CodLocal := sprintf("%02d", LOCALIDADE)]

add_dt[minicensus_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal")] ##test to see if the merge works and it does

### create a matching neighborhood (bairro) code with the shapefile
add_dt <- unique(shp_dt[,c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")])
minicensus_dt[,CodBairro := sprintf("%02d", BAIRRO)]

### merge shapefile with cenus at bairro level
merge_ids <- c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")

shp_dt <- st_as_sf(shp_dt, crs = 4326, agr = "constant")

sf_use_s2(FALSE)
union_shp <- shp_dt %>%
    group_by(CodProv, Provincia, CodDist, Distrito, CodPost, Posto, CodLocal, Localidade, CodBairro, Bairro) %>%
    summarize()

write_sf(union_shp,
         dsn = "//esapov/esapov/MOZ/GEO/Population",
         layer = "neighborhood_shp",
         driver = "ESRI Shapefile",
         append = FALSE)

## create matching household ID
minicensus_dt[,hhid := paste0(as.character(Cod_distrito),
                              as.character(Cod_AF_Distrito))]

minicensus_dt[,pid := 1:.N, by = "hhid"]
minicensus_dt[,pid := sprintf("%02d", pid)]

minicensus_dt[,pid := paste0(hhid, pid)]

minicensus_dt <- write_fst(minicensus_dt,
                           path = "//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst")








