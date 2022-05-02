## a script to extract building rasters

## conditional loading of packages to ensure installation if not present

# Package names
packages <- c("SAEplus", "raster", "sf", "data.table", "fst")

# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
#
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))

load_packages(packages)

sf_use_s2(FALSE)


### read in the building rasters all at once from the same MOZ folder
bldraster_list <- list.files("//esapov/esapov/MOZ/GEO/BuildingFootprints", ".tif")

### create raster names
indname_list <- sub("MOZ_buildings_v1_1_", "", bldraster_list)
indname_list <- sub(".tif", "", indname_list)


### extract all rasters at once
bldraster_list <- multraster_read(region = "esapov",
                                  iso_list = rep("MOZ", length(bldraster_list)),
                                  name_list = bldraster_list,
                                  folder_subpath = "GEO/BuildingFootprints")
names(bldraster_list) <- indname_list

bldraster_list$cell_area <- bldraster_list$count / bldraster_list$density

### extract each raster into the grid in parallel
###### first load the grid

grid_dt <- read_sf(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                   layer = "Moz_poppoly_gridded")



fun_list <- c("sum", "mean", "mean", "mean", "mean", "mean", "mean",
              "sum", "sum", "mean", "sum")
grid_dt <- st_as_sf(grid_dt, crs = 4326, agr = "constant")
gridbld_dt <- parallel_extract(shp_dt = grid_dt,
                               raster_list = bldraster_list,
                               fun_list = fun_list,
                               numCores = length(bldraster_list))

names(gridbld_dt) <- names(bldraster_list)

gridbld_dt <- do.call("cbind", gridbld_dt)
gridbld_dt <- as.data.table(gridbld_dt)
gridbld_dt <- cbind(poly_id = grid_dt$poly_id, gridbld_dt)

fst::write_fst(x = gridbld_dt,
               path = "//esapov/esapov/MOZ/GEO/BuildingFootprints/grid_bldstats.fst")
