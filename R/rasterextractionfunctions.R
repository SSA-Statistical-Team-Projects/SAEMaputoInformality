#' Function to extract rasters
#'
parallel_extract <- function(shp_dt,
                             raster_list,
                             fun_list,
                             numCores){

  ##initiating the parallelization process
  numCores <- min(numCores, parallel::detectCores()) ##use the minimum of the specified processors or the max

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used
  parallelMap::parallelLibrary("foreach") ##loading the parallel looping library
  parallelMap::parallelLibrary("exactextractr") ##loading the parallel looping library

  ##the parallelization process
  grid_list <-
    foreach (i = 1:length(raster_list)) %dopar% {

      exactextractr::exact_extract(x = raster_list[[i]],
                                   y = shp_dt,
                                   fun = fun_list[i])

    }


  return(grid_list)

}
