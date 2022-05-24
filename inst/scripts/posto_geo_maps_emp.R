###############################################################################
######################### GEO-SPATIAL MAPS  ###################################
###############################################################################

#Load all the libraries
if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")
}


packages <- c("sf", "fst", "tidyverse", "dplyr",
              "data.table", "tmap", "leaflet", "questionr",
              "RColorBrewer")
devtools::load_all()
load_packages(packages)

sf_use_s2(FALSE)

################################################################################
################# Employment at the POSTO level ################################
################################################################################


#read in the data
tenpercent_dt<- read.fst("//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst",
                         columns = c("CodProv", "CodDist", "CodPost",
                                     "TRABALHO_DOMESTICO","TRABALHOU_NA_ULTIMA_SEMANA_JULHO",
                                     "TIPO_DE_TRABALHADOR" , "HORAS_TRABALHADAS_ULTIMA_SEMANA", "educat5"),
                         as.data.table=TRUE)

tenpercent_dt$URB_RUR<- as.integer(tenpercent_dt$URB_RUR)

posto_dt<- readRDS("inst/extdata/bldstats_celltower_posto_level.RDS")

tenpercent_dt[,empclass := ifelse(TIPO_DE_TRABALHADOR %in% c(1:7, 11), "wageworker",
                                  ifelse(TIPO_DE_TRABALHADOR %in% 9:10, "selfemployed", "other"))]

le_class <- c("No Education", "Primary complete but Secondary incomplete",
              "Primary incomplete")

he_class <- c("Secondary complete", "Tertiary/post-secondary (complete or incomplete)")


tenpercent_dt[empclass == "selfemployed" &
                educat5 %in% le_class , empclass := "loweduc_se"]

tenpercent_dt[empclass == "selfemployed" &
                educat5 %in% he_class , empclass := "higheduc_se"]

tenpercent_dt[empclass == "selfemployed" &
                is.na(educat5), empclass := "unknowneduc_se"]

tenpercent_dt[is.na(TIPO_DE_TRABALHADOR), empclass := NA]

emp_dt <-
tenpercent_dt[,as.data.table(prop.table(table(empclass))), by = c("CodProv", "CodDist", "CodPost")]

emp_dt <- dcast(emp_dt, CodProv + CodDist + CodPost ~ empclass, value.var = "N")

emp_dt <- emp_dt[posto_dt, on = c("CodProv", "CodDist", "CodPost")]

emp_dt<- st_as_sf(emp_dt, crs = 4326, agr = "constant")

#Save the Employment file as an RDS in temporary folder
saveRDS(emp_dt,"//esapov/esapov/MOZ/GEO/Population/Daylan/emp_posto.rds")



display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)

##create faceted maps
facet_dt <- emp_dt[,c("CodProv", "CodDist", "CodPost", "higheduc_se", "loweduc_se", "other",
                      "unknowneduc_se", "wageworker", "geometry")]

facet_dt <- as.data.table(facet_dt)
facet_dt <-
melt(facet_dt, id.vars = c("CodProv", "CodDist", "CodPost", "geometry"),
     measure.vars = c("higheduc_se", "loweduc_se", "other", "unknowneduc_se", "wageworker"))

facet_dt <- st_as_sf(facet_dt, crs = 4326, agr = "constant")

tmap::tmap_mode("view")

fullempshare_tmap <-
  tm_shape(facet_dt) +
  tm_fill(col = "value",
          title = "Employment Type Distribution", id = "CodPost",
          breaks = seq(0, 1, 0.1),
          popup.vars = c("Name" = "CodPost", "Employment Type" = "variable")) +
  tm_borders() +
  tm_facets("variable")

tmap_save(fullempshare_tmap, "inst/plots/posto_urbanemptype.html")

## add the nighttimelights data
shp_dt <- read_sf(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                  layer = "Moz_poppoly_full_gridded")
shp_dt <- as.data.table(shp_dt)
shp_dt <- shp_dt[,c("CodProv", "CodDist", "CodPost", "poly_id", "population")]

ntl_dt <- readRDS("//esapov/esapov/MOZ/GEO/GEE/mozmap_process/ntl_dt.RDS")
ntl_dt <- shp_dt[ntl_dt, on = "poly_id"]

ntl_dt <- ntl_dt[population > 0,]
ntl_dt <- ntl_dt[,c("CodProv", "CodDist", "CodPost", "mean_ntl", "min_ntl",
                    "max_ntl", "median_ntl", "sd_ntl", "lowlightrate_ntl")]

ntl_cols <- c("mean_ntl", "min_ntl", "max_ntl", "median_ntl", "sd_ntl",
              "lowlightrate_ntl")

ntl_dt <- ntl_dt[,lapply(.SD, mean), by = c("CodProv", "CodDist", "CodPost")]

emp_dt <- as.data.table(emp_dt)
emp_dt <- ntl_dt[emp_dt, on = c("CodProv", "CodDist", "CodPost")]


### compute correlation matrix
cor_vars <- colnames(emp_dt)[!colnames(emp_dt) %in% c("CodProv", "CodDist",
                                                      "CodPost", "geometry",
                                                      "poly_id")]
cor_dt <- as.data.table(emp_dt)

cor_dt <- cor_dt[,cor_vars, with = F]

cor_dt <- na.omit(cor_dt)

write.csv(cor_dt[,cor(.SD)],
          "inst/extdata/correlationmatrix.csv")




































