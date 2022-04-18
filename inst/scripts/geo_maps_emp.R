###############################################################################
######################### GEO-SPATIAL MAPS  ###################################
###############################################################################

#Load all the libraries
if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")
}


packages <- c("sf", "fst", "tidyverse", "dplyr",
              "data.table", "tmap", "leaflet")
devtools::load_all()
load_packages(packages)

sf_use_s2(FALSE)

################################################################################
################# Employment at the neighbourhood level ########################
################################################################################


#read in the data
tenpercent_dt<- read.fst("//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst",
                         columns = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro",
                                     "TRABALHO_DOMESTICO","TRABALHOU_NA_ULTIMA_SEMANA_JULHO",
                                     "TIPO_DE_TRABALHADOR" , "HORAS_TRABALHADAS_ULTIMA_SEMANA"),
                         as.data.table=TRUE)

bairro_dt <- readRDS("inst/extdata/bldstats_celltower_bairrolevel.RDS")

tenpercent_dt$wage_emp<-0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1,2,3,4,5,6,7,11), wage_emp:=1]
tenpercent_dt$wage_emp<- as.integer(tenpercent_dt$wage_emp)
tenpercent_dt$self_emp<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(9,10), self_emp:=1]
tenpercent_dt$self_emp<- as.integer(tenpercent_dt$self_emp)
tenpercent_dt$employed<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1:11, 99), employed := 1]
tenpercent_dt$employed<- as.integer(tenpercent_dt$employed)



##Get the employment columns at neighborhood level
emp_dt<-setDT(tenpercent_dt[,lapply(.SD, sum),
                               by = c("CodProv", "CodDist", "CodPost", "CodLocal",
                                      "CodBairro"), .SDcols = c("wage_emp","self_emp","employed")])

bairro_dt <- as.data.table(bairro_dt)
emp_dt <- emp_dt[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]




#Create the share of self employment and wage worker
emp_dt$self_emp_share <-emp_dt$self_emp/emp_dt$employed
emp_dt$wage_emp_share <-emp_dt$wage_emp/emp_dt$employed

emp_dt<- st_as_sf(emp_dt, crs = 4326, agr = "constant")


##Plot the Geo-spatial maps
#See palettes avaiable
display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)


  #Share of self-employment map
  self_emp_share<- tmap::tmap_mode("view")+
    tmap::tm_shape(emp_dt) +
    tmap::tm_fill("self_emp_share",
                  title="Share of self-employment",palette="-Spectral",
                  id="Bairro", breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1.0),
                  popup.vars=c("Name"="Bairro", "Share"="self_emp_share"))+
    tmap::tm_borders()
  tmap_save(self_emp_share, "self_emp_share.html")
  rm(self_emp_share)

  #Share of wage-employment map
  wage_emp_share<- tmap::tmap_mode("view")+
    tmap::tm_shape(emp_dt) +
    tmap::tm_fill("wage_emp_share",
                  title="Share of wage employment",palette="-Spectral",
                  id="Bairro", breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1.0),
                  popup.vars=c("Name"="Bairro", "Share"="wage_emp_share"))+
    tmap::tm_borders()
  tmap_save(wage_emp_share, "wage_emp_share.html")
  rm(wage_emp_share)




#Share of self/wage employment maps side by side
tmap::tmap_mode("view")+
tmap::tm_shape(emp_dt) +
  tmap::tm_style("cobalt")+
  tmap::tm_polygons(c("self_emp_share","wage_emp_share"))



##Cell phone towers per sqkm in each neighborhood.
tmap::tmap_mode("view")+
  tmap::tm_shape(emp_dt) +
  tmap::tm_borders()+
  tmap::tm_bubbles("Tower_per_sqkm", col = "firebrick2",
                   palette="-RdYlBu", contrast=1,
                   title.size="Tower_per_sqkm",
                   title.col="Cellphone towers per sqkm", id="Bairro")









