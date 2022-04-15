###############################################################################
######################### GEO-SPATIAL MAPS  ###################################
###############################################################################

#Load all the libraries
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

################################################################################
################# Employment at the neighbourhood level ########################
################################################################################


#read in the data
tenpercent_dt<- read.fst("//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst",
                         columns = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro",
                                     "TRABALHO_DOMESTICO","TRABALHOU_NA_ULTIMA_SEMANA_JULHO",
                                     "TIPO_DE_TRABALHADOR" , "HORAS_TRABALHADAS_ULTIMA_SEMANA"),
                         as.data.table=TRUE)
bairro_dt<- readRDS("//esapov/esapov/MOZ/GEO/Population/Daylan/bairro.fst")

tenpercent_dt$wage_emp<-0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1,2,3,4,5,6,7,11), wage_emp:=1]
tenpercent_dt$wage_emp<- as.integer(tenpercent_dt$wage_emp)
tenpercent_dt$self_emp<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(9,10), self_emp:=1]
tenpercent_dt$self_emp<- as.integer(tenpercent_dt$self_emp)
tenpercent_dt$employed<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1:11), employed:=1]
tenpercent_dt$employed<- as.integer(tenpercent_dt$employed)



##Get the employment columns at neighborhood level
emp_dt<-setDT(tenpercent_dt[,lapply(.SD, sum),
                               by = c("CodProv", "CodDist", "CodPost", "CodLocal",
                                      "CodBairro"), .SDcols = c("wage_emp","self_emp","employed")])

emp_dt<-emp_dt[bairro_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]




#Create the share of self employment and wage worker
emp_dt$self_emp_share <-emp_dt$self_emp/emp_dt$employed
emp_dt$wage_emp_share <-emp_dt$wage_emp/emp_dt$employed

emp_dt<- st_as_sf(emp_dt)

#Save the Employment file as an RDS in temporary folder
saveRDS(emp_dt,"//esapov/esapov/MOZ/GEO/Population/Daylan/emp.rds")



##Plot the Geo-spatial maps


# Share of self and wage employment
tmap::tmap_mode(mode=c("plot","view"))+
  tmap::tm_style("cobalt")
  tmap::tm_shape(emp_dt)+
  tmap::tm_polygons()+
  tmap::tm_symbols(col = "blue", border.col = "white", size = "self_emp_share")+
    tmap::tm_shape(emp_dt)+
  tmap::tm_symbols(col = "red", border.col = "white",  size = "wage_emp_share")


#Share of self/wage employment maps side by side
tmap::tmap_mode("view")+
tmap::tm_shape(emp_dt) +
  tmap::tm_style("cobalt")+
  tmap::tm_polygons(c("self_emp_share","wage_emp_share"))


#Cell phone towers per sqkm in each neighborhood.
tmap::tmap_mode(mode=c("plot"))+
  tmap::tm_shape(emp_dt)+
  tmap::tm_style("col_blind")+
  tmap::tm_polygons()+
  tm_bubbles(size = "Tower_per_sqkm", col = "red")



##Cell phone towers per sqkm in each neighborhood.
  tmap::tmap_mode(mode=c("view"))+
  tmap::tm_shape(emp_dt)+
    tmap::tm_polygons()+
    tm_basemap(server = 'https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png', group ="Stadia.AlidadeSmoothDark", alpha = NA, tms = FALSE)+
    tm_tiles('https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png', group ="Stadia.AlidadeSmoothDark", alpha = 1, zindex = NA, tms = FALSE)+
    tm_bubbles(size = "Tower_per_sqkm", col = "red")



