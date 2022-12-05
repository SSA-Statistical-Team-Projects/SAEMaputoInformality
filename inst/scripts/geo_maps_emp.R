###############################################################################
######################### GEO-SPATIAL MAPS  ###################################
###############################################################################

#Load all the libraries
if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")
}


packages <- c("sf", "fst", "tidyverse", "dplyr",
              "data.table", "tmap", "leaflet", "questionr", "Hmisc",
              "corrr", "ggbiplot", "lmtest", "sandwich", "ggspatial")
# devtools::load_all()
load_packages(packages)

sf_use_s2(FALSE)

################################################################################
################# Employment at the neighborhood level ########################
################################################################################


#read in the data
tenpercent_dt<- read.fst("//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst",
                         columns = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro",
                                     "TRABALHO_DOMESTICO","TRABALHOU_NA_ULTIMA_SEMANA_JULHO",
                                     "TIPO_DE_TRABALHADOR" , "HORAS_TRABALHADAS_ULTIMA_SEMANA", "URB_RUR",
                                     "hhid", "Educat", "educat7", "educat5", "educat4", "Ponderador_AF",
                                     "Ponderador_Pessoa", "TEM_TELEFONE_CELULAR",
                                     "TEM_CONTA_BANCARIA", "BENSX_E", "USOU_INTERNET_ULTIMOS_3_MESES",
                                     "USA_MPESA_MKESH", "BENSX_A", "BENSX_B", "BENSX_C", "BENSX_D",
                                     "BENSX_E", "BENSX_F", "BENSX_G", "BENSX_H", "BENSX_I", "BENSX_J",
                                     "BENSX_K", "D4_PROPRIEDADE_DA_CASA"),
                         as.data.table=TRUE)
tenpercent_dt$URB_RUR<- as.integer(tenpercent_dt$URB_RUR)

bairro_dt <- readRDS("inst/extdata/bldstats_celltower_bairrolevel.RDS")

## lets rename the type of work variable levels
## Industry
levels(tenpercent_dt$TIPO_DE_TRABALHADOR)

tenpercent_dt$industry <-NA
tenpercent_dt$industry<- as.character(tenpercent_dt$industry)
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 1, industry:="Public administration"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 2, industry:= "Local authority worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 3, industry:= "Public enterprise worker"]


tenpercent_dt[TIPO_DE_TRABALHADOR %in% 4, industry:= "Private enterprise worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 5, industry:= "Cooperative worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 6, industry:= "NGO worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 7, industry:= "Private house worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 8, industry:= "Self employed with employees"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 9, industry:= "Self employed without employees"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 10, industry:= "Family employee without remuneration"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 11, industry:= "International organism worker"]
tenpercent_dt[TIPO_DE_TRABALHADOR %in% 99, industry:= NA]



## Generate the employment variables.
tenpercent_dt$wage_emp<-0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1,2,3,4,6,7,11), wage_emp:=1]
tenpercent_dt$wage_emp<- as.integer(tenpercent_dt$wage_emp)
tenpercent_dt$self_emp<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(5,9), self_emp:=1]
tenpercent_dt$self_emp<- as.integer(tenpercent_dt$self_emp)
tenpercent_dt$employed<- 0
tenpercent_dt[TIPO_DE_TRABALHADOR %in% c(1:11, 99), employed := 1]
tenpercent_dt$employed<- as.integer(tenpercent_dt$employed)
tenpercent_dt$employer <- 0
tenpercent_dt[TIPO_DE_TRABALHADOR == 8, employer := 1]
tenpercent_dt[,employer := as.integer(employer)]
tenpercent_dt$unpaidfamilyworker <- 0
tenpercent_dt[TIPO_DE_TRABALHADOR == 10, unpaidfamilyworker := 1]
tenpercent_dt[,unpaidfamilyworker := as.integer(unpaidfamilyworker)]

# ### Change the values from rural areas to 0.
# tenpercent_dt[URB_RUR %in% 2, wage_emp:=0]
# tenpercent_dt[URB_RUR %in% 2, self_emp:=0]
# tenpercent_dt[URB_RUR %in% 2, employed:=0]


### generate one employment class variable containing self employed, unpaid family workers, employers and wage workers
tenpercent_dt[,empclass := ifelse(wage_emp == 1, "wage",
                                  ifelse(self_emp == 1, "selfemp",
                                         ifelse(employer == 1, "employer",
                                                ifelse(unpaidfamilyworker == 1, "unpaidfam", NA))))]

### include educational attainment
le_class <- c("No Education", "Primary complete but Secondary incomplete",
              "Primary incomplete")

he_class <- c("Secondary complete", "Tertiary/post-secondary (complete or incomplete)")

tenpercent_dt[,educlevel := ifelse(educat5 %in% le_class, "loweduc",
                                   ifelse(educat5 %in% he_class, "higheduc", "unknown"))]

tenpercent_dt[,informal_type := paste(educlevel, empclass, sep = "-")]

### drop the unknown and NA educ and emptype classes as requested and keep only unpaid family workers
tenpercent_dt[grepl("unpaid", informal_type), informal_type := "unpaidfam"]
tenpercent_dt[grepl("employer", informal_type), informal_type := "employer"]

tenpercent_dt[grepl("NA", informal_type), informal_type := NA]
tenpercent_dt[grepl("unknown", informal_type), informal_type := NA]
### compute the rates of employment by type
tenpercent_dt[,weight_p := Ponderador_Pessoa]
tenpercent_dt[,weight_h := Ponderador_AF]
# informal_dt <-
# tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
#               as.data.table(prop.table(wtd.table(informal_type,
#                                                  weights = weight_p))),
#               by = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]

informal_dt <-
tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE &
                !grepl("NA", informal_type) & !grepl("unknown", informal_type) &
                is.na(informal_type) == FALSE,
              .(count = sum(weight_p)),
              by = c("CodProv", "CodDist",
                     "CodPost", "CodLocal",
                     "CodBairro", "informal_type")][,rate := count / sum(count),
                                   by = c("CodProv", "CodDist",
                                          "CodPost", "CodLocal",
                                          "CodBairro")][]


### merge in the bairro_dt information
add_dt <- as.data.table(bairro_dt[,c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro",
                                     "Provincia", "Distrito", "Posto", "Localidade", "Bairro",
                                     "geometry")])
informal_dt <- add_dt[informal_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]

## include count since we forgot that
# informal_dt <- tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
#                              round(sum(weight_p)),
#                              by = c("CodProv", "CodDist", "CodPost",
#                                     "CodLocal", "CodBairro", "informal_type")][informal_dt,
#                                                                                on = c("CodProv", "CodDist", "CodPost",
#                                                                                       "CodLocal", "CodBairro", "informal_type")]
#
# setnames(informal_dt, "V1", "count")
#

### make some pretty plots
informal_dt <- st_as_sf(informal_dt, crs = 4326, agr = "constant")

## plot low educated self employed
plota <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Low Educated Self Employed Workers")
ggsave(filename = "inst/plots/loweduc_selfemp_national_rate.png")

plotb <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of Low Educated Self Employed Workers")
ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_national_count.png")

## plot high educated self employed
plotc <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of High Educated Self Employed Workers")
ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_national_rate.png")

plotd <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of High Educated Self Employed Workers")
ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_national_count.png")

## plot low educated wage workers
plote <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Low Educated Wage Workers")
ggsave(filename = "inst/plots/informalplots/loweduc_wage_national_rate.png")

plotf <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of Low Educated Wage Workers")
ggsave(filename = "inst/plots/informalplots/loweduc_wage_national_count.png")

## plot high educated wage workers
plotg <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of high Educated Wage Workers")
ggsave(filename = "inst/plots/informalplots/higheduc_wage_national_rate.png")

ploth <-
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of high Educated Wage Workers")
ggsave(filename = "inst/plots/informalplots/higheduc_wage_national_count.png")

## plot wage and self employment for those with unknown education
ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Wage Workers with \n Unknown Educational Attainment") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "inst/plots/informalplots/unknown_wage_national_rate.png")

ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of Wage Workers with \n Unknown Educational Attainment") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "inst/plots/informalplots/unknown_wage_national_count.png")

ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp",],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Self Employed Workers with \n Unknown Educational Attainment") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "inst/plots/informalplots/unknown_selfemp_national_rate.png")

ggplot() +
  geom_sf(data = bairro_dt) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp",],
          aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Distribution of Self Employed Workers with \n Unknown Educational Attainment") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "inst/plots/informalplots/unknown_selfemp_national_count.png")


#### create same plots for specific regions of interest
city_list <- c("Chimoio", "Cidade da Beira", "Quelimane", "Nampula")

## in chimoio
ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Chimoio City")
ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_chimoio_rate.png")

ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of High Educated Self Employed Workers in Chimoio City")
ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_chimoio_rate.png")

ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Self Employed Workers with \n unknown educational attainment in Chimoio City") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "inst/plots/informalplots/unknown_selfemp_chimoio_rate.png")

ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Chimoio City")
ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_chimoio_rate.png")

ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of High Educated Self Employed Workers in Chimoio City")
ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_chimoio_rate.png")

ggplot() +
  geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[1],]) +
  geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage" &
                               informal_dt$Distrito %in% city_list[1],],
          aes(fill = rate)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("Spatial Concentration of Wage Workers with \n unknown educational attainment in Chimoio City") +
  theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_chimoio_rate.png")

##in cidade de beira
  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Beira City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_beira_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Beira City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_beira_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Self Employed Workers with \n unknown educational attainment in Beira City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_beira_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Beira City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_beira_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Beira City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_beira_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[2],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage" &
                                 informal_dt$Distrito %in% city_list[2],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Wage Workers with \n unknown educational attainment in Beira City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_beira_rate.png")

## quelimane city
  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Quelimane City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_quelimane_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Quelimane City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_quelimane_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Self Employed Workers with \n unknown educational attainment in Quelimane City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_quelimane_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Quelimane City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_quelimane_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Quelimane City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_quelimane_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[3],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage" &
                                 informal_dt$Distrito %in% city_list[3],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Wage Workers with \n unknown educational attainment in Quelimane City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_quelimane_rate.png")


## in nampula
  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Nampula City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_Nampula_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-selfemp" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Nampula City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_Nampula_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-selfemp" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Self Employed Workers with \n unknown educational attainment in Nampula City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_Nampula_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "loweduc-wage" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Low Educated Self Employed Workers in Nampula City")
  ggsave(filename = "inst/plots/informalplots/loweduc_selfemp_Nampula_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "higheduc-wage" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of High Educated Self Employed Workers in Nampula City")
  ggsave(filename = "inst/plots/informalplots/higheduc_selfemp_Nampula_rate.png")

  ggplot() +
    geom_sf(data = bairro_dt[bairro_dt$Distrito %in% city_list[4],]) +
    geom_sf(data = informal_dt[informal_dt$informal_type == "unknown-wage" &
                                 informal_dt$Distrito %in% city_list[4],],
            aes(fill = rate)) +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle("Spatial Concentration of Wage Workers with \n unknown educational attainment in Nampula City") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = "inst/plots/informalplots/unknown_selfemp_Nampula_rate.png")

  #### histograms of distribution of share of informal workers by city
  informal_dt$informal_name[informal_dt$informal_type == "employer"] <- "Employer"
  informal_dt$informal_name[informal_dt$informal_type == "higheduc-selfemp"] <- "High-Educated Self-Employed Worker"
  informal_dt$informal_name[informal_dt$informal_type == "loweduc-selfemp"] <- "Low-Educated Self-Employed Worker"
  informal_dt$informal_name[informal_dt$informal_type == "higheduc-wage"] <- "High-Educated Wage Worker"
  informal_dt$informal_name[informal_dt$informal_type == "loweduc-wage"] <- "Low-Educated Wage Worker"
  informal_dt$informal_name[informal_dt$informal_type == "unpaidfam"] <- "Unpaid Family Worker"

  ggplot(informal_dt) +
    geom_histogram(aes(x = rate), bins = 100) +
    facet_wrap(~informal_name)
  ggsave(filename = "inst/plots/informalplots/hist_informalrate_national.png")

  # hist_dt <-
  # tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
  #               as.data.table(prop.table(wtd.table(informal_type,
  #                                                  weights = Ponderador_Pessoa))),
  #               by = c("CodProv", "CodDist")]
  #
  # bairro_dt <- as.data.table(bairro_dt)
  #
  # district_dt <- unique(bairro_dt[,c("CodProv", "CodDist", "Provincia", "Distrito")])
  #
  # hist_dt <- district_dt[hist_dt, on = c("CodProv", "CodDist")]
  #
  # setnames(hist_dt, c("V1", "N"), c("informal_type", "rate"))

  # ###### include maputo cidade within the information
  # maputo_dt <-
  #   tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
  #                 as.data.table(prop.table(wtd.table(informal_type,
  #                                                    weights = Ponderador_Pessoa))),
  #                 by = c("CodProv")]
  #
  # province_dt <- unique(bairro_dt[,c("CodProv", "Provincia")])
  #
  # maputo_dt <- province_dt[maputo_dt, on = "CodProv"]
  #
  # setnames(maputo_dt, c("V1", "N"), c("informal_type", "rate"))
  #
  # maputo_dt <- maputo_dt[CodProv == 11,]
  # maputo_dt[,c("CodDist", "Distrito") := list(NA, "Maputo Cidade")]
  #
  # maputo_dt <- maputo_dt[,colnames(hist_dt),with=F]
  #
  # dt <- rbind(hist_dt[Distrito %in% city_list,], maputo_dt)
  #
  # setnames(dt, "informal_type", "education-employment")
  # ggplot(dt, aes(x = `education-employment`, y = rate, fill = `education-employment`)) +
  #   geom_bar(stat="identity") +
  #   facet_wrap(~Distrito) +
  #   theme(axis.text.x = element_text(angle = 90, hjust=1))
  # ggsave("inst/plots/informalplots/hist_educemptype_selectcities.png")

  ### compute



  ##Check the levels.
  table(tenpercent_dt$TEM_TELEFONE_CELULAR)
  #create educat7
  tenpercent_dt$cell<- as.integer(tenpercent_dt$TEM_TELEFONE_CELULAR)
  tenpercent_dt$cellphone<-NA
  tenpercent_dt$cellphone<- as.character(tenpercent_dt$cellphone)
  tenpercent_dt[cell %in% 0, cellphone:=NA]
  tenpercent_dt[cell %in% 1, cellphone:= 1]
  tenpercent_dt[cell %in% 2, cellphone:= 0]



  ##Create Bank_account variable
  tenpercent_dt$bank_account<-NA
  tenpercent_dt$bank_account<- as.character(tenpercent_dt$bank_account)
  tenpercent_dt[TEM_CONTA_BANCARIA %in% 0, bank_account:=NA]
  tenpercent_dt[TEM_CONTA_BANCARIA %in% 1, bank_account:= 1]
  tenpercent_dt[TEM_CONTA_BANCARIA %in% 2, bank_account:= 0]

  table(tenpercent_dt$bank_account, useNA= "always")


  #create Internet Usage

  tenpercent_dt$internet<-NA
  tenpercent_dt$internet<- as.character(tenpercent_dt$internet)
  tenpercent_dt[BENSX_E %in% "Sim", internet:=1]
  tenpercent_dt[BENSX_E %in% "Nao", internet:=0]

  table(tenpercent_dt$internet, useNA= "always")



  #create Internet  usage 3 months

  tenpercent_dt$use_internet_3months<-NA
  tenpercent_dt$use_internet_3months<- as.character(tenpercent_dt$use_internet_3months)
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 1, use_internet_3months:= 1]
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 2, use_internet_3months:= 0]
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 0, use_internet_3months:=NA]

  table(tenpercent_dt$use_internet_3months, useNA= "always")

  #create times  Internet  usage 3 months

  tenpercent_dt$times_internet_3months<-NA
  tenpercent_dt$times_internet_3months<- as.character(tenpercent_dt$times_internet_3months)
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 1, times_internet_3months:="Less than 10 times"]
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 2, times_internet_3months:= "10 to 50 times"]
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 3, times_internet_3months:= "More than 50 times"]
  tenpercent_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 0, times_internet_3months:=NA]

  table(tenpercent_dt$times_internet_3months, useNA= "always")




  #Mobile money usage

  tenpercent_dt$mobile_money<-NA
  tenpercent_dt$mobile_money<- as.character(tenpercent_dt$mobile_money)
  tenpercent_dt[USA_MPESA_MKESH %in% 1, mobile_money:= 1]
  tenpercent_dt[USA_MPESA_MKESH %in% 2, mobile_money:= 0]
  tenpercent_dt[USA_MPESA_MKESH %in% 0, mobile_money:=NA]
  table(tenpercent_dt$mobile_money, useNA= "always")



#### profile tables for different types of informal workers

  profilevar_list <- c("cellphone", "bank_account", "internet", "use_internet_3months", "mobile_money")

  tenpercent_dt$cellphone <- as.integer(tenpercent_dt$cellphone)
  tenpercent_dt$bank_account <- as.integer(tenpercent_dt$bank_account)
  tenpercent_dt$internet <- as.integer(tenpercent_dt$internet)
  tenpercent_dt$use_internet_3months <- as.integer(tenpercent_dt$use_internet_3months)
  tenpercent_dt$mobile_money <- as.integer(tenpercent_dt$mobile_money)

  profile_dt <-
  tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE & is.na(informal_type) == FALSE,
                list(cellownership_rate = weighted.mean(x = cellphone, w = weight_p, na.rm = TRUE),
                     bankaccount_rate = weighted.mean(x = bank_account, w = weight_p, na.rm = TRUE),
                     haveinternet_rate = weighted.mean(x = internet, w = weight_p, na.rm = TRUE),
                     use_internet_rate = weighted.mean(x = use_internet_3months, w = weight_p, na.rm = TRUE),
                     mobile_money_rate = weighted.mean(x = mobile_money, w = weight_p, na.rm = TRUE)),
                by = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro", "informal_type")]


  informal_dt <- as.data.table(informal_dt)

  master_dt <- profile_dt[informal_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro", "informal_type")]

  # master_dt <- bairro_dt[master_dt, on = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]

  # setnames(master_dt, c("i.count", "rate"), c("educemp_count", "educemp_rate"))

  # master_dt[,c("i.Provincia", "i.Distrito", "i.Posto",
  #              "i.Localidade", "i.Bairro", "i.geometry") := NULL]


  ### quickly estimate profile variables are the district level
  profiledist_dt <-
  tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
                list(cellownership_rate = weighted.mean(x = cellphone, w = weight_p, na.rm = TRUE),
                     bankaccount_rate = weighted.mean(x = bank_account, w = weight_p, na.rm = TRUE),
                     haveinternet_rate = weighted.mean(x = internet, w = weight_p, na.rm = TRUE),
                     use_internet_rate = weighted.mean(x = use_internet_3months, w = weight_p, na.rm = TRUE),
                     mobile_money_rate = weighted.mean(x = mobile_money, w = weight_p, na.rm = TRUE)),
                by = c("CodProv", "CodDist", "informal_type")]

  profileprov_dt <-
    tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE,
                  list(cellownership_rate = weighted.mean(x = cellphone, w = weight_p, na.rm = TRUE),
                       bankaccount_rate = weighted.mean(x = bank_account, w = weight_p, na.rm = TRUE),
                       haveinternet_rate = weighted.mean(x = internet, w = weight_p, na.rm = TRUE),
                       use_internet_rate = weighted.mean(x = use_internet_3months, w = weight_p, na.rm = TRUE),
                       mobile_money_rate = weighted.mean(x = mobile_money, w = weight_p, na.rm = TRUE)),
                  by = c("CodProv", "informal_type")]

  bairro_dt <- as.data.table(bairro_dt)
  district_dt <- unique(bairro_dt[,c("CodProv", "CodDist", "Provincia", "Distrito")])
  province_dt <- unique(bairro_dt[,c("CodProv", "Provincia")])

  profiledist_dt <- district_dt[profiledist_dt, on = c("CodProv", "CodDist")]
  profileprov_dt <- province_dt[profileprov_dt, on = "CodProv"]

  profileprov_dt[,c("CodDist", "Distrito") := list(rep(NA, .N), rep("Maputo Cidade", .N))]

  profileprov_dt <- profileprov_dt[CodProv == 11,]


  profileprov_dt <- profileprov_dt[,colnames(profiledist_dt), with = F]

  profiledist_dt <- rbind(profiledist_dt, profileprov_dt)

  profiledist_dt <- rbind(profiledist_dt[!(CodProv == 11),], profileprov_dt)



#### create the bar charts
  profiledist_dt$informal_name[profiledist_dt$informal_type == "employer"] <- "Employer"
  profiledist_dt$informal_name[profiledist_dt$informal_type == "higheduc-selfemp"] <- "High-Educated Self-Employed Worker"
  profiledist_dt$informal_name[profiledist_dt$informal_type == "loweduc-selfemp"] <- "Low-Educated Self-Employed Worker"
  profiledist_dt$informal_name[profiledist_dt$informal_type == "higheduc-wage"] <- "High-Educated Wage Worker"
  profiledist_dt$informal_name[profiledist_dt$informal_type == "loweduc-wage"] <- "Low-Educated Wage Worker"
  profiledist_dt$informal_name[profiledist_dt$informal_type == "unpaidfam"] <- "Unpaid Family Worker"

ggplot(profiledist_dt[Distrito %in% c(city_list, "Maputo Cidade") & is.na(informal_type) == FALSE,],
       aes(x = informal_name, y = cellownership_rate*100)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Distrito) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("Education Level - Employment Class") +
  ylab("Cell Phone Ownership Rate (%)")
ggsave("inst/plots/informalplots/barplot_cellphoneownership_selectcities.png")


ggplot(profiledist_dt[Distrito %in% c(city_list, "Maputo Cidade") & is.na(informal_type) == FALSE,],
       aes(x = informal_name, y = cellownership_rate*100)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Distrito) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("Education Level - Employment Class") +
  ylab("Bank Account Ownership Rate (%)")
ggsave("inst/plots/informalplots/barplot_bankaccountownership_selectcities.png")


ggplot(profiledist_dt[Distrito %in% c(city_list, "Maputo Cidade") & is.na(informal_type) == FALSE,],
       aes(x = informal_name, y = cellownership_rate*100)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Distrito) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("Education Level - Employment Class") +
  ylab("Internet Access Rate (%)")
ggsave("inst/plots/informalplots/barplot_internetaccess_selectcities.png")


ggplot(profiledist_dt[Distrito %in% c(city_list, "Maputo Cidade") & is.na(informal_type) == FALSE,],
       aes(x = informal_name, y = cellownership_rate*100)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Distrito) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("Education Level - Employment Class") +
  ylab("Internet Usage Rate (%)")
ggsave("inst/plots/informalplots/barplot_internetusage_selectcities.png")


ggplot(profiledist_dt[Distrito %in% c(city_list, "Maputo Cidade") & is.na(informal_type) == FALSE,],
       aes(x = informal_name, y = cellownership_rate*100)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Distrito) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("Education Level - Employment Class") +
  ylab("Mobile Money Platform Usage Rate (%)")
ggsave("inst/plots/informalplots/barplot_mobilemoney_selectcities.png")


### one last thing, lets add the night light data that correlates really well with electricity consumption
shp_dt <- sf::read_sf(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                      layer = "moz_poppoly_full_gridded")

elect_loc <- "inst/extdata/elect"
raster_list <- list.files(elect_loc, "_2019")

raster_list <- unlist(lapply(elect_loc, paste, raster_list,  sep = "/"))

raster_list <- lapply(raster_list, raster::raster)

### drop unpopulated areas
shp_dt <- shp_dt[shp_dt$population > 0,]

elect_dt <- parallel_extract(shp_dt = shp_dt[,c("poly_id", "geometry")],
                             raster_list = raster_list,
                             fun_list = rep("mean", length(raster_list)),
                             numCores = length(raster_list))

elect_dt <- as.data.table(elect_dt)

colnames(elect_dt) <- c("rade9lmu", "set_lightscore_sy", "set_prplit_conf90", "set_zscore_sy")

shp_dt <- cbind(shp_dt, elect_dt)

shp_dt <- as.data.table(shp_dt)

bairroelect_dt <- shp_dt[,lapply(.SD, weighted.mean, w = population, na.rm = TRUE),
                         .SDcols = colnames(elect_dt),
                         by = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")]

master_dt <- bairroelect_dt[master_dt, on = c("CodProv", "CodDist", "CodPost",
                                              "CodLocal", "CodBairro")]

### quickly include population data
pop_dt <- shp_dt[,sum(population, na.rm = TRUE), by = c("CodProv", "CodDist", "CodPost",
                                                        "CodLocal", "CodBairro")]

popd_dt <- shp_dt[,mean(population, na.rm = TRUE), by = c("CodProv", "CodDist", "CodPost",
                                                         "CodLocal", "CodBairro")]


setnames(pop_dt, "V1", "population")
master_dt <- pop_dt[,c("population", "CodProv", "CodDist",
                       "CodPost", "CodLocal", "CodBairro")][master_dt,
                                                            on = c("CodProv", "CodDist", "CodPost",
                                                                   "CodLocal", "CodBairro")]

#### some regression models to estimate the relationship between the location of each education-labor class group
#### and the variables we care about


###### add all the variables into master_dt
geo_vars <- c("count", "cv_area", "cv_length", "density",
              "imagery_year", "mean_area", "mean_length",
              "total_area", "total_length", "urban", "cell_area")

setnames(bairro_dt, geo_vars, unlist(lapply("bld", paste, geo_vars, sep = "_")))

setnames(master_dt, "count", "iw_count")
setnames(master_dt, "rate", "iw_rate")


master_dt[,liw_count := log(iw_count)]

master_dt <- bairro_dt[,c(unlist(lapply("bld", paste, geo_vars, sep = "_"),
                                 "Tower_per_sqkm"), "CodProv",
                          "CodDist", "CodPost",
                          "CodLocal", "CodBairro"), with = F][master_dt, on = c("CodProv", "CodDist",
                                                                                "CodPost", "CodLocal",
                                                                                "CodBairro")]

master_dt[is.na(bld_count), bld_count := 0.1]
master_dt[is.na(bld_total_length), bld_total_length := 0.1]
master_dt[,lbld_count := log(bld_count)]
master_dt[,lpop := log(population)]

## include Tower_per_sqkm
joinvars_list <- c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro")
master_dt <- bairro_dt[,c("Tower_per_sqkm", joinvars_list), with = F][master_dt, on = joinvars_list]

master_dt[,c("lbld_mean_area", "lbld_mean_length", "lbld_total_area",
             "lbld_total_length", "lbld_density", "ltower_count",
             "llight") := list(log(bld_mean_area), log(bld_mean_length), log(bld_total_area),
                               log(bld_total_length), log(bld_density), log(Tower_per_sqkm),
                               log(rade9lmu))]

### dummify the tower count variable
master_dt[,dtower_count := as.integer(ifelse(Tower_per_sqkm <= 0, 0, 1))]




### stacked bar-charts for the categories HE self-employed, LE self-employed, HE wage, LE wage, employer, unpaid and other
newinformal_dt <-
tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE &
                !grepl("NA", informal_type) & !grepl("unknown", informal_type) &
                is.na(informal_type) == FALSE,
              .(count = sum(weight_p)),
              by = c("CodProv", "CodDist", "informal_type")][,rate := count / sum(count),
                                                    by = c("CodProv", "CodDist")][]

newinformal_dt <- unique(bairro_dt[,c("CodProv", "CodDist", "Provincia",
                               "Distrito")])[newinformal_dt, on = c("CodProv", "CodDist")]


newinformaldist_dt <-
  tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE &
                  !grepl("NA", informal_type) & !grepl("unknown", informal_type) &
                  is.na(informal_type) == FALSE,
                .(count = sum(weight_p)),
                by = c("CodProv", "informal_type")][,rate := count / sum(count),
                                                               by = c("CodProv")][]

newinformaldist_dt[,CodDist := NA]
newinformaldist_dt[,Distrito := "Maputo Cidade"]

newinformaldist_dt <- unique(bairro_dt[,c("CodProv","Provincia")])[newinformaldist_dt, on = "CodProv"]

newinformal_dt <- rbind(newinformal_dt[!(CodProv == 11),],
                        newinformaldist_dt[CodProv == 11, colnames(newinformal_dt), with = F])

newinformal_dt$informal_name[newinformal_dt$informal_type == "employer"] <- "Employer"
newinformal_dt$informal_name[newinformal_dt$informal_type == "higheduc-selfemp"] <- "High-Educated Self-Employed Worker"
newinformal_dt$informal_name[newinformal_dt$informal_type == "loweduc-selfemp"] <- "Low-Educated Self-Employed Worker"
newinformal_dt$informal_name[newinformal_dt$informal_type == "higheduc-wage"] <- "High-Educated Wage Worker"
newinformal_dt$informal_name[newinformal_dt$informal_type == "loweduc-wage"] <- "Low-Educated Wage Worker"
newinformal_dt$informal_name[newinformal_dt$informal_type == "unpaidfam"] <- "Unpaid Family Worker"

ggplot(data = newinformal_dt[Distrito %in% c(city_list, "Maputo Cidade"),],
       aes(x = Distrito, y = rate, fill = informal_name)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_discrete(name = "Class")
ggsave("inst/plots/informalplots/stackbar_informality_selectcities.png")

write.csv(newinformal_dt[Distrito %in% c(city_list, "Maputo Cidade"),],
          "inst/plots/informalplots/stackbar_informality_selectcities.csv")


### compute the plots
plot_educworktype <- function(group,
                              title){

  informal_dt$District <- informal_dt$Distrito
  informal_dt$District[informal_dt$Provincia %in% "Maputo Cidade"] <- "Maputo Cidade"

  max_count <- max(informal_dt$count, na.rm = TRUE)
  max_rate <- max(informal_dt$rate, na.rm = TRUE)

  m <-
    informal_dt[informal_dt$informal_type == group &
                     informal_dt$Distrito %in% city_list |
                     informal_dt$Provincia %in% "Maputo Cidade",] %>%
    tm_shape() +
    tm_polygons(col = "rate", title = title, breaks = seq(0, max_rate, by = max_rate/20), palette = "YlOrRd") +
    tm_facets("District") +
    tm_layout(legend.outside.position = "bottom",
              legend.position = c(0.8, 1.1))
  tmap_save(m, paste0("inst/plots/informalfacets/", group, "_city", paste0("rate", ".png")))

  m <-
    informal_dt[informal_dt$informal_type == group &
                  informal_dt$Distrito %in% city_list |
                  informal_dt$Provincia %in% "Maputo Cidade",] %>%
    tm_shape() +
    tm_polygons(col = "count", title = title, breaks = seq(0, max_count, by = (max_count)/20), palette = "YlOrRd") +
    tm_facets("District") +
    tm_layout(legend.outside.position = "bottom",
              legend.position = c(0.8, 1.1))
  tmap_save(m, paste0("inst/plots/informalfacets/", group, "_city", paste0("count", ".png")))


}

group_list <- unique(tenpercent_dt$informal_type)
group_list <- group_list[is.na(group_list) == FALSE]
title_list <- c("Low-Educated \nSelf-Employed Workers",
                "High-Educated \nWage Workers",
                "Employer",
                "Unpaid Family Workers",
                "High-Educated \nSelf-Employed Workers",
                "Low-Educated \nWage Workers")

###include geometry column to data

informal_dt <- as.data.frame(informal_dt)

informal_dt <- informal_dt[,colnames(informal_dt)[!grepl("geometry", colnames(informal_dt))]]

informal_dt <- left_join(informal_dt,
                         st_as_sf(bairro_dt[,c("CodProv", "CodDist", "CodPost",
                                               "CodLocal", "CodBairro", "Provincia",
                                               "Distrito", "Posto", "Localidade",
                                               "Bairro", "geometry")],
                                  crs = 4326,
                                  agr = "constant"),
                         by = colnames(informal_dt)[colnames(informal_dt) %in% colnames(bairro_dt)])

informal_dt <- st_as_sf(informal_dt, crs = 4326, agr = "constant")


mapply(plot_educworktype, group = group_list, title = title_list)


#### geom_sf plots using grid.arrange
plot_list <- list()

informal_dt$District <- informal_dt$Distrito
informal_dt$District[informal_dt$Provincia %in% "Maputo Cidade"] <- "Maputo Cidade"

dlist <- c(city_list, "Maputo Cidade")
work_group <- unique(informal_dt$informal_name)

for (worktype in seq_along(work_group)){

  max_rate <- max(informal_dt$rate[informal_dt$informal_name == work_group[worktype]], na.rm = TRUE)

  for (district in seq_along(dlist)){

    dt <- informal_dt[informal_dt$informal_name == work_group[worktype] &
                        informal_dt$District %in% dlist[district],]

    plot_list[[district]] <-
      ggplot(data = dt) +
      geom_sf(aes(fill = rate)) +
      scale_fill_viridis_c(limits = c(0, max_rate), option = "plasma") +
      ggtitle(dlist[district])

  }

  gridExtra::grid.arrange(grobs = plot_list, nrow = 2)
  ggsave(file = paste0("inst/plots/informalfacets/",
                       paste(gsub(" ","", tolower(work_group[worktype])), "rate.png", sep = "_")),
         gridExtra::arrangeGrob(grobs = plot_list, nrow = 2))

}

plot_list <- list()
rm(worktype)
rm(district)

for (worktype in seq_along(work_group)){

  max_count <- max(informal_dt$count[informal_dt$informal_name == work_group[worktype]], na.rm = TRUE)

  for (district in seq_along(dlist)){

    dt <- informal_dt[informal_dt$informal_name == work_group[worktype] &
                        informal_dt$District %in% dlist[district],]

    plot_list[[district]] <-
      ggplot(data = dt) +
      geom_sf(aes(fill = count)) +
      scale_fill_viridis_c(limits = c(0, max_count), option = "plasma") +
      ggtitle(dlist[district])

  }

  gridExtra::grid.arrange(grobs = plot_list, nrow = 2)
  ggsave(file = paste0("inst/plots/informalfacets/",
                       paste(gsub(" ","", tolower(work_group[worktype])), "count.png", sep = "_")),
         gridExtra::arrangeGrob(grobs = plot_list, nrow = 2))

}



### compute a correlation matrix
cols <- c("cellownership_rate", "bankaccount_rate", "haveinternet_rate", "use_internet_rate",
          "mobile_money_rate", "iw_count", "iw_rate", "Tower_per_sqkm", "rade9lmu", "set_lightscore_sy",
          "set_prplit_conf90", "set_zscore_sy", "bld_count", "bld_cv_area", "bld_cv_length",
          "bld_density", "bld_imagery_year", "bld_mean_area", "bld_mean_length", "bld_total_area",
          "bld_total_length", "bld_urban", "bld_cell_area", "population")

corr_dt <-
master_dt[Distrito %in% city_list | Provincia %in% "Maputo Cidade",] %>%
  group_by(informal_name) %>%
  do({
    correlate(select(., c(bld_count:mobile_money_rate, iw_count, iw_rate, liw_count:Tower_per_sqkm)))
  })

corr_dt <-
corr_dt %>%
  filter(!is.na(informal_name))

corr_dt <-
corr_dt %>%
  select(informal_name, term, liw_count, iw_rate)


add_dt <-
master_dt %>%
  sjmisc::to_dummy(Provincia)

colnames(add_dt) <- gsub(" ", "", unique(master_dt$Provincia)[!is.na(unique(master_dt$Provincia))])

master_dt <-
master_dt %>%
  cbind(., add_dt)

corr_dt$labels[corr_dt$term == "bld_count"] <- "Building Count"
corr_dt$labels[corr_dt$term == "bld_cv_area"] <- "Building Area Coefficient of Variation"
corr_dt$labels[corr_dt$term == "bld_cv_length"] <- "Building Length Coefficient of Variation"
corr_dt$labels[corr_dt$term == "bld_density"] <- "Building Density"
corr_dt$labels[corr_dt$term == "bld_imagery_year"] <- "Year of Building Imagery"
corr_dt$labels[corr_dt$term == "bld_mean_area"] <- "Building Mean Area"
corr_dt$labels[corr_dt$term == "bld_mean_length"] <- "Building Mean Length"
corr_dt$labels[corr_dt$term == "bld_total_area"] <- "Building Total Area"
corr_dt$labels[corr_dt$term == "bld_total_length"] <- "Building Total Length"
corr_dt$labels[corr_dt$term == "bld_urban"] <- "Proportion of Area that is Urban"
corr_dt$labels[corr_dt$term == "population"] <- "Population"
corr_dt$labels[corr_dt$term == "rade9lmu"] <- "Nighttime light annual composite"
corr_dt$labels[corr_dt$term == "set_lightscore_sy"] <- "Average likelihood of electrification"
corr_dt$labels[corr_dt$term == "set_prplit_conf90"] <- "Proportion of nights area is statistically brighter than matched unhabited areas"
corr_dt$labels[corr_dt$term == "set_zscore_sy"] <- "Statistically estimated brightness levels"
corr_dt$labels[corr_dt$term == "cellownership_rate"] <- "Rate of Cell Phone Ownership"
corr_dt$labels[corr_dt$term == "bankaccount_rate"] <- "Rate of Bank Account Ownership"
corr_dt$labels[corr_dt$term == "haveinternet_rate"] <- "Internet Ownership Rate"
corr_dt$labels[corr_dt$term == "use_internet_rate"] <- "Internet Use Rate"
corr_dt$labels[corr_dt$term == "mobile_money_rate"] <- "Mobile Money Usage Rate"
corr_dt$labels[corr_dt$term == "iw_count"] <- "# workers in specific class"
corr_dt$labels[corr_dt$term == "iw_rate"] <- "Proportion of workers in specific class"
corr_dt$labels[corr_dt$term == "liw_count"] <- "Log of workers in specific class"
corr_dt$labels[corr_dt$term == "lbld_count"] <- "Log Building Count"
corr_dt$labels[corr_dt$term == "lpop"] <- "Log Population"
corr_dt$labels[corr_dt$term == "Tower_per_sqkm"] <- "Number of Cell towers per sqkm"

write.csv(corr_dt, "inst/extdata/corr.csv")

#### run some regressions

master_dt[,c("lbld_count", "lbld_cv_area", "lbld_cv_length", "lbld_density",
             "lbld_mean_area", "lbld_mean_length", "lbld_total_area",
             "lbld_total_length", "lpop") :=
            list(log(bld_count), log(bld_cv_area), log(bld_cv_length),
                 log(bld_density), log(bld_mean_area), log(bld_mean_length),
                 log(bld_total_area), log(bld_total_length), log(population))]


xvars <- c("lbld_count", "lbld_cv_area", "lbld_total_area",
           "lbld_total_length", "lpop", "rade9lmu", "set_lightscore_sy",
           "set_prplit_conf90", "set_zscore_sy","cellownership_rate",
           "bankaccount_rate", "haveinternet_rate", "use_internet_rate",
           "mobile_money_rate", "Tower_per_sqkm", "bld_urban",
           colnames(add_dt))


# model_formula <- formula(paste("iw_rate ~ ", paste(xvars, collapse = " + ")))
#
# reg_model <- lm(formula = model_formula,
#                 data = na.omit(master_dt[(Distrito %in% city_list |
#                                            Provincia %in% "Maputo Cidade") &
#                                            informal_type == "loweduc-selfemp",
#                                          c(xvars, "iw_rate", "population"),
#                                          with = FALSE]))
tenpercent_dt[,c("own_radio", "own_tv", "own_fixedphone", "own_laptop", "own_internet", "own_iron",
                 "own_woodstove", "own_electricstove", "own_fridge", "own_car",
                 "own_motorcycle") := list(ifelse(BENSX_A == "Sim", 1, ifelse(BENSX_A == "Nao", 0, NA)),
                                           ifelse(BENSX_B == "Sim", 1, ifelse(BENSX_B == "Nao", 0, NA)),
                                           ifelse(BENSX_C == "Sim", 1, ifelse(BENSX_C == "Nao", 0, NA)),
                                           ifelse(BENSX_D == "Sim", 1, ifelse(BENSX_D == "Nao", 0, NA)),
                                           ifelse(BENSX_E == "Sim", 1, ifelse(BENSX_E == "Nao", 0, NA)),
                                           ifelse(BENSX_F == "Sim", 1, ifelse(BENSX_F == "Nao", 0, NA)),
                                           ifelse(BENSX_G == "Sim", 1, ifelse(BENSX_G == "Nao", 0, NA)),
                                           ifelse(BENSX_H == "Sim", 1, ifelse(BENSX_H == "Nao", 0, NA)),
                                           ifelse(BENSX_I == "Sim", 1, ifelse(BENSX_I == "Nao", 0, NA)),
                                           ifelse(BENSX_J == "Sim", 1, ifelse(BENSX_J == "Nao", 0, NA)),
                                           ifelse(BENSX_K == "Sim", 1, ifelse(BENSX_K == "Nao", 0, NA)))]

asset_dt <-
  tenpercent_dt[URB_RUR == 1 & employed == 1 & is.na(empclass) == FALSE & is.na(informal_type) == FALSE,
                list(ownradio_rate = weighted.mean(x = own_radio, w = weight_p, na.rm = TRUE),
                     owntv_rate = weighted.mean(x = own_tv, w = weight_p, na.rm = TRUE),
                     ownfixedphone_rate = weighted.mean(x = own_fixedphone, w = weight_p, na.rm = TRUE),
                     ownlaptop_rate = weighted.mean(x = own_laptop, w = weight_p, na.rm = TRUE),
                     owniron_rate = weighted.mean(x = own_iron, w = weight_p, na.rm = TRUE),
                     ownwoodstock_rate = weighted.mean(x = own_woodstove, w = weight_p, na.rm = TRUE),
                     ownelectricstove_rate = weighted.mean(x = own_electricstove, w = weight_p, na.rm = TRUE),
                     ownfridge_rate = weighted.mean(x = own_fridge, w = weight_p, na.rm = TRUE),
                     owncar_rate = weighted.mean(x = own_car, w = weight_p, na.rm = TRUE)),
                by = c("CodProv", "CodDist", "CodPost", "CodLocal", "CodBairro", "informal_type")]


master_dt <- asset_dt[master_dt, on = c(joinvars_list, "informal_type")]

#### compute pca variables from the ownership variables
asset_pca <-
na.omit(master_dt[,colnames(asset_dt)[grepl("own",
                                    colnames(asset_dt))],
          with = F])[,prcomp(.SD, center = TRUE, scale = TRUE)]

ggbiplot(asset_pca, alpha = 0)

#### merge pca variables back into master_dt
assetpca_dt <- as.data.table(asset_pca$x)

assetvar_list <- colnames(asset_dt)[grepl("own", colnames(asset_dt))]

complete_vector <- complete.cases(master_dt[,assetvar_list, with = F])

pca_ids <- master_dt[complete_vector, c(joinvars_list, "informal_type"), with = F]

assetpca_dt <- cbind(pca_ids, assetpca_dt)

master_dt <- assetpca_dt[master_dt, on = c(joinvars_list, "informal_type")]

model_function <- function(x,
                           y,
                           inf_type){

  y <- paste0(y, "", sep = " ~ ")
  model_formula <- formula(paste(y, paste(x, collapse = " + ")))

  master_dt <- as.data.table(master_dt)

  reg_model <- lm(formula = model_formula,
                  data = na.omit(master_dt[(Distrito %in% city_list |
                                              Provincia %in% "Maputo Cidade") &
                                             informal_type == inf_type,
                                           c(x, "iw_rate", "population"),
                                           with = FALSE]))
  robust_model <- coeftest(reg_model, vcov = vcovHC(reg_model, type = "HC1", save = TRUE))

  reg_model[]

  return(robust_model)

}


model_function2 <- function(x,
                            y,
                            inf_type){

  y <- paste0(y, "", sep = " ~ ")
  model_formula <- formula(paste(y, paste(x, collapse = " + ")))

  master_dt <- as.data.table(master_dt)

  reg_model <- lm(formula = model_formula,
                  data = na.omit(master_dt[(Distrito %in% city_list |
                                              Provincia %in% "Maputo Cidade") &
                                             informal_type == inf_type,
                                           c(x, "iw_rate", "population"),
                                           with = FALSE]))

  return(reg_model)

}

master_dt <- popd_dt[master_dt, on = c("CodProv", "CodDist", "CodPost",
                                       "CodLocal", "CodBairro")]
setnames(master_dt, "V1", "pop_density")



### using the first 4 PCA variables that account for 86.7% of the variation

master_dt[,lrade9lmu := log(rade9lmu)]
master_dt[,ltower := log(Tower_per_sqkm)]

lese_vars <- c("lbld_count",
               "lpop", "lrade9lmu", "set_lightscore_sy",
               "ltower", "bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model.Rdata")



## include results with both building footprint variables
lese_vars <- c("lbld_count", "lbld_total_length",
               "lpop", "lrade9lmu", "set_lightscore_sy",
               "ltower", "bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model2.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model2.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model2.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model2.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model2.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model2.Rdata")


## include only log population
lese_vars <- c("lpop",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model3.Rdata")


hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model3.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model3.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model3.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model3.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model3.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                  y = "iw_rate",
                                  inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                  y = "iw_rate",
                                  inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)

## include only night time light intensity
lese_vars <- c("lrade9lmu",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model4.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model4.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model4.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model4.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model4.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model4.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)


## include only average electrification likelihood
lese_vars <- c("set_lightscore_sy",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model5.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model5.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model5.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model5.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model5.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model5.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)

## include only cell phone tower
lese_vars <- c("ltower",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model6.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model6.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model6.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model6.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model6.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model6.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)

## include only building urban
lese_vars <- c("bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model7.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model7.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model7.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model7.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model7.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model7.Rdata")


## include only internet usage
lese_vars <- c("use_internet_rate",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model8.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model8.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model8.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model8.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model8.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model8.Rdata")


## include only money usage rate
lese_vars <- c("mobile_money_rate",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model9.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model9.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model9.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model9.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model9.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model9.Rdata")


## include only log building density
lese_vars <- c("lbld_count",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model10.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model10.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model10.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model10.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model10.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model10.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)

## include only log total building length
lese_vars <- c("lbld_total_length",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model11.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model11.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model11.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model11.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model11.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model11.Rdata")

### add the actual regression that Wendy speicified
### building density, average night lights, electrification, cell phone towers,
### proportion of urban area, city dummies, and the asset variables

lese_vars <- c("lbld_count",
               "lrade9lmu", "set_lightscore_sy",
               "ltower", "bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model_wendy.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model_wendy.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model_wendy.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model_wendy.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model_wendy.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model_wendy.Rdata")


lese_vars <- c("lpop", "lrade9lmu", "set_lightscore_sy",
               "ltower", "bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model_wendy2.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model_wendy2.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model_wendy2.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model_wendy2.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model_wendy2.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model_wendy2.Rdata")


lese_vars <- c("lpop", "lbld_count", "lrade9lmu", "set_lightscore_sy",
               "ltower", "bld_urban",
               "Zambezia", "Tete", "Sofala", "MaputoCidade",
               "ownradio_rate", "owntv_rate", "ownfixedphone_rate",
               "ownlaptop_rate", "owniron_rate", "ownelectricstove_rate",
               "ownfridge_rate", "owncar_rate")

lese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "loweduc-selfemp")
save(lese_model, file = "inst/extdata/lese_model_wendy3.Rdata")

hewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "higheduc-wage")
save(hewage_model, file = "inst/extdata/hewage_model_wendy3.Rdata")

emp_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "employer")
save(emp_model, file = "inst/extdata/emp_model_wendy3.Rdata")

upf_model <- model_function(x = lese_vars,
                            y = "iw_rate",
                            inf_type = "unpaidfam")
save(upf_model, file = "inst/extdata/upf_model_wendy3.Rdata")

hese_model <- model_function(x = lese_vars,
                             y = "iw_rate",
                             inf_type = "higheduc-selfemp")
save(hese_model, file = "inst/extdata/hese_model_wendy3.Rdata")

lewage_model <- model_function(x = lese_vars,
                               y = "iw_rate",
                               inf_type = "loweduc-wage")
save(lewage_model, file = "inst/extdata/lewage_model_wendy3.Rdata")

###just for the rsquares
lesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "loweduc-selfemp")

hewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "higheduc-wage")

emprsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "employer")

upfrsq_model <- model_function2(x = lese_vars,
                                y = "iw_rate",
                                inf_type = "unpaidfam")

hesersq_model <- model_function2(x = lese_vars,
                                 y = "iw_rate",
                                 inf_type = "higheduc-selfemp")

lewagersq_model <- model_function2(x = lese_vars,
                                   y = "iw_rate",
                                   inf_type = "loweduc-wage")

rsq_set <- c(summary(lesersq_model)$r.squared,
             summary(hewagersq_model)$r.squared,
             summary(emprsq_model)$r.squared,
             summary(upfrsq_model)$r.squared,
             summary(hesersq_model)$r.squared,
             summary(lewagersq_model)$r.squared)

#### save certain datasets
save.image(file = "inst/extdata/informal_envir.Rdata")


###### geospatial plot for night-time lights, cell phone towers,

bairro_dt <- st_as_sf(bairro_dt, crs = 4326, agr = "constant")

bairro_dt$District <- bairro_dt$Distrito

bairro_dt$District[bairro_dt$Provincia == "Maputo Cidade"] <- "Maputo Cidade"


m <-
bairro_dt[bairro_dt$Distrito %in% city_list |
              bairro_dt$Provincia %in% "Maputo Cidade",] %>%
  tm_shape() +
  tm_polygons(col = "Tower_per_sqkm", title = "Number of Towers per sqkm") +
  tm_facets("District") +
  tm_layout(legend.outside.position = "bottom",
            legend.position = c(0.8, 1.1))
tmap_save(m, "inst/plots/informalfacets/numtowerspersqkm.png")


m <-
  bairro_dt[bairro_dt$Distrito %in% city_list |
              bairro_dt$Provincia %in% "Maputo Cidade",] %>%
  tm_shape() +
  tm_polygons(col = "Tower_per_sqkm", title = "Number of Towers per sqkm") +
  tm_facets("District") +
  tm_layout(legend.outside.position = "bottom",
            legend.position = c(0.8, 1.1))
tmap_save(m, "inst/plots/informalfacets/numtowerspersqkm.png")




#### compute the maps prescribed by Wendy
bairro_dt <-
bairro_dt %>%
  mutate(District = ifelse(Provincia %in% "Maputo Cidade", "Maputo Cidade", Distrito))

master_dt <-
  master_dt %>%
  mutate(District = ifelse(Provincia %in% "Maputo Cidade", "Maputo Cidade", Distrito))


plot_bairro_indicator <- function(dt,
                                  var,
                                  city,
                                  legend_name){

  ggplot(dt[dt$District %in% city,]) +
    geom_sf(aes_string(fill = var)) +
    scale_fill_viridis_c(option = "plasma", name = legend_name) +
    ggtitle(city) +
    theme(text = element_text(size = 7.5))

}


# png(filename = "inst/plots/informalfacets/ntl_bairro3.png",
#     width = 5953, height = 2332,
#     units = "px", pointsize = 12, bg = "white", res = 1000)

plot_set <-
lapply(X = c("Maputo Cidade", city_list),
       FUN = plot_bairro_indicator,
       dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
       var = "lrade9lmu",
       legend_name = "Log NTL")


p <- cowplot::plot_grid(plot_set[[1]], plot_set[[2]],
                        plot_set[[3]], plot_set[[4]],
                        plot_set[[5]])
cowplot::save_plot("inst/plots/informalfacets/ntl_bairro3.png", p, ncol = 3)

dev.off()


#### include electrification access

plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "set_lightscore_sy",
         legend_name = "Electrification \n Likelihood")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)


#### include the population
plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "population",
         legend_name = "population")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)


#### include the cell tower counts
plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "Tower_per_sqkm",
         legend_name = "# Towers \n per sqkm")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)


#### include the rates of urbanization
plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "bld_urban",
         legend_name = "Proportion of \n Urban Area")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)



#### include the building density maps
plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "bld_count",
         legend_name = "Building Density")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)

#### include the population density
plot_set <-
  lapply(X = c("Maputo Cidade", city_list),
         FUN = plot_bairro_indicator,
         dt = master_dt %>% st_as_sf(crs = 4326, agr = "constant"),
         var = "pop_density",
         legend_name = "Population Density")

gridExtra::grid.arrange(grobs = plot_set, nrow = 2)


### correlation matrix for the 6 variables Wendy showed interest in
### luminosity, electrification likelihood, population density,
### building density, urban/rural, ICT density
write.csv(master_dt[District %in% c(city_list, "Maputo Cidade") &
                      informal_type == "loweduc-selfemp",
                    c("rade9lmu", "set_lightscore_sy", "pop_density",
                      "bld_count", "bld_urban", "Tower_per_sqkm")][,cor(.SD, use = "complete.obs")],
          "inst/extdata/correlation.csv")




