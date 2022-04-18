##A script to estimate the relation between education and emptype
##First Load Packages

if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")

}

packages <- c("sf", "fst", "tidyverse", "dplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))




minicensus_dt <- read_fst(path = "//esapov/esapov/MOZ/GEO/Population/tenpercent_census.fst",
                          columns=c("P24_NIVEL_DE_ENSINO_CONCLUIDO_1"),
                          as.data.table = TRUE)

##Create educat4 variable
minicensus_dt$Educat<- as.integer(minicensus_dt$P24_NIVEL_DE_ENSINO_CONCLUIDO_1)
minicensus_dt$educat4<-NA
minicensus_dt$educat4<- as.character(minicensus_dt$educat4)
minicensus_dt[Educat %in% 0:2, educat4:="No Education"]
minicensus_dt[Educat %in% 3:4, educat4:="Primary (complete or incomplete)"]
minicensus_dt[Educat %in% 5:9, educat4:="Secondary (complete or incomplete)"]
minicensus_dt[Educat %in% 9:15, educat4:="Tertiary (complete or incomplete)"]
table(minicensus_dt$educat4, useNA= "always")










