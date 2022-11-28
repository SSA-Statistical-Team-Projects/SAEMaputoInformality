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



minicensus_dt <- read_fst(path = "//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst",
                          as.data.table = TRUE)

##Check the levels of teh concluded class.
levels(minicensus_dt$P24_NIVEL_DE_ENSINO_CONCLUIDO_1)
#create educat7
minicensus_dt$Educat<- as.integer(minicensus_dt$P24_NIVEL_DE_ENSINO_CONCLUIDO_1)
minicensus_dt$educat7<-NA
minicensus_dt$educat7<- as.character(minicensus_dt$educat7)
minicensus_dt[Educat %in% 0:3, educat7:="No Education"]
minicensus_dt[Educat %in% 4, educat7:="Primary incomplete"]
minicensus_dt[Educat %in% 5, educat7:="Primary complete"]
minicensus_dt[Educat %in% 6, educat7:="Secondary incomplete"]
minicensus_dt[Educat %in% 7, educat7:="Secondary complete"]
minicensus_dt[Educat %in% 8:11, educat7:="Post-secondary but not university"]
minicensus_dt[Educat %in% 12:15, educat7:="University (complete or incomplete"]


##Create educat5 variable
minicensus_dt$educat5<-NA
minicensus_dt$educat5<- as.character(minicensus_dt$educat5)
minicensus_dt[Educat %in% 0:3, educat5:="No Education"]
minicensus_dt[Educat %in% 4, educat5:="Primary incomplete"]
minicensus_dt[Educat %in% 5:6, educat5:="Primary complete but Secondary incomplete"]
minicensus_dt[Educat %in% 7, educat5:="Secondary complete"]
minicensus_dt[Educat %in% 8:15, educat5:="Tertiary/post-secondary (complete or incomplete)"]
table(minicensus_dt$educat5, useNA= "always")


#create educat4
minicensus_dt$educat4<-NA
minicensus_dt$educat4<- as.character(minicensus_dt$educat4)
minicensus_dt[Educat %in% 0:3, educat4:="No Education"]
minicensus_dt[Educat %in% 4:5, educat4:="Primary (complete or incomplete)"]
minicensus_dt[Educat %in% 6:7, educat4:="Secondary (complete or incomplete)"]
minicensus_dt[Educat %in% 8:15, educat4:="Tertiary (complete or incomplete)"]
table(minicensus_dt$educat4, useNA= "always")

write_fst(minicensus_dt,"//esapov/esapov/MOZ/GEO/Population/tenpercent_cleancensus.fst")




