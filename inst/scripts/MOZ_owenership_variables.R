
### Harmonize Variables from the Census 

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

##Check the levels.
levels(minicensus_dt$TEM_TELEFONE_CELULAR)
#create educat7
minicensus_dt$cell<- as.integer(minicensus_dt$TEM_TELEFONE_CELULAR)
minicensus_dt$cellphone<-NA
minicensus_dt$cellphone<- as.character(minicensus_dt$cellphone)
minicensus_dt[cell %in% 0, cellphone:=NA]
minicensus_dt[cell %in% 1, cellphone:="Yes"]
minicensus_dt[cell %in% 2, cellphone:="No"]



##Create Bank_account variable
minicensus_dt$bank_account<-NA
minicensus_dt$bank_account<- as.character(minicensus_dt$bank_account)
minicensus_dt[TEM_CONTA_BANCARIA %in% 0, bank_account:=NA]
minicensus_dt[TEM_CONTA_BANCARIA %in% 1, bank_account:="Yes"]
minicensus_dt[TEM_CONTA_BANCARIA %in% 2, bank_account:="No"]

table(minicensus_dt$bank_account, useNA= "always")


#create Internet Usage 

minicensus_dt$internet<-NA
minicensus_dt$internet<- as.character(minicensus_dt$internet)
minicensus_dt[BENSX_E %in% 1, internet:="Yes"]
minicensus_dt[BENSX_E %in% 2, internet:="No"]

table(minicensus_dt$internet, useNA= "always")



#create Internet  usage 3 months

minicensus_dt$use_internet_3months<-NA
minicensus_dt$use_internet_3months<- as.character(minicensus_dt$use_internet_3months)
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 1, use_internet_3months:="Yes"]
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 2, use_internet_3months:="No"]
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 0, use_internet_3months:=NA]

table(minicensus_dt$use_internet_3months, useNA= "always")

#create times  Internet  usage 3 months

minicensus_dt$times_internet_3months<-NA
minicensus_dt$times_internet_3months<- as.character(minicensus_dt$times_internet_3months)
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 1, times_internet_3months:="Less than 10 times"]
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 2, times_internet_3months:= "10 to 50 times"]
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 3, times_internet_3months:= "More than 50 times"]
minicensus_dt[USOU_INTERNET_ULTIMOS_3_MESES %in% 0, times_internet_3months:=NA]

table(minicensus_dt$times_internet_3months, useNA= "always")




#Mobile money usage 

minicensus_dt$mobile_money<-NA
minicensus_dt$mobile_money<- as.character(minicensus_dt$mobile_money)
minicensus_dt[USA_MPESA_MKESH %in% 1, mobile_money:="Yes"]
minicensus_dt[USA_MPESA_MKESH %in% 2, mobile_money:= "No"]
minicensus_dt[USA_MPESA_MKESH %in% 0, mobile_money:=NA]
table(minicensus_dt$mobile_money, useNA= "always")

### AGE
minicensus_dt$age<-IDADE
minicensus_dt$age<- as.character(minicensus_dt$age)


##SEX
minicensus_dt$sex<-NA
minicensus_dt$sex<- as.character(minicensus_dt$sex)
minicensus_dt[SEXO %in% 1, sex:="male"]
minicensus_dt[SEXO %in% 2, sex:= "female"]
table(minicensus_dt$sex, useNA= "always")




## Industry 
levels(minicensus_dt$TIPO_DE_TRABALHADOR)

minicensus_dt$industry <-NA
minicensus_dt$industry<- as.character(minicensus_dt$industry)
minicensus_dt[TIPO_DE_TRABALHADOR %in% 1, industry:="Public administration"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 2, industry:= "Local authority worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 3, industry:= "Public enterprise worker"]


minicensus_dt[TIPO_DE_TRABALHADOR %in% 4, industry:= "Private enterprise worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 5, industry:= "Cooperative worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 6, industry:= "NGO worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 7, industry:= "Private house worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 8, industry:= "Self employed with employees"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 9, industry:= "Self employed without employees"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 10, industry:= "Family employee without remuneration"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 11, industry:= "International organism worker"]
minicensus_dt[TIPO_DE_TRABALHADOR %in% 99, industry:= NA]

table(minicensus_dt$industry, useNA= "always")



write_fst(minicensus_dt,"//esapov/esapov/MOZ/GEO/Population/owership_variables.fst")

