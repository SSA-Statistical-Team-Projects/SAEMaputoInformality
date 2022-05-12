################################################################################
#### PREPARE PACKAGE DATA FOR INFORMALITY STATUS PREDICTION AND IMPUTATION #####
################################################################################


## this quick script will prepare the data into a package readable format to
## for predictin informality in the 2014/15 survey and then impute this into
## the census
load_packages(c("readstata13", "data.table"))

## read in the 2014/15 data
base_dir <- "//WBGMSAFR1001/AFR_Database/Datalib-SSA/datalib/MOZ/MOZ_2014_IOF"

version <- "/MOZ_2014_IOF_v01_M_v05_A_SSAPOV/Data/Harmonized/"

##load labor module
iof14l_dt <- read.dta13(paste0(base_dir,
                               version,
                               "MOZ_2014_IOF_v01_M_v05_A_SSAPOV_L.dta"))
iof14l_dt <- as.data.table(iof14l_dt)

#load income module
iof14i_dt <- read.dta13(paste0(base_dir,
                               version,
                               "MOZ_2014_IOF_v01_M_v05_A_SSAPOV_I.dta"))
iof14i_dt <- as.data.table(iof14i_dt)

iof14il_dt <- iof14i_dt[iof14l_dt, on = c("pid", "hid")]


## read in the select the portion of the census of interest
census_dir <- "//WBGMSAFR1001/AFR_Database/SSAPOV-Harmonization/Daylan/GMD"
fileloc <- "/MOZ_Census/02.Program/10%/Output/MOZ_10percent_L.dta"

minicensus_dt <- as.data.table(read.dta13(paste0(census_dir, fileloc)))

## model informality
scaler <- function(x){

  average_x <- mean(x, na.rm = TRUE)

  y <- x / average_x

  return(y)

}

infmodel_dt[,weight := scaler(wta_hh)]

## build a simple logit model
xvars <- c("ageyrs", "sex", "industrycat10", "educat10", "whours")

model <- paste0("socialsec ~ ", paste(xvars, collapse = " + "))

logitmodel <- glm(model,
                  family = binomial(link = "logit"),
                  data = na.omit(infmodel_dt[empstat == "1.Paid Employee",
                                             c(xvars, "socialsec", "weight"), with = F]),
                  weights = weight)


minicensus_dt[,informal_prob := predict(logitmodel, newdata = minicensus_dt, type = "response")]

#### introduce area level variables for us to be able to map wage informality and
#### urban informality

### read in the 10% census L module











