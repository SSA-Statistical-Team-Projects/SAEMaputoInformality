
<!-- README.md is generated from README.Rmd. Please edit that file -->

## A project to Model Urban Informality in Mozambique

This ReadME shows how to the reproduce the results of the analytics use
in the geospatial/big data section of the report. Please do the
following:

1)  Please clone the repository and open up the project within RStudio.
    Make sure you are in the project environment. This is extremely
    important. Otherwise you cannot automatically load the user defined
    functions. You will see the project name in the top right corner of
    the RStudio window.

2)  Please install the R `devtools` package, run
    `install.packages("devtools")`

3)  Run : `devtools::load_all()` function. This will run all the
    functions in the R folder.

4)  Now you can execute the script in “inst/scripts/geo_maps_emp.R”.

The final regression models in the study are of the form
\*\_model2.RData

5)  Apply the `load` function to load the regression objects in the
    “inst/extdata/\*\_model2.RData” to see the results
