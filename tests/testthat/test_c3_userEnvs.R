##### QUESTIONS
  # 1. problems with the warning message (warning message appear in R but not in Wallace)
    # >> They'll fix it


#### COMPONENT 3: Obtain Environmental Data
#### MODULE: User-specified
context("userEnvs")

source("test_helper_functions.R")


### get data 
## path
resPath <- list.files(path='./wc10/', pattern = "*.bil", full.names = TRUE)
## names 
rasName <- list.files(path='./wc10/', pattern = "*.bil", full.names = FALSE)


### run function
userEnvs <- c3_userEnvs(rasPath = resPath, rasName = rasName)


### test if the warning messages appear when they are supposed to 
test_that("warning checks", {
  # input rasters have undefined coordinate reference system (CRS)  
  expect_warning(c3_userEnvs(rasPath = './wc10/No_Projection/no_Projection.tif',
                             rasName = 'no_Projection'))
})

### test output features
test_that("output type checks", {
  # the output is a RasterStack
  expect_is(userEnvs, "RasterStack")
  # the number of layer is the same as loaded by the user 
  expect_equal(length(rasName), raster::nlayers(userEnvs))
})
