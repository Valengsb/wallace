#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgSample - Step 2.2")

source("test_helper_functions.R")


### Set parameters

## occurrences
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs$cleaned)

## background mask
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))
# background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 
# background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)

## Number of background points to sample
bgPtsNum = 1000


### run function 
bgsample <- c4_bgSample(occs, bgMask, bgPtsNum)


### test output features
test_that("output type checks", {
  # the output is a data frame
  expect_is(bgsample, "data.frame")
  # both latitude and longitude were sampled
  expect_equal(ncol(bgsample), 2)
  # the headers of columns correspond to longitude and latitude
  expect_equal(c('longitude', 'latitude'), names(bgsample))
  # the number of background pints sampled are the same as specified in the function
  expect_equal(nrow(bgsample), bgPtsNum)
  # all the points sampled overlap with the study region
  sp::coordinates(bgsample) <- ~ longitude + latitude
  sp::proj4string(bgsample) <- sp::proj4string(bgExt)
  overlap <- sp::over(bgsample, bgExt)
  expect_false(0 %in% overlap$x)
  })
