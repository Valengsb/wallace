##### QUESTIONS
  # 1. How to test the maxent.jar error message 


#### COMPONENT X: Build and Evaluate Niche Model
#### MODULE: MAXENT.JAR
context("MaxentEvalPlot")

source("test_helper_functions.R")


### Set parameters

## occurrences
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$cleaned)

records <- which(is.na(raster::extract(envs$bio1.1, occs[,3:4])) == TRUE)
occs <- occs[-records, ] 


## background mask
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))
# background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 
# background masked 
bgMsk <- c4_bgMask(occs, envs, bgExt)

## background sample
bg <- c4_bgSample(occs, bgMsk, bgPtsNum = 10000) 

## partition data
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL) 
# occurrences partitioned
occsGrp = partblock$occ.grp
# background points partitioned
bgGrp = partblock$bg.grp

## regularization multipliers 
rms <- c(1:5)
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L')

## algorithm
maxentJar <- 'maxent.jar'
maxnet <- 'maxnet'
bioclim <- 'bioclim'


### run function

## Maxent.jar
maxentjar <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                       algMaxent = maxentJar)


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  #
  expect_error(runMaxent(occs, bg, occsGrp = NULL, bgGrp, bgMsk, rms, rmsStep, fcs, 
                         clampSel = TRUE, algMaxent = 'maxent.jar'), "Before building a model, please partition 
                        occurrences for cross-validation.")
})

### test output features 
test_that("output type checks", {
  # the output is a list
  expect_is(maxentjar, "list")
  expect_is(maxentjar[c("evalTbl","evalTblBins", "models")], "list")
  expect_is(maxentjar$predictions, "RasterStack")
  expect_is(maxentjar$occPredVals, "matrix")
  # the list has two elements
  expect_equal(length(maxentjar), 5)
  expect_equal(length(maxentjar$models), (length(rms)/rmsStep)*length(fcs))
  expect_equal(nrow(maxentjar$evalTbl), (length(rms)/rmsStep)*length(fcs))
  expect_equal(nrow(maxentjar$evalTblBins), (length(rms)/rmsStep)*length(fcs))
  expect_equal(ncol(maxentjar$evalTbl), nrow(maxentjar$evalTbl)*4)
  expect_equal(ncol(maxentjar$evalTblBins), nrow(maxentjar$evalTblBins)*4)
  expect_equal(length(maxentjar$models), raster::nlayers(maxentjar$occPredVals))
})



#maxentjar$models$
#maxentjar$evalTbl
#maxentjar$evalTblBins
#maxentjar$predictions$L_1
#maxentjar$predictions$L_1.5
#maxentjar$predictions$L_2
#maxentjar$occPredVals
