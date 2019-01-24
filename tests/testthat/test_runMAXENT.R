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

## background mask
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))
# remove records without enviromental values 
records <- which(is.na(raster::extract(envs$bio1.1, occs[,3:4])) == TRUE)
occs <- occs[-records, ] 
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


### Test BIOCLIM

## test if the error messages appear when they are supposed to 
test_that("error checks", {
  #
  expect_error(runMaxent(occs, bg, occsGrp = NULL, bgGrp, bgMsk, rms, rmsStep, fcs, 
                         clampSel = TRUE, algMaxent = 'maxent.jar'), "Before building a model, please partition 
               occurrences for cross-validation.")
})


### Test MAXENT

## regularization multipliers 
rms <- c(1:2)
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L', 'LQ', 'H', 'LQH', 'LQHP', 'LQHPT')

## algorithm
algoritm <- c('maxent.jar','maxnet')

### test output features 
test_that("output type checks", {
  i <- algoritm[1]
  for (i in algoritm) { 
    maxentAlg <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                           algMaxent = i)
    # the output is a list
    expect_is(maxentAlg, "list")
    expect_is(maxentAlg[c("evalTbl","evalTblBins", "models")], "list")
    expect_is(maxentAlg$predictions, "RasterStack")
    expect_is(maxentAlg$occPredVals, "matrix")
    # the list has two elements
    expect_equal(length(maxentAlg), 5)
    expect_equal(length(maxentAlg$models), (length(rms)/rmsStep)*length(fcs))
    expect_equal(nrow(maxentAlg$evalTbl), (length(rms)/rmsStep)*length(fcs))
    expect_equal(ncol(maxentAlg$evalTbl), 16)
    expect_equal(nrow(maxentAlg$evalTblBins), (length(rms)/rmsStep)*length(fcs))
    expect_equal(ncol(maxentAlg$evalTblBins), (nlevels(factor(occsGrp)))*4)
    expect_equal(length(maxentAlg$models), raster::nlayers(maxentAlg$predictions))
  }
  })


### run function

## Maxent.jar
#maxentjar <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                       #algMaxent = maxentJar)

#maxnet <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                       #algMaxent = maxnet)


#maxentjar$models$
#maxentjar$evalTbl
#maxentjar$evalTblBins
#maxentjar$predictions$L_1
#maxentjar$predictions$L_1.5
#maxentjar$predictions$L_2
#maxentjar$occPredVals
