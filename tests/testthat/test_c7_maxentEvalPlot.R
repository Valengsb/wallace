#### COMPONENT 7: Visualize Model Results
#### MODULE: Maxent Evaluation Plots
context("maxentEvalPlot")

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

## regularization multipliers 
rms <- c(1:2)
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L', 'LQ')

## algorithm
algoritm <- c('maxent.jar','maxnet')

## model
maxentAlg <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
          algMaxent = algoritm[1])

## value
eVal <- c("delta.AICc", "avg.test.AUC", "avg.diff.AUC", "avg.test.orMTP", "avg.test.or10pct")


### run function 
maxentPlot <- makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = eVal[1])


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the input value isn't right
  expect_error(makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = "test.or10pct"))
  })

### test function stepts 
test_that("output data checks", {
  # create a empty list to save the plots 
  maxentPlot <- list() 
  # index to plot using all the values 
  i <- eVal[1]
  # for each value generate and save the plot in "maxentPlot" list
  for (i in eVal) {
    ### run function
    x <- recordPlot(makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = i))
    maxentPlot <- c(maxentPlot, x)
  }
  # the amount of list should be the same as eVal (varibales used) *3
  expect_equal(length(maxentPlot), length(eVal)*3)
  })

