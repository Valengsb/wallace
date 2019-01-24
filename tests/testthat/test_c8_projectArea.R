#### COMPONENT 8: Project Model
#### MODULE: Project to New Extent
context("projectArea")

source("test_helper_functions.R")

## occurrences
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$cleaned)

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

## model
# regularization multipliers 
rms <- c(1:2)
# regularization multipliers step value
rmsStep <- 1
# feature classes
fcs <- c('L', 'H', 'LQH')
# algorithm
algoritm <- c('maxent.jar','maxnet')
# build model
maxentAlg  <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                         algMaxent = algoritm[1])

## extention to project
# set coordinates 
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
expertAddedPoly <- matrix(c(longitude, latitude), byrow = F, ncol = 2)

### run function
c8_projectArea(results = maxentAlg$models, curModel, envs, outputType, 
               polyPjXY = expertAddedPoly , polyPjID = 1)



