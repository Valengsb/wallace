#### COMPONENT 7: Build and Evaluate Niche Model
#### MODULE: BIOCLIM 
context("bioclimPlot")

source("test_helper_functions.R")

### get records
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$cleaned)
## process data
occs <- c2_thinOccs(occs = occs, thinDist = 10)

### background
## enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))
## background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 
## background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)
## background sample
bg <- c4_bgSample(occs, bgMask, bgPtsNum = 10000) 

### Partition 
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL)

occs$partition
occs$partition <- partblock$occ.grp

bg$partition
bg$partition <- partblock$bg.grp

x <- runBIOCLIM(occs, bg, bgMask) 
