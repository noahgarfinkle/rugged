require(raster)
require(sp)
require(rgdal)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(randomForest)
library("doParallel")
require("spatial.tools")
require(stats)



path <- "/home/noah/Desktop/Data From Chuck/From Dragon"
setwd(path)

studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/smaller_sa/w001001.adf"
studyExtentRaster <- raster(studyExtentRasterPath)

largeExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/sa_200m/w001001x.adf"

# define the data files we're going to read
landCoverPath <- "/home/noah/Desktop/NRES 565 Data/Land Cover/LC_Output/C20151.tif"
landCoverRaster <- raster(landCoverPath)
slopePath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/slope_projec1/w001001.adf"
slopeRaster <- raster(slopePath)

createMasterRaster <- function(folderPath,maxFiles,rasterName="rasterized_",listOfCategories = categoricalList)
{
  print(folderPath)
  
  # set the directory
  setwd(folderPath)
  
  # set the extents
  #studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/smaller_sa/w001001.adf"
  studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/sa_200m/w001001x.adf"
#   studyExtentRaster <- raster(studyExtentRasterPath)
  rast <- raster(studyExtentRasterPath)
  # ext <- extent(studyExtentRaster)
  
  # process the first file, because it is the only one with a header
  fileName <- "xaa"
  pts <- read.csv(fileName,header=TRUE)
  header <- colnames(pts)
  coordinates(pts)=~x+y
#   rast<-raster()
#   extent(rast) <- ext
#    ncol(rast) <- 13
#    nrow(rast) <- 12
  res(rast) <- c(800,800)
  # add new columns for the categorical variables
  for (colname in header)
  {
    if (exists(colname,where=listOfCategories))
    {
      categories <- listOfCategories[[colname]]
      pts <- convertCategoricalVariablesToBoolean(pts,colname,categories)
    }
  }
  # rasterize
  masterRaster <- rasterize(pts,rast,fun=mean)
  countRaster <- rasterize(pts,rast,fun='count')
  layerNames <- names(masterRaster)

  # loop through all files except the first now, up to the limit
  filePaths <- list.files(pattern = "^x.")
  filesCount <- 1
  
  for (fileName in filePaths)
  {
    if (fileName != "xaa")
    {
      print(fileName)
      pts <- read.csv(fileName,header=FALSE)
      colnames(pts) <- header
      coordinates(pts)=~x+y
#       rast<-raster()
#       extent(rast) <- ext
#       ncol(rast) <- 13
#       nrow(rast) <- 12
      res(rast) <- c(800,800)
      
      # add new columns for the categorical variables
      for (colname in header)
      {
        if (exists(colname,where=listOfCategories))
        {
          categories <- listOfCategories[[colname]]
          pts <- convertCategoricalVariablesToBoolean(pts,colname,categories)
        }
      }
      newRast <- rasterize(pts,rast,fun=mean)
      masterRaster <- mosaic(masterRaster,newRast,fun=mean)
      newCountRaster <- rasterize(pts,rast,fun='count')
      countRaster <- mosaic(countRaster,newCountRaster,fun=sum)
      names(masterRaster) <- layerNames
      filesCount <- filesCount + 1
      if (filesCount >= maxFiles)
      {
        toReturn <- list()
        toReturn[["masterRaster"]] <- masterRaster
        toReturn[["countRaster"]] <- countRaster
        return(toReturn)
      }
    }
  }
  # save each layer of the raster
  # writeRaster(masterRaster,rasterName,format="GTiff",bylayer=TRUE,suffix="names")
#   for (layerName in layerNames)
#   {
#     rast <- masterRaster[[layerName]]
#     writeRaster(rast,paste(layerName,".tif",sep = ""))
#   }
  toReturn <- list()
  toReturn[["masterRaster"]] <- masterRaster
  toReturn[["countRaster"]] <- countRaster
  return(toReturn)
} 

createRasters <- function()
{
  householdsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon"
  individualsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon/Raw"
  
  toReturn <- createMasterRaster(householdsPath,10000,rasterName = "households_")
  householdsMasterRaster <- toReturn[["masterRaster"]]
  householdsMasterRaster <- createFocalRasters(householdsMasterRaster)
  householdsCountRaster <- toReturn[["countRaster"]][[1]]
  
  setwd("/home/noah/Desktop/NRES 565 Final Run/Results")
  writeRaster(householdsMasterRaster,"households",format="GTiff",bylayer=TRUE,suffix="names",overwrite=TRUE)
  writeRaster(householdsCountRaster,"householdsCounts",format="GTiff",bylayer=TRUE,suffix="names",overwrite=TRUE)
  writeRaster(householdsMasterRaster,filename="householdsMasterRaster.grd",bandorder="BIL",overwrite=TRUE)
  writeRaster(householdsCountRaster,filename="householdsCounts.grd",bandorder="BIL",overwrite=TRUE)
  
  
  toReturn <- createMasterRaster(individualsPath,10000,rasterName = "individuals_")
  individualsMasterRaster <- toReturn[["masterRaster"]]
  individualsMasterRaster <- createFocalRasters(individualsMasterRaster)
  individualsCountRaster <- toReturn[["countRaster"]][[1]]
  
  setwd("/home/noah/Desktop/NRES 565 Final Run/Results")
  writeRaster(individualsMasterRaster,"individuals",format="GTiff",bylayer=TRUE,suffix="names",overwrite=TRUE)
  writeRaster(individualsCountRaster,"individualsCounts",format="GTiff",bylayer=TRUE,suffix="names",overwrite=TRUE)
  writeRaster(individualsMasterRaster,filename="individualsMasterRaster.grd",bandorder="BIL",overwrite=TRUE)
  writeRaster(individualsCountRaster,filename="individualsCounts.grd",bandorder="BIL",overwrite=TRUE)
  
  toReturn <- list()
  toReturn[["householdsMasterRaster"]] <- householdsMasterRaster
  toReturn[["householdsCountRaster"]] <- householdsCountRaster
  toReturn[["individualsMasterRaster"]] <- individualsMasterRaster
  toReturn[["individualsCountRaster"]] <- individualsCountRaster
  
  return(toReturn)
}

createFocalRasters <- function(masterRaster)
{
  # create the focal rastesr and add to the masterRaster
  for (name in names(masterRaster))
  {
    rasterLayer <- masterRaster[[name]]
    w <- matrix(rep(1,9),ncol=3,nrow=3)
    
    meanName <- paste(name,"_mean",sep="")
    meanFocal <- focal(rasterLayer,w,fun=mean,na.rm=TRUE,pad=TRUE,padvalue=NA)
    masterRaster[[meanName]] <- meanFocal
    
    minName <- paste(name,"_min",sep="")
    minFocal <- focal(rasterLayer,w,fun=min,na.rm=TRUE,pad=TRUE,padvalue=NA)
    masterRaster[[minName]] <- minFocal
    
    maxName <- paste(name,"_max",sep="")
    maxFocal <- focal(rasterLayer,w,fun=max,na.rm=TRUE,pad=TRUE,padvalue=NA)
    masterRaster[[maxName]] <- maxFocal
  }
  return(masterRaster)
}

createRegressionTree <- function()
{
  # create the dataframe from the raster brick
  df <- as.data.frame(householdsMasterRaster,xy=TRUE)
  
  # create a rasterbrick from the dataframe
  dfToRast <- rasterFromXYZ(df)
  
  # compare the rasters to confirm they're equal
  compareRaster(dfToRast,householdsMasterRaster)
  
  # create the regression
  formula = households_PERSONS ~ .
  fit <- rpart(formula,df) # remove method, replace with anova
  rpart.plot(fit)  
  
  # test the data
  testData <- df[10,]


  
  
  # glm version
  #build a model, here an example with glm 
  model <- glm(formula=PERSONS ~URBAN+OWNRSHP+NFAMS, data=df)
  
  #predict to a raster
  r_glm <- predict(householdsMasterRaster, model, progress='text')
  
  
  # random forest version  
  rfmod <- randomForest(households_PERSONS ~households_URBAN+households_OWNRSHP+households_NFAMS, data=df)
  r_randomforest <- predict(householdsMasterRaster,rfmod, type='response', progress='window')
}

testRasterCalc <- function(fit,masterRaster)
{
  calc_function <- function(c)
  {
    # print(c)
    if (is.data.frame(c))
    {
      # c_prediction <- predict(fit,c, na.exclude(c))
      c_prediction <- predict(fit,c)
      
      print(c_prediction)
      return(c_prediction)
    }
  }
  
  prediction_raster <- calc(masterRaster,calc_function)
  
}

oldCalcFailures <- function()
{
predfun <- function(model,data)
{
  print(data)
  prediction <- predict(model,data,type="vector")
  print(prediction)
  return(prediction)	
}

prediction_raster <- calc(masterRaster,fit,fun=predfun)

predCell <- function(fit,cell)
{
  print(cell)
  prediction <- predict(fit,cell,type="vector")
  return(prediction)
}

prediction_raster <- calc(masterRaster,fun=predCell)

doPred <- function(x)
{
  print(x)
  prediction <- predict(fit,x)
  return(prediction)
}

pred <- calc(masterRaster,doPred)

horrible <- function()
{
  studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/smaller_sa/w001001.adf"
  emptyRaster <- raster(studyExtentRasterPath)
  df <- as.data.frame(masterRaster,xy=TRUE)
  
}
}

convertCategoricalVariablesToBoolean <- function(df,colName,listOfCategories)
{
  for (category in names(listOfCategories))
  {
    maskValue <- listOfCategories[[category]]
    categoryColName <- paste(colName,"_",category,sep="")
    df[[categoryColName]] <- df[[colName]] == maskValue
    df[[categoryColName]] <- df[[categoryColName]] * 1
  }
  return(df)
}

testConvertCategorical <- function()
{
  exampleList <- list()
  exampleList[["Category_1"]] <- 1
  exampleList[["Category_2"]] <- 2
  
  urbanList <- list()
  urbanList[["Rural"]] <- 1
  urbanList[["Urban"]] <- 2
  pts2 <- convertCategoricalVariablesToBoolean(pts,"URBAN",urbanList)
}

categoricalList <- list()

categoricalList[["SEX"]] <- list()
  categoricalList[["SEX"]][["Male"]] <- 1
  categoricalList[["SEX"]][["Female"]] <- 2
categoricalList[["SCHOOL"]] <- list()
  categoricalList[["SCHOOL"]][["Yes"]] <- 
  categoricalList[["SCHOOL"]][["No_not_specified"]] <- 2
  categoricalList[["SCHOOL"]][["No_attended_in_the_past"]] <- 3
  categoricalList[["SCHOOL"]][["No_never_attended"]] <- 4
categoricalList[["RELIG"]] <- list()
  categoricalList[["RELIG"]][["No_religion"]] <- 1
  categoricalList[["RELIG"]][["Buddhist"]] <- 2
  categoricalList[["RELIG"]][["Hindu"]] <- 3
  categoricalList[["RELIG"]][["Jewish"]] <- 4
  categoricalList[["RELIG"]][["Muslim"]] <- 5
  categoricalList[["RELIG"]][["Christian"]] <- 6
  categoricalList[["RELIG"]][["Other"]] <- 7
categoricalList[["IND"]] <- list()
  categoricalList[["IND"]][["Agriculture"]] <- 1
  categoricalList[["IND"]][["Industry"]] <- 2
  categoricalList[["IND"]][["Services"]] <- 3
categoricalList[["MARST"]] <- list()
  categoricalList[["MARST"]][["Single_never_married"]] <- 1
  categoricalList[["MARST"]][["Married"]] <- 2
  categoricalList[["MARST"]][["Separated"]] <- 3
  categoricalList[["MARST"]][["Widowed"]] <- 4
categoricalList[["EMPSTAT"]] <- list()
  categoricalList[["EMPSTAT"]][["Employed"]] <- 1
  categoricalList[["EMPSTAT"]][["Unemployed"]] <- 2
  categoricalList[["EMPSTAT"]][["Inactive"]] <- 3
categoricalList[["EDATTAN"]] <- list()
  categoricalList[["EDATTAN"]][["Less_than_primary_completed"]] <- 1
  categoricalList[["EDATTAN"]][["Primary_completed"]] <- 2
  categoricalList[["EDATTAN"]][["Secondary_completed"]] <- 3
  categoricalList[["EDATTAN"]][["University_completed"]] <- 4
categoricalList[["LIT"]] <- list()
  categoricalList[["LIT"]][["Illiterate"]] <- 1
  categoricalList[["LIT"]][["Literate"]] <- 2


categoricalList[["URBAN"]] <- list()
  categoricalList[["URBAN"]][["Rural"]] <- 1
  categoricalList[["URBAN"]][["Urban"]] <- 2
categoricalList[["OWNRSHP"]] <- list()
  categoricalList[["OWNRSHP"]][["Owned"]] <- 1
  categoricalList[["OWNRSHP"]][["Not_owned"]] <- 2
categoricalList[["OWNRSHPD"]] <- list()
  categoricalList[["OWNRSHPD"]][["Renting_government"]] <- 211
  categoricalList[["OWNRSHPD"]][["Renting_local_authority"]] <- 212
  categoricalList[["OWNRSHPD"]][["Renting_parastatal"]] <- 213
  categoricalList[["OWNRSHPD"]][["Renting_collective"]] <- 217
  categoricalList[["OWNRSHPD"]][["Renting_public_subsidized"]] <- 219
  categoricalList[["OWNRSHPD"]][["Renting_private_subsidized"]] <- 220
  categoricalList[["OWNRSHPD"]][["Renting_cooperative"]] <- 223
  categoricalList[["OWNRSHPD"]][["Renting_with_a_job_or_business"]] <- 224
  categoricalList[["OWNRSHPD"]][["Sharecropping"]] <- 228
  categoricalList[["OWNRSHPD"]][["Squatting"]] <- 240
  categoricalList[["OWNRSHPD"]][["Free"]] <- 250
  categoricalList[["OWNRSHPD"]][["Free_provided_by_employer"]] <- 251
  categoricalList[["OWNRSHPD"]][["Free_public"]] <- 255
  categoricalList[["OWNRSHPD"]][["Free_condemned"]] <- 256
categoricalList[["HHTYPE"]] <- list()
  categoricalList[["HHTYPE"]][["Vacant_hosuehold"]] <- 0
  categoricalList[["HHTYPE"]][["One_person_household"]] <- 1
  categoricalList[["HHTYPE"]][["Couple_no_children"]] <- 2
  categoricalList[["HHTYPE"]][["Couple_children"]] <- 3
  categoricalList[["HHTYPE"]][["Single_parent_family"]] <- 4
  categoricalList[["HHTYPE"]][["Polygamous_family"]] <- 5
  categoricalList[["HHTYPE"]][["Extended_family"]] <- 6
  categoricalList[["HHTYPE"]][["Composite_household"]] <- 7
  categoricalList[["HHTYPE"]][["Non_family_household"]] <- 8
  categoricalList[["HHTYPE"]][["Unclassified_subfamily"]] <- 9
  categoricalList[["HHTYPE"]][["Other_relative_or_non_relative_household"]] <- 10
  categoricalList[["HHTYPE"]][["Group_quarters"]] <- 11
categoricalList[["GQ"]] <- list()
  categoricalList[["GQ"]][["Vacant"]] <- 00
  categoricalList[["GQ"]][["Households"]] <- 10
  categoricalList[["GQ"]][["Group_quarters"]] <- 20
  categoricalList[["GQ"]][["Institutions"]] <- 21
  categoricalList[["GQ"]][["Other_group_quarters"]] <- 22
  categoricalList[["GQ"]][["One_person_unit_created_by_splitting_large_househodl"]] <- 29
categoricalList[["ELECTRC"]] <- list()
  categoricalList[["ELECTRC"]][["Yes"]] <- 1
  categoricalList[["ELECTRC"]][["No"]] <- 2
categoricalList[["BD11A_WATSRC"]] <- list()
  categoricalList[["BD11A_WATSRC"]][["Tap"]] <- 1
  categoricalList[["BD11A_WATSRC"]][["Tube_well"]] <- 2
  categoricalList[["BD11A_WATSRC"]][["Other"]] <- 3
categoricalList[["BD11A_TOILET"]] <- list()
  categoricalList[["BD11A_TOILET"]][["Sanitary_with_water_seal"]] <- 1
  categoricalList[["BD11A_TOILET"]][["Sanitary_no_water_seal"]] <- 2
  categoricalList[["BD11A_TOILET"]][["Non_sanitary"]] <- 3
  categoricalList[["BD11A_TOILET"]][["None"]] <- 4
categoricalList[["BD11A_FLOATPOP"]] <- list()
  categoricalList[["BD11A_FLOATPOP"]][["Yes"]] <- 1
  categoricalList[["BD11A_FLOATPOP"]][["No"]] <- 2
categoricalList[["BD11A_ETHPOPHH"]] <- list()
  categoricalList[["BD11A_ETHPOPHH"]][["Yes"]] <- 1
  categoricalList[["BD11A_ETHPOPHH"]][["No"]] <- 2
categoricalList[["BD11A_ETHNHH"]] <- list()
  categoricalList[["BD11A_ETHNHH"]][["Chakma"]] <- 01
  categoricalList[["BD11A_ETHNHH"]][["Marma"]] <- 02
  categoricalList[["BD11A_ETHNHH"]][["Tripura"]] <- 03
  categoricalList[["BD11A_ETHNHH"]][["Mro"]] <- 04
  categoricalList[["BD11A_ETHNHH"]][["Tanchaynga"]] <- 05
  categoricalList[["BD11A_ETHNHH"]][["Bawm"]] <- 06
  categoricalList[["BD11A_ETHNHH"]][["Chak"]] <- 08
  categoricalList[["BD11A_ETHNHH"]][["Khiyang"]] <- 09
  categoricalList[["BD11A_ETHNHH"]][["Khumi"]] <- 10
  categoricalList[["BD11A_ETHNHH"]][["Coach"]] <- 12
  categoricalList[["BD11A_ETHNHH"]][["Sawntal"]] <- 13
  categoricalList[["BD11A_ETHNHH"]][["Rakhain"]] <- 16
  categoricalList[["BD11A_ETHNHH"]][["Monipuri"]] <- 17
  categoricalList[["BD11A_ETHNHH"]][["Garo"]] <- 18
  categoricalList[["BD11A_ETHNHH"]][["Hajong"]] <- 19
  categoricalList[["BD11A_ETHNHH"]][["Khasia"]] <- 20
  categoricalList[["BD11A_ETHNHH"]][["Orao"]] <- 22
  categoricalList[["BD11A_ETHNHH"]][["Barmon"]] <- 23
  categoricalList[["BD11A_ETHNHH"]][["Pahari"]] <- 24
  categoricalList[["BD11A_ETHNHH"]][["Malpahari"]] <- 25
  categoricalList[["BD11A_ETHNHH"]][["Monda"]] <- 26
  categoricalList[["BD11A_ETHNHH"]][["Cool"]] <- 27
  categoricalList[["BD11A_ETHNHH"]][["Other"]] <- 28
categoricalList[["BD11A_ELECTRC"]] <- list()
  categoricalList[["BD11A_ELECTRC"]][["Yes"]] <- 1
  categoricalList[["BD11A_ELECTRC"]][["No"]] <- 2





recreateMasterRaster <- function(dataset="households",folder="/home/noah/Desktop/Upload to Drive")
{
  setwd(folder)
  
  filePattern <- paste(dataset,"_",".*","tif",sep="")
  filePaths <- list.files(pattern = filePattern)
  rasters <- c()
  
  for (fileName in filePaths)
  {
    if (!grepl("aux",fileName))
    {
      rast <- raster(fileName)
      rasters <- c(rasters,rast)
    }
  }
  masterRaster <- stack(rasters)
  
  return(masterRaster)
}

clairesWinningIdea <- function(masterRaster,fit)
{
  studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/sa_200m/w001001x.adf"
  emptyRaster <- raster(studyExtentRasterPath)
  res(emptyRaster) <- c(800,800)
  ncells <- dim(masterRaster)[1] * dim(masterRaster)[2]
  xs <- c()
  ys <- c()
  preds <- c()
  locs <- 1:ncells
  for (i in 1:ncells)
  {
    df <- as.data.frame(masterRaster[i])
    coord <- coordinates(masterRaster)[i,]
    x <- coord[1]
    xs <- c(xs,x)
    y <- coord[2]
    ys <- c(ys,y)
    pred <- predict(fit,df)
    delta <- 0.5
    predRandomized <- runif(1,pred-delta,pred+delta)
    preds <- c(preds,predRandomized)
  }
  spdf <- data.frame(x=xs,y=ys,pred=preds)
  coordinates(spdf)=~x+y
  predictionRaster <- rasterize(spdf,emptyRaster,fun=mean)
  predictionRaster <- predictionRaster[["pred"]]
  return(predictionRaster)
}

doSimulation <- function(masterRaster,growthFit,elecFit,nYears,annualPercentGrowth=0.012)
{
  growthLayer <- "growth"
  elecLayer <- "elec"
  growthEstimates_Yearly <- c()
  elecEstimates_Yearly <- c()
  
  for (year in 1:nYears)
  {
    # 1. Growth Estimate
    growthEstimate <- clairesWinningIdea(masterRaster,growthFit)
    growthEstimateSum <- cellStats(growthEstimate,sum)
    growthEstimate_Standardized <- growthEstimate / growthEstimateSum
    growthEstimate_Population = growthEstimate_Standardized * annualPercentGrowth + 1
    growthEstimate_NewPopulation <- growthEstimate_Population * masterRaster[[growthLayer]]
    growthEstimate_NewPopulation[[growthEstimate_NewPopulation < 0]] <- 0
    masterRaster[[growthLayer]] <- growthEstimate_NewPopulation
    
    # 2. Electricity Estimate
    elecEstimate <- clairesWinningIdea(masterRaster,elecFit)
    masterRaster[[elecLayer]] <- elecEstimate
    
    # 3. Save each year's results so we can make a slide show
    growthEstimates_Yearly <- c(growthEstimates_Yearly,growthEstimate_NewPopulation)
    elecEstimates_Yearly <- c(elecEstimates_Yearly,elecEstimate)
  }
  growthEstimates_Yearly_Brick <- brick(growthEstimates_Yearly)
  elecEstimates_Yearly_Brick <- brick(elecEstimates_Yearly)
  toReturn <- list()
  toReturn[["masterRaster"]] <- masterRaster
  toReturn[["growthEstimates_Yearly_Brick"]] <- growthEstimates_Yearly_Brick
  toReturn[["elecEstimates_Yearly_Brick"]] <- elecEstimates_Yearly_Brick
  return(toReturn)
}

monteCarlo <- function(masterRaster,growthFit,elecFit,nYears,annualPercentGrowth=0.012,nSimulations=1000)
{
  growthLayer <- "growth"
  elecLayer <- "elec"
  growthEstimates_Simulation_Final <- list()
  elecEstimates_Simulation_Final <- list()
  for (sim in 1:nSimulations-1)
  {
    toReturn <- doSimulation(masterRaster,growthFit,elecFit,nYears,annualPercentGrowth)
    growthEstimate <- toReturn[["masterRaster"]][[growthLayer]]
    elecEstimate <- toReturn[["masterRaster"]][[elecLayer]]
    growthEstimates_Simulation_Final[[sim]] <- growthEstimate
    elecEstimates_Simulation_Final[[sim]] <- elecEstimate
  }
  toReturn <- list()
  toReturn[["growth"]] <- growthEstimates_Simulation_Final # may want to save instead
  toReturn[["elec"]] <- elecEstimates_Simulation_Final # may want to save instead
  return(toReturn)
}
  
applyDistributionToRaster <- function(raster,distribution,distParameters)
{
  return(NULL)
}

getAllRasterNames <- function()
{
  householdsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon"
  individualsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon/Raw"
  
  toReturn <- createMasterRaster(householdsPath,1,rasterName = "households_")
  householdsMasterRaster <- toReturn[["masterRaster"]]
  householdsMasterRaster <- createFocalRasters(householdsMasterRaster)
  householdsCountRaster <- toReturn[["countRaster"]][[1]]
  names(householdsCountRaster) <- c("households_density")
  
  householdsCombinedRaster <- brick(c(householdsMasterRaster,householdsCountRaster))
  householdNames <- names(householdsCombinedRaster)

  toReturn <- createMasterRaster(individualsPath,1,rasterName = "individuals_")
  individualsMasterRaster <- toReturn[["masterRaster"]]
  individualsMasterRaster <- createFocalRasters(individualsMasterRaster)
  individualsCountRaster <- toReturn[["countRaster"]][[1]]
  names(individualsCountRaster) <- c("individuals_density")
  
  individualsCombinedRaster <- brick(c(individualsMasterRaster,individualsCountRaster))
  individualNames <- names(individualsCombinedRaster)
  
  combinedMasterRaster <- brick(c(householdsMasterRaster,individualsMasterRaster,householdsCountRaster,individualsCountRaster))

  columnNames <- names(combinedMasterRaster)
  setwd("/home/noah/Desktop/NRES 565 Final Run")
  write(columnNames,file="allFileNames.txt")
  write(householdNames,file="householdColumns.txt")
  write(individualNames,file="individualColumns.txt")
  
  
  return(toReturn)
}

readVariableNamesAndSubsetMasterRaster <- function(householdMasterRaster,individualMasterRaster)
{
  setwd("/home/noah/Desktop/NRES 565 Final Run")
  
  households <- scan("householdColumnsToKeep.txt", what="", sep="\n") # courtesy http://stackoverflow.com/questions/6602881/text-file-to-list-in-r
  individuals <- scan("individualColumsToKeep.txt", what="", sep="\n")
  
  householdsSubset <- householdMasterRaster[[households]]
  individualsSubset <- individualMasterRaster[[individuals]]
  
  masterRaster <- brick(c(householdsSubset,individualsSubset))
  return(masterRaster)
}

alignRasters <- function(rast,ext)
{
  
  returnRaster <- resample(rast,ext,method="ngb")
  
  
  returnRaster <- extend(rast, ext, value=NA)
  return(returnRaster)
}

convertToReturnToMasterRaster <- function(toReturn)
{
  households <- toReturn$householdsMasterRaster
  householdsCounts <- toReturn$householdsCountRaster
  individuals <- toReturn$individualsMasterRaster
  individualsCounts <- toReturn$individualsCountRaster
  populationLayerSum <- cellStats(individualsCounts,sum)
  attractivenessLayer <- individualsCounts / populationLayerSum
  names(attractivenessLayer) <- c("growth")
  populationLayer <- individualsCounts
  names(populationLayer) <- c("population")
  masterRaster <- brick(c(households,individuals,attractivenessLayer,populationLayer))
  return(masterRaster)
}


run <- function(masterRaster,nYears,annualPercentGrowth=0.012)
{
  t1 <- Sys.time()
  startYear <- 2011
  growthLayer <- "growth"
  elecLayer <- "ELECTRC_Yes"  
  populationLayer <- "population"
  growthEstimates_Yearly_tree <- c()
  elecEstimates_Yearly_tree <- c()
  growthEstimates_Yearly_lm <- c()
  elecEstimates_Yearly_lm <- c()
  populationEstimates_Yearly_tree <- c()
  years <- c()
  
  print("Pre-processing the masterRaster")
  
  # remove the rasters which are all na or all inf
  allGood <- checkIfAllNA(masterRaster)
  masterRaster <- masterRaster[[allGood]]
  
  # remove any layers from masterRaster which are categorical variables, since they don't mean much in a regression and have been replaced
  masterRaster <- removeCategoricalLayers(masterRaster)
  
  # replace any cells with na or inf in the remaining raster with the average for the raster as a whole
  masterRaster <- replaceNACellsWithAverage(masterRaster)
  
  # build the fits
  df <- as.data.frame(masterRaster,xy=TRUE)
  
  dt <-  difftime(Sys.time(),t1,units="min")
  print(paste("Finished pre-processing the masterRaster in ",dt,sep=""))
  t1 <- Sys.time()
  
  # create seperate copies of the master raster for the analyses
  masterRaster_tree <- masterRaster
  masterRaster_lm <- masterRaster
  
  print("Generating the regression trees")
  
  # growth
  # formula_growth <- paste(growthLayer,"~.",sep="")
  growthVars <- c()
  for (var in names(df))
  {
    if (is.na(pmatch("pop",var)))
    {
      growthVars <- c(growthVars,var)
    }
  }
  # learned from http://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
  formula_growth <- paste(growthLayer,"~",paste(growthVars,collapse="+"))
  fit_growth_tree <- rpart(formula_growth,df,minsplit=10)
  
  # electric
  # formula_elec <- paste(elecLayer,"~.",sep="")
  # formula_elec <- paste(elecLayer,"~",growthLayer,"+ URBAN_Urban + OWNRSHP_Owned + EMPSTAT_Employed_mean + EMPSTAT_Employed_min + EMPSTAT_Employed_max",sep="")
  elecVars <- c()
  for (var in names(df))
  {
    if (is.na(pmatch("ELEC",var)))
    {
      if (is.na(pmatch("BD11A_EL",var)))
      {
        elecVars <- c(elecVars,var)
      }
    }
  }
  # learned from http://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
  formula_elec <- paste(elecLayer,"~",paste(elecVars,collapse="+"))
  fit_elec_tree <- rpart(formula_elec,df)
  
  dt <-  difftime(Sys.time(),t1,units="min")
  print(paste("Finished generating regression trees in ",dt,sep=""))
  t1 <- Sys.time()
  
  # lm
  print("Generating the lm models")
  
  notHaveNA <- checkForNA(masterRaster)

  # 1. growth model
  notHaveNA_filtered <- c()
  for (col in notHaveNA)
  {
    if (col != growthLayer)
    {
      if (is.na(pmatch("pop",col)))
      {
        notHaveNA_filtered <- c(notHaveNA_filtered,col)
      }
    }
  }
  formula_growth_lm <- paste(growthLayer,"~",paste(notHaveNA_filtered,collapse="+"))
  fit_growth_lm <- lm(formula_growth_lm,as.data.frame(masterRaster_lm[[notHaveNA]]))
  
  # 2. elec model
  notHaveNA_filtered <- c()
  for (col in notHaveNA)
  {
    if (col != growthLayer)
    {
      if (is.na(pmatch("ELEC",col)))
      {
        if (is.na(pmatch("BD11A_EL",col)))
        {
          notHaveNA_filtered <- c(notHaveNA_filtered,col)
        }
      }
    }
  }
  formula_elec_lm <- paste(elecLayer,"~",paste(notHaveNA_filtered,collapse="+"))
  fit_elec_lm <- lm(formula_elec_lm,as.data.frame(masterRaster_lm[[notHaveNA]]))
  
  dt <-  difftime(Sys.time(),t1,units="min")
  print(paste("Finished generating lm models in ",dt,sep=""))
  tstartsim <- Sys.time()
  
  print("Beginning the simulations")
  for (year in 1:nYears)
  {
    t1 <- Sys.time()
    currentYear <- toString(startYear + year)
    currentYear <- paste("Year: ",currentYear,sep="")
    years <- c(years,currentYear)
    print(currentYear)
    # A. Tree Version
    print("Growth: Tree")
    # 1. Growth Estimate
    growthEstimate <- clairesWinningIdea_ForEach_FasterHopefully(masterRaster_tree,fit_growth_tree)
    growthEstimateSum <- cellStats(growthEstimate,sum)
    growthEstimate_Standardized <- growthEstimate / growthEstimateSum
    totalNumberOfPeeps <- cellStats(masterRaster_tree[[populationLayer]],sum) * annualPercentGrowth
    growthEstimate_Population <- growthEstimate_Standardized * totalNumberOfPeeps
    growthEstimate_NewPopulation <- growthEstimate_Population + masterRaster_tree[[populationLayer]]
    #growthEstimate_Population = growthEstimate_Standardized * (annualPercentGrowth + 1) #TODO, see if this would have worked if you (#noahgarfinkle) weren't a fucking dumbass who doesn't know PEMDAS
    # growthEstimate_NewPopulation <- growthEstimate_Population * masterRaster_tree[[populationLayer]]
    # masterRaster_tree[[populationLayer]] <- growthEstimate_NewPopulation
    # TODO: Update all of the focal values for population
    toChange <- c(populationLayer)
    toChange_Raster <- brick(c(growthEstimate_NewPopulation))
    names(toChange_Raster) <- toChange
    # Run createFocalRasters(raster) on each
    replacementRaster <- createFocalRasters(toChange_Raster)
    # foreach layer in the brick, replace the layer with the same name in masterRaster_tree
    for (layerName in names(replacementRaster))
    {
      masterRaster_tree[[layerName]] <- replacementRaster[[layerName]]
    }
    
    populationEstimate_Yearly_tree <- cellStats(masterRaster_tree[[populationLayer]],sum)
    populationEstimates_Yearly_tree <- c(populationEstimates_Yearly_tree,populationEstimate_Yearly_tree)
    
    dt <-  difftime(Sys.time(),t1,units="min")
    print(paste("Finished modeling growth using tree in ",dt,sep=""))
    t1 <- Sys.time()
    
    print("Elec: Tree")
    # 2. Electricity Estimate
    elecEstimate <- clairesWinningIdea_ForEach_FasterHopefully(masterRaster_tree,fit_elec_tree)
    # TODO: Update all of the focal values for electric yes and electric no
    # update yes and no (no calculated)
    elecEstimate_No <- 1 - elecEstimate
    # list out the rasters we need to change (yes and no)
    toChange <- c("ELECTRC_Yes","ELECTRC_No")
    toChange_Raster <- brick(c(elecEstimate,elecEstimate_No))
    names(toChange_Raster) <- toChange
    # Run createFocalRasters(raster) on each
    replacementRaster <- createFocalRasters(toChange_Raster)
    # foreach layer in the brick, replace the layer with the same name in masterRaster_tree
    for (layerName in names(replacementRaster))
    {
      masterRaster_tree[[layerName]] <- replacementRaster[[layerName]]
    }
    #masterRaster_tree[[elecLayer]] <- elecEstimate
    
    # 3. Save each year's results so we can make a slide show
    growthEstimates_Yearly_tree <- c(growthEstimates_Yearly_tree,growthEstimate_NewPopulation)
    # TODO: calculate percent growth
    elecEstimates_Yearly_tree <- c(elecEstimates_Yearly_tree,elecEstimate)
    
    dt <-  difftime(Sys.time(),t1,units="min")
    print(paste("Finished modeling elec using tree in ",dt,sep=""))
#     t1 <- Sys.time()
#     
# #     # B. LM version
#     print("Growth: LM")
#  # 1. Growth Estimate
#     growthEstimate <- clairesWinningIdea(masterRaster_lm,fit_growth_lm)
#     growthEstimateSum <- cellStats(growthEstimate,sum)
#     growthEstimate_Standardized <- growthEstimate / growthEstimateSum
#     totalNumberOfPeeps <- cellStats(masterRaster_lm[[populationLayer]],sum) * annualPercentGrowth
#     growthEstimate_Population <- growthEstimate_Standardized * totalNumberOfPeeps
#     growthEstimate_NewPopulation <- growthEstimate_Population + masterRaster_lm[[populationLayer]]
#     toChange <- c(populationLayer)
#     toChange_Raster <- brick(c(growthEstimate_NewPopulation))
#     names(toChange_Raster) <- toChange
#     # Run createFocalRasters(raster) on each
#     replacementRaster <- createFocalRasters(toChange_Raster)
#     # foreach layer in the brick, replace the layer with the same name in masterRaster_tree
#     for (layerName in names(replacementRaster))
#     {
#       masterRaster_lm[[layerName]] <- replacementRaster[[layerName]]
#     }
#     
#     dt <-  difftime(Sys.time(),t1,units="min")
#     print(paste("Finished modeling growth using lm in ",dt,sep=""))
#     t1 <- Sys.time()
#     
#     print("Elec: LM")
#     # 2. Electricity Estimate
#     elecEstimate <- clairesWinningIdea_ForEach_FasterHopefully(masterRaster_lm,fit_elec_lm)
#     # TODO: Update all of the focal values for electric yes and electric no
#     # update yes and no (no calculated)
#     elecEstimate_No <- 1 - elecEstimate
#     # list out the rasters we need to change (yes and no)
#     toChange <- c("ELECTRC_Yes","ELECTRC_No")
#     toChange_Raster <- brick(c(elecEstimate,elecEstimate_No))
#     names(toChange_Raster) <- toChange
#     # Run createFocalRasters(raster) on each
#     replacementRaster <- createFocalRasters(toChange_Raster)
#     # foreach layer in the brick, replace the layer with the same name in masterRaster_tree
#     for (layerName in names(replacementRaster))
#     {
#       masterRaster_lm[[layerName]] <- replacementRaster[[layerName]]
#     }
#     
#     dt <-  difftime(Sys.time(),t1,units="min")
#     print(paste("Finished modeling elec using lm in ",dt,sep=""))
#     t1 <- Sys.time()
#     
#     # 3. Save each year's results so we can make a slide show
#     growthEstimates_Yearly_lm <- c(growthEstimates_Yearly_lm,growthEstimate_NewPopulation)
#     elecEstimates_Yearly_lm <- c(elecEstimates_Yearly_lm,elecEstimate)
  }
  dt <-  difftime(Sys.time(),tstartsim,units="min")
  print(paste("Simulations complete in ",dt,sep=""))
  t1 <- Sys.time()
  growthEstimates_Yearly_Brick_tree <- brick(growthEstimates_Yearly_tree)
  names(growthEstimates_Yearly_Brick_tree) <- years
  elecEstimates_Yearly_Brick_tree <- brick(elecEstimates_Yearly_tree)
  names(elecEstimates_Yearly_Brick_tree) <- years
  # growthEstimates_Yearly_Brick_lm <- brick(growthEstimates_Yearly_lm)
  # elecEstimates_Yearly_Brick_lm <- brick(elecEstimates_Yearly_lm)
  runResults <- list()
  runResults[["fit_growth_tree"]] <- fit_growth_tree
  runResults[["fit_elec_tree"]] <- fit_elec_tree
  runResults[["fit_growth_lm"]] <- fit_growth_lm
  runResults[["fit_elec_lm"]] <- fit_elec_lm
  runResults[["masterRaster"]] <- masterRaster
  runResults[["growthEstimates_Yearly_Brick_tree"]] <- growthEstimates_Yearly_Brick_tree
  runResults[["elecEstimates_Yearly_Brick_tree"]] <- elecEstimates_Yearly_Brick_tree
  runResults[["populationEstimates_Yearly_tree"]] <- populationEstimates_Yearly_tree
  # runResults[["growthEstimates_Yearly_Brick_lm"]] <- growthEstimates_Yearly_Brick_lm
  # runResults[["elecEstimates_Yearly_Brick_lm"]] <- elecEstimates_Yearly_Brick_lm
  return(runResults)
}

# TODO:
# 1. pare down the number of variables a bit
# 2. currently, we're only regressing (changing) population and electrc
# Proposal: what if regress every variable against every other variable,
# and thus update everything each run
# 3. Plan: 1) run the full model, updating only 2 variables
#          2) run a small subset of variables (10-20 vars), but regress and update them all
# 4. make sure to omit NA's from df before running lm
#       na.rm, na.omit, na.exclude
# takeaways: 1) purely data-driven model, capable of of 2) being parallelized, 3) simultaneously predicting population and infrastructure
# implications: that we want to better enable planners to utilize the data avaialble to them when predicting the future, not as dependent on hard-coded model

# too early for policy, but can use to replace hard-coded models which take significant investment
# paper due Wednesday at midnight


clairesWinningIdea_ForEach_FasterHopefully <- function(masterRaster,fit)
{
  studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/sa_200m/w001001x.adf"
  emptyRaster <- raster(studyExtentRasterPath)
  res(emptyRaster) <- c(800,800)
  ncells <- dim(masterRaster)[1] * dim(masterRaster)[2]
  locs <- 1:ncells
  
  # Create a cluster using parallel:
  myCluster <- makeCluster(spec=4,type="PSOCK")
  # Register the backend with foreach:
  registerDoParallel(myCluster)
  coord <- coordinates(masterRaster)
  x <- coord[,1]
  y <- coord[,2]
  
  # question to determine: do these match up properly?
  df <- as.data.frame(masterRaster)
  df[["x"]] <- x
  df[["y"]] <- y
  
  indCalc <- function(df,fit,i)
  {
    pred <- predict(fit,df[i,])
#     delta <- cellStats(p)
#     # predRandomized <- runif(1,pred-delta,pred+delta)
#     predRandomized <- pred
    return(pred)
  }
  
  preds <- foreach(i = locs, .packages=c("rpart","stats"), .combine="rbind") %dopar% { indCalc(df,fit,i) }
  delta <- sd(preds)
  randomPreds <- c(rep(0,length(preds)))
  place <- 1
  for (pred in preds)
  {
    predRandomized <- runif(1,pred-delta,pred+delta)
    randomPreds[place] <- predRandomized
    place <- place + 1
  }
  df[["pred"]] <- preds
  spdf <- data.frame(x=df[["x"]],y=df[["y"]],pred=df[["pred"]])
  coordinates(spdf)=~x+y
  predictionRaster <- rasterize(spdf,emptyRaster,fun=mean)
  names(predictionRaster) <- c("ID","pred")
  predictionRaster <- predictionRaster[["pred"]]
  
  stopCluster(myCluster)
  
  return(predictionRaster)
}

processResults <- function(runResults,index1,index2)
{
  r1 <- runResults$growthEstimates_Yearly_Brick_tree[[index1]]
  r2 <- runResults$growthEstimates_Yearly_Brick_tree[[index2]]
  r3 <- r2 - r1
  return(r3)
}

checkForNA <- function(masterRaster)
{
  df <- as.data.frame(masterRaster)
  colNames <- names(df)
  notHaveNA <- c()
  for (colName in colNames)
  {
    if (!any(is.na(df[[colName]])))
    {
      if(!any(is.infinite(df[[colName]])))
      {
        notHaveNA <- c(notHaveNA,colName)
      }
    }
  }
  return(notHaveNA)
}

checkIfAllNA <- function(masterRaster)
{
  df <- as.data.frame(masterRaster)
  allNA <- c()
  allGood <- c()
  for (layer in names(masterRaster))
  {
    if (all(is.na(df[[layer]])))
    {
      allNA <- c(allNA,layer)
    }
    else if (all(is.infinite(df[[layer]])))
    {
      allNA <- c(allNA,layer)
    }
    else
    {
      allGood <- c(allGood,layer)
    }
  }
  return(allGood)
}

replaceNACellsWithAverage <- function(masterRaster)
{
  for (layer in names(masterRaster))
  {
    rast <- masterRaster[[layer]]
    layerAverage <- cellStats(rast,mean)
    rast[is.na(rast)] <- layerAverage
    rast[is.infinite(rast)] <- layerAverage
    masterRaster[[layer]] <- rast
  }
  return(masterRaster)
}

removeCategoricalLayers <- function(masterRaster)
{ 
  layersToExclude <- c()
  for (layer in names(categoricalList))
  {
    meanName <- paste(layer,"_mean",sep="")
    minName <- paste(layer,"_min",sep="")
    maxName <- paste(layer,"_max",sep="")
    layersToExclude <- c(layersToExclude,layer,meanName,minName,maxName)
  }
  
  layersToKeep <- c()
  for (layer in names(masterRaster))
  {
    if (!any(grepl(layer,layersToExclude)))
    {
      layersToKeep <- c(layersToKeep,layer)
    }
  }
  
#   newNames <- c()
#   for (layer in names(masterRaster))
#   {
#     if (is.null(categoricalList[[layer]])) # check if the layer is a categorical variable
#     {
#       newRasters <- c(newRasters,masterRaster[[layer]])
#       newNames <- c(newNames,layer)
#     }
#   }
#   focalsToExclude <- c()
#   for (layer in names(categoricalList))
#   {
#     meanName <- paste(layer,"_mean",sep="")
#     minName <- paste(layer,"_min",sep="")
#     maxName <- paste(layer,"_max",sep="")
#     focalsToExclude <- c(focalsToExclude,meanName,minName,maxName)
#   }
#   finalNames <- c()
#   for (layer in newNames)
#   {
#     
#   }

  masterRaster <- masterRaster[[layersToKeep]]
  return(masterRaster)
}