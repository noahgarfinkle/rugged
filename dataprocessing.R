require(raster)
require(sp)
require(rgdal)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(randomForest)


path <- "/home/noah/Desktop/Data From Chuck/From Dragon"
setwd(path)

studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/smaller_sa/w001001.adf"
studyExtentRaster <- raster(studyExtentRasterPath)

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
  studyExtentRasterPath <- "/home/noah/Desktop/NRES 565 Data/Dhaka_Data/smaller_sa/w001001.adf"
  studyExtentRaster <- raster(studyExtentRasterPath)
  ext <- extent(studyExtentRaster)
  
  # process the first file, because it is the only one with a header
  fileName <- "xaa"
  pts <- read.csv(fileName,header=TRUE)
  header <- colnames(pts)
  coordinates(pts)=~x+y
  rast<-raster()
  extent(rast) <- ext
  ncol(rast) <- 13
  nrow(rast) <- 12
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
      rast<-raster()
      extent(rast) <- ext
      ncol(rast) <- 13
      nrow(rast) <- 12
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
      names(masterRaster) <- layerNames
      filesCount <- filesCount + 1
      if (filesCount >= maxFiles)
      {
        return(masterRaster)
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
  return(masterRaster)
} 

createRasters <- function()
{
  householdsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon"
  individualsPath <- "/home/noah/Desktop/Data From Chuck/From Dragon/Raw"
  
  householdsMasterRaster <- createMasterRaster(householdsPath,10000,rasterName = "households_")
  writeRaster(householdsMasterRaster,"households",format="GTiff",bylayer=TRUE,suffix="names")
  
  individualsMasterRaster <- createMasterRaster(individualsPath,10000,rasterName = "individuals_")
  writeRaster(individualsMasterRaster,"individuals",format="GTiff",bylayer=TRUE,suffix="names")
  
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
  formula = PERSONS ~ .
  fit <- rpart(formula,method="class",df) # remove method, replace with anova
  rpart.plot(fit)  
  
  # test the data
  testData <- df[10,]


  
  
  # glm version
  #build a model, here an example with glm 
  model <- glm(formula=PERSONS ~URBAN+OWNRSHP+NFAMS, data=df)
  
  #predict to a raster
  r_glm <- predict(householdsMasterRaster, model, progress='text')
  
  
  # random forest version  
  rfmod <- randomForest(PERSONS ~URBAN+OWNRSHP+NFAMS, data=df)
  r_randomforest <- predict(householdsMasterRaster,rfmod, type='response', progress='window')
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

# categoricalList <- list()
# 
# categoricalList[["individuals_SEX"]] <- list()
#   categoricalList[["individuals_SEX"]][["Male"]] <- 1
#   categoricalList[["individuals_SEX"]][["Female"]] <- 2
# categoricalList[["individuals_SCHOOL"]] <- list()
#   categoricalList[["individuals_SCHOOL"]][["Yes"]] <- 
#   categoricalList[["individuals_SCHOOL"]][["No_not_specified"]] <- 2
#   categoricalList[["individuals_SCHOOL"]][["No_attended_in_the_past"]] <- 3
#   categoricalList[["individuals_SCHOOL"]][["No_never_attended"]] <- 4
# categoricalList[["individuals_RELIG"]] <- list()
#   categoricalList[["individuals_RELIG"]][["No_religion"]] <- 1
#   categoricalList[["individuals_RELIG"]][["Buddhist"]] <- 2
#   categoricalList[["individuals_RELIG"]][["Hindu"]] <- 3
#   categoricalList[["individuals_RELIG"]][["Jewish"]] <- 4
#   categoricalList[["individuals_RELIG"]][["Muslim"]] <- 5
#   categoricalList[["individuals_RELIG"]][["Christian"]] <- 6
#   categoricalList[["individuals_RELIG"]][["Other"]] <- 7
# categoricalList[["individuals_IND"]] <- list()
#   categoricalList[["individuals_IND"]][["Agriculture"]] <- 1
#   categoricalList[["individuals_IND"]][["Industry"]] <- 2
#   categoricalList[["individuals_IND"]][["Services"]] <- 3
# categoricalList[["individuals_MARST"]] <- list()
#   categoricalList[["individuals_MARST"]][["Single_never_married"]] <- 1
#   categoricalList[["individuals_MARST"]][["Married"]] <- 2
#   categoricalList[["individuals_MARST"]][["Separated"]] <- 3
#   categoricalList[["individuals_MARST"]][["Widowed"]] <- 4
# categoricalList[["individuals_EMPSTAT"]] <- list()
#   categoricalList[["individuals_EMPSTAT"]][["Employed"]] <- 1
#   categoricalList[["individuals_EMPSTAT"]][["Unemployed"]] <- 2
#   categoricalList[["individuals_EMPSTAT"]][["Inactive"]] <- 3
# categoricalList[["individuals_EDATTAN"]] <- list()
#   categoricalList[["individuals_EDATTAN"]][["Less_than_primary_completed"]] <- 1
#   categoricalList[["individuals_EDATTAN"]][["Primary_completed"]] <- 2
#   categoricalList[["individuals_EDATTAN"]][["Secondary_completed"]] <- 3
#   categoricalList[["individuals_EDATTAN"]][["University_completed"]] <- 4
# categoricalList[["individuals_LIT"]] <- list()
#   categoricalList[["individuals_LIT"]][["Illiterate"]] <- 1
#   categoricalList[["individuals_LIT"]][["Literate"]] <- 2
#   
# 
# categoricalList[["households_URBAN"]] <- list()
#   categoricalList[["households_URBAN"]][["Rural"]] <- 1
#   categoricalList[["households_URBAN"]][["Urban"]] <- 2
# categoricalList[["households_OWNRSHP"]] <- list()
#   categoricalList[["households_OWNRSHP"]][["Owned"]] <- 1
#   categoricalList[["households_OWNRSHP"]][["Not_owned"]] <- 2
# categoricalList[["households_OWNRSHPD"]] <- list()
#   categoricalList[["households_OWNRSHPD"]][["Renting_government"]] <- 211
#   categoricalList[["households_OWNRSHPD"]][["Renting_local_authority"]] <- 212
#   categoricalList[["households_OWNRSHPD"]][["Renting_parastatal"]] <- 213
#   categoricalList[["households_OWNRSHPD"]][["Renting_collective"]] <- 217
#   categoricalList[["households_OWNRSHPD"]][["Renting_public_subsidized"]] <- 219
#   categoricalList[["households_OWNRSHPD"]][["Renting_private_subsidized"]] <- 220
#   categoricalList[["households_OWNRSHPD"]][["Renting_cooperative"]] <- 223
#   categoricalList[["households_OWNRSHPD"]][["Renting_with_a_job_or_business"]] <- 224
#   categoricalList[["households_OWNRSHPD"]][["Sharecropping"]] <- 228
#   categoricalList[["households_OWNRSHPD"]][["Squatting"]] <- 240
#   categoricalList[["households_OWNRSHPD"]][["Free"]] <- 250
#   categoricalList[["households_OWNRSHPD"]][["Free_provided_by_employer"]] <- 251
#   categoricalList[["households_OWNRSHPD"]][["Free_public"]] <- 255
#   categoricalList[["households_OWNRSHPD"]][["Free_condemned"]] <- 256
# categoricalList[["households_HHTYPE"]] <- list()
#   categoricalList[["households_HHTYPE"]][["Vacant_hosuehold"]] <- 0
#   categoricalList[["households_HHTYPE"]][["One_person_household"]] <- 1
#   categoricalList[["households_HHTYPE"]][["Couple_no_children"]] <- 2
#   categoricalList[["households_HHTYPE"]][["Couple_children"]] <- 3
#   categoricalList[["households_HHTYPE"]][["Single_parent_family"]] <- 4
#   categoricalList[["households_HHTYPE"]][["Polygamous_family"]] <- 5
#   categoricalList[["households_HHTYPE"]][["Extended_family"]] <- 6
#   categoricalList[["households_HHTYPE"]][["Composite_household"]] <- 7
#   categoricalList[["households_HHTYPE"]][["Non_family_household"]] <- 8
#   categoricalList[["households_HHTYPE"]][["Unclassified_subfamily"]] <- 9
#   categoricalList[["households_HHTYPE"]][["Other_relative_or_non_relative_household"]] <- 10
#   categoricalList[["households_HHTYPE"]][["Group_quarters"]] <- 11
# categoricalList[["households_GQ"]] <- list()
#   categoricalList[["households_GQ"]][["Vacant"]] <- 00
#   categoricalList[["households_GQ"]][["Households"]] <- 10
#   categoricalList[["households_GQ"]][["Group_quarters"]] <- 20
#   categoricalList[["households_GQ"]][["Institutions"]] <- 21
#   categoricalList[["households_GQ"]][["Other_group_quarters"]] <- 22
#   categoricalList[["households_GQ"]][["One_person_unit_created_by_splitting_large_househodl"]] <- 29
# categoricalList[["households_ELECTRC"]] <- list()
#   categoricalList[["households_ELECTRC"]][["Yes"]] <- 1
#   categoricalList[["households_ELECTRC"]][["No"]] <- 2
# categoricalList[["households_BD11A_WATSRC"]] <- list()
#   categoricalList[["households_BD11A_WATSRC"]][["Tap"]] <- 1
#   categoricalList[["households_BD11A_WATSRC"]][["Tube_well"]] <- 2
#   categoricalList[["households_BD11A_WATSRC"]][["Other"]] <- 3
# categoricalList[["households_BD11A_TOILET"]] <- list()
#   categoricalList[["households_BD11A_TOILET"]][["Sanitary_with_water_seal"]] <- 1
#   categoricalList[["households_BD11A_TOILET"]][["Sanitary_no_water_seal"]] <- 2
#   categoricalList[["households_BD11A_TOILET"]][["Non_sanitary"]] <- 3
#   categoricalList[["households_BD11A_TOILET"]][["None"]] <- 4
# categoricalList[["households_BD11A_FLOATPOP"]] <- list()
#   categoricalList[["households_BD11A_FLOATPOP"]][["Yes"]] <- 1
#   categoricalList[["households_BD11A_FLOATPOP"]][["No"]] <- 2
# categoricalList[["households_BD11A_ETHPOPHH"]] <- list()
#   categoricalList[["households_BD11A_ETHPOPHH"]][["Yes"]] <- 1
#   categoricalList[["households_BD11A_ETHPOPHH"]][["No"]] <- 2
# categoricalList[["households_BD11A_ETHNHH"]] <- list()
#   categoricalList[["households_BD11A_ETHNHH"]][["Chakma"]] <- 01
#   categoricalList[["households_BD11A_ETHNHH"]][["Marma"]] <- 02
#   categoricalList[["households_BD11A_ETHNHH"]][["Tripura"]] <- 03
#   categoricalList[["households_BD11A_ETHNHH"]][["Mro"]] <- 04
#   categoricalList[["households_BD11A_ETHNHH"]][["Tanchaynga"]] <- 05
#   categoricalList[["households_BD11A_ETHNHH"]][["Bawm"]] <- 06
#   categoricalList[["households_BD11A_ETHNHH"]][["Chak"]] <- 08
#   categoricalList[["households_BD11A_ETHNHH"]][["Khiyang"]] <- 09
#   categoricalList[["households_BD11A_ETHNHH"]][["Khumi"]] <- 10
#   categoricalList[["households_BD11A_ETHNHH"]][["Coach"]] <- 12
#   categoricalList[["households_BD11A_ETHNHH"]][["Sawntal"]] <- 13
#   categoricalList[["households_BD11A_ETHNHH"]][["Rakhain"]] <- 16
#   categoricalList[["households_BD11A_ETHNHH"]][["Monipuri"]] <- 17
#   categoricalList[["households_BD11A_ETHNHH"]][["Garo"]] <- 18
#   categoricalList[["households_BD11A_ETHNHH"]][["Hajong"]] <- 19
#   categoricalList[["households_BD11A_ETHNHH"]][["Khasia"]] <- 20
#   categoricalList[["households_BD11A_ETHNHH"]][["Orao"]] <- 22
#   categoricalList[["households_BD11A_ETHNHH"]][["Barmon"]] <- 23
#   categoricalList[["households_BD11A_ETHNHH"]][["Pahari"]] <- 24
#   categoricalList[["households_BD11A_ETHNHH"]][["Malpahari"]] <- 25
#   categoricalList[["households_BD11A_ETHNHH"]][["Monda"]] <- 26
#   categoricalList[["households_BD11A_ETHNHH"]][["Cool"]] <- 27
#   categoricalList[["households_BD11A_ETHNHH"]][["Other"]] <- 28
# categoricalList[["households_BD11A_ELECTRC"]] <- list()
#   categoricalList[["households_BD11A_ELECTRC"]][["Yes"]] <- 1
#   categoricalList[["households_BD11A_ELECTRC"]][["No"]] <- 2
  

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
  
  
  
  
  


