
getMasterData <- function(){
  # incomePath <- "quickstats-income-tables.xls"
  retentionPath <- "retention_by_TA.csv"
  attendingPath <- "attending_by_TA.csv"
  depPath <- "otago070161.csv"
  
  
  library(readxl)
  library(dplyr)
  
  # ######### INCOME
  # # read in household income data, clean up column names and filter out notes
  # household_income_2013 <- read_xls(incomePath, sheet = 29, skip = 7)[-c(1:2),]
  # colnames(household_income_2013) <- c("Territorial.Authority", colnames(household_income_2013[-1]))
  # household_income_2013 <- household_income_2013 %>%
  #   .[!is.na(.$Territorial.Authority),] %>%
  #   .[-c(93:nrow(.)),]
  
  
  ########### EDUCATION
  # read in education data
  retention <- read.csv(retentionPath, header=TRUE, sep=",", stringsAsFactors = F)
  attending <- read.csv(attendingPath, header=TRUE, sep=",", stringsAsFactors = F)
  
  ## rename columns of years of interest
  colnames(retention)[colnames(retention) %in% c("X2013", "X2014","X2015")] <- c("Retention_2013", "Retention_2014", "Retention_2015")
  colnames(attending)[colnames(attending) %in% c("X2013", "X2014","X2015")] <- c("Participation_2013", "Participation_2014", "Participation_2015")
  
  # education datasets
  retentionYears <- retention[,c("Territorial.Authority", "Retention_2013", "Retention_2014", "Retention_2015")] 
  attendingYears <- attending[, c("Territorial.Authority", "Participation_2013", "Participation_2014", "Participation_2015")] 
  
  # merge education datasets and make numeric
  education_mergeYears <- retentionYears %>% merge(attendingYears, by = c("Territorial.Authority"), all = T)
  education_mergeYears[-1] <- sapply(education_mergeYears[-1], as.numeric )
  
  
  ######### DEPRIVATION
  # read in deprivation data, clean up column names and filter out notes
  nzDep_2013 <- read.csv(depPath, header=TRUE, sep=",", stringsAsFactors = F)
  colnames(nzDep_2013)[8] <- "Territorial.Authority"
  
  ## reshape the data by territorial authority and filter by nzDep score
  library(reshape2)
  melted <- melt(nzDep_2013, id.vars=c("Territorial.Authority")) %>%
    filter(variable == "NZDep2013") %>%
    mutate(value = as.numeric(value))
  
  # group by TA and summarise mean
  grouped <- group_by(melted, Territorial.Authority)
  nzDep_Summary_2013 <- summarise(grouped, Deprivation_Index_2013=mean(value, na.rm = T))
  
  
  ######### CLEANED MERGED DATA - EMMA
  data <- read.csv("test_dataset.csv", stringsAsFactors = F)[-c(2,4,9,20)]
  
  
  ########## MERGE ALL
  
  data_All <- left_join(data, education_mergeYears, by = c("Territorial.Authority")) %>%
    left_join(nzDep_Summary_2013, by = c("Territorial.Authority")) %>%
    select(Territorial.Authority, Deprivation_Index_2013, Median_Household_Income, Low_Income, Medium_Income, Upper_Income, 
           Participation_2013, Participation_2014, Participation_2015, Participation_2016, 
           Retention_2013, Retention_2014, Retention_2015, everything())
  
  
  
  ###################### SHAPEFILE
  ### Shapefile 2013
  
  library(sf)
  # read in shapefile
  income2013 <- read_sf("age-and-income-in-2013-by-talb2013.shp")
  colnames(income2013)[2] <- "Territorial.Authority"
  
  # # rename shapefile auckland local boards to common naming convention
  # income2013$Territorial.Authority[grepl("Local Board", income2013$Territorial.Authority)] <- 
  #   paste("Auckland -", income2013$Territorial.Authority[grepl("Local Board", income2013$Territorial.Authority)])
  
  shapefile_MASTER <- merge(income2013, data_All, by = c("Territorial.Authority"))
  shapefile_MASTER <- shapefile_MASTER[c(1,10,11,15,19,ncol(shapefile_MASTER))]
  
  return(list(data_All, shapefile_MASTER))
}

