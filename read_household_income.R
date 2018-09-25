# install.packages("xlsx")
# install.packages("tidyverse")

# library(xlsx)
# library(tidyverse)
library(readxl)


household_income_2013 <- read_xls("D:/Documents/GitHub/Datalands/Hackathon/Data/quickstats-income-tables.xls", sheet = 29, skip = 7)[-c(1:2),]
colnames(household_income_2013) <- c("Territorial Authority Area", colnames(household_income_2013[-1]))
