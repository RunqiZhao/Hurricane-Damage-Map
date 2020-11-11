library(dplyr)
library(stringr)
library(lubridate)
# library(usmap)

# # FEMA data link: https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1
# data <- read.csv('PublicAssistanceFundedProjectsDetails.csv', head = TRUE)

# # orginal data is too large to upload, select hurricane and stoems from 2009 - 2018 and save as new data file.
# hurricane <- data %>% filter(incidentType %in% c("Hurricane","Severe Storm(s)","Coastal Storm"))
# hurricane <- hurricane %>% select(-c("hash","lastRefresh","id"))
# hurricane$declarationDate <-  as_datetime(hurricane$declarationDate)
# hurricane$declarationYear <-  year(hurricane$declarationDate)
# hurricane <- hurricane %>% filter(declarationYear > 2009 & declarationYear < 2018)
# write.csv(hurricane,"hurricane.csv",row.names=FALSE)

# Read data as hrc
hrc <- read.csv("hurricane.csv")

# get fips
hrc$stateNumberCode <- as.character(hrc$stateNumberCode)
hrc$stateNumberCode <- str_pad(hrc$stateNumberCode, 2, side = "left", "0")

hrc$countyCode <- as.character(hrc$countyCode)
hrc$countyCode <- str_pad(hrc$countyCode, 3, side = "left", "0")

hrc_statewide <- filter(hrc,countyCode == "000")
hrc_county <- filter(hrc,countyCode != "000")

# unique(hrc_county$countyCode)
hrc_county$Fips <- paste(hrc_county$stateNumberCode, hrc_county$countyCode, sep = "")

# select useful columns for mapping
colnames(hrc)

# # check the information in dcc-damagecategory and damageCategoryCode, reserve damageCategoryCode.
# unique(paste(hrc$dcc,hrc$damageCategory,sep = " - "))
# unique(hrc$damageCategoryCode)

hrc_statewide <- hrc_statewide %>% select("disasterNumber", "declarationYear", "incidentType","applicationTitle","damageCategoryCode","projectSize","state","stateNumberCode","county","projectAmount", "federalShareObligated", "totalObligated","obligatedDate")
hrc_county <- hrc_county %>% select("disasterNumber", "declarationYear", "incidentType","applicationTitle","damageCategoryCode","projectSize","state","stateNumberCode","county","projectAmount", "federalShareObligated", "totalObligated","Fips","obligatedDate")

write.csv(hrc_statewide,"hrc_statewide.csv",row.names=FALSE)
write.csv(hrc_county,"hrc_county.csv",row.names=FALSE)

# dt <- hrc_county %>% filter(incidentType == "Hurricane")
