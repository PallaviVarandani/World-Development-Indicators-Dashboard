###Setting the working directory###
setwd("D:/WDI/Business_Analytics_Project_WDI")

###Loading the libraries###
library(dplyr)
library(readr)
require(reshape2)

###Reading the source files from the directory##
Indicators <- read.csv("WDIData.csv", header = TRUE)
Country <- read.csv("WDICountry.csv", header = TRUE)
Series <- read.csv("WDISeries.csv", header = TRUE)

##Changing the orientation of the Indicators###
mdata <- melt(Indicators, Indicator.Code=c("i..Country.Name","Country.Code","Indicaotor.Name"))

##Cleaning the data###
mdata <- mdata[,-5]
mdata_1 <- subset(mdata, is.na(mdata$value) == FALSE )
colnames(mdata_1) <- c("CountryName","CountryCode","IndicatorName","IndicatorCode","Year","Value")

mdata_1$Year <- gsub("X","", mdata_1$Year)
mdata_1$Year <- as.numeric(mdata_1$Year)

mdata_1$Value <- round(mdata_1$Value, digits = 2)

###changing the column name of supporting data sets for merging###
colnames(Country)[1] <- "CountryCode"
colnames(Series)[1] <- "IndicatorCode"

###Merging datasets####
mdata_1 <- merge(mdata_1, Country[,c("CountryCode","Income.Group","Region")],by = "CountryCode", all.x = TRUE)
mdata_1 <- merge(mdata_1, Series[,c("IndicatorCode","Topic","Limitations.and.exceptions")], by =  "IndicatorCode" , all.x = TRUE)

###writing the dataset in the present directory as a record###
write.csv(mdata_1, "Indicators.csv", row.names = FALSE)

###subseting by only african countries####
mdata_1.africa <- subset(mdata_1, mdata_1$CountryName=="Algeria"|mdata_1$CountryName=="Angola"|mdata_1$CountryName=="Benin"|
                      mdata_1$CountryName=="Botswana"|mdata_1$CountryName=="Burkina Faso"|mdata_1$CountryName=="Cameroon"|
                      mdata_1$CountryName=="Burundi" |mdata_1$CountryName=="Cape Verde"|mdata_1$CountryName=="Central African Republic"|
                      mdata_1$CountryName=="Chad"| mdata_1$CountryName=="Comoros"|mdata_1$CountryName=="Congo, Rep."|
                      mdata_1$CountryName=="Congo, Dem. Rep."| mdata_1$CountryName=="Cote d'Ivoire"|mdata_1$CountryName=="Djibouti"|
                      mdata_1$CountryName=="Egypt, Arab Rep."|mdata_1$CountryName=="Equatorial Guinea"|mdata_1$CountryName=="Eritrea"|
                      mdata_1$CountryName=="Ethiopia"|mdata_1$CountryName=="Gabon"|mdata_1$CountryName=="Gambia, The"|
                      mdata_1$CountryName=="Ghana"|mdata_1$CountryName=="Guinea"|mdata_1$CountryName=="Guinea-Bissau"|
                      mdata_1$CountryName=="Kenya"|mdata_1$CountryName=="Lesotho"|mdata_1$CountryName=="Liberia"|
                      mdata_1$CountryName=="Libya"|mdata_1$CountryName=="Madagascar"|mdata_1$CountryName=="Malawi"|
                      mdata_1$CountryName=="Mali"|mdata_1$CountryName=="Mauritania"|mdata_1$CountryName=="Mauritius"|
                      mdata_1$CountryName=="Morocco"|mdata_1$CountryName=="Mozambique"|mdata_1$CountryName=="Namibia"|
                      mdata_1$CountryName=="Niger"|mdata_1$CountryName=="Nigeria"|mdata_1$CountryName=="Rwanda"|
                      mdata_1$CountryName=="Sao Tome and Principe"|mdata_1$CountryName=="Senegal"|mdata_1$CountryName=="Seychelles"|
                      mdata_1$CountryName=="Seychelles"|mdata_1$CountryName=="Somalia"|mdata_1$CountryName=="South Africa"|
                      mdata_1$CountryName=="South Sudan"|mdata_1$CountryName=="Sudan"|mdata_1$CountryName=="Swaziland"|
                      mdata_1$CountryName=="Tanzania"|mdata_1$CountryName=="Togo"|mdata_1$CountryName=="Tunisia"|
                      mdata_1$CountryName=="Uganda"|mdata_1$CountryName=="Zambia"|mdata_1$CountryName=="Zimbabwe")

### Replacing blank limitations with "No limitations listed"####
mdata_1.africa$Limitations.and.exceptions <- as.character(mdata_1.africa$Limitations.and.exceptions)
a <- which(mdata_1.africa$Limitations.and.exceptions == "")
mdata_1.africa$Limitations.and.exceptions[a] <- "No Limitations Listed"
mdata_1.africa$Limitations.and.exceptions <- as.factor(mdata_1.africa$Limitations.and.exceptions)


###writing the dataset in the present directory as a record###
write.csv(mdata_1.africa, "Africa.csv", row.names = FALSE)

###reading the final clean data for processing from the present directory###
Indicators <- read.csv("Africa.csv", header = TRUE)

###creating a new table just like a metadata###
counts <- Indicators %>%
  group_by(Topic,IndicatorCode, IndicatorName) %>%
  summarise(NumCountries = n_distinct(CountryName),
            NumYears = n_distinct(Year),
            FirstYear = min(Year),
            LastYear = max(Year))
counts <- counts[-1,]

