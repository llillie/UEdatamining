library(plyr)
library(dplyr)
library(readr)

setwd(dir="C:/Users/alillie/Documents/RSQAData/30YearRatioMapping")

sitewideresults <- read.csv("Results.csv")
  
sitewidesites <- read.csv("Sites.csv")

setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")

PNSQAPRISMdata <- PNSQASPRISMdata[rowSums(is.na(PNSQASPRISMdata))==0,]

CSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201701_201709_CSQA.csv")

CSQAPRISMdata <- CSQAPRISMdata[rowSums(is.na(CSQAPRISMdata))==0,]

NESQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201601_201609_NESQA.csv")

NESQAPRISMdata <- NESQAPRISMdata[rowSums(is.na(NESQAPRISMdata))==0,]

SESQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201401_201409_SESQA.csv")

SESQAPRISMdata <- SESQAPRISMdata[rowSums(is.na(SESQAPRISMdata))==0,]

MSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201301_201309_MSQA.csv")

MSQAPRISMdata <- MSQAPRISMdata[rowSums(is.na(MSQAPRISMdata))==0,]

PNSQAPRISMmean <- PNSQASPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

CSQAPRISMmean <- CSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

NESQAPRISMmean <- NESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

SESQAPRISMmean <- SESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

MSQAPRISMmean <- MSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))
#SOMEHOW ADDING UP THE PRISM DATA FOR ALL SITES GETS ME 483, WONDERING IF SAME SITE MISSING FROM BOTH?


setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

thirtyyear <- read.csv("PNSQAPRISM30yr.csv")

thirtyyear <- thirtyyear[rowSums(is.na(thirtyyear))==0,]

library(stringr)

thirtymodified <- thirtyyear %>%
  filter(str_detect(Date,"October",negate = TRUE))

thirtymodified <- thirtymodified %>%
  filter(str_detect(Date,"November",negate = TRUE))

thirtymodified <- thirtymodified %>%
  filter(str_detect(Date,"December",negate = TRUE))

thirtymodified <- thirtymodified %>%
  filter(str_detect(Date,"Annual",negate = TRUE))

thirtymeans <- thirtymodified %>%
  group_by(Name) %>%
  summarize(Thirty_Year_Mean = mean(ppt..inches.))

PNSQAPRISMmean <- PNSQAPRISMmean %>%
  inner_join(thirtymeans)

PNSQAPRISMmean$Ratio_of_Rain <- PNSQAPRISMmean$Mean / PNSQAPRISMmean$Thirty_Year_Mean

CSQAPRISMmean <- CSQAPRISMmean %>%
  inner_join(thirtymeans)
#This drops obstacles from 85 to 78.

CSQAPRISMmean$Ratio_of_Rain <- CSQAPRISMmean$Mean / CSQAPRISMmean$Thirty_Year_Mean

NESQAPRISMmean <- NESQAPRISMmean %>%
  inner_join(thirtymeans)

NESQAPRISMmean$Ratio_of_Rain <- NESQAPRISMmean$Mean / NESQAPRISMmean$Thirty_Year_Mean

SESQAPRISMmean <- SESQAPRISMmean %>%
  inner_join(thirtymeans)

SESQAPRISMmean$Ratio_of_Rain <- SESQAPRISMmean$Mean / SESQAPRISMmean$Thirty_Year_Mean

MSQAPRISMmean <- MSQAPRISMmean %>%
  inner_join(thirtymeans)

MSQAPRISMmean$Ratio_of_Rain <- MSQAPRISMmean$Mean / MSQAPRISMmean$Thirty_Year_Mean

allmeans <- rbind(PNSQAPRISMmean, CSQAPRISMmean, NESQAPRISMmean, SESQAPRISMmean, MSQAPRISMmean)

big30 <- merge(sitewideresults, allmeans, by = c("Name"), all.x=TRUE)



setwd(dir="C:/Users/alillie/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

#To fix the SHORT_NAME issue of having the state abbreviation, an underscore, and then the name:
watershed$Name <- sub("_","",watershed$SHORT_NAME)

nametoRSQA <- select(watershed,SITE_NO,Name)

sitewideresults <- sitewideresults %>%
  inner_join(nametoRSQA)

#Mapping
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

world <- ne_countries(scale="medium", returnclass = "sf")


world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="PNSQA Sampling Year to Thirty Year Average Ratio",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(bigdataandsites$SITE_NO))),"sites")+
  geom_point(data=bigdataandsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Ratio_of_Rain),size=1,shape=19) + scale_color_gradient(low="red",high="blue") +
  coord_sf(xlim=c((min(bigdataandsites$DEC_LONG_VA)-2.5),(max(bigdataandsites$DEC_LONG_VA)+2.5)),ylim=c((min(bigdataandsites$DEC_LAT_VA)),(max(bigdataandsites$DEC_LAT_VA))))
world_map