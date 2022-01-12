library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAFall")

PNSQARdata <- read.csv("Results.csv")
View(PNSQARdata)


#Mkay, time to figure out how to map.

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

PNSQA_sites <- read.csv("Sites.csv")
View(PNSQA_sites)

world <- ne_countries(scale="medium", returnclass = "sf")

#the following is a map of just PNSQA data
world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Map of PNSQA Sampling Sites",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(PNSQA_sites$SITE_NO))),"sites")+
  geom_point(data=PNSQA_sites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA),size=1,shape=19,color='red') +
  coord_sf(xlim=c((min(PNSQA_sites$DEC_LONG_VA)-2.5),(max(PNSQA_sites$DEC_LONG_VA)+2.5)),ylim=c((min(PNSQA_sites$DEC_LAT_VA)),(max(PNSQA_sites$DEC_LAT_VA))))
world_map



setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQASPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")
View(PNSQASPRISMdata)

PNSQASPRISMdata <- PNSQASPRISMdata[rowSums(is.na(PNSQASPRISMdata))==0,]
#^ used this function to remove all rows that contained any NA values.

#the following is a map of just the PRISM data
world_map2 <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Map of PRISM Sampling Sites",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(PNSQASPRISMdata$Name))),"sites")+
  geom_point(data=PNSQASPRISMdata,aes(x=Longitude,y=Latitude),size=1,shape=19,color='blue') +
  coord_sf(xlim=c((min(PNSQASPRISMdata$Longitude)-2.5),(max(PNSQASPRISMdata$Longitude)+2.5)),ylim=c((min(PNSQASPRISMdata$Latitude)),(max(PNSQASPRISMdata$Latitude))))
world_map2

#do not need the following two lines of code, but essentially you used them to add a column to a data frame. So I am keeping it because I'm proud lol.
PNSQASPRISMdata$type <- "PRISM"
PNSQA_sites$type <- "PNSQA"

#the following map has both the PRISM and PNSQA data on it. Check the code to determine which is red and which is blue.
world_map3 <- ggplot(data=world) +
geom_sf() + theme_classic() +
  labs(title="Map of PNSQA and PRISM sites",x="Longitude",y="Latitude")+
  geom_point(data=PNSQA_sites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA),size=1,shape=19,color='red') + geom_point(data=PNSQASPRISMdata,aes(x=Longitude,y=Latitude),size=1,shape=5,color='blue') +
  coord_sf(xlim=c((min(PNSQA_sites$DEC_LONG_VA)-2.5),(max(PNSQA_sites$DEC_LONG_VA)+2.5)),ylim=c((min(PNSQA_sites$DEC_LAT_VA)),(max(PNSQA_sites$DEC_LAT_VA))))
world_map3