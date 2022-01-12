#This is my "real" PCA code - going to use PNSQA, PRISM, and landuse data in one go.

library(plyr)
library(dplyr)
library(readr)

#PNSQA DATA ISH

setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAFall")

results <- read.csv("Results.csv")

sites <- read.csv("Sites.csv")

medianvalues <- results %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA))
#Don't do this anymore because three day precipitation sum, each site = site + time stamp to distinguish them and match.

library(tidyr)

widemedianvalues <- pivot_wider(medianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)

PCA1 <- widemedianvalues[,colSums(is.na(widemedianvalues)) < 1]

PCA1 <- as.data.frame(PCA1)
rownames(PCA1)<-PCA1$SITE_NO
PCA1 <- PCA1[,-1]
PCA1 <- PCA1[,which(apply(PCA1,2,var) != 0)]

finaldata1 <- PCA1[complete.cases(PCA1), ]

metadata1<-sites[match(rownames(PCA1),sites$SITE_NO),]

data_cor1 <- cor(PCA1)

library(pheatmap)

pheatmap(data_cor1, annotations=rownames(data_cor1),show_rownames=T, show_colnames=T)

data_cor1t<-cor(t(PCA1))

pheatmap(data_cor1t,show_rownames=F, show_colnames=F)

library(ggbiplot)

prcompData<-prcomp(PCA1,center=T, scale=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=F, groups=paste(metadata1$STATE_NM), ellipse=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=T, groups=paste(metadata1$STATE_NM), ellipse=T)


#PRISM ISH
setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQASPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")

PNSQASPRISMdata <- PNSQASPRISMdata[rowSums(is.na(PNSQASPRISMdata))==0,]
#removes any values from the df that are NA

PNSQASPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20150401_20150630_PNSQA.csv")

PNSQASPRISMdailydata <- PNSQASPRISMdailydata[rowSums(is.na(PNSQASPRISMdailydata))==0,]
#removes any values of NA

#Land Use ISH

setwd(dir="C:/Users/alillie/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")
watershed <- watershed[which(watershed$RSQA_STUDY=='PNSQA'),]
#Perfect 88 obstacles. USE THIS - this is most important.

#To fix the SHORT_NAME issue of having the state abbreviation, an underscore, and then the name:
watershed$Name <- sub("_","",watershed$SHORT_NAME)

#I only want landuse information that is connected with 2012, 2011 is no longer used according to Peter.
watershed2012 <- select(watershed, -contains("2011"))

lowerbasindata <- read.csv("RSQA_Characteristics_LowerBasinData.csv")
lowerbasindata <- lowerbasindata[which(lowerbasindata$RSQA_STUDY=='PNSQA'),]
#This gives me 0 obstacles. Makes sense - they only did this for NESQA and CSQA.

ripariandata <- read.csv("RSQA_Characteristics_RiparianData.csv")
ripariandata <- ripariandata[which(ripariandata$RSQA_STUDY=='PNSQA'),]
#This gives me 87 obstacles  (one less than the actual site number total of 88). This is a subset of watershed.

#Trying to figure out how to use/manipulate the PRISM data:
library(lubridate)

#thinking... create a new data frame that filters the monthly precipitation values and creates the average, max, and min, for each site?
#That is exactly what the code below does.

PNSQAPRISMmean <- PNSQASPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

PNSQAPRISMmaximum <- PNSQASPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

PNSQAPRISMminimum <- PNSQASPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

PNSQAPRISMmodified <- PNSQAPRISMmean %>%
  inner_join(PNSQAPRISMmaximum)

PNSQAPRISMmodified <- PNSQAPRISMmodified %>%
  inner_join(PNSQAPRISMminimum)

PNSQAPRISMmodified <- PNSQAPRISMmodified %>%
  inner_join(PNSQAPRISMminimum)


#Need to compare the mean of each site in the region to the 30 year mean to determine if it was a wet or dry year. Add this to PNSQAPRISMmodified.

setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

thirtyyear <- read.csv("PNSQAPRISM30yr.csv")

thirtyyear <- thirtyyear[rowSums(is.na(thirtyyear))==0,]

#trying to get rid of months that are not included in the sampling time frame (aka want to keep months 1-9). Check that this is a good idea though, I'm just going off of gut.

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

PNSQAPRISMmodified <- PNSQAPRISMmodified %>%
  inner_join(thirtymeans)

#Added the Thirty_Year_Mean column to my PNSQAPRISMmodified - this mean is for only the 9 months that I had data on for the duration of the sampling.

PNSQAPRISMmodified$Ratio_of_Rain <- PNSQAPRISMmodified$Mean / PNSQAPRISMmodified$Thirty_Year_Mean
  
#Need to calculate the total daily rainfall for the 3 days preceding each sample.If the total is 1cm or more, it was "wet" weather.
#I would have to make a code that somehow manages to determine each of the days of each chemical and then pull the three days before it from the PRISM data and find a total of that and insert it into my results column?

#Finding the three day totals.
three_day_rain_sum <- function(data){
  for(i in 1:length(unique(data$Name))){
    activedata <- data[data$Name == unique(data$Name)[i], ]
    for(j in 4:nrow(activedata)){
      activesum <- sum(activedata[j-1,6],activedata[j-2,6], activedata[j-3,6]) ##change from sum() to average() for means
      if(i==1 & j==4){
        out <- c(as.character(activedata[j,1]), as.character(activedata[j,5]), activesum)
      } else {
        outtemp <- c(as.character(activedata[j,1]), as.character(activedata[j,5]), activesum)
        out <- rbind(out, outtemp)
      }
    }
  }
  rownames(out) <- c()
  colnames(out) <- c("Name", "Day", "3_Day_Sum_Rain")
  out
}

three_day_sums <- three_day_rain_sum(PNSQASPRISMdailydata)
#This takes the data frame I'm using and runs it through the function to obtain three day prior totals.

library(lubridate)

results$Day <- as.character(as.Date(results$SAMPLE_START_DT,"%m/%d/%Y"))
#This code chops off the time component of SAMPLE_START_DT and creates a new column called Day that is only the date.

three_day_sums <- as.data.frame(three_day_sums)
#turns the matrix into a data frame

three_day_sums$Day <- as.character(as.Date(three_day_sums$Day,"%m/%d/%Y"))
#Use this to convert three_day_sums Day order to the same order as your Day in results.


#Finding a way to merge the three day sums with PNSQA data.
#Began by pulling up my watershed data frame, because that's where the site number and short name are located.
watershed$Name <- sub("_","",watershed$SHORT_NAME)
nametoRSQA <- select(watershed,SITE_NO,Name)

results <- results %>%
  inner_join(nametoRSQA)

bigdata <- merge(results, three_day_sums, by = c("Name","Day"), all.x=TRUE)
#This code merges results and three_day_sums perfectly :D

#Now that three day totals have been merged to results, let's merge the rest of our PRISM variables found in PNSQAPRISMmodified.
bigdata <- merge(bigdata, PNSQAPRISMmodified, by = c("Name"), all.x=TRUE)

#Out of curiosity, I decided to find the summary statistics on the Ratio_of_Rain for OR and WA separately.
ORratios <- PNSQAPRISMmodified[str_detect(PNSQAPRISMmodified$Name, "OR"),]
WAratios <- PNSQAPRISMmodified[str_detect(PNSQAPRISMmodified$Name, "WA"),]

summary(ORratios)
summary(WAratios)


#SAMPLE_START_DT in results of PNSQA data tells you the date and the time that the sample was taken.
#Make a new column that is the same year and month - match by longitude and year/month
#Ask Peter? How best to do the precipitation? Make an average precipitation for the sampling period, max, and min (for the monthly), and 30 year precipitation. DONE
#Do some searching for R functions that manipulate date/time functions. DONE
#Practice R functions with date and time. DONE


#Convert RSQA to wide/short and then tack on LandUse. For PRISM PCA, group_by site and date point. Order by site and date.
library(tidyr)

bigdata <- unite(bigdata, SITE_DATE, sep = " ", c(SITE_NO, Day))
#Combines the SITE_NO and Day columns that you made.



RSQAPCA <- pivot_wider(results, id_cols=SITE_DATE, names_from=PARM_NM, values_from=RESULT_VA)

table(colSums(is.na(RSQAPCA)))

RSQAPCA <- RSQAPCA[,colSums(is.na(RSQAPCA)) < 1]

#Plotting the PNSQA PRISM 30 year ratios. Yeah this isn't working at all right now... Figure out next week.
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

world <- ne_countries(scale="medium", returnclass = "sf")

bigdataandsites <- bigdata %>%
  inner_join(sites)

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="PNSQA Sampling Year to Thirty Year Average Ratio",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(bigdataandsites$SITE_NO))),"sites")+
  geom_point(data=bigdataandsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Ratio_of_Rain),size=1,shape=19) + scale_color_gradient(low="red",high="blue") +
  coord_sf(xlim=c((min(bigdataandsites$DEC_LONG_VA)-2.5),(max(bigdataandsites$DEC_LONG_VA)+2.5)),ylim=c((min(bigdataandsites$DEC_LAT_VA)),(max(bigdataandsites$DEC_LAT_VA))))
world_map

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Data Sites by State",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(bigdataandsites$SITE_NO))),"sites")+
  geom_point(data=bigdataandsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=STATE_ABBREV),size=1,shape=19) +
  coord_sf(xlim=c((min(bigdataandsites$DEC_LONG_VA)-2.5),(max(bigdataandsites$DEC_LONG_VA)+2.5)),ylim=c((min(bigdataandsites$DEC_LAT_VA)),(max(bigdataandsites$DEC_LAT_VA))))
world_map
#Me messing around with the maps to figure out how to do things, got State colorations to work.