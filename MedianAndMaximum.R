##################################################################################
#Converting all average pesticide concentration values to median and maximum and substituting non-detectables for zero while keeping E values


library(plyr)
library(dplyr)
library(readr)

#Looking at the pesticide concentrations across all 5 regions.

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/AllRegPest")

results <- read.csv("Results.csv")

#Adjusting the non-detectables to zero
results$RESULT_VA <- with(results, ifelse(REMARK_CD=='<', 0, RESULT_VA))

#Ensuring that I am only working with Pesticide values
results <- results[(results$PARM_SEQ_GRP_DS=="Organics, Pesticides"),]

sites <- read.csv("Sites.csv")



library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

#Setting up the template map
world <- ne_countries(scale="medium", returnclass = "sf")

#Finding Three-Day Rain Sums
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
  colnames(out) <- c("Name", "Day", "Three_Day_Sum_Rain")
  out
}

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20150401_20150630_PNSQA.csv")

PNSQAPRISMdailydata <- PNSQAPRISMdailydata[rowSums(is.na(PNSQAPRISMdailydata))==0,]

MSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20130501_20130831_MSQA.csv")

MSQAPRISMdailydata <- MSQAPRISMdailydata[rowSums(is.na(MSQAPRISMdailydata))==0,]

SESQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20140401_20140630_SESQA.csv")

SESQAPRISMdailydata <- SESQAPRISMdailydata[rowSums(is.na(SESQAPRISMdailydata))==0,]

NESQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20160601_20160831_NESQA.csv")

NESQAPRISMdailydata <- NESQAPRISMdailydata[rowSums(is.na(NESQAPRISMdailydata))==0,]

CSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20170301_20170531_CSQA.csv")

CSQAPRISMdailydata <- CSQAPRISMdailydata[rowSums(is.na(CSQAPRISMdailydata))==0,]

fulldaily <- as.data.frame(rbind(CSQAPRISMdailydata,NESQAPRISMdailydata,PNSQAPRISMdailydata,SESQAPRISMdailydata,MSQAPRISMdailydata))

three_day_sums <- three_day_rain_sum(fulldaily)

library(lubridate)

results$Day <- as.character(as.Date(results$SAMPLE_START_DT,"%m/%d/%Y"))
#This code chops off the time component of SAMPLE_START_DT and creates a new column called Day that is only the date.

three_day_sums <- as.data.frame(three_day_sums)
#turns the matrix into a data frame

three_day_sums$Day <- as.character(as.Date(three_day_sums$Day,"%m/%d/%Y"))
#Use this to convert three_day_sums Day order to the same order as your Day in results.


#Fixing name inconsistencies

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

#To fix the SHORT_NAME issue of having the state abbreviation, an underscore, and then the name:
watershed$Name <- as.character(gsub("_","",watershed$SHORT_NAME))

#This got rid of all of the dot/period errors. ***RERUN YOUR ERROR DATA AND FIGURE OUT HOW MANY YOU HAVE TO FIX.****
watershed$Name <- gsub("\\.","",watershed$Name)

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/30YearRatioMapping")

namechanges <- read.csv("NameChanges.csv")

namechanges <- subset(namechanges, select=-c(Region))

#This creates a key of the correct names.
temp <- unique(three_day_sums[!(three_day_sums$Name %in% namechanges$Name),1])
#which of the sites are in namechanges and they have a correction to be done. T/F list. Not = show us the ones that aren't, aka all the ones that are right. grabbing the right ones
#grabbing first column and all the right stuff. Taking the unique values = so no dups.
#Get a vector of the correct sites that were always right.
tempdup <- as.data.frame(cbind(temp, temp))
#Duplicating the data.
colnames(tempdup) <- c("Name", "RSQAName")
namechanges <- rbind(namechanges, tempdup)

three_day_sums <- merge(namechanges, three_day_sums, by="Name", all.y = T)
three_day_sums <- subset(three_day_sums,select=-c(Name))
three_day_sums <- three_day_sums %>%
  rename(Name=RSQAName)


nametoRSQA <- select(watershed,SITE_NO,Name)

results <- results %>%
  inner_join(nametoRSQA)

allbigdata <- merge(results, three_day_sums, by = c("Name","Day"), all.x=TRUE)

allbigdata2 <- allbigdata[!is.na(allbigdata$Three_Day_Sum_Rain),]
allbigdata2 <- allbigdata2[!is.na(allbigdata2$RESULT_VA),]




##########################################################################

#Since I'm working with individual pesticides, I no longer need to convert them into a uniform unit


#parm <- read.csv("USGSParameterCodes.csv")

#Merging my pesticides results frame with the parameters frame so I can convert the units to a consistent value.
#units <- results %>%
#  left_join(parm)

#This tells me how many unique parameter units there are.
#unique(units[c("PARM_UNT_TX")])

#This tells me exactly how many of each unit measurement there is in my data frame.
#table(units$PARM_UNT_TX)

#Since ug/kg and ug/l are essentially the same, I decided to convert ng/l to match that value.
#ng2 <- units[units$PARM_UNT_TX == 'ng/l',]

#ng2$RESULT_VA <- ng2$RESULT_VA * 0.001

#I then pulled all the original pesticide values that were measured in ug/kg and ug/l
#original2 <- units[units$PARM_UNT_TX != 'ng/l',]

#I am now combining the two separate dataframes into a single one.
#polished2 <- rbind(ng2, original2)

#polished2 <- polished2[!is.na(polished2$RESULT_VA),]


############################################################

#maximums <- polished2 %>%
 # group_by(SITE_NO) %>%
  #summarize(Maximum_Conc = max(RESULT_VA))

#max2 <- maximums %>%
 # left_join(polished2)

#table(maximums$PARM_NM)

#############################################################

#Continue here

expcount <- results %>% filter(results$REMARK_CD != "<") %>% group_by(PARM_NM) %>% summarise(count=n())
expcount <- expcount[expcount$count > 50, ]
expcount <- expcount[order(expcount$count, decreasing = T), ]
expcount
# A tibble: 114 x 2
#PARM_NM            count
#<chr>              <int>
#  1 Atrazine, wf        2351
#2 OIET, wf            1983
#3 Metolachlor, wf     1796
#4 CIAT, wf            1611
#5 Prometon, wf        1497
#6 Metolachlor SA, wf  1479
#7 Imidacloprid, wf    1449
#8 Pymetrozine, wf     1391
#9 2,4-D, wf           1389
#10 CAAT, wf            1266
# ... with 104 more rows

atrazine <- results[results$PARM_NM == 'Atrazine, wf', ]

maximumatrazine <- atrazine %>%
 group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxatsites <- maximumatrazine %>%
 left_join(sites)

table(maxatsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

library(ggplot2)
library(plyr)
library(dplyr)
library(readr)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

#Setting up the template map
world <- ne_countries(scale="medium", returnclass = "sf")

#Mapping the maximum atrazine concentrations across all sites
maxatrazine <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum Atrazine Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxatsites$SITE_NO))),"sites")+
  geom_point(data=maxatsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=60000,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxatsites$DEC_LONG_VA)),(max(maxatsites$DEC_LONG_VA))),ylim=c((min(maxatsites$DEC_LAT_VA)),(max(maxatsites$DEC_LAT_VA)))) 
maxatrazine



medianatrazine <- atrazine %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medatsites <- medianatrazine %>%
  left_join(sites)

table(medatsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


#Mapping the median atrazine concentrations across all sites
medatrazine <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median Atrazine Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medatsites$SITE_NO))),"sites")+
  geom_point(data=medatsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=2500,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medatsites$DEC_LONG_VA)),(max(medatsites$DEC_LONG_VA))),ylim=c((min(medatsites$DEC_LAT_VA)),(max(medatsites$DEC_LAT_VA)))) 
medatrazine

#Re-work Atrazine three-day correlation

#Finding Three-Day Rain Sums
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
  colnames(out) <- c("Name", "Day", "Three_Day_Sum_Rain")
  out
}

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20150401_20150630_PNSQA.csv")

PNSQAPRISMdailydata <- PNSQAPRISMdailydata[rowSums(is.na(PNSQAPRISMdailydata))==0,]

MSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20130501_20130831_MSQA.csv")

MSQAPRISMdailydata <- MSQAPRISMdailydata[rowSums(is.na(MSQAPRISMdailydata))==0,]

SESQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20140401_20140630_SESQA.csv")

SESQAPRISMdailydata <- SESQAPRISMdailydata[rowSums(is.na(SESQAPRISMdailydata))==0,]

NESQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20160601_20160831_NESQA.csv")

NESQAPRISMdailydata <- NESQAPRISMdailydata[rowSums(is.na(NESQAPRISMdailydata))==0,]

CSQAPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20170301_20170531_CSQA.csv")

CSQAPRISMdailydata <- CSQAPRISMdailydata[rowSums(is.na(CSQAPRISMdailydata))==0,]

fulldaily <- as.data.frame(rbind(CSQAPRISMdailydata,NESQAPRISMdailydata,PNSQAPRISMdailydata,SESQAPRISMdailydata,MSQAPRISMdailydata))

three_day_sums <- three_day_rain_sum(fulldaily)

library(lubridate)

results$Day <- as.character(as.Date(results$SAMPLE_START_DT,"%m/%d/%Y"))
#This code chops off the time component of SAMPLE_START_DT and creates a new column called Day that is only the date.

three_day_sums <- as.data.frame(three_day_sums)
#turns the matrix into a data frame

three_day_sums$Day <- as.character(as.Date(three_day_sums$Day,"%m/%d/%Y"))
#Use this to convert three_day_sums Day order to the same order as your Day in results.


#Fixing name inconsistencies

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

#To fix the SHORT_NAME issue of having the state abbreviation, an underscore, and then the name:
watershed$Name <- as.character(gsub("_","",watershed$SHORT_NAME))

#This got rid of all of the dot/period errors. ***RERUN YOUR ERROR DATA AND FIGURE OUT HOW MANY YOU HAVE TO FIX.****
watershed$Name <- gsub("\\.","",watershed$Name)

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/30YearRatioMapping")

namechanges <- read.csv("NameChanges.csv")

namechanges <- subset(namechanges, select=-c(Region))

#This creates a key of the correct names.
temp <- unique(three_day_sums[!(three_day_sums$Name %in% namechanges$Name),1])
#which of the sites are in namechanges and they have a correction to be done. T/F list. Not = show us the ones that aren't, aka all the ones that are right. grabbing the right ones
#grabbing first column and all the right stuff. Taking the unique values = so no dups.
#Get a vector of the correct sites that were always right.
tempdup <- as.data.frame(cbind(temp, temp))
#Duplicating the data.
colnames(tempdup) <- c("Name", "RSQAName")
namechanges <- rbind(namechanges, tempdup)

three_day_sums <- merge(namechanges, three_day_sums, by="Name", all.y = T)
three_day_sums <- subset(three_day_sums,select=-c(Name))
three_day_sums <- three_day_sums %>%
  rename(Name=RSQAName)


nametoRSQA <- select(watershed,SITE_NO,Name)

results <- results %>%
  inner_join(nametoRSQA)

allbigdata <- merge(results, three_day_sums, by = c("Name","Day"), all.x=TRUE)

allbigdata2 <- allbigdata[!is.na(allbigdata$Three_Day_Sum_Rain),]

allatrazine <- allbigdata2[(allbigdata2$PARM_NM=="Atrazine, wf"),]

allatrazine <- allatrazine[!is.na(allatrazine$Three_Day_Sum_Rain),]
allatrazine <- allatrazine[!is.na(allatrazine$RESULT_VA),]


plot(as.numeric(allatrazine$Three_Day_Sum_Rain), as.numeric(allatrazine$RESULT_VA), main = "Atrazine", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
allatrazine_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allatrazine)
abline(allatrazine_model, col='blue')

test <- cor.test(as.numeric(allatrazine$Three_Day_Sum_Rain), as.numeric(allatrazine$RESULT_VA), method="spearman", exact=FALSE)
test
#Spearman's rank correlation rho

#data:  as.numeric(allatrazine$Three_Day_Sum_Rain) and as.numeric(allatrazine$RESULT_VA)
#S = 4762304807, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.2034436 

test <- cor.test(as.numeric(allatrazine$Three_Day_Sum_Rain), as.numeric(allatrazine$RESULT_VA), method="kendall")
test
#Kendall's rank correlation tau

#data:  as.numeric(allatrazine$Three_Day_Sum_Rain) and as.numeric(allatrazine$RESULT_VA)
#z = 11.742, p-value < 2.2e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#      tau 
#0.1475725 

#Running an actual Linear Regression Test on the Atrazine Data
summary(allatrazine_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = allatrazine)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-3143   -369   -229   -184 119656 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      228.51      52.21   4.376 1.24e-05 ***
#  as.numeric(Three_Day_Sum_Rain)   485.01      72.72   6.670 3.00e-11 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2584 on 3296 degrees of freedom
#Multiple R-squared:  0.01332,	Adjusted R-squared:  0.01302 
#F-statistic: 44.48 on 1 and 3296 DF,  p-value: 2.996e-11


#Another look at this correlation
ggplot(allatrazine, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of Atrazine',x='Rain Three Days Preceeding (in)',y='Concentration of Atrazine (ng/l)') +
  theme_classic()

#Let's do some further investigation into that outlier
max(allatrazine$RESULT_VA)
#[1] 119884.6

atrazineoutlier <- allatrazine[(allatrazine$RESULT_VA==119884.61),]
atrazineoutlier <- atrazineoutlier %>%
  left_join(watershed)
#LU_cat = Ag_high and Major_LU = Ag



#OIET, wf
OIET <- results[results$PARM_NM == 'OIET, wf', ]

maximumOIET <- OIET %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxOIETsites <- maximumOIET %>%
  left_join(sites)

table(maxOIETsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxOIET <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum OIET Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxOIETsites$SITE_NO))),"sites")+
  geom_point(data=maxOIETsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=750,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxOIETsites$DEC_LONG_VA)),(max(maxOIETsites$DEC_LONG_VA))),ylim=c((min(maxOIETsites$DEC_LAT_VA)),(max(maxOIETsites$DEC_LAT_VA)))) 
maxOIET

medianOIET <- OIET %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medOIETsites <- medianOIET %>%
  left_join(sites)

table(medOIETsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


#Mapping the median OIET concentrations across all sites
medOIET <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median OIET Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medOIETsites$SITE_NO))),"sites")+
  geom_point(data=medOIETsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=250,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medOIETsites$DEC_LONG_VA)),(max(medOIETsites$DEC_LONG_VA))),ylim=c((min(medOIETsites$DEC_LAT_VA)),(max(medOIETsites$DEC_LAT_VA)))) 
medOIET

allOIET <- allbigdata2[(allbigdata2$PARM_NM=="OIET, wf"),]

allOIET <- allOIET[!is.na(allOIET$Three_Day_Sum_Rain),]
allOIET <- allOIET[!is.na(allOIET$RESULT_VA),]


plot(as.numeric(allOIET$Three_Day_Sum_Rain), as.numeric(allOIET$RESULT_VA), main = "OIET", xlab="Rain 3 Days Preceding (in)", ylab="OIET Concentration (ng/l)", pch=19)
allOIET_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allOIET)
abline(allOIET_model, col='blue')

test <- cor.test(as.numeric(allOIET$Three_Day_Sum_Rain), as.numeric(allOIET$RESULT_VA), method="spearman", exact=FALSE)
test
#Spearman's rank correlation rho

#data:  as.numeric(allOIET$Three_Day_Sum_Rain) and as.numeric(allOIET$RESULT_VA)
#S = 4.85e+09, p-value = 6.873e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.1412539 

test <- cor.test(as.numeric(allOIET$Three_Day_Sum_Rain), as.numeric(allOIET$RESULT_VA), method="kendall")
test
#Kendall's rank correlation tau

#data:  as.numeric(allOIET$Three_Day_Sum_Rain) and as.numeric(allOIET$RESULT_VA)
#z = 8.0828, p-value = 6.33e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#      tau 
#0.1047354 

#Running an actual Linear Regression Test on the Atrazine Data
summary(allOIET_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = allOIET)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-135.42  -40.60  -36.74    0.99 1414.15 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      39.368      1.943  20.266  < 2e-16 ***
#  as.numeric(Three_Day_Sum_Rain)   15.983      2.697   5.927 3.42e-09 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 95.38 on 3234 degrees of freedom
#Multiple R-squared:  0.01074,	Adjusted R-squared:  0.01044 
#F-statistic: 35.13 on 1 and 3234 DF,  p-value: 3.415e-09

ggplot(allOIET, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of OIET',x='Rain Three Days Preceeding (in)',y='Concentration of OIET (ng/l)') +
  theme_classic()



#Metolachlor, wf


metolachlor <- results[results$PARM_NM == 'Metolachlor, wf', ]

maximummetolachlor <- metolachlor %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxmetolachlorsites <- maximummetolachlor %>%
  left_join(sites)

table(maxmetolachlorsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxmetolachlor <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum Metolachlor Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxmetolachlorsites$SITE_NO))),"sites")+
  geom_point(data=maxmetolachlorsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=15000,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxmetolachlorsites$DEC_LONG_VA)),(max(maxmetolachlorsites$DEC_LONG_VA))),ylim=c((min(maxmetolachlorsites$DEC_LAT_VA)),(max(maxmetolachlorsites$DEC_LAT_VA)))) 
maxmetolachlor

medianmetolachlor <- metolachlor %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medmetolachlorsites <- medianmetolachlor %>%
  left_join(sites)

table(medmetolachlorsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


#Mapping the median OIET concentrations across all sites
medmetolachlor <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median Metolachlor Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medmetolachlorsites$SITE_NO))),"sites")+
  geom_point(data=medmetolachlorsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=800,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medmetolachlorsites$DEC_LONG_VA)),(max(medmetolachlorsites$DEC_LONG_VA))),ylim=c((min(medmetolachlorsites$DEC_LAT_VA)),(max(medmetolachlorsites$DEC_LAT_VA)))) 
medmetolachlor

allmetolachlor <- allbigdata2[(allbigdata2$PARM_NM=="Metolachlor, wf"),]

allmetolachlor <- allmetolachlor[!is.na(allmetolachlor$Three_Day_Sum_Rain),]
allmetolachlor <- allmetolachlor[!is.na(allmetolachlor$RESULT_VA),]


plot(as.numeric(allmetolachlor$Three_Day_Sum_Rain), as.numeric(allmetolachlor$RESULT_VA), main = "Metolachlor", xlab="Rain 3 Days Preceding (in)", ylab="Metolachlor Concentration (ng/l)", pch=19)
allmetolachlor_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allmetolachlor)
abline(allmetolachlor_model, col='blue')

test <- cor.test(as.numeric(allmetolachlor$Three_Day_Sum_Rain), as.numeric(allmetolachlor$RESULT_VA), method="spearman", exact=FALSE)
test
#Spearman's rank correlation rho

#data:  as.numeric(allmetolachlor$Three_Day_Sum_Rain) and as.numeric(allmetolachlor$RESULT_VA)
#S = 4895351512, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.1804446 

test <- cor.test(as.numeric(allmetolachlor$Three_Day_Sum_Rain), as.numeric(allmetolachlor$RESULT_VA), method="kendall")
test
#	Kendall's rank correlation tau

#data:  as.numeric(allmetolachlor$Three_Day_Sum_Rain) and as.numeric(allmetolachlor$RESULT_VA)
#z = 10.454, p-value < 2.2e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.1361729 


ggplot(allmetolachlor, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of Metolachlor',x='Rain Three Days Preceeding (in)',y='Concentration of Metolachlor (ng/l)') +
  theme_classic()

#CIAT, wf
CIAT <- results[results$PARM_NM == 'CIAT, wf', ]

maximumCIAT <- CIAT %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxCIATsites <- maximumCIAT %>%
  left_join(sites)

table(maxCIATsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxCIAT <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum CIAT Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxCIATsites$SITE_NO))),"sites")+
  geom_point(data=maxCIATsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=2250,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxCIATsites$DEC_LONG_VA)),(max(maxCIATsites$DEC_LONG_VA))),ylim=c((min(maxCIATsites$DEC_LAT_VA)),(max(maxCIATsites$DEC_LAT_VA)))) 
maxCIAT

medianCIAT <- CIAT %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medCIATsites <- medianCIAT %>%
  left_join(sites)

table(medCIATsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


medCIAT <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median CIAT Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medCIATsites$SITE_NO))),"sites")+
  geom_point(data=medCIATsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=225,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medCIATsites$DEC_LONG_VA)),(max(medCIATsites$DEC_LONG_VA))),ylim=c((min(medCIATsites$DEC_LAT_VA)),(max(medCIATsites$DEC_LAT_VA)))) 
medCIAT

allCIAT <- allbigdata2[(allbigdata2$PARM_NM=="CIAT, wf"),]

allCIAT <- allCIAT[!is.na(allCIAT$Three_Day_Sum_Rain),]
allCIAT <- allCIAT[!is.na(allCIAT$RESULT_VA),]


plot(as.numeric(allCIAT$Three_Day_Sum_Rain), as.numeric(allCIAT$RESULT_VA), main = "CIAT", xlab="Rain 3 Days Preceding (in)", ylab="CIAT Concentration (ng/l)", pch=19)
allCIAT_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allCIAT)
abline(allCIAT_model, col='blue')

test <- cor.test(as.numeric(allCIAT$Three_Day_Sum_Rain), as.numeric(allCIAT$RESULT_VA), method="spearman", exact=FALSE)
test
#Spearman's rank correlation rho

#data:  as.numeric(allCIAT$Three_Day_Sum_Rain) and as.numeric(allCIAT$RESULT_VA)
#S = 4531155537, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.1977038 

test <- cor.test(as.numeric(allCIAT$Three_Day_Sum_Rain), as.numeric(allCIAT$RESULT_VA), method="kendall")
test
#		Kendall's rank correlation tau

#data:  as.numeric(allCIAT$Three_Day_Sum_Rain) and as.numeric(allCIAT$RESULT_VA)
#z = 11.318, p-value < 2.2e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.15048 


ggplot(allCIAT, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of CIAT',x='Rain Three Days Preceeding (in)',y='Concentration of CIAT (ng/l)') +
  theme_classic()

max(allCIAT$RESULT_VA)
#[1] 4486.145
#ILGalum AGAIN



#MINOR DIVERGENCE TO TRICLOPYR
expcount[expcount$PARM_NM == "Triclopyr, wf", ]
tri <- results[results$PARM_NM == "Triclopyr, wf",]
table(tri$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#1197   609   797   679 
#Well that just debunked one of my theories...

#Triclopyr, wf
triclopyr <- results[results$PARM_NM == 'Triclopyr, wf', ]

maximumtriclopyr <- triclopyr %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxtriclopyrsites <- maximumtriclopyr %>%
  left_join(sites)

table(maxtriclopyrsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxtriclopyr <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum Triclopyr Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxtriclopyrsites$SITE_NO))),"sites")+
  geom_point(data=maxtriclopyrsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=7500,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxtriclopyrsites$DEC_LONG_VA)),(max(maxtriclopyrsites$DEC_LONG_VA))),ylim=c((min(maxtriclopyrsites$DEC_LAT_VA)),(max(maxtriclopyrsites$DEC_LAT_VA)))) 
maxtriclopyr

mediantriclopyr <- triclopyr %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medtriclopyrsites <- mediantriclopyr %>%
  left_join(sites)

table(medtriclopyrsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


medtriclopyr <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median Triclopyr Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medtriclopyrsites$SITE_NO))),"sites")+
  geom_point(data=medtriclopyrsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=300,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medtriclopyrsites$DEC_LONG_VA)),(max(medtriclopyrsites$DEC_LONG_VA))),ylim=c((min(medtriclopyrsites$DEC_LAT_VA)),(max(medtriclopyrsites$DEC_LAT_VA)))) 
medtriclopyr

alltriclopyr <- allbigdata2[(allbigdata2$PARM_NM=="Triclopyr, wf"),]

alltriclopyr <- alltriclopyr[!is.na(alltriclopyr$Three_Day_Sum_Rain),]
alltriclopyr <- alltriclopyr[!is.na(alltriclopyr$RESULT_VA),]


plot(as.numeric(alltriclopyr$Three_Day_Sum_Rain), as.numeric(alltriclopyr$RESULT_VA), main = "Triclopyr", xlab="Rain 3 Days Preceding (in)", ylab="Triclopyr Concentration (ng/l)", pch=19)
alltriclopyr_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=alltriclopyr)
abline(alltriclopyr_model, col='blue')

test <- cor.test(as.numeric(alltriclopyr$Three_Day_Sum_Rain), as.numeric(alltriclopyr$RESULT_VA), method="spearman", exact=FALSE)
test
#	Spearman's rank correlation rho

#data:  as.numeric(alltriclopyr$Three_Day_Sum_Rain) and as.numeric(alltriclopyr$RESULT_VA)
#S = 5.311e+09, p-value = 0.0006904
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.05962197 

test <- cor.test(as.numeric(alltriclopyr$Three_Day_Sum_Rain), as.numeric(alltriclopyr$RESULT_VA), method="kendall")
test
#	Kendall's rank correlation tau

#data:  as.numeric(alltriclopyr$Three_Day_Sum_Rain) and as.numeric(alltriclopyr$RESULT_VA)
#z = 3.3873, p-value = 0.0007058
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.0489376 


ggplot(alltriclopyr, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of Triclopyr',x='Rain Three Days Preceeding (in)',y='Concentration of Triclopyr (ng/l)') +
  theme_classic()

max(alltriclopyr$RESULT_VA)
#[1] 4864.388
#INFall




############################################################################
#Another minor divergence
#I want to run a Kruskal-Willis test commparing if there are significant differences of atrazine in the four different regions

library(ggpubr)

#Q1: Does maximum atrazine concentrations differ based on the region where the streams were sampled?
compare_means(Maximum_Conc ~ RSQA_STUDY, maxatsites, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
# A tibble: 1 x 6
#.y.                 p    p.adj p.format p.signif method        
#<chr>           <dbl>    <dbl> <chr>    <chr>    <chr>         
#  1 Maximum_Conc 1.17e-52 1.20e-52 <2e-16   ****     Kruskal-Wallis

#Post-Hoc
compare_means(Maximum_Conc ~ RSQA_STUDY , maxatsites, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
# A tibble: 6 x 8
#.y.          group1 group2        p    p.adj p.format p.signif method  
#<chr>        <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr>   
#  1 Maximum_Conc NESQA  SESQA  3.62e- 7 3.60e- 7 3.6e-07  ****     Wilcoxon
#2 Maximum_Conc NESQA  MSQA   1.71e-31 5.10e-31 < 2e-16  ****     Wilcoxon
#3 Maximum_Conc NESQA  PNSQA  2.79e- 7 3.40e- 7 2.8e-07  ****     Wilcoxon
#4 Maximum_Conc SESQA  MSQA   1.83e-25 3.70e-25 < 2e-16  ****     Wilcoxon
#5 Maximum_Conc SESQA  PNSQA  6.59e-17 9.90e-17 < 2e-16  ****     Wilcoxon
#6 Maximum_Conc MSQA   PNSQA  6.32e-32 3.80e-31 < 2e-16  ****     Wilcoxon


#Q2: Does median atrazine concentrations differ based on the region where the streams were sampled?
compare_means(Median_Conc ~ RSQA_STUDY, medatsites, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
# A tibble: 1 x 6
#.y.                p    p.adj p.format p.signif method        
#<chr>          <dbl>    <dbl> <chr>    <chr>    <chr>         
#  1 Median_Conc 1.35e-49 1.40e-49 <2e-16   ****     Kruskal-Wallis

compare_means(Median_Conc ~ RSQA_STUDY , medatsites, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
# A tibble: 6 x 8
#.y.         group1 group2        p    p.adj p.format p.signif method  
#<chr>       <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr>   
#  1 Median_Conc NESQA  SESQA  5.90e- 3 5.90e- 3 0.0059   **       Wilcoxon
#2 Median_Conc NESQA  MSQA   9.99e-31 3.00e-30 < 2e-16  ****     Wilcoxon
#3 Median_Conc NESQA  PNSQA  6.35e- 8 7.60e- 8 6.3e-08  ****     Wilcoxon
#4 Median_Conc SESQA  MSQA   3.28e-23 6.60e-23 < 2e-16  ****     Wilcoxon
#5 Median_Conc SESQA  PNSQA  5.62e-14 8.40e-14 5.6e-14  ****     Wilcoxon
#6 Median_Conc MSQA   PNSQA  9.71e-33 5.80e-32 < 2e-16  ****     Wilcoxon

#Q3: Does atrazine concentrations differ based on the region where the streams were sampled?
compare_means(RESULT_VA ~ RSQA_STUDY, atrazine, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
# A tibble: 1 x 6
#.y.           p p.adj p.format p.signif method        
#<chr>     <dbl> <dbl> <chr>    <chr>    <chr>         
#  1 RESULT_VA     0     0 <2e-16   ****     Kruskal-Wallis

compare_means(RESULT_VA ~ RSQA_STUDY , atrazine, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
# A tibble: 6 x 8
#.y.       group1 group2         p     p.adj p.format p.signif method  
#<chr>     <chr>  <chr>      <dbl>     <dbl> <chr>    <chr>    <chr>   
#  1 RESULT_VA MSQA   SESQA  3.36e-162 6.70e-162 < 2e-16  ****     Wilcoxon
#2 RESULT_VA MSQA   NESQA  2.35e-179 7.10e-179 < 2e-16  ****     Wilcoxon
#3 RESULT_VA MSQA   PNSQA  1.01e-259 6.10e-259 < 2e-16  ****     Wilcoxon
#4 RESULT_VA SESQA  NESQA  3.93e-  9 3.90e-  9 3.9e-09  ****     Wilcoxon
#5 RESULT_VA SESQA  PNSQA  6.27e- 83 9.40e- 83 < 2e-16  ****     Wilcoxon
#6 RESULT_VA NESQA  PNSQA  7.32e- 47 8.80e- 47 < 2e-16  ****     Wilcoxon


###################################################################################################


#Prometon, wf
prometon <- results[results$PARM_NM == 'Prometon, wf', ]

maximumprometon <- prometon %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxprometonsites <- maximumprometon %>%
  left_join(sites)

table(maxprometonsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxprometon <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum Prometon Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxprometonsites$SITE_NO))),"sites")+
  geom_point(data=maxprometonsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=800,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxprometonsites$DEC_LONG_VA)),(max(maxprometonsites$DEC_LONG_VA))),ylim=c((min(maxprometonsites$DEC_LAT_VA)),(max(maxprometonsites$DEC_LAT_VA)))) 
maxprometon

medianprometon <- prometon %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medprometonsites <- medianprometon %>%
  left_join(sites)

table(medprometonsites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


medprometon <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median Prometon Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medprometonsites$SITE_NO))),"sites")+
  geom_point(data=medprometonsites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=25,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medprometonsites$DEC_LONG_VA)),(max(medprometonsites$DEC_LONG_VA))),ylim=c((min(medprometonsites$DEC_LAT_VA)),(max(medprometonsites$DEC_LAT_VA)))) 
medprometon

allprometon <- allbigdata2[(allbigdata2$PARM_NM=="Prometon, wf"),]

allprometon <- allprometon[!is.na(allprometon$Three_Day_Sum_Rain),]
allprometon <- allprometon[!is.na(allprometon$RESULT_VA),]


plot(as.numeric(allprometon$Three_Day_Sum_Rain), as.numeric(allprometon$RESULT_VA), main = "Prometon", xlab="Rain 3 Days Preceding (in)", ylab="Prometon Concentration (ng/l)", pch=19)
allprometon_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allprometon)
abline(allprometon_model, col='blue')

test <- cor.test(as.numeric(allprometon$Three_Day_Sum_Rain), as.numeric(allprometon$RESULT_VA), method="spearman", exact=FALSE)
test
#	Spearman's rank correlation rho

#data:  as.numeric(allprometon$Three_Day_Sum_Rain) and as.numeric(allprometon$RESULT_VA)
#S = 5416181932, p-value = 8.139e-08
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.09324973 

test <- cor.test(as.numeric(allprometon$Three_Day_Sum_Rain), as.numeric(allprometon$RESULT_VA), method="kendall")
test
#		Kendall's rank correlation tau

#data:  as.numeric(allprometon$Three_Day_Sum_Rain) and as.numeric(allprometon$RESULT_VA)
#z = 5.3797, p-value = 7.459e-08
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#       tau 
#0.07156258 


ggplot(allprometon, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of Prometon',x='Rain Three Days Preceeding (in)',y='Concentration of Prometon (ng/l)') +
  theme_classic()

max(allprometon$RESULT_VA)
#[1] 1638.168
#NCSandy - SESQA




#Metolachlor SA, wf
metolachlorsa <- results[results$PARM_NM == 'Metolachlor SA, wf', ]

maximummetolachlorsa <- metolachlorsa %>%
  group_by(SITE_NO) %>%
  summarize(Maximum_Conc = max(RESULT_VA))

maxmetolachlorsasites <- maximummetolachlorsa %>%
  left_join(sites)

table(maxmetolachlorsasites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 

maxmetolachlorsa <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Maximum Metolachlor SA Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(maxmetolachlorsasites$SITE_NO))),"sites")+
  geom_point(data=maxmetolachlorsasites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Maximum_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=4000,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(maxmetolachlorsasites$DEC_LONG_VA)),(max(maxmetolachlorsasites$DEC_LONG_VA))),ylim=c((min(maxmetolachlorsasites$DEC_LAT_VA)),(max(maxmetolachlorsasites$DEC_LAT_VA)))) 
maxmetolachlorsa

medianmetolachlorsa <- metolachlorsa %>%
  group_by(SITE_NO) %>%
  summarize(Median_Conc = median(RESULT_VA))

medmetolachlorsasites <- medianmetolachlorsa %>%
  left_join(sites)

table(medmetolachlorsasites$RSQA_STUDY)
#MSQA NESQA PNSQA SESQA 
#100    95    88    77 


medmetolachlorsa <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Median Metolachlor SA Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(medmetolachlorsasites$SITE_NO))),"sites")+
  geom_point(data=medmetolachlorsasites,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Median_Conc),size=1,shape=19) + scale_color_gradient2(midpoint=2250,low="red", mid="yellow", high="blue3", space ="Lab" ) +
  coord_sf(xlim=c((min(medmetolachlorsasites$DEC_LONG_VA)),(max(medmetolachlorsasites$DEC_LONG_VA))),ylim=c((min(medmetolachlorsasites$DEC_LAT_VA)),(max(medmetolachlorsasites$DEC_LAT_VA)))) 
medmetolachlorsa

allmetolachlorsa <- allbigdata2[(allbigdata2$PARM_NM=="Metolachlor SA, wf"),]

allmetolachlorsa <- allmetolachlorsa[!is.na(allmetolachlorsa$Three_Day_Sum_Rain),]
allmetolachlorsa <- allmetolachlorsa[!is.na(allmetolachlorsa$RESULT_VA),]


plot(as.numeric(allmetolachlorsa$Three_Day_Sum_Rain), as.numeric(allmetolachlorsa$RESULT_VA), main = "Metolachlor SA", xlab="Rain 3 Days Preceding (in)", ylab="Metolachlor Concentration (ng/l)", pch=19)
allmetolachlorsa_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allmetolachlorsa)
abline(allmetolachlorsa_model, col='blue')

test <- cor.test(as.numeric(allmetolachlorsa$Three_Day_Sum_Rain), as.numeric(allmetolachlorsa$RESULT_VA), method="spearman", exact=FALSE)
test
#		Spearman's rank correlation rho

#data:  as.numeric(allmetolachlorsa$Three_Day_Sum_Rain) and as.numeric(allmetolachlorsa$RESULT_VA)
#S = 4913765860, p-value = 8.081e-14
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.1307641 


test <- cor.test(as.numeric(allmetolachlorsa$Three_Day_Sum_Rain), as.numeric(allmetolachlorsa$RESULT_VA), method="kendall")
test
#		Kendall's rank correlation tau

#data:  as.numeric(allmetolachlorsa$Three_Day_Sum_Rain) and as.numeric(allmetolachlorsa$RESULT_VA)
#z = 7.465, p-value = 8.331e-14
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau 
#0.1001864 


ggplot(allmetolachlorsa, aes(x=Three_Day_Sum_Rain,y=RESULT_VA,color=RSQA_STUDY)) +
  geom_point(alpha=0.5) +
  labs(title='Relationship Between Rain Three Days Prior and Amount of Metolachlor SA',x='Rain Three Days Preceeding (in)',y='Concentration of Metolachlor SA (ng/l)') +
  theme_classic()

max(allmetolachlorsa$RESULT_VA)
#[1] 7850.934
#INOtter2

