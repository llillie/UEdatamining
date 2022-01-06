#Fixing my PNSQA correlations

library(plyr)
library(dplyr)
library(readr)

#PNSQA DATA ISH

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PNSQACorrelation")

results <- read.csv("Results.csv")

#In the results for the RSQA data, a "<" in the REMARK_CD column indicates a non-detected pesticide. 
#Thus, we need to remove those from our data to have accurate measurements in our analysis.

results <- results[!(results$REMARK_CD=="<"),]

#I'm going to go ahead and remove the "Es" from that column as well because I believe Barbara told me they are also indicative of a nondetectable.

results <- results[!(results$REMARK_CD=="E"),]

#Alright now let's start with just pesticides instead of all organics to be nice to my computer and run some preliminary analysis.

results <- results[(results$PARM_SEQ_GRP_DS=="Organics, Pesticides"),]

sites <- read.csv("Sites.csv")

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQASPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")

PNSQASPRISMdata <- PNSQASPRISMdata[rowSums(is.na(PNSQASPRISMdata))==0,]
#removes any values from the df that are NA

PNSQASPRISMdailydata <- read.csv("PRISM_ppt_tmean_stable_4km_20150401_20150630_PNSQA.csv")

PNSQASPRISMdailydata <- PNSQASPRISMdailydata[rowSums(is.na(PNSQASPRISMdailydata))==0,]
#removes any values of NA

#Land Use ISH

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")
watershed <- watershed[which(watershed$RSQA_STUDY=='PNSQA'),]
#Perfect 88 obstacles. USE THIS - this is most important.

#To fix the SHORT_NAME issue of having the state abbreviation, an underscore, and then the name:
watershed$Name <- sub("_","",watershed$SHORT_NAME)

PNSQASPRISMdata <-  PNSQASPRISMdata[PNSQASPRISMdata$Date == "2015-03" | PNSQASPRISMdata$Date == "2015-04" | PNSQASPRISMdata$Date == "2015-05" | PNSQASPRISMdata$Date == "2015-06",]

PNSQASPRISMdata$Name <- gsub("WAMinterWQ","WAMinter",PNSQASPRISMdata$Name)

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
  colnames(out) <- c("Name", "Day", "Three_Day_Sum_Rain")
  out
}

three_day_sums <- three_day_rain_sum(PNSQASPRISMdailydata)

library(lubridate)

results$Day <- as.character(as.Date(results$SAMPLE_START_DT,"%m/%d/%Y"))
#This code chops off the time component of SAMPLE_START_DT and creates a new column called Day that is only the date.

three_day_sums <- as.data.frame(three_day_sums)
#turns the matrix into a data frame

three_day_sums$Day <- as.character(as.Date(three_day_sums$Day,"%m/%d/%Y"))
#Use this to convert three_day_sums Day order to the same order as your Day in results.


nametoRSQA <- select(watershed,SITE_NO,Name)

results <- results %>%
  inner_join(nametoRSQA)

bigdatamodified <- merge(results, three_day_sums, by = c("Name","Day"), all.x=TRUE)


#Actual Correlations.

table(bigdatamodified$PARM_NM)

atrazine <- bigdatamodified[(bigdatamodified$PARM_NM=="Atrazine, wf"),]

atrazine <- atrazine[!is.na(atrazine$Three_Day_Sum_Rain),]
atrazine <- atrazine[!is.na(atrazine$RESULT_VA),]


plot(as.numeric(atrazine$Three_Day_Sum_Rain), as.numeric(atrazine$RESULT_VA), main = "Atrazine", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
atrazine_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=atrazine)
abline(atrazine_model, col='blue')

#Running an actual Linear Regression Test on the Atrazine Data
summary(atrazine_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = atrazine)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-10.314  -7.789  -5.924  -2.707 164.930 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       9.903      1.629   6.081 5.61e-09 ***
#  as.numeric(Three_Day_Sum_Rain)    2.060      5.595   0.368    0.713    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 19.49 on 209 degrees of freedom
#Multiple R-squared:  0.0006482,	Adjusted R-squared:  -0.004133 
#F-statistic: 0.1356 on 1 and 209 DF,  p-value: 0.7131


###########################################################################################################
#06/07/2021 - trying my plots without using linear modeling

library(ggplot2)

atrazineggplot <- atrazine[!is.na(atrazine$Three_Day_Sum_Rain),]

ggplot(data=atrazineggplot, aes(x=Three_Day_Sum_Rain,y=RESULT_VA)) + geom_point() #+ geom_smooth(method='lm',se=FALSE)
#Ends up the same as the alternative way I have been doing it.


###############################################################################################################


triclopyr <- bigdatamodified[(bigdatamodified$PARM_NM=="Triclopyr, wf"),]

triclopyr <- triclopyr[!is.na(triclopyr$Three_Day_Sum_Rain),]
triclopyr <- triclopyr[!is.na(triclopyr$RESULT_VA),]

plot(as.numeric(triclopyr$Three_Day_Sum_Rain), as.numeric(triclopyr$RESULT_VA), main = "Triclopyr", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
triclopyr_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=triclopyr)
abline(triclopyr_model, col='blue')

summary(triclopyr_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = triclopyr)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-113.52  -66.90  -40.76    9.42 1713.09 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       95.44      16.27   5.865 2.54e-08 ***
#  as.numeric(Three_Day_Sum_Rain)    41.04      51.36   0.799    0.425    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 164.6 on 158 degrees of freedom
#(51 observations deleted due to missingness)
#Multiple R-squared:  0.004024,	Adjusted R-squared:  -0.002279 
#F-statistic: 0.6384 on 1 and 158 DF,  p-value: 0.4255

hexazinone <- bigdatamodified[(bigdatamodified$PARM_NM=="Hexazinone, wf"),]

hexazinone <- hexazinone[!is.na(hexazinone$Three_Day_Sum_Rain),]
hexazinone <- hexazinone[!is.na(hexazinone$RESULT_VA),]


plot(as.numeric(hexazinone$Three_Day_Sum_Rain), as.numeric(hexazinone$RESULT_VA), main = "Hexazinone", xlab="Rain 3 Days Preceding (in)", ylab="Hexazinone Concentration (ng/l)", pch=19)
hexazinone_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=hexazinone)
abline(hexazinone_model, col='blue')

#Running an actual Linear Regression Test on the Hexazinone Data
summary(hexazinone_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = hexazinone)
#Residuals:
#  Min     1Q Median     3Q    Max 
#-5.375 -2.493 -1.460  0.570 67.702 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      3.6944     0.4753   7.773  1.5e-13 ***
#  as.numeric(Three_Day_Sum_Rain)   3.3595     1.5375   2.185   0.0297 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 6.516 on 278 degrees of freedom
#Multiple R-squared:  0.01688,	Adjusted R-squared:  0.01335 
#F-statistic: 4.774 on 1 and 278 DF,  p-value: 0.02972


#Re-running our PCA stuff to see what happens now. Ditching this idea for now. DOesn't work because of all the NAs.

library(tidyr)

medianvalues <- results %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA))
View(medianvalues)

medianvalues <- medianvalues[complete.cases(medianvalues),]

widemedianvalues <- pivot_wider(medianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)

PCA1 <- widemedianvalues

PCA1 <- as.data.frame(PCA1)
rownames(PCA1)<-PCA1$SITE_NO
PCA1 <- PCA1[,-1]
PCA1 <- PCA1[,which(apply(PCA1,2,var) != 0)]

finaldata1 <- PCA1[complete.cases(PCA1),]

metadata1<-sites[match(rownames(PCA1),sites$SITE_NO),]

data_cor1 <- cor(PCA1)



###################################################################################################################
#Let's run some basic T-tests to get some ideas flowing

#This gives me an average rainfall value for each site that I can then treat as a single variable. It averages the three active sampling months.
PNSQAPRISMmean <- PNSQASPRISMdata %>%
  group_by(Name) %>%
  summarize(Mean = mean(ppt..inches.))

#The following two lines of code gives me basic information on all 88 of the sites which I can then pull from to run some ttests.

nametoRSQA <- select(watershed,SITE_NO,Name)

sites <- sites %>%
  inner_join(nametoRSQA)

#This sets up my data. Currently, this simply tacks on the average rainfall received at each site during the active sampling months.
ttest <- merge(sites, PNSQAPRISMmean, by = c("Name"), all.x=TRUE)

#Research question: Is there a difference in mean average rainfall received by streams in Oregon as compared to streams in Washington?
# Create vectors for Oregon and Washington average rainfall
oregon <- ttest$Mean[ttest$STATE_ABBREV=='OR']
washington <- ttest$Mean[ttest$STATE_ABBREV=='WA']

# Histogram of Oregon average rainfall
hist(oregon, main='Distribution of Average Rainfall in Oregon', xlab='Average Rainfall (in)', right=F)
#This has a slight right skew, but for now I am going to classify it as "normal enough" so I don't have to transform my variable.
#NOTE TO SELF: Come back and try transforming.

# Histogram of Washington average rainfall
hist(washington, main='Distribution of Average Rainfall in Washington', xlab='Average Rainfall (in)', right=F)
#This has a fairly heavy right skew, again going to assume "normal enough" as of now.(06/02/2021)

#Time to run a Levene's Test to check for equal variances.
#Call the car package
library(car)

#Confirm equal variances
leveneTest(Mean~STATE_ABBREV,data=ttest)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1   0.003 0.9568
#      86    
#LARGE p-value, which is great!
#The null hypothesis is that both groups have equal variance, which we fail to reject (p = 0.96). Thus, this data passes Levene's test of equal variance.

#Run independent t test with equal variances
t.test(Mean~STATE_ABBREV,data=ttest,var.equal=T)
#Two Sample t-test
#data:  Mean by STATE_ABBREV
#t = 1.0634, df = 86, p-value = 0.2906
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.08969875  0.29605185
#sample estimates:
#  mean in group OR mean in group WA 
#2.316714         2.213538 

#There is no evidence of a significant difference in the mean average rainfall received during the active sampling months between Oregon and Washington (t = 1.06, df = 86, p = 0.29).


#Going back and transforming variables in t-test 1.
logo <- log(oregon)
hist(logo, main='Distribution of the Log of Average Rainfall in Oregon', xlab='Log of Average Rainfall (log(in))', right=F)


logw <- log(washington)
hist(logw, main='Distribution of the Log of Average Rainfall in Washington', xlab='Log of Average Rainfall (log(in)', right=F)
#The logs are better, but not beautiful. I'm going to go ahead and run the test and see what happens. Can try transforming with another function later on if needed.

ttest$lograin <- log(ttest$Mean)

leveneTest(lograin~STATE_ABBREV,data=ttest)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1  0.0042 0.9485
#      86        
#Still a large p-value, which is great!
#The null hypothesis is that both groups have equal variance, which we fail to reject (p = 0.95). Thus, this data passes Levene's test of equal variance.

#Run independent t test with equal variances
t.test(lograin~STATE_ABBREV,data=ttest,var.equal=T)
#Two Sample t-test
#data:  lograin by STATE_ABBREV
#t = 1.2193, df = 86, p-value = 0.226
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.03138804  0.13098106
#sample estimates:
#  mean in group OR mean in group WA 
#0.8249582        0.7751617 
#This brought the p-value CLOSER to significance, but not by much. Only by ~0.05. So rainfall difference is still not necessarily significant. 
#There is no evidence of a significant difference in the mean average rainfall received during the active sampling months between Oregon and Washington (t = 1.2193, df = 86, p-value = 0.226).


#TTEST ROUND 2 - let's look at some average pesticide values.

#The following code reduces my results dataframe to only the pesticides.
pesticides <- results[(results$PARM_SEQ_GRP_DS=="Organics, Pesticides"),]

#Changing my working directory so I can pull the parameter file which contains the units of each pesticide measured.
setwd(dir="C:/Users/llill/OneDrive/Documents/Biostats/PNSQAPesticides")

#Pulling the file.
parm <- read.csv("USGSParameterCodes.csv")

#Merging my pesticides results frame with the parameters frame so I can convert the units to a consistent value.
newpesticides <- merge(pesticides, parm, by = c("PARM_NM"), all.x=TRUE)

#This tells me how many unique parameter units there are.
unique(newpesticides[c("PARM_UNT_TX")])

#This tells me exactly how many of each unit measurement there is in my fata frame.
table(newpesticides$PARM_UNT_TX)

#Since ug/kg and ug/l are essentially the same, I decided to convert ng/l to match that value.
ng <- newpesticides[newpesticides$PARM_UNT_TX == 'ng/l',]

ng$RESULT_VA <- ng$RESULT_VA * 0.001

#I then pulled all the original pesticide values that were measured in ug/kg and ug/l
original <- newpesticides[newpesticides$PARM_UNT_TX != 'ng/l',]

#I am now combining the two separate dataframes into a single one.
polished <- rbind(ng, original)

#This adds each unique pesticide measured at each site into one total measurement for it. Ex, if one pesticide was measured 5 times at stream A, I am adding all 5 of those times to a total measurement for stream A.
PNSQAtotals <- polished %>%
  group_by(PARM_NM,SITE_NO) %>%
  summarize(Total = sum(RESULT_VA))

#Getting rid of the pesticides with a NA for its total value
PNSQAtotals <- PNSQAtotals[rowSums(is.na(PNSQAtotals))==0,]

#Removing the name of each pesticided.
PNSQAtotals <- subset(PNSQAtotals, select=-c(PARM_NM))

#Averaging the amount of pesticides found at each site.
PNSQAmeans <- PNSQAtotals %>%
  group_by(SITE_NO) %>%
  summarize(Mean_Pesticide_Concentration = mean(Total))

#Now that we have our data set up, time to run the actual ttest comparing if the average amount of pesticides is different between Oregon and Washington.

#Combining the average amount of pesticides with site information so I can get the state abbreviation associated with each site.
ttest2 <- merge(sites, PNSQAmeans, by = c("SITE_NO"), all.x=TRUE)

#Research question: Is there a difference in mean average pesticides received by streams in Oregon as compared to streams in Washington?
# Create vectors for Oregon and Washington average pesticides
oregon2 <- ttest2$Mean_Pesticide_Concentration[ttest2$STATE_ABBREV=='OR']
washington2 <- ttest2$Mean_Pesticide_Concentration[ttest2$STATE_ABBREV=='WA']

#Removing the NAs from the washington2 vector
washington2 <- washington2[!is.na(washington2)]

# Histogram of Oregon average pesticide conc
hist(oregon2, main='Distribution of Average Pesticides in Oregon', xlab='Average Pesticide Concentration (ug/kg)', right=F)
#This has a heavy right skew, but for now I am going to classify it as "normal enough" so I don't have to transform my variable.

#Alright, let's try transforming because if I don't, I fail Levene's test by a landslide.
logo2 <- log(oregon2)
hist(logo2, main='Distribution of the Log of Average Pesticides in Oregon', xlab='Log of Average Pesticide Concentration (ug/kg)', right=F)
#Not perfect, but MUCH better.

# Histogram of Washington average pesticice conc
hist(washington2, main='Distribution of Average Pesticides in Washington', xlab='Average Pesticide Concentration (log(ug/kg))', right=F)
#This has a  heavy right skew, again going to assume "normal enough" as of now.(06/02/2021)

#Alright, let's try transforming because if I don't, I fail Levene's test by a landslide.
logw2 <- log(washington2)
hist(logw2, main='Distribution of the Log of Average Pesticides in Washington', xlab='Log of Average Pesticide Concentration (log(ug/kg)', right=F)
#Oh now this is a beautiful distribution.

#Time to run a Levene's Test to check for equal variances.
#Call the car package
library(car)

#Now for the sake of Levene's I need to go back into my ttest2 dataframe and convert the average pesticide concentrations into the transformed log version.
ttest2$logpestconc <- log(ttest2$Mean_Pesticide_Concentration)

#Getting rid of all rows that have NA for logpestconc, reduces my number of streams to 84. Interesting & concerning
ttest2 <- ttest2[!is.na(ttest2$logpestconc),]
#However, running this line of code has no impact on my results for the Levene's test or the actual t-test. Probably because the NAs can't be included in either of these tests... which makes sense.
#I should probably figure out why I'm losing sites and which they are though.

#Confirm equal variances
leveneTest(logpestconc~STATE_ABBREV,data=ttest2)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1  1.6757 0.1991
#     82        
#p-value is great!
#The null hypothesis is that both groups have equal variance, which we fail to reject (p = 0.1991). Thus, this data passes Levene's test of equal variance.

#Run independent t test with equal variances
t.test(logpestconc~STATE_ABBREV,data=ttest2,var.equal=T)

#Two Sample t-test
#data:  logpestconc by STATE_ABBREV
#t = 2.2159, df = 82, p-value = 0.02947
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.1012255 1.8783405
#sample estimates:
#  mean in group OR mean in group WA 
#-2.403597        -3.393380 
#There IS evidence of a significant difference in the log of average pesticide concentrations based on this data (t = 2.2159, df = 82, p-value = 0.02947).

############################################################################################################################################################################
#Looking at the pesticide concentrations and rainfall across all 5 regions.

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/AllRegPest")

results2 <- read.csv("Results.csv")

#Removing stuff I don't need from the dataframe - aka the pesticides that were non-detectables 
results2 <- results2[!(results2$REMARK_CD=="<"),]
results2 <- results2[!(results2$REMARK_CD=="E"),]
#Brings my obsercations down to 53,114 - crazy.


#RQ2: Does mean average pesticide concentrations differ based on the region where the streams were sampled?

parm <- read.csv("USGSParameterCodes.csv")

#Merging my pesticides results frame with the parameters frame so I can convert the units to a consistent value.
units <- merge(results2, parm, by = c("PARM_NM"), all.x=TRUE)

#This tells me how many unique parameter units there are.
unique(units[c("PARM_UNT_TX")])

#This tells me exactly how many of each unit measurement there is in my data frame.
table(units$PARM_UNT_TX)

#Since ug/kg and ug/l are essentially the same, I decided to convert ng/l to match that value.
ng2 <- units[units$PARM_UNT_TX == 'ng/l',]

ng2$RESULT_VA <- ng2$RESULT_VA * 0.001

#I then pulled all the original pesticide values that were measured in ug/kg and ug/l
original2 <- units[units$PARM_UNT_TX != 'ng/l',]

#I am now combining the two separate dataframes into a single one.
polished2 <- rbind(ng2, original2)

#polished2 <- polished2[!is.na(polished2$RESULT_VA),]

#This adds each unique pesticide measured at each site into one total measurement for it. Ex, if one pesticide was measured 5 times at stream A, I am adding all 5 of those times to a total measurement for stream A.
alltotals <- polished2 %>%
  group_by(PARM_NM,SITE_NO) %>%
  summarize(Total = sum(RESULT_VA))

#Getting rid of the pesticides with a NA for its total value
alltotals <- alltotals[rowSums(is.na(alltotals))==0,]

#Removing the name of each pesticided.
alltotals <- subset(alltotals, select=-c(PARM_NM))

#Averaging the amount of pesticides found at each site.
allmeans <- alltotals %>%
  group_by(SITE_NO) %>%
  summarize(Mean_Pesticide_Concentration = mean(Total))
#This ends up with average pesticide concentrations for 427 streams - I'm losing quite a few, investigate this later.

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/AllRegPest")
sites2 <- read.csv("Sites.csv")

anova2 <- merge(allmeans, sites2, by = c("SITE_NO"), all.x=TRUE)

#Confirm normality
boxplot(Mean_Pesticide_Concentration~RSQA_STUDY, data=anova2, xlab='Region of the Study', ylab='Average Pesticide Concentration (ug/kg)', main='Average Pesticide Concentration by Region', col=c('orchid','pale green','salmon', 'light blue', 'yellow'))
#Looks like I might need to transform this. Come back to later.



#06/04/2021 - Continuing work on my ANOVA tests

#Confirming right-skew suspicion that emerged from the box plots above.
hist(anova2$Mean_Pesticide_Concentration, main='Distribution of Average Pesticide Concentrations', xlab='Average Pesticide Concentration (ug/kg)', right=F)

#Now time to transform the avg pest conc variable in my anova2 data frame.
anova2$logpestconc <- log(anova2$Mean_Pesticide_Concentration)

#Went from a strong right-skew to a slight left-skew. Let's look at the box plots to decide if I think this is sufficient enough or if I should transform more.
hist(anova2$logpestconc, main='Distribution of the Log of the Average Pesticide Concentrations', xlab='Log of Average Pesticide Concentration (log(ug/kg))', right=F)

boxplot(logpestconc~RSQA_STUDY, data=anova2, xlab='Region of the Study', ylab='Average Pesticide Concentration (log(ug/kg))', main='Log of Average Pesticide Concentration by Region', col=c('orchid','pale green','salmon', 'light blue', 'yellow'))
#OH WOW. That is SOOOO much better - honestly kind of beautiful how significant of a change that simple log function just created.
#I would say the majority of the regions look VERY normal - my one concern is the Levene's test for equal variance.

#Testing a series of transformations
#anova2$Log10_Pesticide_Concentration <- log10(anova2$Mean_Pesticide_Concentration)
#anova2$Sqrt_Pesticide_Concentration <- sqrt(anova2$Mean_Pesticide_Concentration)
#anova2$Inverse_Pesticide_Concentration <- 1/(anova2$Mean_Pesticide_Concentration)
#anova2$Exp_Pesticide_Concentration <- exp(anova2$Mean_Pesticide_Concentration)

#hist(anova2$Log10_Pesticide_Concentration, main='Distribution of the Log10 of the Average Pesticide Concentrations', xlab='Log10 of Average Pesticide Concentration (ln(ug/kg))', right=F)
#hist(anova2$Sqrt_Pesticide_Concentration, main='Distribution of the Square Root of the Average Pesticide Concentrations', xlab='Square Root of Average Pesticide Concentration (sqrt(ug/kg))', right=F)
#hist(anova2$Inverse_Pesticide_Concentration, main='Distribution of the Inverse of the Average Pesticide Concentrations', xlab='Inverse of Average Pesticide Concentration (1/(ug/kg))', right=F)
#hist(anova2$Exp_Pesticide_Concentration, main='Distribution of the Exponential of the Average Pesticide Concentrations', xlab='Exponential of Average Pesticide Concentration (exp(ug/kg))', right=F)


#Okay, the log transformation was the best by a landslide, going to stick with it.

#Confirm equal variance
library(car)
leveneTest(logpestconc~RSQA_STUDY, data=anova2)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)    
#group   4  28.314 < 2.2e-16 ***
#      422                      
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#This data does not pass the Levene's Test, the equal variance assumption is not met.

#Define ANOVA model
my_anova <- lm(logpestconc~RSQA_STUDY, data=anova2)

Anova(my_anova,type=3)
#Anova Table (Type III tests)
#Response: logpestconc
#Sum Sq  Df F value Pr(>F)    
#(Intercept)    2.93   1  0.9144 0.3395    
#RSQA_STUDY   648.13   4 50.6493 <2e-16 ***
#  Residuals   1350.03 422                   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#According to these results, there is evidence of a significant difference in the mean of the log of the avg pesticide concentration across the five regions of the RSQA study (F = 50.65, df = (4,422), p < 2e-16).

#Calculate R-squared for one-way ANOVA
summary(my_anova)$r.squared
#[1] 0.3243647

library(emmeans)

#Post-hoc comparisons
emmeans(my_anova, pairwise~RSQA_STUDY)
#$emmeans
#RSQA_STUDY  emmean    SE  df lower.CL upper.CL
#CSQA        0.1900 0.199 422   -0.201    0.581
#MSQA        0.0433 0.179 422   -0.308    0.395
#NESQA      -1.9556 0.192 422   -2.332   -1.579
#PNSQA      -2.9820 0.195 422   -3.366   -2.598
#SESQA      -1.7347 0.207 422   -2.141   -1.329
#Confidence level used: 0.95 
#$contrasts
#contrast      estimate    SE  df t.ratio p.value
#CSQA - MSQA      0.147 0.267 422  0.549  0.9820 
#CSQA - NESQA     2.146 0.276 422  7.769  <.0001 
#CSQA - PNSQA     3.172 0.279 422 11.389  <.0001 
#CSQA - SESQA     1.925 0.287 422  6.715  <.0001 
#MSQA - NESQA     1.999 0.262 422  7.623  <.0001 
#MSQA - PNSQA     3.025 0.265 422 11.428  <.0001 
#MSQA - SESQA     1.778 0.273 422  6.508  <.0001 
#NESQA - PNSQA    1.026 0.274 422  3.752  0.0019 
#NESQA - SESQA   -0.221 0.282 422 -0.784  0.9353 
#PNSQA - SESQA   -1.247 0.284 422 -4.390  0.0001 
#P value adjustment: tukey method for comparing a family of 5 estimates 

#For RQ1: Does mean average rainfall differ based on the region where the streams were sampled?
#Going to borrow a lot of code from my 30 Year Ratios Script

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")

PNSQAPRISMdata <- PNSQAPRISMdata[rowSums(is.na(PNSQAPRISMdata))==0,]

CSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201701_201709_CSQA.csv")

CSQAPRISMdata <- CSQAPRISMdata[rowSums(is.na(CSQAPRISMdata))==0,]

NESQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201601_201609_NESQA.csv")

NESQAPRISMdata <- NESQAPRISMdata[rowSums(is.na(NESQAPRISMdata))==0,]

SESQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201401_201409_SESQA.csv")

SESQAPRISMdata <- SESQAPRISMdata[rowSums(is.na(SESQAPRISMdata))==0,]

MSQAPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201301_201309_MSQA.csv")

MSQAPRISMdata <- MSQAPRISMdata[rowSums(is.na(MSQAPRISMdata))==0,]

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/LandUseData")

watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

watershed$Name <- as.character(gsub("_","",watershed$SHORT_NAME))

watershed$Name <- gsub("\\.","",watershed$Name)

nametoRSQA <- select(watershed,SITE_NO,Name)

#Reset sites2 before running this code if you are running all of the code in this script at once.
sitewidesites <- sites2 %>%
  inner_join(nametoRSQA)

#PNSQA
PNSQAPRISMdata <-  PNSQAPRISMdata[PNSQAPRISMdata$Date == "2015-03" | PNSQAPRISMdata$Date == "2015-04" | PNSQAPRISMdata$Date == "2015-05" | PNSQAPRISMdata$Date == "2015-06",]

#CSQA
CSQAPRISMdata <-  CSQAPRISMdata[CSQAPRISMdata$Date == "2017-03" | CSQAPRISMdata$Date == "2017-04" | CSQAPRISMdata$Date == "2017-05",]

#NESQA
NESQAPRISMdata <-  NESQAPRISMdata[NESQAPRISMdata$Date == "2016-05" | NESQAPRISMdata$Date == "2016-06" | NESQAPRISMdata$Date == "2016-07",]

#SESQA
SESQAPRISMdata <-  SESQAPRISMdata[SESQAPRISMdata$Date == "2014-03" | SESQAPRISMdata$Date == "2014-04" | SESQAPRISMdata$Date == "2014-05" | SESQAPRISMdata$Date == "2014-06",]

#MSQA
MSQAPRISMdata <-  MSQAPRISMdata[MSQAPRISMdata$Date == "2013-04" | MSQAPRISMdata$Date == "2013-05" | MSQAPRISMdata$Date == "2013-06" | MSQAPRISMdata$Date == "2013-07",]


#ONLY using thirty year stuff here to get the necessary name changes set-up, otherwise ignore 30 year info. I only want the avg rainfall.

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

thirtyyear <- read.csv("PRISM_ppt_tmean_30yr_normal_4km_monthly_normals_RSQA.csv")

thirtyyear <- thirtyyear[rowSums(is.na(thirtyyear))==0,]

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/30YearRatioMapping")

namechanges <- read.csv("NameChanges.csv")

namechanges <- subset(namechanges, select=-c(Region))

temp <- unique(thirtyyear[!(thirtyyear$Name %in% namechanges$Name),1])

tempdup <- as.data.frame(cbind(temp, temp))

colnames(tempdup) <- c("Name", "RSQAName")
namechanges <- rbind(namechanges, tempdup)


PNSQAPRISMdata <- merge(namechanges, PNSQAPRISMdata, by="Name", all.y = T)
PNSQAPRISMdata <- subset(PNSQAPRISMdata,select=-c(Name))
PNSQAPRISMdata <- PNSQAPRISMdata %>%
  rename(Name=RSQAName)

CSQAPRISMdata <- merge(namechanges, CSQAPRISMdata, by="Name", all.y = T)
CSQAPRISMdata <- subset(CSQAPRISMdata,select=-c(Name))
CSQAPRISMdata <- CSQAPRISMdata %>%
  rename(Name=RSQAName)

NESQAPRISMdata <- merge(namechanges, NESQAPRISMdata, by="Name", all.y = T)
NESQAPRISMdata <- subset(NESQAPRISMdata,select=-c(Name))
NESQAPRISMdata <- NESQAPRISMdata %>%
  rename(Name=RSQAName)

SESQAPRISMdata <- merge(namechanges, SESQAPRISMdata, by="Name", all.y = T)
SESQAPRISMdata <- subset(SESQAPRISMdata,select=-c(Name))
SESQAPRISMdata <- SESQAPRISMdata %>%
  rename(Name=RSQAName)

MSQAPRISMdata <- merge(namechanges, MSQAPRISMdata, by="Name", all.y = T)
MSQAPRISMdata <- subset(MSQAPRISMdata,select=-c(Name))
MSQAPRISMdata <- MSQAPRISMdata %>%
  rename(Name=RSQAName)

PNSQAPRISMmean <- PNSQAPRISMdata %>%
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

#This gives me the average rainfall for all of the sites.
fullmeans <- as.data.frame(rbind(CSQAPRISMmean,NESQAPRISMmean,PNSQAPRISMmean,SESQAPRISMmean,MSQAPRISMmean))

anova1 <- merge(sitewidesites, fullmeans, by = c("Name"), all.x=TRUE)
#Losing one site, unfortunately. Go back and figure out where later on.

#Alright, data frame is officially set-up, now going to run the anova test.

boxplot(Mean~RSQA_STUDY, data=anova1, xlab='Region of the Study', ylab='Average Rainfall (in)', main='Average Rainfall by Region', col=c('orchid','pale green','salmon', 'light blue', 'yellow'))
#Looks like I might need to transform this for the sake of equal variance. Going to try now and see if anything makes it better.

#Normality still looks pretty good to me.
hist(anova1$Mean, main='Distribution of Average Rainfall', xlab='Average Rainfall (in)', right=F)

#Attempt one at transformation of the avg rainfall variable.
anova1$lograin <- log(anova1$Mean)

#Didn't seem to do too much in terms of normality. Shifted the graph, that's for sure. But I don't think it's better/worse than what I was working with before.
hist(anova1$lograin, main='Distribution of the Log of the Average Rainfall', xlab='Log of Average Rainfall (log(in))', right=F)

boxplot(lograin~RSQA_STUDY, data=anova1, xlab='Region of the Study', ylab='Average Rainfall (log(in))', main='Log of Average Rainfall by Region', col=c('orchid','pale green','salmon', 'light blue', 'yellow'))
#I think this would harm equal variance. I've got four that are similar in this case but one that's dramatically different.

#Testing a series of transformations
#anova1$Log10_Rain <- log10(anova1$Mean)
anova1$Sqrt_Rain <- sqrt(anova1$Mean)
#anova1$Inverse_Rain <- 1/(anova1$Mean)
#anova1$Exp_Rain <- exp(anova1$Mean)

#hist(anova1$Log10_Rain, main='Distribution of the Log10 of the Average Pesticide Concentrations', xlab='Log10 of Average Pesticide Concentration (ln(ug/kg))', right=F)
hist(anova1$Sqrt_Rain, main='Distribution of the Square Root of the Average Pesticide Concentrations', xlab='Square Root of Average Pesticide Concentration (sqrt(ug/kg))', right=F)
#Okay the sqrt of Avg Rain looks beautiful in terms of normality - let's see what it does for my boxplot though.
#hist(anova1$Inverse_Rain, main='Distribution of the Inverse of the Average Pesticide Concentrations', xlab='Inverse of Average Pesticide Concentration (1/(ug/kg))', right=F)
#hist(anova1$Exp_Rain, main='Distribution of the Exponential of the Average Pesticide Concentrations', xlab='Exponential of Average Pesticide Concentration (exp(ug/kg))', right=F)

boxplot(Sqrt_Rain~RSQA_STUDY, data=anova1, xlab='Region of the Study', ylab='Average Rainfall (sqrt(in))', main='Sqrt of Average Rainfall by Region', col=c('orchid','pale green','salmon', 'light blue', 'yellow'))
#It did a little but I don't know if it's ENOUGH to change my use of the original raw data...


#Confirm equal variance
library(car)
leveneTest(Mean~RSQA_STUDY, data=anova1)
#Levene's Test for Homogeneity of Variance (center = median)
#Df F value    Pr(>F)    
#group   4  10.938 1.709e-08 ***
#  477                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#This data does not pass the Levene's Test, the equal variance assumption is not met.

#Define ANOVA model
my_anova1 <- lm(Mean~RSQA_STUDY, data=anova1)

Anova(my_anova1,type=3)
#Anova Table (Type III tests)
#Response: Mean
#Sum Sq  Df F value    Pr(>F)  
#(Intercept) 330.11   1  519.80 < 2.2e-16 ***
#  RSQA_STUDY  755.41   4  297.37 < 2.2e-16 ***
#  Residuals   302.93 477   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#According to these results, there is evidence of a significant difference in the mean of the avg rainfall across the five regions of the RSQA study (F = 297.37,df = (4,477), p < 2e-16).

#Calculate R-squared for one-way ANOVA
summary(my_anova1)$r.squared
#[1] 0.713767

library(emmeans)

#Post-hoc comparisons
emmeans(my_anova1,pairwise~RSQA_STUDY)
#$emmeans
#RSQA_STUDY emmean     SE  df lower.CL upper.CL
#CSQA         1.97 0.0864 477     1.80     2.14
#MSQA         5.19 0.0797 477     5.04     5.35
#NESQA        2.84 0.0818 477     2.68     3.00
#PNSQA        2.25 0.0850 477     2.09     2.42
#SESQA        4.41 0.0746 477     4.26     4.56
#Confidence level used: 0.95 
#$contrasts
#contrast      estimate    SE  df t.ratio p.value
#CSQA - MSQA     -3.222 0.118 477 -27.409 <.0001
#CSQA - NESQA    -0.872 0.119 477  -7.327 <.0001 
#CSQA - PNSQA    -0.284 0.121 477  -2.342 0.1335 
#CSQA - SESQA    -2.438 0.114 477 -21.348 <.0001 
#MSQA - NESQA     2.351 0.114 477  20.589 <.0001 
#MSQA - PNSQA     2.939 0.116 477  25.228 <.0001 
#MSQA - SESQA     0.785 0.109 477   7.185 <.0001 
#NESQA - PNSQA    0.588 0.118 477   4.986 <.0001 
#NESQA - SESQA   -1.566 0.111 477 -14.147 <.0001 
#PNSQA - SESQA   -2.154 0.113 477 -19.049 <.0001 
#P value adjustment: tukey method for comparing a family of 5 estimates 

###########################################################################################################
##########################################################################################################
#IGNORE FOR NOW

#PCA is a mess so far. I might just skip to my correlations directly when I start working again.

library(tidyr)

medianvalues <- results2 %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA))
View(medianvalues)

widemeanvalues <- pivot_wider(medianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)

PCA1 <- widemedianvalues[,colSums(is.na(widemedianvalues)) < 1]

PCA1 <- as.data.frame(PCA1)
rownames(PCA1)<-PCA1$SITE_NO
PCA1 <- PCA1[,-1]
PCA1 <- PCA1[,which(apply(PCA1,2,var) != 0)]

finaldata1 <- PCA1[complete.cases(PCA1),]

metadata1<-sites[match(rownames(PCA1),sites$SITE_NO),]

data_cor1 <- cor(PCA1)

library(pheatmap)

pheatmap(data_cor1, annotations=rownames(data_cor1),show_rownames=T, show_colnames=T)

data_cor1t<-cor(t(PCA1))

pheatmap(data_cor1t,show_rownames=F, show_colnames=F)

pheatmap(data_cor1,show_rownames=F, show_colnames=F)

library(ggbiplot)

prcompData<-prcomp(PCA1,center=T, scale=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=F, groups=paste(metadata1$STATE_NM), ellipse=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=T, groups=paste(metadata1$STATE_NM), ellipse=T)

##############################################################################################################
############################################################################################################

#6/24/2021

library(ggpubr)

#RQ2 now with a Kruskal-Wallis Test

compare_means(Mean_Pesticide_Concentration ~ RSQA_STUDY, anova2, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")

# A tibble: 1 x 6
#.y.                                 p    p.adj p.format p.signif method        
#<chr>                           <dbl>    <dbl> <chr>    <chr>    <chr>         
#  1 Mean_Pesticide_Concentration 3.44e-42 3.40e-42 <2e-16   ****     Kruskal-Wallis

compare_means(Mean_Pesticide_Concentration ~ RSQA_STUDY , anova2, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")

# A tibble: 10 x 8
#.y.                          group1 group2        p    p.adj p.format p.signif method  
#<chr>                        <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr>   
#  1 Mean_Pesticide_Concentration NESQA  SESQA  2.78e- 2 4.00e- 2 0.0278   *        Wilcoxon
#2 Mean_Pesticide_Concentration NESQA  MSQA   2.15e-25 7.20e-25 < 2e-16  ****     Wilcoxon
#3 Mean_Pesticide_Concentration NESQA  CSQA   9.19e- 1 9.20e- 1 0.9191   ns       Wilcoxon
#4 Mean_Pesticide_Concentration NESQA  PNSQA  7.79e- 2 8.70e- 2 0.0779   ns       Wilcoxon
#5 Mean_Pesticide_Concentration SESQA  MSQA   5.54e-22 1.40e-21 < 2e-16  ****     Wilcoxon
#6 Mean_Pesticide_Concentration SESQA  CSQA   8.58e- 3 1.40e- 2 0.0086   **       Wilcoxon
#7 Mean_Pesticide_Concentration SESQA  PNSQA  2.91e- 5 5.80e- 5 2.9e-05  ****     Wilcoxon
#8 Mean_Pesticide_Concentration MSQA   CSQA   5.24e-28 5.20e-27 < 2e-16  ****     Wilcoxon
#9 Mean_Pesticide_Concentration MSQA   PNSQA  1.15e-27 5.80e-27 < 2e-16  ****     Wilcoxon
#10 Mean_Pesticide_Concentration CSQA   PNSQA  3.26e- 2 4.10e- 2 0.0326   *        Wilcoxon

#RQ1 now with a Kruskal-Wallis Test


compare_means(Mean ~ RSQA_STUDY, anova1, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")

# A tibble: 1 x 6
#.y.          p    p.adj p.format p.signif method        
#<chr>    <dbl>    <dbl> <chr>    <chr>    <chr>         
#  1 Mean  3.88e-69 3.90e-69 <2e-16   ****     Kruskal-Wallis


compare_means(Mean ~ RSQA_STUDY , anova1, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")

# A tibble: 10 x 8
#.y.   group1 group2        p    p.adj p.format p.signif method  
#<chr> <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr>   
#  1 Mean  SESQA  CSQA   2.32e-27 4.60e-27 < 2e-16  ****     Wilcoxon
#2 Mean  SESQA  NESQA  2.50e-23 4.20e-23 < 2e-16  ****     Wilcoxon
#3 Mean  SESQA  MSQA   5.09e- 7 5.70e- 7 5.1e-07  ****     Wilcoxon
#4 Mean  SESQA  PNSQA  3.59e-28 9.00e-28 < 2e-16  ****     Wilcoxon
#5 Mean  CSQA   NESQA  5.27e-11 7.50e-11 5.3e-11  ****     Wilcoxon
#6 Mean  CSQA   MSQA   2.89e-31 1.40e-30 < 2e-16  ****     Wilcoxon
#7 Mean  CSQA   PNSQA  7.34e- 3 7.30e- 3 0.0073   **       Wilcoxon
#8 Mean  NESQA  MSQA   5.94e-30 2.00e-29 < 2e-16  ****     Wilcoxon
#9 Mean  NESQA  PNSQA  1.22e- 7 1.50e- 7 1.2e-07  ****     Wilcoxon
#10 Mean  MSQA   PNSQA  4.80e-32 4.80e-31 < 2e-16  ****     Wilcoxon


#Three Day Correlations Across all Sites

#subset <- subset(anova1, select=c("SITE_NO", "Mean"))
#fulldata <- anova2 %>%
#  left_join(subset)

fulldata <- merge(results2, sites2, by = c("SITE_NO"), all.x=TRUE)

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

results2$Day <- as.character(as.Date(results2$SAMPLE_START_DT,"%m/%d/%Y"))
#This code chops off the time component of SAMPLE_START_DT and creates a new column called Day that is only the date.

three_day_sums <- as.data.frame(three_day_sums)
#turns the matrix into a data frame

three_day_sums$Day <- as.character(as.Date(three_day_sums$Day,"%m/%d/%Y"))
#Use this to convert three_day_sums Day order to the same order as your Day in results.


nametoRSQA <- select(watershed,SITE_NO,Name)

results2 <- results2 %>%
  inner_join(nametoRSQA)

allbigdata <- merge(results2, three_day_sums, by = c("Name","Day"), all.x=TRUE)

allbigdata2 <- allbigdata[!is.na(allbigdata$Three_Day_Sum_Rain),]

allatrazine <- allbigdata2[(allbigdata2$PARM_NM=="Atrazine, wf"),]

allatrazine <- allatrazine[!is.na(allatrazine$Three_Day_Sum_Rain),]
allatrazine <- allatrazine[!is.na(allatrazine$RESULT_VA),]


plot(as.numeric(allatrazine$Three_Day_Sum_Rain), as.numeric(allatrazine$RESULT_VA), main = "Atrazine", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
allatrazine_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allatrazine)
abline(allatrazine_model, col='blue')

#Running an actual Linear Regression Test on the Atrazine Data
summary(allatrazine_model)

#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = allatrazine)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-3226   -514   -286   -168 119595 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      289.23      86.84   3.331 0.000883 ***
#  as.numeric(Three_Day_Sum_Rain)   668.16     119.12   5.609 2.32e-08 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3224 on 1959 degrees of freedom
#Multiple R-squared:  0.01581,	Adjusted R-squared:  0.0153 
#F-statistic: 31.46 on 1 and 1959 DF,  p-value: 2.321e-08


alltriclopyr <- allbigdata2[(allbigdata2$PARM_NM=="Triclopyr, wf"),]

alltriclopyr <- alltriclopyr[!is.na(alltriclopyr$Three_Day_Sum_Rain),]
alltriclopyr <- alltriclopyr[!is.na(alltriclopyr$RESULT_VA),]

plot(as.numeric(alltriclopyr$Three_Day_Sum_Rain), as.numeric(alltriclopyr$RESULT_VA), main = "Triclopyr", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
alltriclopyr_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=alltriclopyr)
abline(alltriclopyr_model, col='blue')

summary(alltriclopyr_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = alltriclopyr)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-174.4 -134.4  -91.5  -24.0 4684.4 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      179.95      21.29   8.454 4.01e-16 ***
#  as.numeric(Three_Day_Sum_Rain)   -36.70      29.99  -1.224    0.222    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 376.2 on 448 degrees of freedom
#Multiple R-squared:  0.003332,	Adjusted R-squared:  0.001107 
#F-statistic: 1.498 on 1 and 448 DF,  p-value: 0.2217

table(allbigdata2$PARM_NM)


allmetolachlor <- allbigdata2[(allbigdata2$PARM_NM=="Metolachlor, wf"),]

allmetolachlor <- allmetolachlor[!is.na(allmetolachlor$Three_Day_Sum_Rain),]
allmetolachlor <- allmetolachlor[!is.na(allmetolachlor$RESULT_VA),]

plot(as.numeric(allmetolachlor$Three_Day_Sum_Rain), as.numeric(allmetolachlor$RESULT_VA), main = "Metolachlor", xlab="Rain 3 Days Preceding (in)", ylab="Atrazine Concentration (ng/l)", pch=19)
allmetolachlor_model <-lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), data=allmetolachlor)
abline(allmetolachlor_model, col='blue')

summary(allmetolachlor_model)
#Call:
#  lm(formula = as.numeric(RESULT_VA) ~ as.numeric(Three_Day_Sum_Rain), 
#     data = allmetolachlor)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1102.2  -256.3  -188.3  -104.2 31428.1 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      196.04      33.05   5.931 3.67e-09 ***
#  as.numeric(Three_Day_Sum_Rain)   259.85      44.16   5.885 4.84e-09 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1110 on 1618 degrees of freedom
#Multiple R-squared:  0.02095,	Adjusted R-squared:  0.02035 
#F-statistic: 34.63 on 1 and 1618 DF,  p-value: 4.84e-09



#07/01/2021

#Mapping the average pesticide concentrations across all sites
#Pulling all necessary packages
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

#Mapping the actual average pesticide concentrations - the gradient of the map indicates the mean concentration value
meanpesticides <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="All Average Pesticide Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(anova2$SITE_NO))),"sites")+
  geom_point(data=anova2,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) + scale_colour_gradient(low = "red", high = "blue") +
  coord_sf(xlim=c((min(anova2$DEC_LONG_VA)),(max(anova2$DEC_LONG_VA))),ylim=c((min(anova2$DEC_LAT_VA)),(max(anova2$DEC_LAT_VA)))) 
meanpesticides

#What are the first things I noticed?
#One, my gradient is ugly, so I'm going to go pull the code for a better gradient.
#Two, MSQA is HEAVILY skewing the gradient - just like it did in the one-year ratio map... interesting...

meanpesticides2 <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="All Average Pesticide Concentrations",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(anova2$SITE_NO))," sites"))+
  geom_point(data=anova2,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(anova2$DEC_LONG_VA)),(max(anova2$DEC_LONG_VA))),ylim=c((min(anova2$DEC_LAT_VA)),(max(anova2$DEC_LAT_VA)))) +
  scale_color_gradient2(midpont=400,low="red", mid="yellow", high="blue3", space ="Lab" )
meanpesticides2

#Making the map without MSQA involved

noMSQA <- anova2[!(anova2$RSQA_STUDY=="MSQA"),]

noMSQAmap <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="All Average Pesticide Concentrations Without MSQA",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(noMSQA$SITE_NO))," sites"))+
  geom_point(data=noMSQA,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(noMSQA$DEC_LONG_VA)),(max(noMSQA$DEC_LONG_VA))),ylim=c((min(noMSQA$DEC_LAT_VA)),(max(noMSQA$DEC_LAT_VA)))) +
  scale_color_gradient2(midpoint=56.25,low="red", mid="yellow", high="blue3", space ="Lab" )
noMSQAmap

#Making the map without NESQA involved

noNESQA <- noMSQA[!(noMSQA$RSQA_STUDY=="NESQA"),]

noNESQAmap <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="All Average Pesticide Concentrations Without MSQA & NESQA",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(noNESQA$SITE_NO))," sites"))+
  geom_point(data=noNESQA,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(noNESQA$DEC_LONG_VA)),(max(noNESQA$DEC_LONG_VA))),ylim=c((min(noNESQA$DEC_LAT_VA)),(max(noNESQA$DEC_LAT_VA)))) +
  scale_color_gradient2(midpoint=30,low="red", mid="yellow", high="blue3", space ="Lab" )
noNESQAmap


#Changing the gradient by setting a midpoint.
noNESQAmap <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="All Average Pesticide Concentrations Without MSQA & NESQA",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(noNESQA$SITE_NO))," sites"))+
  geom_point(data=noNESQA,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(noNESQA$DEC_LONG_VA)),(max(noNESQA$DEC_LONG_VA))),ylim=c((min(noNESQA$DEC_LAT_VA)),(max(noNESQA$DEC_LAT_VA)))) +
  scale_color_gradient2(midpoint=30,low="red", mid="yellow", high="blue3", space ="Lab" )
noNESQAmap


#Seeing if the maximum avg pesticide concentration and maximum average rainfall received are the same site

max(anova2$Mean_Pesticide_Concentration)
#[1] 756.7085
#5599100 - Illinois - MSQA - average rainfall 5.085

max(anova1$Mean)
#[1] 8.2425
#IAWapsipinicon - 5420520 - Iowa - MSQA - avg pesticice concentration 10.1390675


######################
#07/02/2021

#Mapping the average pesticide concentrations on a regional scale.


PNSQApest <- anova2[anova2$RSQA_STUDY=='PNSQA',]

PNSQA_map <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="PNSQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(PNSQApest$SITE_NO))," sites"))+
  geom_point(data=PNSQApest,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(-124)),(max(-120))),ylim=c((min(43)),(max(49)))) + 
  scale_color_gradient2(midpoint=30,low="red", mid="yellow", high="blue3", space ="Lab" )
PNSQA_map

max(PNSQApest$Mean_Pesticide_Concentration)
#[1] 61.84527 - 4.455511e+14 - Salem, OR - ORPringle - 2.2100 avg rain

max(PNSQAPRISMmean$Mean)
#[1] 3.75 - WASalmon



CSQApest <- anova2[anova2$RSQA_STUDY=='CSQA',]

CSQA_map <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="CSQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(CSQApest$SITE_NO))," sites"))+
  geom_point(data=CSQApest,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(-124)),(max(-119))),ylim=c((min(33)),(max(39)))) + 
  scale_color_gradient2(midpoint=10, low="red", mid="yellow", high="blue3", space ="Lab" )
CSQA_map

max(CSQApest$Mean_Pesticide_Concentration)
#[1] 19.76587 - 11152650 (SITE_NO) - RECLAMATION DITCH NR SALINAS CA (STATION_NM)

max(CSQAPRISMmean$Mean)
#[1] 3.9 - CACorte


NESQApest <- anova2[anova2$RSQA_STUDY=='NESQA',]

NESQA_map <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="NESQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(NESQApest$SITE_NO))," sites"))+
  geom_point(data=NESQApest,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(-79)),(max(-70))),ylim=c((min(40)),(max(44)))) + 
  scale_color_gradient2(midpoint=55, low="red", mid="yellow", high="blue3", space ="Lab" )
NESQA_map

max(NESQApest$Mean_Pesticide_Concentration)
#[1] 110.3538 - 4.300561e+14 (SITE_NO) - BLACK CREEK AT MORGANVILLE NY (STATION_NM)

max(NESQAPRISMmean$Mean)
#[1] 4.85 - NYRamapo


SESQApest <- anova2[anova2$RSQA_STUDY=='SESQA',]

SESQA_map <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="SESQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(SESQApest$SITE_NO))," sites"))+
  geom_point(data=SESQApest,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(-87)),(max(-77))),ylim=c((min(32)),(max(40)))) + 
  scale_color_gradient2(midpoint=22.5, low="red", mid="yellow", high="blue3", space ="Lab" )
SESQA_map

max(SESQApest$Mean_Pesticide_Concentration)
#[1] 45.64485 - 2094659 (SITE_NO) - SOUTH BUFFALO CREEK NR POMONA, NC (STATION_NM)

max(SESQAPRISMmean$Mean)
#[1] 7.055 - ALHatchet




MSQApest <- anova2[anova2$RSQA_STUDY=='MSQA',]

MSQA_map <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="MSQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(MSQApest$SITE_NO))," sites"))+
  geom_point(data=MSQApest,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration),size=1,shape=19) +
  coord_sf(xlim=c((min(-98)),(max(-82))),ylim=c((min(37)),(max(45)))) + 
  scale_color_gradient2(midpoint=400, low="red", mid="yellow", high="blue3", space ="Lab" )
MSQA_map

max(MSQApest$Mean_Pesticide_Concentration)
#[1] 756.7085 - 5599100 (SITE_NO) - GALUM CREEK NEAR PYATTS, IL (STATION_NM)

max(MSQAPRISMmean$Mean)
#[1] 8.2425 - IAWapsipinicon

#Modifying the size of the points so they are equal to the amount of rain received at each site... an idea for how I can visualize this info... we will see if it works

MSQApest <- MSQApest %>%
  inner_join(nametoRSQA)

allMSQA <- merge(MSQApest, MSQAPRISMmean, by = c("Name"), all.x=TRUE)

MSQA_map2 <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="MSQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(MSQApest$SITE_NO))," sites"))+
  geom_point(data=allMSQA,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration,size=Mean),shape=19,alpha=0.65) +
  scale_size_continuous(range = c(1,7)) +
  coord_sf(xlim=c((min(-98)),(max(-82))),ylim=c((min(37)),(max(45)))) + 
  scale_color_gradient2(midpoint=400, low="red", mid="yellow", high="blue3", space ="Lab" )
MSQA_map2

SESQApest <- SESQApest %>%
  inner_join(nametoRSQA)

allSESQA <- merge(SESQApest, SESQAPRISMmean, by = c("Name"), all.x=TRUE)

SESQA_map2 <- ggplot(data=world) +
  geom_sf(fill="gray75") + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),panel.background = element_rect(fill = "lightblue")) +
  labs(title="SESQA Distribution of Average Pesticide Concentration",x="Longitude",y="Latitude",subtitle=paste0("A total of ", length(unique(SESQApest$SITE_NO))," sites"))+
  geom_point(data=allSESQA,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=Mean_Pesticide_Concentration,size=Mean),shape=19,alpha=0.65) +
  scale_size_continuous(range = c(1,7)) +
  coord_sf(xlim=c((min(-87)),(max(-77))),ylim=c((min(32)),(max(40)))) + 
  scale_color_gradient2(midpoint=22.5, low="red", mid="yellow", high="blue3", space ="Lab" )
SESQA_map2




#So now that I've visualized the relationship between average amount of rain received during the sampling months and the average pesticide concentration... let's make a scatterplot

plot(as.numeric(allMSQA$Mean), as.numeric(allMSQA$Mean_Pesticide_Concentration), main = "MSQA", xlab="Average Rainfall (in)", ylab="Average Pesticide Concentration (ng/l)", pch=19)
MSQA_model <-lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), data=allMSQA)
abline(MSQA_model, col='blue')

summary(MSQA_model)
#Call:
#  lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), 
#     data = allMSQA)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-125.27  -66.82  -43.36   23.51  661.54 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)         30.35      63.82   0.476    0.635
#as.numeric(Mean)    12.75      12.06   1.057    0.293

#Residual standard error: 123.9 on 98 degrees of freedom
#Multiple R-squared:  0.01128,	Adjusted R-squared:  0.00119 
#F-statistic: 1.118 on 1 and 98 DF,  p-value: 0.293


plot(as.numeric(allSESQA$Mean), as.numeric(allSESQA$Mean_Pesticide_Concentration), main = "SESQA", xlab="Average Rainfall (in)", ylab="Average Pesticide Concentration (ng/l)", pch=19)
SESQA_model <-lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), data=allSESQA)
abline(SESQA_model, col='blue')

summary(SESQA_model)
#Call:
#lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), 
#   data = allSESQA)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-7.011 -5.958 -3.937  1.417 38.522 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)        7.7842     7.0673   1.101    0.274
#as.numeric(Mean)  -0.1976     1.5624  -0.126    0.900

#Residual standard error: 9.73 on 73 degrees of freedom
#Multiple R-squared:  0.000219,	Adjusted R-squared:  -0.01348 
#F-statistic: 0.01599 on 1 and 73 DF,  p-value: 0.8997


#Scatterplot of avg rain and avg pest conc across all sites

anova2 <- anova2 %>%
  inner_join(nametoRSQA)

alldata <- merge(anova2, fullmeans, by = c("Name"), all.x=TRUE)

plot(as.numeric(alldata$Mean), as.numeric(alldata$Mean_Pesticide_Concentration), main = "All Regions", xlab="Average Rainfall (in)", ylab="Average Pesticide Concentration (ng/l)", pch=19)
regionwide_model <-lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), data=alldata)
abline(regionwide_model, col='blue')

summary(regionwide_model)

#Call:
#  lm(formula = as.numeric(Mean_Pesticide_Concentration) ~ as.numeric(Mean), 
#     data = alldata)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-106.85  -27.35   -7.83    5.36  698.55 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -36.578      7.799  -4.690 3.69e-06 ***
#  as.numeric(Mean)   18.631      2.108   8.838  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 66.12 on 425 degrees of freedom
#Multiple R-squared:  0.1553,	Adjusted R-squared:  0.1533 
#F-statistic: 78.12 on 1 and 425 DF,  p-value: < 2.2e-16




#07/12


#I had this idea to do a grouped boxplot showing the amount of runoff received by the stream based on the amount of urbanization
#Kind of just for fun and to visualize it with no other reason... yet...

#I only want landuse information that is connected with 2012, 2011 is no longer used according to Peter. These two lines get rid of all 20 NLCD variables.
watershed2012 <- select(watershed, -contains("2011"))
watershed2012 <- select(watershed2012, -contains("imperviousness"))

boxplot(Runoff2015~LU_cat, data=watershed2012, xlab='Type of Land Use', ylab='Amount of Runoff Received (units)', main='Amount of Runoff by Type of Watershed', col=c('orchid','pale green','salmon','yellow','red','orange'))

library(ggpubr)

compare_means(Runoff2015 ~ LU_cat, watershed2012, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
# A tibble: 1 x 6
#.y.            p p.adj p.format p.signif method        
#<chr>      <dbl> <dbl> <chr>    <chr>    <chr>         
#  1 Runoff2015 0.455  0.45 0.45     ns       Kruskal-Wallis


#Let's do the numeric variable of total urban against the amount of runoff received in 2015.
plot(as.numeric(watershed2012$Total_Urban), as.numeric(watershed2012$Runoff2015), main = "Urbanization and Runoff", xlab="Amount of Urbanization (units)", ylab="Average Runoff (units)", pch=19)
runoff_model <-lm(formula = as.numeric(Runoff2015) ~ as.numeric(Total_Urban), data=watershed2012)
abline(runoff_model, col='blue')

summary(runoff_model)

#Call:
#  lm(formula = as.numeric(Runoff2015) ~ as.numeric(Total_Urban), 
#     data = watershed2012)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-788.03 -334.53  -68.54  349.36  960.30 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             1114.775     69.781  15.975   <2e-16 ***
#  as.numeric(Total_Urban)   -1.343      1.334  -1.007    0.317    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 454.5 on 86 degrees of freedom
#Multiple R-squared:  0.01165,	Adjusted R-squared:  0.000153 
#F-statistic: 1.013 on 1 and 86 DF,  p-value: 0.3169


#Comparing the land-use characteristic and runoff across all regions

PNSQA <- select(watershed2012,c('SITE_NO','RSQA_STUDY','Runoff2015','LU_cat','Total_Urban'))
PNSQA <- rename(PNSQA, Runoff = Runoff2015)

watershed <- select(watershed, -contains("2011"))
watershed <- select(watershed2012, -contains("imperviousness"))

MSQAwatershed <- watershed[which(watershed$RSQA_STUDY=='MSQA'),]
MSQA <- select(MSQAwatershed,c('SITE_NO','RSQA_STUDY','Runoff2013','LU_cat','Total_Urban'))
MSQA <- rename(MSQA, Runoff = Runoff2013)

SESQAwatershed <- watershed[which(watershed$RSQA_STUDY=='SESQA'),]
SESQA <- select(SESQAwatershed,c('SITE_NO','RSQA_STUDY','Runoff2014','LU_cat','Total_Urban'))
SESQA <- rename(SESQA, Runoff = Runoff2014)

NESQAwatershed <- watershed[which(watershed$RSQA_STUDY=='NESQA'),]
NESQA <- select(NESQAwatershed,c('SITE_NO','RSQA_STUDY','Runoff2016','LU_cat','Total_Urban'))
NESQA <- rename(NESQA, Runoff = Runoff2016)

CSQAwatershed <- watershed[which(watershed$RSQA_STUDY=='CSQA'),]
CSQA <- select(CSQAwatershed,c('SITE_NO','RSQA_STUDY','Runoff2017','LU_cat','Total_Urban'))
CSQA <- rename(CSQA, Runoff = Runoff2017)


fullrunoff <- as.data.frame(rbind(PNSQA,MSQA,SESQA,NESQA,CSQA))

ggplot(data=fullrunoff, aes(x=Total_Urban,y=Runoff,color=RSQA_STUDY)) + geom_point()
plot(as.numeric(fullrunoff$Total_Urban), as.numeric(fullrunoff$Runoff), main = "Urbanization and Runoff", xlab="Amount of Urbanization (units)", ylab="Average Runoff (units)", pch=19)
runoff_model <-lm(formula = as.numeric(Runoff) ~ as.numeric(Total_Urban), data=fullrunoff)
abline(runoff_model, col='blue')

summary(runoff_model)
#Call:
#  lm(formula = as.numeric(Runoff) ~ as.numeric(Total_Urban), data = fullrunoff)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-534.02 -207.89 -130.22   54.31 1564.13 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             506.5515    22.2260  22.791   <2e-16 ***
#  as.numeric(Total_Urban)   1.0696     0.4918   2.175   0.0301 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 377.3 on 481 degrees of freedom
#Multiple R-squared:  0.00974,	Adjusted R-squared:  0.007682 
#F-statistic: 4.731 on 1 and 481 DF,  p-value: 0.03011


boxplot(Runoff~LU_cat, data=fullrunoff, xlab='Type of Land Use', ylab='Amount of Runoff Received (units)', main='Amount of Runoff by Type of Watershed', col='white',border='black')

compare_means(Runoff ~ LU_cat, fullrunoff, method="kruskal.test", paired = FALSE , p.adjust.method="fdr")
# A tibble: 1 x 6
#.y.           p    p.adj p.format p.signif method        
#<chr>     <dbl>    <dbl> <chr>    <chr>    <chr>         
#  1 Runoff 2.70e-13 2.70e-13 2.7e-13  ****     Kruskal-Wallis

runoffsig <- compare_means(Runoff ~ LU_cat , fullrunoff, method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
# A tibble: 28 x 8
#.y.    group1      group2                p     p.adj p.format p.signif method  
#<chr>  <chr>       <chr>             <dbl>     <dbl> <chr>    <chr>    <chr>   
#  1 Runoff Undeveloped Urban_low  0.369        0.45      0.36940  ns       Wilcoxon
#2 Runoff Undeveloped Urban_med  0.874        0.87      0.87419  ns       Wilcoxon
#3 Runoff Undeveloped Urban_high 0.323        0.41      0.32256  ns       Wilcoxon
#4 Runoff Undeveloped Mixed      0.268        0.37      0.26762  ns       Wilcoxon
#5 Runoff Undeveloped Ag_high    0.0000000886 0.0000005 8.9e-08  ****     Wilcoxon
#6 Runoff Undeveloped Ag_low     0.00339      0.011     0.00339  **       Wilcoxon
#7 Runoff Undeveloped Hydro      0.412        0.46      0.41210  ns       Wilcoxon
#8 Runoff Urban_low   Urban_med  0.180        0.3       0.18006  ns       Wilcoxon
#9 Runoff Urban_low   Urban_high 0.515        0.56      0.51543  ns       Wilcoxon
#10 Runoff Urban_low   Mixed      0.0352       0.07      0.03523  *        Wilcoxon
# ... with 18 more rows


psig <- runoffsig[runoffsig$p.signif<0.05, ]

fullrunoff %>%
  ggplot(aes(x=LU_cat,y=Runoff,fill=LU_cat)) +
  geom_boxplot(color='black') +
  theme_classic() + 
  theme(axis.text.x = element_blank()) +
  labs(title='Average Runoff for Each Site by Land Use Characteristic',subtitle=paste0("A total of ", (length(unique(fullrunoff$SITE_NO))), " sites"),y='Amount of Runoff Received (units)',fill='Land USe Characteristic') + 
  stat_pvalue_manual(inherit.aes=FALSE, data=psig, label = "p.signif", y.position=2500,step.increase=0.1)









#Creating a new pheatmap
#A WIP that I need more opinions on during office hours tonight...

#################################
#This block of code is the data I am using to set-up my data frame for the heat map.
#IT will contain only result_va that were detected and has been reduced simply to the pesticice concentrations of the PNSQA region, not all organincs

setwd(dir="C:/Users/llill/OneDrive/Documents/RSQAData/PNSQACorrelation")

results <- read.csv("Results.csv")

#In the results for the RSQA data, a "<" in the REMARK_CD column indicates a non-detected pesticide. 
#Thus, we need to remove those from our data to have accurate measurements in our analysis.

results <- results[!(results$REMARK_CD=="<"),]

#I'm going to go ahead and remove the "Es" from that column as well because I believe Barbara told me they are also indicative of a nondetectable.

results <- results[!(results$REMARK_CD=="E"),]

#Alright now let's start with just pesticides instead of all organics to be nice to my computer and run some preliminary analysis.

results <- results[(results$PARM_SEQ_GRP_DS=="Organics, Pesticides"),]

sites <- read.csv("Sites.csv")

#############################################

medianvalues <- results %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA)) %>%
  ungroup()
View(medianvalues)

widemedianvalues <- pivot_wider(medianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)

colSums(is.na(widemedianvalues))

#Pulling specific columns from the widemedianvalues data frame and creating a new data frame
#This narrows down the number of CHEMICALS in the data frame
Interestingcolumns_data<-as.data.frame(widemedianvalues[,c(2,7,8,9,11,12,16,17)])

#Renaming the rows of the newest data frame
rownames(Interestingcolumns_data)<-widemedianvalues$SITE_NO

#Removing any sites that have NAs - narrows down the number of sites in the data frame
finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]

#Creating a dataframe of metadata that matches the set of sites included in the pivotedwider data frame
metadata<-sites[match(rownames(finaldata),sites$SITE_NO),]

#Multicomponent Analysis: Heatmaps and Hierarchical Clustering

data_cor<-cor(finaldata)

library(pheatmap)

pheatmap(data_cor, annotations=rownames(data_cor),  color=colorRampPalette(c("blue", "white", "red"))(50), show_rownames=T, show_colnames=T)



#Multivariable Analysis: Principle Component Analysis

