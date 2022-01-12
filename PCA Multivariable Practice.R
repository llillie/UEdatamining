library(plyr)
library(dplyr)
library(readr)

setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAFall")

results <- read.csv("Results.csv")

sites <- read.csv("Sites.csv")

medianvalues <- results %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA))
View(medianvalues)
#grouped by site number and parameter shortname (aka the chemical). I then created a new column in the
#data frame called MEDIAN_RESULT which contains the median value of each chemical at each site.

library(tidyr)

widemedianvalues <- pivot_wider(medianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)
#need to specify the id_cols argument, the names_from argument, and values_from argument
#id_cols - what is the name of the column of your dataframe that will determine each row, 
#names_from - what is the name of the column of your dataframe that will provide the names of the columns,
#values_from - what is the name of the column of your dataframe that will provide the values in each column.

#Side note: a lot of the numbers look the same in the widemedianvalues, but there is variation, so I don't think I screwed up the code... yet...

colSums(is.na(widemedianvalues))
#Outputs the name of every column and the number of times there is an NA in that column.

table(colSums(is.na(widemedianvalues)))

#no empty variables in PCA; have to drop any site with any NAs; how many NAs to drop to minimize the number of rows; PCA needs complete data frame
#one PCA with all NAs gone
#second round of PCA of 45
#third round of PCA 50
#How dataframe <- [,colSums(is.na(widemedianvalues)) < #ofNAs]. Use this an replace #ofNAs with the rounds.

PCA1 <- widemedianvalues[,colSums(is.na(widemedianvalues)) < 1]
PCA45 <- widemedianvalues[,colSums(is.na(widemedianvalues)) < 45]
table(colSums(is.na(PCA45)))
PCA50 <- widemedianvalues[,colSums(is.na(widemedianvalues)) < 50]
#Use these codes instead of lines below

PCA1 <- as.data.frame(PCA1)
rownames(PCA1)<-PCA1$SITE_NO
PCA1 <- PCA1[,-1]
PCA1 <- PCA1[,which(apply(PCA1,2,var) != 0)]

which(apply(PCA1,2,var) != 0)

PCA1 <- PCA1[which(apply(PCA1,2,var) != 0)]

PCA45 <- as.data.frame(PCA45)
rownames(PCA45)<-PCA45$SITE_NO
PCA45 <- PCA45[,-1]
PCA45 <- PCA45[,which(apply(PCA45,2,var) != 0)]

PCA50 <- as.data.frame(PCA50)
rownames(PCA50)<-PCA50$SITE_NO
PCA50 <- PCA50[,-1]
PCA50 <- PCA50[,which(apply(PCA50,2,var) != 0)]

#At this point we decided to scratch 45 and 50, no need to run them anymore.

finaldata1 <- PCA1[complete.cases(PCA1), ]
#Don't really need this code because PCA1 contains only the chemicals with 0 NAs
finaldata45 <- PCA45[complete.cases(PCA45), ]
finaldata50 <- PCA50[complete.cases(PCA50),]
#If I run complete cases of PCA45 and PCA50, I end up with data frames containing 0 obstacles. How do I bypass this issue?

metadata1<-sites[match(rownames(PCA1),sites$SITE_NO),]
metadata45<-sites[match(rownames(PCA45),sites$SITE_NO),]
metadata50<-sites[match(rownames(PCA50),sites$SITE_NO),]

data_cor1 <- cor(PCA1)
data_cor45 <- cor(PCA45)
data_cor50 <- cor(PCA50)
#makes a correlation matrix for all of the variables. Diagonal down values are all 1 because each variable is perfectly correlated with itself.
#Question for someone: why do I got a warning message saying the standard deviation is 0?
#for 45 and 50, remove all NAs before completing the heatmap.

library(pheatmap)

pheatmap(data_cor1, annotations=rownames(data_cor1),show_rownames=T, show_colnames=T)
pheatmap(data_cor45, annotations=rownames(data_cor45),show_rownames=T, show_colnames=T)
pheatmap(data_cor50, annotations=rownames(data_cor50),show_rownames=T, show_colnames=T)
#These codes did not work. Received the following message:
#Error in hclust(d, method = method) : 
#NA/NaN/Inf in foreign function call (arg 10). Come back and figure out later.
#Tested using different use commands in the cor() function, but that did not change the outcome. In particular, adding the use = "pairwise.complete.obs" argument seemed to make it worse by getting rid of the 1 correlations of the down diagonal and instead replacing it with an NA.
#I tried the argument "na.rm = TRUE/FALSE" and that had no effect on my outcome.
#after searching through the help function, I noticed the argument is "annotation" not "annotations"? but when I tried that I got a new error....

data_cor1t<-cor(t(PCA1))
#this line and the corresponding heatmap is the only one that seems to work properly?
data_cor45t<-cor(t(PCA45))
data_cor50t<-cor(t(PCA50))

pheatmap(data_cor1t,show_rownames=F, show_colnames=F)
pheatmap(data_cor45t,show_rownames=F, show_colnames=F)
pheatmap(data_cor50t,show_rownames=F, show_colnames=F)


library(devtools)

#PCA time
#When you look at pesticides, look at states at first and then maybe look into individual counties?

library(ggbiplot)

prcompData<-prcomp(PCA1,center=T, scale=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=F, groups=paste(metadata1$STATE_NM), ellipse=T)

ggbiplot(prcompData, choices = c(1,2), var.axes=T, groups=paste(metadata1$STATE_NM), ellipse=T)
