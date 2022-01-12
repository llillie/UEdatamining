library(plyr)
library(dplyr)
library(readr)

setwd(dir="C:/Users/alillie/Documents/RSQAData/PCAPractice")

Presults <- read.csv("Results.csv")

Psites <- read.csv("Sites.csv")

pmedianvalues <- Presults %>%
  group_by(SITE_NO,PARM_NM) %>%
  summarize(MEDIAN_RESULT = median(RESULT_VA))
View(pmedianvalues)

library(tidyr)

pwidemedianvalues <- pivot_wider(pmedianvalues, id_cols=SITE_NO, names_from=PARM_NM, values_from=MEDIAN_RESULT)

colSums(is.na(pwidemedianvalues))
table(colSums(is.na(pwidemedianvalues)))


PCA <- pwidemedianvalues[,colSums(is.na(pwidemedianvalues)) < 99]
Interestingcolumns_data<-as.data.frame(pwidemedianvalues[,c(2,3,4,6,8,9)])

rownames(Interestingcolumns_data)<-pwidemedianvalues$SITE_NO

PCA <- as.data.frame(PCA)
rownames(PCA)<-pwidemedianvalues$SITE_NO
PCA <- PCA[,-1]

finaldataI <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
pfinaldata <- PCA[complete.cases(PCA), ]

metadataI<-Psites[match(rownames(finaldataI),Psites$SITE_NO),]
pmetadata<-Psites[match(rownames(pfinaldata),Psites$SITE_NO),]

pdata_cor<-cor(pfinaldata)
data_corI<-cor(finaldataI)
#I didn't get a warning here about standard deviation being zero?

pheatmap(data_corI, annotations=rownames(data_corI),show_rownames=T, show_colnames=T)
pheatmap(pdata_cor, annotations=rownames(pdata_cor),show_rownames=T, show_colnames=T)