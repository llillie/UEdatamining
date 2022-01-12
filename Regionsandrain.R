#Run 30 Years Ratio script for the most part then do the following. Run until line 346
#This is mainly for me to start looking into PCAs and mapping of other regions.

PNSQAPRISMmedian <- PNSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Median = median(ppt..inches.))

PNSQAPRISMmaximum <- PNSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

PNSQAPRISMminimum <- PNSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

PNSQAPRISMmean <- PNSQAPRISMmean %>%
  inner_join(PNSQAPRISMmaximum)

PNSQAPRISMmean <- PNSQAPRISMmean %>%
  inner_join(PNSQAPRISMminimum)

PNSQAPRISMmean <- PNSQAPRISMmean %>%
  inner_join(PNSQAPRISMmedian)





CSQAPRISMmedian <- CSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Median = median(ppt..inches.))

CSQAPRISMmaximum <- CSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

CSQAPRISMminimum <- CSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

CSQAPRISMmean <- CSQAPRISMmean %>%
  inner_join(CSQAPRISMmaximum)

CSQAPRISMmean <- CSQAPRISMmean %>%
  inner_join(CSQAPRISMminimum)

CSQAPRISMmean <- CSQAPRISMmean %>%
  inner_join(CSQAPRISMmedian)





NESQAPRISMmedian <- NESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Median = median(ppt..inches.))

NESQAPRISMmaximum <- NESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

NESQAPRISMminimum <- NESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

NESQAPRISMmean <- NESQAPRISMmean %>%
  inner_join(NESQAPRISMmaximum)

NESQAPRISMmean <- NESQAPRISMmean %>%
  inner_join(NESQAPRISMminimum)

NESQAPRISMmean <- NESQAPRISMmean %>%
  inner_join(NESQAPRISMmedian)




SESQAPRISMmedian <- SESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Median = median(ppt..inches.))

SESQAPRISMmaximum <- SESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

SESQAPRISMminimum <- SESQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

SESQAPRISMmean <- SESQAPRISMmean %>%
  inner_join(SESQAPRISMmaximum)

SESQAPRISMmean <- SESQAPRISMmean %>%
  inner_join(SESQAPRISMminimum)

SESQAPRISMmean <- SESQAPRISMmean %>%
  inner_join(SESQAPRISMmedian)




MSQAPRISMmedian <- MSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Median = median(ppt..inches.))

MSQAPRISMmaximum <- MSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Maximum_Value = max(ppt..inches.))

MSQAPRISMminimum <- MSQAPRISMdata %>%
  group_by(Name) %>%
  summarize(Minimum_Value = min(ppt..inches.))

MSQAPRISMmean <- MSQAPRISMmean %>%
  inner_join(MSQAPRISMmaximum)

MSQAPRISMmean <- MSQAPRISMmean %>%
  inner_join(MSQAPRISMminimum)

MSQAPRISMmean <- MSQAPRISMmean %>%
  inner_join(MSQAPRISMmedian)


allmeans <- rbind(PNSQAPRISMmean, CSQAPRISMmean, NESQAPRISMmean, SESQAPRISMmean, MSQAPRISMmean)

#The above code sets up data frames with all the information I think I will need. Now to do PCAs.
#Download a new data set that contains only Organic data for all the regions. We will use this to run the PCAs.
