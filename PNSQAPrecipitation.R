setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAPrecipitation")

rain <- read.csv("Results.csv")

rain_counts <- rain %>%
  group_by(HYD_EVENT_CD) %>%
  summarize(count=n())
rain_counts