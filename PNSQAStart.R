setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAStart")

PNSQASdata <- read.csv("Results.csv")
View(PNSQASdata)

weather <- PNSQASdata %>%
  arrange(desc(HYD_EVENT_CD))

hyd_counts <- PNSQASdata %>%
  group_by(HYD_EVENT_CD) %>%
  summarize(count=n())
hyd_counts