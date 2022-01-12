#Dates and times practice
#https://r4ds.had.co.nz/dates-and-times.html

now()
#current date and time to the second

today()
#current date

#PNSQAPRISMData
PNSQASPRISMdata %>%
  month(Date)

monthlyrains <- PNSQASPRISMdata %>%
  mutate(month = month(Date, label = TRUE) %>% 
  ggplot(aes(x = month)) +
  geom_bar())

#neither of the last two codes worked... wondering if the date and time is a character string? Maybe it needs to be a numerical?
