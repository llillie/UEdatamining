#Lowkey going to try and do a SEM on my own

library(lavaan)

#Learning SEM via this website: https://stats.idre.ucla.edu/r/seminars/rsem/

dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")

cov(dat)

#simple regression using lm()
m1a <- lm(read ~ motiv, data=dat)
(fit1a <-summary(m1a))

#simple regression using lavaan 
m1b <- '  
  # regressions 
  read ~ 1 + motiv
  # variance (optional)
    motiv ~~ motiv
'
fit1b <- sem(m1b, data=dat)
summary(fit1b)


mean(dat$motiv)
#[1] 2.4e-07
var(dat$motiv)
#[1] 100

m2 <- '
  # regressions
    read ~ 1 + ppsych + motiv
 # covariance
    ppsych ~~ motiv
'
fit2 <- sem(m2, data=dat)
summary(fit2)

#If I am going to do this I will need to work slower and try to understand what is actually happening..