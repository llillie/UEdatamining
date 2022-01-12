#Re-Working and Re-Learning Basic R For Funsies

#R can be used as a calculator.
10^2+36
#136

#Calculating the percentage of my life spent at UT.
2021-2019
#2
2021-2001
#20
(2021-2019)/(2021-2001)
2/20
#Both give 0.1
0.1*100
#10% of my life has been spent at UT Austin.

#In one simple line:
((2021-2019)/(2021-2001))*100

#If you break R and you want it to quit trying to run a command, press ESC on your keyboard.

a = 4
a
a*5
a = a + 10
a

#To remove all variables from R's memory, type:
rm(list=ls())
#AKA this empties the environment in your environment window.
#Can also click the umbrella icon in the environment bar.

#To only remove one variable
rm(a)

#Scalars - single numbers, 0-dimensional
#Vectors - row of numbers, AKA arrays, one-dimensional
#Matrices - table, two-dimensional
#Function concatenate = paste together = how to create a vector

b = c(3,4,5)
b <- c(3,4,5)

mean(x=b)
mean(b)

v = c(4,5,8,11)
sum(v)

#Calls 10 random numbers from a normal distribution.
rnorm(10)
rnorm(n=10)

#Changing things up a bit:
rnorm(10, mean = 1.2, sd = 3.4)

#If you type: rnorm( then click "TAB," R shows you all possible arguments for that function

#Making a VERY simple graph.
x = rnorm(100)
plot(x)

#Section 4 Code

help(rnorm)

example(rnorm)

help.start()
#THe above line of code starts an HTML-based global help.




