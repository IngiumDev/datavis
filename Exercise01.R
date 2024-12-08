library(dslabs)
library(magrittr)
n <- 1000
(n*(n+1))/2
seq(1,100)
sum(1:100)
4 %>% sqrt()
sqrt(100) %>% log(10,.)
log(exp(200))
pi^2 /6
sum(1/seq(100)^2)

data(murders)
colnames(murders)
str(murders)
a <- murders$state
class(a)
length(levels(murders$region))


temp <- c(35,88,42,84,81,30)
city <- c("Beijing", "Lagos",
          "Paris", "Rio de Janeiro", "San Juan",  "Toronto")
names(temp) <- city
class(temp)
temp[1:3]
temp["Paris"]
temp["Lagos"]
12:73
seq(1,99,2)
length(seq(6,55,4/7))


a <- murders$population %>% sort(., decreasing = FALSE)
a[1]
min(a)
murders[population == min(murders$population),"state"]
murders$state[which.min(murders$population)]