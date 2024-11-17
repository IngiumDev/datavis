library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
library(ggrepel)
library(hexbin)
library(patchwork)
mpg$year <- as.factor(mpg$year)
ggplot(data = mpg, aes(x=cty, y=hwy, color = year)) + geom_point() + geom_smooth(method='lm')
mpg

# Section 3
iris_dt <- as.data.table(iris)

ggplot(iris_dt) +geom_histogram()
