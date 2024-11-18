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
iris_dt_long <- melt(iris_dt, measure.vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),variable.name = "Measurement_Type", value.name = "Measurement")
iris_dt_long
ggplot(iris_dt_long, aes(Measurement)) + geom_histogram() + facet_grid(~Measurement_Type)

ggplot(iris_dt_long, aes(x=Measurement_Type ,y=Measurement)) + geom_boxplot()  + facet_grid(~Measurement_Type) +geom_jitter() + geom_violin()

ggplot(iris_dt, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()

x <- rnorm(100)
x <- as.data.table(x)

ggplot(x, aes(sample=x)) +geom_qq() +geom_abline(slope=1, intercept = 0)

medals_dt <- fread('extdata/lec04_data/lec04_data/exercise/medals.csv')
