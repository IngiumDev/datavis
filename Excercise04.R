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

medals_dt <- fread('extdata/lec04_data/exercise/medals.csv')

ggplot(medals_dt, aes(x=population, y=total)) + geom_point()


ggplot(medals_dt, aes(x=population, y=total)) + geom_point() + scale_x_log10() +scale_y_log10()

ggplot(medals_dt, aes(x=population, y=total, label = country)) + geom_point() + scale_x_log10() +scale_y_log10() + geom_text()

ggplot(medals_dt, aes(x=population, y=total,  label = country)) + geom_point() + scale_x_log10() +scale_y_log10() + geom_text_repel()

anscombe <- anscombe
anscombe_reshaped <- anscombe %>%
  as.data.table %>%
  .[, ID := seq(nrow(.))] %>%
  melt(id.var=c("ID")) %>%
  separate(variable, c('xy', "group"), sep=1) %>%
  as.data.table %>%
  dcast(... ~ xy) %>%
  .[, group := paste0("dataset_", group)]


anscombe_reshaped[, .(mean_x = mean(x, na.rm=TRUE), sd_x = sd(x, na.rm=TRUE),   mean_y= mean(y, na.rm=TRUE), sd_y=sd(y,na.rm=TRUE), cor= cor(x,y)), by =  group]


ggplot(anscombe_reshaped, aes(x,y)) +geom_point() +facet_grid(~group)


boxplots_dt <- fread('extdata/lec04_data/exercise/boxplots.csv')
boxplots_melted <- melt(boxplots_dt, measure.vars = c("dataset_1","dataset_2","dataset_3","dataset_4","dataset_5"), variable.name = "dataset_number",value.name = "value")
ggplot(boxplots_melted, aes(x=dataset_number, y=value)) + geom_boxplot()

ggplot(boxplots_melted, aes(x=dataset_number, y=value)) + geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
ggplot(boxplots_melted, aes(x=dataset_number, y=value)) + geom_violin()

#Section 8
# geom_point(alpha=0.4)