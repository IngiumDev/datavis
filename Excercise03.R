library(data.table)
library(magrittr)
library(tidyr)

example_product_data_table <- fread('/Users/simon/IdeaProjects/datavis/data/example_product_data.csv')
melted_pdt <- melt(example_product_data_table, id.vars = ('name'), measure.vars = c('producta', 'productb'), variable.name = "Product", value.name = 'numPurchases')

casted_pdt <- dcast(melted_pdt, ... ~ Product, value.var = "numPurchases")

# Section 2
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, carname := rownames(mtcars)]
dt1 <- mtcars_dt[5:25, .(carname, mpg, cyl)]
dt2 <- mtcars_dt[1:10, .(carname, gear)]
## inner
nrow(merge(dt1, dt2,
           by = "carname", all = FALSE
))
## left
nrow(merge(dt1, dt2,
           by = "carname", all.dt1 = true
))
## All
nrow(merge(dt1, dt2,
           by = "carname", all = TRUE
))

#Section 3
weather_table <- as.data.table(fread('/Users/simon/IdeaProjects/datavis/data/weather.txt'))
weather_table <- melt(weather_table, id.vars=c("id","year","month","element"), measure.vars = paste0("d", 1:31), variable.name = 'day',value.name = "Temperature")
weather_table$day <- gsub("d", "", weather_table$day)
weather_table$day <- sprintf("%02d", as.numeric(weather_table$day))
weather_table <- as.data.table(unite(weather_table, col = Date, year,month,day,sep = '-'))
weather_table <- dcast(weather_table, ... ~ element,value.var = "Temperature")
weather_table <- weather_table[!(is.na(TMAX) & is.na(TMIN))]


# Section 4
gt <- fread('extdata/eqtl/genotype.txt')
dim(gt)
head(gt[,1:5])
growth <- fread('extdata/eqtl/growth.txt')
head(growth)