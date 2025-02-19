# Tutorial
library(data.table)
library(magrittr)
library(tidyr)

# Section 01
#1 --> d
#2 --> b
#3 --> No


product_dt <- fread('extdata/example_product_data.csv')
product_dt

product_dt_melted <- melt(product_dt,id.vars="name", measure.vars = c("producta","productb"),variable.name ="Product_type", value.name = "Count")
product_dt_melted_casted <- dcast(product_dt_melted, ... ~ Product_type, value.var = "Count")

# Section 02 - Merge Warm Up
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, carname := rownames(mtcars)]
dt1 <- mtcars_dt[5:25,.(carname, mpg, cyl)]
dt2 <- mtcars_dt[1:10, .(carname, gear)]

merge(dt1,dt2)  %>% nrow()
merge(dt1,dt2, all.x=T)  %>% nrow()
merge(dt1,dt2, all=T)  %>% nrow()

# Section 03 - Weather dataset
messy_dt <- fread("extdata/weather.txt")
str(messy_dt)

tidy_dt <- melt(messy_dt, id.vars = c("id","element","year","month"), measure.vars = paste0('d',1:31), variable.name = "Day",value.name = "Temperature")
tidy_dt[, Day := as.integer(gsub("d", "", Day))]
tidy_dt <- unite(tidy_dt, col="Date",c("year","month","Day"),sep = '-')
tidy_dt <- as.data.table(tidy_dt)
tidy_dt <- dcast(tidy_dt, ... ~ element, value.var = "Temperature")
tidy_dt <- tidy_dt[(!is.na(TMAX) |  !is.na(TMIN))]
head(tidy_dt)

# Homework
# Section 04
files <- list.files("extdata/baby-names", full.names = TRUE)
# See one file
head(fread(files[1]))
