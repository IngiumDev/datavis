library(data.table)
library(magrittr) # the %>% (pipe) operator
library(dplyr) # contains the 'starwars' dataset needed later

users_dt = fread('data/BX-Users.csv')
books_dt = fread('data/BX-Books.csv')
ratings_dt = fread('data/BX-Book-Ratings.csv')
print(c(class(users_dt), class(books_dt), class(ratings_dt)))

colnames(users_dt)
str(users_dt)

users_dt$Age <- sapply(users_dt$Age, as.numeric)
summary(users_dt)


head(ratings_dt, n = 5)
tail(ratings_dt, n = 5)

colnames(users_dt) = gsub("-", "_", colnames(users_dt))
colnames(books_dt) = gsub("-", "_", colnames(books_dt))
colnames(ratings_dt) = gsub("-", "_", colnames(ratings_dt))

books_dt[, c("Image_URL_S", "Image_URL_M", "Image_URL_L") := NULL]

books_dt2 <- books_dt[Year_Of_Publication >= 1900 & Year_Of_Publication <= 2019]

books_dt[, Book_Author] %>% unique() %>% length()

users_dt$Age %>% is.na() %>% sum()

ratings_dt$Book_Rating %>% max()
ratings_dt[Book_Rating > 0, .N, by = "Book_Rating"][order(-N)][1]

ratings_dt[Book_Rating == 10, ISBN]

ratings_dt <- ratings_dt[order(-Book_Rating)]


dt <- as.data.table(starwars)
dt <- dt[, .(name, height, mass, birth_year, sex, homeworld, species)]
colnames(dt) <- c("name", "height", "mass", "age", "sex", "homeworld", "species")
dt[1:4]

str(dt)
dt[height > 200, .N, by = species]

dt[, .(mean_age = mean(age, na.rm = TRUE)), by = species][mean_age > 70]

dt[,height_m := height/100]
dt[, BMI := (mass/ (height_m*height_m))]

dt[,mean_BMI]