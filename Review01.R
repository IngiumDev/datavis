install.packages("dslabs") # execute only once!
library(dslabs) # execute in every new script
n <- 1000
sum_n <- n*(n+1)/2

x<-seq(1,n)
sum(x)

log(sqrt(100),base=10)
log(2.718283)
pi^2/6
sum(1/seq(100)^2)
# Section 02
data(murders)
str(murders)

colnames(murders)
temp <- c(35, 88, 42, 84, 81, 30)
temp
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city
names(temp) <- city
seq(1,100,2)
# Section 4
library(dslabs)
data("murders")

pop <- order(murders$population)
which.min(murders$population)
murders$state[which.min(murders$population)]
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro",
          "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
rank(murders$population)

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro",
          "San Juan", "Toronto")
names(temp) <- city
temp_c <-(5/9)* (temp-32)

data("na_example")
str(na_example)
table(is.na(na_example))
mean(na_example, na.rm=T)
sort(factor(c("red", "green", "blue")))

sort(factor(c("red", "green", "blue"), levels = c("red", "green", "blue")))

library(data.table)
library(magrittr) # the %>% (pipe) operator
library(dplyr) # contains the 'starwars' dataset needed later

data_folder_name <- "extdata"
users_dt <- fread(file.path("extdata","BX-Users.csv"))
books_dt <- fread(file.path("extdata","BX-Books.csv"))
ratings_dt <- fread(file.path("extdata", "BX-Book-Ratings.csv"))

class(users_dt)

colnames(users_dt)
str(users_dt)
as.numeric(users_dt$Age)

sapply(users_dt, class)
summary(books_dt)

colnames(users_dt) <- gsub("-", "_", colnames(users_dt))
colnames(books_dt) <- gsub("-", "_", colnames(books_dt))
colnames(ratings_dt) <- gsub("-", "_", colnames(ratings_dt))

books_dt[,c("Image_URL_M","Image_URL_S","Image_URL_L"):=NULL]

books_dt_2 <- books_dt[Year_Of_Publication>=1900 & Year_Of_Publication <=2019]

books_dt[,uniqueN(Book_Author)]

books_dt[Year_Of_Publication>=2000 & Year_Of_Publication <=2010,.("Unique_Author"=uniqueN(Book_Author)), by=Year_Of_Publication]

users_dt[is.na(Age),.N]
ratings_dt[,max(Book_Rating)]
ratings_dt[Book_Rating>0,.N, by=Book_Rating]

ratings_dt[Book_Rating==max(Book_Rating),ISBN]
ratings_dt <- ratings_dt[order(-Book_Rating, )]

dt <- as.data.table(starwars)
dt <- dt[, .(name, height, mass, birth_year, sex, homeworld, species)]
colnames(dt) <- c("name", "height", "mass", "age", "sex", "homeworld", "species")
dt[1:4]

dt[height > 200, .N, by=species]

dt[, .(mean_age = mean(age, na.rm = TRUE)), by = species][mean_age > 70]

dt[!is.na(height) & !is.na(mass) & height!=0&mass!=0,BMI:=mass/(height/100)^2]
dt[!is.na(height) & !is.na(mass) & height!=0&mass!=0,.(BMI=mass/(height/100)^2)][,.(mean_bmi=mean(BMI, na.rm=T)),by=species][order(mean_bmi, decreasing = T)[1:10]]


ratings_dt[Book_Rating>7,High_Rating:=1]
ratings_dt[Book_Rating<=7,High_Rating:=0]
ratings_dt[,High_Rating:=ifelse(Book_Rating>7,1,0)]

ratings_dt[High_Rating == 1, .N] / nrow(ratings_dt)

users_who_did_rate <- ratings_dt[,unique(User_ID)]
users_dt[!(User_ID %in% users_who_did_rate)]

users_dt[(User_ID %in% users_who_did_rate) & !is.na(Age),.(amount=.N),by=Age][order(amount, decreasing = T)[1]]
ratings_dt[,.N,by=User_ID][,mean(N,na.rm=T)]
ratings_dt[Book_Title]

ratings_dt[Book_Rating==max(Book_Rating) & Year_Of_Publication!=0][Year_Of_Publication==min(Year_Of_Publication),Book_Title]
ratings_dt[,.N,by=ISBN]
# 1. Count the ratings per ISBN:
isbn_counts <- ratings_dt[, .N, by = .(ISBN)]

# 2. Find the maximum number of ratings:
max_ratings <- max(isbn_counts$N)

# 3. Extract ISBN(s) with that maximum count:
top_isbns <- isbn_counts[N == max_ratings, ISBN]

# 4. Retrieve unique publication year(s) for these ISBN(s):
ratings_dt[ISBN %in% top_isbns, unique(Year_Of_Publication)]

authors <- c("Agatha Christie", "William Shakespeare", "Stephen King",
             "Ann M. Martin", "Carolyn Keene", "Francine Pascal",
             "Isaac Asimov", "Nora Roberts", "Barbara Cartland", "Charles Dickens")

ratings_dt[Book_Author %in% authors][,.(num_ratings=.N, mean_rating=mean(Book_Rating,na.rm=T),max_rating=max(Book_Rating)),by=Book_Author]