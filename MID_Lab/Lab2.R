var1 = readline(prompt = "enter any value: ")
var2 = readline(prompt = "enter any number: ")

var2 = as.integer(var2)

print(var1)
print(var2)


# Scan function using for user input

x = scan()
print(x)


# 
d = scan(what = double())
s = scan (what = " ")
c = scan( what = character())


mydata <- data.frame(age=numeric(0),gender=character(0))
mydata <- edit(mydata)


# Data input from CSV
mydata1 <- read.csv("D:/iris.csv", header = TRUE, sep = ",")

# Show full dataset
mydata1

# Show dimension
dim(mydata1)

# Show number of rows
nrow(mydata1)

# Show number of columns
ncol(mydata1)

# Show first 10 rows
head(mydata1, 10)

# Another way to show first 10 rows
mydata1[1:10, ]

# Show 10 random rows
mydata1[sample(nrow(mydata1), 10), ]

#
subset(mydata1,species=="versicolor")
subset(mydata1, species == "versicolor" & petal_length >= 4.9)


#package install
install.packages("dplyr")
library(dplyr)
packageVersion("dplyr")


#filter
filter(mydata1, species == "versicolor" & petal_length > 4.9)


#remove duplicate row
mydata1[duplicated(mydata1), ]
mydata1 <- distinct(mydata1)
dim(mydata1)


arrange(mydata1, sepal_length)

#feature engineering 
mydata2 <- mutate(mydata1,avg=sepal_length/5)


mydata3 <- summarize(mydata1,
            avg = mean(sepal_length),
            max = max(sepal_length),
            min = min(sepal_length),
            total = sum(sepal_length))


# annotating dataset
mydata1$species <- factor(
  mydata1$species,
  levels = c("virginica", "setosa", "versicolor"),
  labels = c(1, 2, 3)
)

#
s<- mydata1$sepal_length
sd(s)
sd(mydata1$sepal_length)
sd(mydata1$sepal_length, na.rm = TRUE)

library(dplyr)
mydata1 %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)


#find missing value
mydata4 <- read.csv("D:/iris.csv", header = TRUE, sep = ",")
is.na(mydata4)

colSums(is.na(mydata1))
rowSums(is.na(mydata1))


#
which(is.na(mydata1$petal_length))

# delete missing value row
remove <- na.omit(mydata1)



mydata1$sepal_length

str(mydata1)
summary(mydata1)

names(mydata1)
















