
a <- c(1,2,3,4,5)
b <- c("one", "two", "three","four")
c <- c(TRUE, FALSE,TRUE, FALSE)
print(a)

x <- c(4,2,8,1,0,4,3,2,1)
sort(x, decreasing = TRUE)

b[3]
b[c(1,4)]

mymatrix <- matrix(1:20, nrow=5, ncol=4)
print(mymatrix)

rname <- c("row1", "row2", "row3")
cname <- c("col1","col2","col3")
mymatrix2 <- matrix(x, nrow=3, ncol=3, dimnames = list(rname,cname))
print((mymatrix2))

mymatrix2[1,3]
mymatrix2[c(1,3),]
mymatrix2[,2]
mymatrix2[2,]



myArray <- array(1:24, c(2,3,2)) # c(nrow,ncol,narray)
myArray

myArray[1,2,1] # [row,col,arrayNum]


#Data frame -> exactly look a data set
patientID <- c(1,2,3,4)
age <- c(24,34,23,56)
diabetes <- c("Type1", "Type2", "Type2", "Type1")
status <- c("poor","Improved", "Excellent", "poor")

patiendata <- data.frame(patientID, age, diabetes, status)
patiendata

# Add new column
bloodgroup <- c("A+","AB+","B-","O+")
patiendata1 <- cbind(patiendata, bloodgroup)
patiendata1

# add new row




# List -> most complex topic in R

g <- "First List"
h <- c(25,26,18,39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h,j,k)
































