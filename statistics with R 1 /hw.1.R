# problem 1
rm(list=ls())
# (a) What is the class of the object defined be vec <-c(5,TRUE) ?
vec<-c(5,TRUE)
class(vec)

# (b) Suppose I have vectors x <- 1:4 and y <- 1:2. What is the result of the expression x + y?
x<-1:4
y<-1:2
x+y

# (c) What is returned by the R command c(1,2) %*% t(c(1,2)) ?
c(1,2)%*%t(c(1,2))

# (d) Suppose I define the following function in R:
# f <- function(x) {
# g <- function(y) {
# y+z
# }
# z<-4
# x+g(x)
# }
# If I then run in R the following statements
# z<-15
# f(3)
# What value is returned?
f <- function(x) { 
  g <- function(y) { 
    y+z
  }
  z<-4
  x+g(x)
}
f(3)

# problem 2
rm(list=ls())
x<-1:1000
sum(x^2)

# problem 3
# Consider a group of 10 randomly selected people of different ages.
rm(list=ls())
# (a) Create a vector named “age” to represent this. You can pick any
# reasonable age (whole numbers only please) for each person.
age<-c(12, 14, 16, 18, 20, 22, 24, 25, 27, 29)
age
# (b) Multiply each person’s age by 12 (to convert into months). 
age*12
# (c) Find the sum of ages of all these people.
sum(age)
# (d) Find the age of the youngest person
min(age)
# (e) Find the age of the oldest person.
max(age)
# (f) Find the square root of the age of each person. (Not sure what this means, but who cares?) 
sqrt(age)

# problem 4
rm(list=ls())
# (a) Create a vector X of length 30, with the kth element in X = 3k, for
# k=1…30. Print out the values of X.
X1<-seq(1,30)
X<-3*X1
X

# (b) Create a vector Y of length 30, with all elements in Y equal to 0. Print
# out the values of Y.
Y<-rep(0,30)
Y

# (c) 

for (k in 1:30) {             
  if (k < 20) {                
    Y[k] <- sin(2*k)             
  } else {         
    Y[k] <- integrate(function(t) sqrt(t), lower=0, upper=k)$value 
  }
}
print(Y)
