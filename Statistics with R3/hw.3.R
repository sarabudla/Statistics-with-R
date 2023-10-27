# 1st question

x<- 1
f.x <- (3^x*exp(-3))/factorial(x)
f.x


X_range <- c(0,1,2,3,4)
f.x<-function(x)(3^x*exp(-3))/factorial(x)
f_x<- function(x)f.x(x)*(x %in% X_range)
sum(f_x(X_range)*(-3<X_range & X_range<5))


# 2nd question
# If two carriers of the gene for albinism marry and have children, then each of
# their children has a probability of 1/7 of being albino. Let the random variable Y denote
# the number of their children having the gene for albinism out of all 5 of their children.
# Then Y follows a binomial(n, p) distribution. Find the values for n and p.
# n =_____ p = _____
n = 5
p = 0.14

# 3rd problem 
# For Y following a binomial (n = 3, p = 0.25) distribution
# (a) P(Y ≤ 2) =
set.Y <- c(0,1,2) 
sum(dbinom(set.Y, size = 3, p=0.25))

# (b) E(Y) =
Y.range<-(0:3)
EY<- sum(Y.range*dbinom(Y.range,size=3, p=0.25))
EY

# (c) Var(Y) =
VarY<-sum((Y.range-EY)^2*dbinom(Y.range,size=3,p=0.25))
VarY

# 4 th problem
# For X following a Chi-square distribution with degree of freedom m = 5
# (a) P(2 < X < 5) =
integrate(function(x) dchisq(x,5),lower = 2, upper=5)$value

# (b) E(X) =
EX <- 5
EX

# (c) Var(X) =
VarX <- 2*5
VarX

# (d) Also, use a Monte Carlo simulation with sample size n=100,000 to
# estimate P(2 < X < 5). What is your Monte Carlo estimate? Does it agree with
# the answer in a)?
Chiseq <- rchisq(n=100000, df=5)
mean(Chiseq > 2 & Chiseq < 5)

# 5th problem
# Suppose X follows a Chi-square distribution with degree of freedom m = 6 so that E(X) =
# 8 and Var(X) = 12. Also, let Y = 3X - 5.

# (a) Find E(Y) and Var (Y).
EY <- 3*(6)-5
EY

VarY <- (3^2)*12
VarY


# (b) Does Y follow a Chi-square distribution with degree of freedom m=6?

# It will not follow. variance is 108 so it doesn't follow because variance always should 
# be twice of mean


# 6th problem
# The distribution of the expression values of the patients with the Zyxin gene
# are distributed according to 𝑁(𝜇 = 1.6, 𝜎 = 0.4).

# (a) What is the probability that a randomly chosen patient have the Zyxin gene
# expression values between 1 and 1.6?
pnorm(1.6, mean=1.6, sd=0.4) - pnorm(1, mean=1.6, sd=0.4)

# (b)Use a Monte Carlo simulation of sample size n=500,000 to estimate the
# probability in part (a). Give your R code, and show the value of your estimate.
MCM<-rnorm(500000, mean = 1.6, sd = 0.4)
mean(MCM > 1 & MCM < 1.6)

# (c) What is the probability that exactly 2 out of 5 patients have the Zyxin gene
# expression values between 1 and 1.6? Please show your work on how to arrive at
# the answer. Give your answer to at least four decimal places.

dbinom(2, size=5, p=0.4331928)


