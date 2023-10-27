# problem_1
# A random sample of size 6 from the exp(λ) distribution results in observations:
# 1.433, 0.524, 0.384, 4.515, 1.852, 0.429. Find the MLE on this data set in two ways:
#(a) by numerical optimization of the likelihood 
lik<-function(lam) prod(dexp(c(1.433, 0.524, 0.384, 4.515, 1.852, 0.429), rate=lam))
nlik<-function(lam) -lik(lam) 
optim(par=1, nlik)$par

#(b) by the analytic formula.
n<- 6
Observations<- sum(1.433, 0.524, 0.384, 4.515, 1.852, 0.429)
analytic <- n/Observations
analytic

# problem_2
# A random sample X1, X2, ………., X75 follows chi-square distribution with m degree of
# freedom, has sample mean 𝑋= 98.6 and sample standard deviation 𝑠 = 9.4.
#(a)  Find the point estimator of m using the method of moments.
#In chi-square Distribution the population mean(m) is equal to sample mean(X)
m=98.6

#(b) Find a one-sided 90% lower confidence interval of m.
CI<- 98.6-qt(0.10, 74)*(9.4/sqrt(74))
CI

# problem_3
# On the Golub et al. (1999) data set, analyze the Zyxin gene expression data
# separately for the ALL and AML groups.
# (a) Find the bootstrap 95% CIs for the mean and for the variance of the gene
# expression in each group separately.
# mean and variance for ALL
data(golub, package = "multtest") 

ZyxinALL<-golub[2124,1:27]
n<-length(ZyxinALL)
nboot<-1000
boot.xbarmALL<-rep(NA,nboot)
boot.xbarvALL<-rep(NA,nboot)
for(i in 1:nboot) {
  data.star<-ZyxinALL[sample(1:n,replace=TRUE)]
  boot.xbarmALL[i]<-mean(data.star)
  boot.xbarvALL[i]<-var(data.star)
}
quantile(boot.xbarmALL,c(0.025,0.975))
quantile(boot.xbarvALL,c(0.025,0.975))

#mean and variance for AML
ZyxinAML<-golub[2124,28:38]
n<-length(ZyxinAML)
nboot<-1000
boot.xbarmAML<-rep(NA,nboot)
boot.xbarvAML<-rep(NA,nboot)
for(i in 1:nboot) {
  data.star<-ZyxinAML[sample(1:n,replace=TRUE)]
  boot.xbarmAML[i]<-mean(data.star)
  boot.xbarvAML[i]<-var(data.star)
  }
quantile(boot.xbarmAML,c(0.025,0.975))
quantile(boot.xbarvAML,c(0.025,0.975))


# (b)Find the parametric 95% CIs for the mean and for the variance of the gene
# expression in each group separately. (You need to choose the appropriate
# approximate formula to use: z-interval, t-interval or chi-square interval.)

# parametric mean for ALL and AML
gol.fac <- factor(golub.cl,levels=0:1,labels=c("ALL","AML")) 
xmALL <- golub[2124, gol.fac=="ALL"]
nmALL<-length(xmALL)
ci.mALL <- mean(xmALL)+qt(c(0.025,0.957),df=nmALL-1)*sd(xmALL)/sqrt(nmALL) 

gol.fac <- factor(golub.cl,levels=0:1,labels=c("ALL","AML"))
xmAML <- golub[2124, gol.fac=="AML"]
nmAML<-length(xMAML) 
ci.mAML <- mean(xmAML)+qt(c(0.025,0.975),df=nmAML-1)*sd(xmAML)/sqrt(nmAML)
print(ci.mALL)
print(ci.mAML)

# parametric varaiance for ALL and AML
gol.fac <- factor(golub.cl,levels=0:1,labels=c("ALL","AML"))  
xvALL <- golub[2124, gol.fac=="ALL"]
nvALL<-length(xvALL) #sample size n
ci.vALL <- var(xvALL)+qchisq(c(0.025,0.975),df=nvALL-1)*sd(xvALL)/sqrt(nvALL) 

gol.fac <- factor(golub.cl,levels=0:1,labels=c("ALL","AML")) 
xvAML <- golub[2124, gol.fac=="AML"]
nvAML<-length(xvAML) #sample size n
ci.vAML <- var(xvAML)+qchisq(c(0.025,0.975),df=nvAML-1)*sd(xvAML)/sqrt(nvAML) 
print(ci.vALL)
print(ci.vAML)

# (C) Find the bootstrap 95% CI for the median gene expression in both groups separately.

#median of ALL
ZyxinmdALL<-golub[2124,1:27]
n<-length(ZyxinmdALL)
nboot<-1000
boot.xbarmdALL <- rep(NA, nboot)
for (i in 1:nboot) {
  data.star <- ZyxinmdALL[sample(1:n,replace=TRUE)]
  boot.xbarmdALL[i]<-median(data.star)
}
quantile(boot.xbarmdALL,c(0.025,0.975))

# median for AML
ZyxinmdAML<-golub[2124,28:38]
n<-length(ZyxinmdAML)
nboot<-1000
boot.xbarmdAML <- rep(NA, nboot)
for (i in 1:nboot) {
  data.star <- ZyxinmdAML[sample(1:n,replace=TRUE)]
  boot.xbarmdAML[i]<-median(data.star)
}
quantile(boot.xbarmdAML,c(0.025,0.975))

# problem_4
nsim <- 1000
MCsim<- function(nsim, lambda) {
  cov1<-cov2<-rep(NA,nsim) # create empty matrices to store data
  for (i in 1:nsim) {
    x<- rpois(50, lambda) # The question says Poisson
    xbar<- mean(x) #find mean of x
    Xsd<- sd(x)# find sd of x
    CI1<-c(xbar+(qt(0.05, 49)*sqrt(xbar/50)), xbar+qt(0.95, 49)*sqrt(xbar/50)) #use the formula for conf interval for mean given in the question
    CI2<-c(49*(Xsd^2)/qchisq(0.95, 49), 49*(Xsd^2)/qchisq(0.05, 49))
    cov1[i]<-(CI1[1]<lambda)&(lambda<CI1[2])
    cov2[i]<-(CI2[1]<lambda)&(lambda<CI2[2])
  }
  
  print(paste("When lambda=", lambda, ": coverage for first CI is", mean(cov1), ", coverage for second CI is", mean(cov2), ".")) # Just to keep your output presentable
}

# (b)
MCsim(1000, 0.1)
MCsim(1000, 1)
MCsim(1000, 10)


