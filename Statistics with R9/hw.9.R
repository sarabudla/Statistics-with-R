# problem_1
# On the Golub et al. (1999) data set, find the expression values for the GRO2 GRO2
# oncogene and the GRO3 GRO3 oncogene. (Hint: Use grep() to find the gene rows in golub.gnames. 
library("multtest")
data(golub)
# (a) Find the correlation between the expression values of these two genes.
GRO2<-golub[2714,]
GRO3<-golub[2715,]
cor(GRO2,GRO3)

# (b) Find the parametric 90% confident interval for the correlation with cor.test(). (Hint:
# use ?cor.test to learn how to set the confidence level different from the default value of 95%.)
cor.test(GRO2, GRO3 , conf.level =0.90)

# (c) Find the bootstrap 90% confident interval for the correlation.
nboot <- 2000
boot.cor<- matrix(0, nrow=nboot, ncol = 1)
data<- cbind(GRO2,GRO3)
for (i in 1:nboot){ 
  dat.star <- data[sample(1:nrow(data), replace=TRUE),]
  boot.cor[i,]<-cor(dat.star[,1], dat.star[,2])
}
quantile(boot.cor[,1],c(0.025,0.90))

#problem_2
# On the Golub et al. (1999) data set, we consider the correlation between the Zyxin gene
# expression values and each of the gene in the data set.
#(a) How many of the genes have correlation values less than negative 0.5? (Those genes
# are highly negatively correlated with Zyxin gene).
grep("Zyxin",golub.gnames[,2])
gene_zy <- golub[2124,]
allgenes <- golub
cor <- apply(allgenes, 1, function(x) cor(x, gene_zy))
totalgenes<-sum(cor < (-0.5))
totalgenes

#(b) Find the gene names for the top five genes that are most negatively correlated with
# Zyxin gene.
gene_5 <- (order(cor)[1:5])
gene_5_names <- golub.gnames[gene_5,2]
gene_5_names

#(c) Using the correlation test, how many genes are negatively correlated with the Zyxin
# gene? Use a false discovery rate of 0.05. 
corr_test<- apply(golub, 1, function(x) cor.test(x, gene_zy))
p.values <- sapply(corr_test, function(x) x$p.value)
adj_val <- p.adjust(p=p.values, method="fdr")
neg_gene <- sum(adj_val < 0.05)
neg_gene

# problem_3
# On the Golub et al. (1999) data set, regress the expression values for the GRO3 GRO3
# oncogene on the expression values of the GRO2 GRO2 oncogene.
#(a) Is there a statistically significant linear relationship between the two genes’
# expression? Use appropriate statistical analysis to make the conclusion. What proportion
# of the GRO3 GRO3 oncogene expression’s variation can be explained by the regression
# on GRO2 GRO2 oncogene expression?
GRO2<-golub[2714,]
GRO3<-golub[2715,]
reg.fit_3<- lm(GRO3~GRO2)
reg.fit_3
summary(reg.fit_3)

#(b) Find an 80% prediction interval for the GRO3 GRO3 oncogene expression when
# GRO2 GRO2 oncogene is not expressed (zero expression value).
predict(reg.fit, data.frame(GRO2=0), interval="prediction", level = 0.80)

#(c) Check the regression model assumptions. Can we trust the statistical inferences from
# the regression fit?
shapiro.test(resid(reg.fit_3))


# problem_4
# For this problem, work with the data set stack.loss that comes with R. You can get help
# on the data set with ?stackloss command. That shows you the basic information and
# source reference of the data set. Note: it is a data frame with four variables. The variable
# stack.loss contains the ammonia loss in a manufacturing (oxidation of ammonia to nitric
# acid) plant measured on 21 consecutive days. We try to predict it using the other three
# variables: air flow (Air.Flow) to the plant, cooling water inlet temperature (C)
# (Water.Temp), and acid concentration (Acid.Conc.)

#(a) Regress stack.loss on the other three variables. What is the fitted regression equation?
data("stackloss")
stackloss_data <- data.frame(stackloss[,c('Air.Flow','Water.Temp','Acid.Conc.','stack.loss')]) 
names(stackloss_data) <- c('Air.Flow','Water.Temp','Acid.Conc.','stack.loss')
reg <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
summary(reg)
# we can see that fitted regression equation is 
# stack.loss = -39+0.71Air.Flow+1.29Water.Temp-0.15Acid.Conc.

#(b)Do all three variables have statistical significant effect on stack.loss? What proportion
# of variation in stack.loss is explained by the regression on the other three variables?

#According to the output, the multiple R-squared is 0.9136, which indicates that the regression model explains for 91.36% of the variation in stack.loss. This shows that the model fits the data well and that there is a strong correlation between the predictor variables and stack.loss

#(c)Find a 90% confidence interval and 90% prediction interval for stack.loss when
# Air.Flow=60, Water.Temp=20 and Acid.Conc.=90.
c_p_data <- data.frame(Air.Flow=60, Water.Temp=20, Acid.Conc.=90)
conf_interval <- predict(reg, c_p_data, interval="confidence", level=0.90)
conf_interval
pred_internal <- predict(reg, c_p_data, interval="prediction", level=0.90)
pred_internal




