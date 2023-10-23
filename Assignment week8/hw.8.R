# problem_1
# On the ALL data set, consider the ANOVA on the gene with the probe “109_at”
# expression values on B-cell patients in 5 groups: B, B1, B2, B3 and B4.
# (a) Conduct the one-way ANOVA. Do the disease stages affect the mean gene expression value?

data(ALL,package="ALL");library(ALL)
library(lmtest)
ALLB12345 <- ALL[,ALL$BT %in% c("B","B1","B2","B3","B4")]
y<-exprs(ALLB12345)["109_at",]
anova(lm(y ~ ALLB12345$BT))
# From Anova table, p-value=0.01082 is very small and we reject the null hypothesis.hence we conclude that the 109_at gene expression is related to the disease stages for B-cells:B,b1,b2,b3,b4

# (b) From the linear model fits, find the mean gene expression value among B3 patients. 
ALLB3 <- ALL[,ALL$BT =="B3"]
mean <- lm(exprs(ALLB3)["109_at",]~1)
summary(mean)

# (c) Use the pairwise comparisons at FDR=0.05 to find which group means are different.
ALLB12345 <- ALL[,ALL$BT %in% c("B","B1","B2","B3","B4")]
y<-exprs(ALLB12345)["109_at",]
pairwise.t.test(y, ALLB12345$BT, p.adjust.method = 'fdr')

# (d) Check the ANOVA model assumptions with diagnostic tests? Do we need to
# apply robust ANOVA tests here? 
ALLB12345 <- ALL[,ALL$BT %in% c("B","B1","B2","B3","B4")]
y<-exprs(ALLB12345)["109_at",]
shapiro.test(residuals(lm(y ~ ALLB12345$BT)))
bptest(lm(y~ALLB12345$BT), studentize=FALSE)
# for shapiro test, the p-value is 0.1177, so we don't reject null hypothesis of normally distributed residuals. Therefore, the normality assumption does  hold.
# for Besusch-Pagan test, the p-value is 0.883, so we don't reject the null hypothesis of equal variances (homoscedasticity).

# problem_2
# Apply the nonparametric Kruskal-Wallis tests for every gene on the B-cell ALL
# patients in stage B, B1, B2, B3, B4 from the ALL data.
# (a) Use FDR adjustments at 0.05 level. How many genes are expressed different in
# some of the groups?
ALLB12345 <- ALL[,ALL$BT %in% c("B","B1","B2","B3","B4")]
y<-exprs(ALLB12345)
kruskal_test <- apply(y, 1, function(x) kruskal.test(x ~ ALLB12345$BT))
p.values<- sapply(kruskal_test, function(x) x$p.value)
fdr <- p.adjust(p=p.values, method ='fdr')
sum(fdr<0.05)

# (b) Find the probe names for the top five genes with smallest p-values.
genes5 <- names(sort(fdr)[1:5])
genes5

# problem_3
# On the ALL data set, we consider the ANOVA on the gene with the probe
# 38555_at” expression values on two factors. The first factor is the disease stages:
# B1, B2, B3 and B4 (we only take patients from those four stages). The second
# factor is the gender of the patient (stored in the variable ALL$sex).
# (a) Conduct the appropriate ANOVA analysis. Does any of the two factors affects
# the gene expression values? 
ALLBs <- ALL[ALL$BT %in% c("B1","B2","B3","B4")]
y<-exprs(ALLBs)["38555_at",]
anova(lm(y~ALLBs$BT * ALL$sex))

#(b) Check the ANOVA model assumption with diagnostic tests? Are any of the
# assumptions violated?
ALLBs <- ALL[ALL$BT %in% c("B1","B2","B3","B4")]
y<-exprs(ALLBs)["38555_at",]
shapiro.test(residuals(lm(y~ALLBs$BT * ALL$sex)))
#Since the p-value 0.02282 is very small, we reject the null-hypothesis of 
#normally distributed residuals. Therefore, the normality assumption does not hold. 

ALLBs <- ALL[ALL$BT %in% c("B1","B2","B3","B4")]
y<-exprs(ALLBs)["38555_at",]
bptest(lm(y~ALLBs$BT * ALL$sex), studentize = FALSE)
#From the p-value 0.6557, we don't reject the null hypothesis of 
#equal variances (homoscedasticity).
