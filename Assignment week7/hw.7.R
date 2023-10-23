# Problem 1 
# For the Golub et al. (1999) data set, use appropriate Wilcoxon two-sample tests to
# find the genes whose mean expression values are higher in the ALL group than in
# the AML group. Use FDR adjustments at the 0.05 level. How many genes are
# expressed higher in the ALL group? Find the gene names for the top three genes with smallest p-values.
#(a)
data(golub, package = "multtest")
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
wilcox_test <- apply(golub, 1, function(x) wilcox.test(x~gol.fac, alternative = "greater", exact=F)$p.value)
p.fdr <-p.adjust(p=wilcox_test, method="fdr")
sum(p.fdr<0.05)

#(b)
wilcox_test <- apply(golub, 1, function(x) wilcox.test(x~gol.fac, alternative = "greater", exact=F)$p.value)
o <- order(wilcox_test,decreasing=FALSE)
golub.gnames[o[1:3],2]

# Problem 2 
# For the Golub et al. (1999) data set, apply the Shapiro-Wilk test of normality to
# every gene’s expression values in the AML group. How many genes do not pass
# the test at 0.05 level with FDR adjustment? Please submit your R script with the answer.
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL","AML"))
AML<-(golub[,gol.fac=="AML"])
shapiro <- apply(AML,1,shapiro.test)
p.values <- sapply(shapiro, function(x) x$p.value)
p.fdr <- p.adjust(p=p.values, method="fdr")
p_fdr <- sum(p.fdr <0.05)
p_fdr

# Problem 3 
# Gene "HOXA9 Homeo box A9" can cause leukemia (Golub et al., 1999). Use
# appropriate Wilcoxon two-sample tests to test if, for the ALL patients, the gene
# "HOXA9 Homeo box A9" expresses at the same level as the “CD33” gene.

wilcox.test(golub[1391,gol.fac=="ALL"],golub[808,gol.fac=="ALL"], paired = T, alternative="two.sided")

# Problem 4 
# The data set “UCBAdmissions” in R contains admission decisions by gender at six
# departments of UC Berkeley. For this data set, carry out appropriate test for
# independence between the admission decision and gender for each of the departments.
data("UCBAdmissions")
for (i in 1:6) {
  dept_ad <- UCBAdmissions[,,i]
  value_t <- fisher.test(dept_ad)
  dept_name <- colnames(UCBAdmissions)[i+2]  
  cat("Fisher test result for department", i, value_t$p.value,"\n")
}

  
#(5)

gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
dataALL <- golub[808,gol.fac=="ALL"]
dataAML <- golub[808,gol.fac=="AML"]
T.obs<- var(dataALL)/var(dataAML)
n.perm<-2000
T.perm<-numeric(n.perm)
for(i in 1:n.perm) {
  data.perm=sample(c(dataALL, dataAML), length(c(dataALL, dataAML)), replace = FALSE)
  s1 <- var(data.perm[1:length(dataALL)])
  s2 <- var(data.perm[(length(dataALL)+1):(length(dataALL)+length(dataAML))])
  T.perm[i]<-s1/s2
  
}
mean(T.perm<=T.obs)