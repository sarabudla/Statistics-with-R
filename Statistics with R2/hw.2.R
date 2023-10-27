# Problem 1
# (a)Write an R code to compute the mean expression values for every gene among“ALL” patients.
##compute values of all genes among ALL
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL", "AML"))
meanALL <- apply(golub[,gol.fac=="ALL"], 1, mean)

# (b) Write an R code to compute the mean expression values for every gene among“AML” patients.
## compute values of all genes amoung AML
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL", "AML"))
meanAML <- apply(golub[,gol.fac=="AML"], 1, mean)

# (c) Give the biological names of the three genes with the largest mean expression value among “ALL” patients.
## biological names of three largest mean expression genes of ALL
largeval_genes_ALL = head(order(meanALL, decreasing = TRUE), 3)
gene_symbols <- golub.gnames[largeval_genes_ALL,2]
print(gene_symbols)

# (d) Give the biological names of the three genes with the largest mean expression value among “AML” patients.
##biological names of three largest mean expression genes of AML 
largeAML_genes = head(order(meanAML, decreasing = TRUE), 3)
gene_symbols <- golub.gnames[largeAML_genes,2]
print(gene_symbols)

# Problem 2
# (a) Save the expression values of the first five genes (in the first five rows) for the
# AML patients in a csv file “AML5.csv”
## saving exp values of first fives genes of AML 
first_five_AML <- golub[1:5,gol.fac=="AML"]
write.csv(first_five_AML,"AML5.csv",row.names = TRUE)

# (b) Save the expression values of the first five genes for the ALL patients in a plain
# text file “ALL5.txt”
## saving exp values of first five genes of ALL 
first_five_ALL <- golub[1:5,gol.fac=="ALL"]
write.csv(first_five_ALL,"ALL5.csv",row.names = TRUE)

# (c) Compute the standard deviation of the expression values on the first patient, of
# the 100th to 200th genes (total 101 genes).
##standard deviation of the expression values on the first patient, of the 100th to 200th genes 
sd_first_patient <- sd(golub[100:200],1)
print(sd_first_patient)

# (d) Compute the standard deviation of the expression values of every gene, across
# all patients. Find the number of genes with standard deviation greater than 1.
sd_genes <- apply(golub, 1, sd)
number_of_genes_with_sd_gt_1 <- length(which(sd_genes > 1))
print(number_of_genes_with_sd_gt_1)

# (e) Do a scatter plot of the 101th gene expressions against the 102th gene
# expressions, label the x-axis and the y-axis with the genes’ biological names using
# xlab= and ylab= control options.
## scatter plot of the 101th gene expressions against the 102th gene expressions
x <- golub[101,]
y <- golub[102,]
plot(x,y, xlab = "NUCLEAR PORE COMPLEX PROTEIN NUP214",ylab = "PHOSPHATIDYLSERINE SYNTHASE I",
     main = "Scatter plot of gene expression of data",pch = 16,col = "red")

# Problem 3 
# (a) Use exprs(ALL[,ALL$BT=="B1"] to extract the gene expressions from the
# patients in disease stage B1. Produce one histogram of these gene expressions in
# this matrix.

library(ALL)
data(ALL)
B1_gene_exprs <- exprs(ALL[,ALL$BT=="B1"])
hist(B1_gene_exprs, main = "Histogram of gene expressions in disease stage B1", 
     xlab = "Expression level", ylab = "Frequency")

#(b) Compute the mean gene expressions for every gene over these B1 patients

mean_gene_exprs <- rowMeans(B1_gene_exprs)

#(c) Give the gene identifiers of the three genes with the largest mean

sorted_mean_gene_exprs <- sort(mean_gene_exprs, decreasing = TRUE)
top_mean_exprs <- sort(mean_gene_exprs, decreasing = TRUE)[1:3]
top_three_genes <- rownames(B1_gene_exprs)[mean_gene_exprs %in% top_mean_exprs]
top_three_genes


# Problem 4 
# We work with the “trees” data set that comes with R. Produce a figure with two
# overlaid scatterplots: Height versus Girth, Volume versus Girth(The Girth is on the
# x-axis). Do the Height plot with blue “+” symbols, and do the Volume plot with
# red “o” symbols. You need to learn to set the ylim= control option so that all points
# from the two plots can all show up on the merged figure.

data(trees)
plot(trees$Girth, trees$Height, xlab = "Girth", ylab = "Height", 
     ylim=c(0,max(trees$Height, trees$Volume)), 
     main = "Height and Volume vs Girth")
points(trees$Girth, trees$Volume, col = "red", pch = "o")
points(trees$Girth, trees$Height, col = "blue", pch = "+")
