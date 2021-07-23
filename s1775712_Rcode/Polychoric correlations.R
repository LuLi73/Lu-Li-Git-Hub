# Date: 18 June 2019
# Task: 1. Import CET data from .csv, clean it and save it as an .RData file
#       2. Conduct polychoric correlation for CET-A and CET-B and calculate 
#          p-values for the polychoric correlations

### Import the data
cet_data <- read.csv('/Users/lilu/Desktop/CET.csv')
dim(cet_data)

### Obtain the data from CET-A and CET-B respectively and delete 
### all the missing values
# for CET-A:
l1 <- cet_data[c(8:16)]
l2 <- na.omit(l1)
dim(l2)

# for CET-B:
l3 <- cet_data[c(17:25)]
l4 <- na.omit(l3)
dim(l4)


########### Polychoric Correlations ####################
### install and load the package "polycor" 
install.packages("polycor")
library(polycor)

### use "polychor" to calculate the polychoric correlation between 
### two ordinal variables and determine the p-values for the 
### correlation. 

#The polychoric correlation is "C" and p-values for
# the polychoric correlation is "C_pval" for CET-A
C=matrix(0,nrow=9,ncol=9)
C_pval=matrix(0,nrow=9,ncol=9)
for (i in 1:8){
  for (j in (i+1):9){
    polycor<- polychor(l2[,i],l2[,j],std.err=TRUE)
    C[i,j]<-polycor$rho
    C_pval[i,j]<-pchisq(polycor$chisq, polycor$df, lower.tail = FALSE)
  }
}
C=C+t(C);
C_pval=C_pval+t(C_pval)
diag(C)=rep(1,9)

# add colnames and rownames to C and C_pval
colnames(C)<-colnames(l2)
rownames(C)<-colnames(l2)
colnames(C_pval)<-colnames(l2)
rownames(C_pval)<-colnames(l2)

#The polychoric correlation is "M" and p-values for
# the polychoric correlation is "M_pval" for CET-B
M=matrix(0,nrow=9,ncol=9)
M_pval=matrix(0,nrow=9,ncol=9)
for (i in 1:8){
  for (j in (i+1):9){
    polycor_B<- polychor(l4[,i],l4[,j],std.err=TRUE)
    M[i,j] <- polycor_B$rho
    M_pval[i,j]<-pchisq(polycor_B$chisq, polycor_B$df, lower.tail = FALSE)
  }
}
M=M+t(M);
M_pval=M_pval+t(M_pval)
diag(M)=rep(1,9)

# add colnames and rownames to M and M_pval
colnames(M)<-colnames(l4)
rownames(M)<-colnames(l4)
colnames(M_pval)<-colnames(l4)
rownames(M_pval)<-colnames(l4)

### keep two decimals of C, C_pval, M and M_pval
corr_CETA <-  round(C, 2)
p.mat_CETA <- round(C_pval,2)
corr_CETB <-  round(M, 2)
p.mat_CETB <- round(M_pval,2)

### install and load the package 
install.packages("ggcorrplot")
library(ggcorrplot)

### plot the heatmap for polychoric correlation and barring the 
### nonsignificant coefficient
# CET-A:
ggcorrplot(corr_CETA, hc.order = FALSE, type = "lower",
           lab = TRUE)
ggcorrplot(corr_CETA, hc.order = FALSE, type = "lower"
           ,p.mat=p.mat_CETA)

# CET-B
ggcorrplot(corr_CETB, hc.order = FALSE, type = "lower",
           lab = TRUE)
ggcorrplot(corr_CETB, hc.order = FALSE, type = "lower"
           ,p.mat=p.mat_CETB)


