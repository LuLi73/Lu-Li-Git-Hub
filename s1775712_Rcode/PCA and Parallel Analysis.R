# Date: 19 June 2019
# Task: 1. Perform Principal Component Analysis (PCA) on the Polychoric 
#          Correlations of CET-A and CET-B
#       2. Conduct parallel analysis 

############ PCA #############
### CET-A
pc_ceta<-princomp(l2, score=TRUE,covmat=C)
pc_ceta2<-princomp(l2, score=TRUE,center=TRUE,scale=FALSE)

# additional information
summary(pc_ceta)
plot(pc_ceta)
plot(pc_ceta,type="l")
pc_ceta$loadings

## scores for CET-A
ev<-eigen(C);
evectors<-ev$vectors
evalues<-ev$values
meanl2=apply(l2,2,mean)
pc_ceta$center=meanl2
pc_ceta$scale=stdl2
pc_ceta$n.obs=198
l2_2<-scale(l2)
pc_ceta$scores=as.matrix(l2_2)%*%(-evectors)
pc_ceta$scores

# Bi-plot
library(ggbiplot)
print(ggbiplot(pc_ceta, obs.scale = 1, var.scale = 1, 
               groups = as.factor(l2.class), ellipse = TRUE, circle = TRUE)+
        scale_color_manual(name="Aetiology Type", 
                           values=c("red","blue","yellow",
                                    "green","purple","black")))


#### CET-B
pc_ceta_B<-princomp(l4, score=TRUE,covmat=M)

# additional information
summary(pc_ceta_B)
plot(pc_ceta_B)
plot(pc_ceta_B,type="l")
pc_ceta_B$loadings

# scores for CET-B
ev_B<-eigen(M);
evectors_B<-ev_B$vectors
evalues_B<-ev_B$values
meanl4=apply(l4,2,mean)
stdl4=apply(l4,2,sd)
l4_2<-scale(l4)
pc_ceta_B$center=meanl4
pc_ceta_B$scale=stdl4
pc_ceta_B$n.obs=75
pc_ceta_B$scores=as.matrix(l4_2)%*%(-evectors_B)
pc_ceta_B$scores

# Bi-plot
print(ggbiplot(pc_ceta_B, obs.scale = 1, 
               var.scale = 1, groups = as.factor(l4.class), 
               ellipse = TRUE, circle = TRUE)+
        scale_color_manual(name="Aetiology Type", 
                           values=c("red","blue","yellow",
                                    "green","purple","black")))

############ Parallel Analysis #############
# loading the package
library("psych")

# CET-A
fa.parallel(l2, cor="poly", fa="pc")

# CET-B
fa.parallel(l4, cor="poly", fa="pc")


