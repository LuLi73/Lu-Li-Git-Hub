# Date: 21 June 2019
# Task: 1. Find the golden rule according to p-values
#       2. Model checking

############# Finding the Golden Rule ##################
### CET A
# calculate p-values for nine items against six aetiologies
P=matrix(0,nrow=9,ncol=6)
newcet_data <- na.omit(cet_data[c(2:5,8:16)])
newcet_data[,1:2] <- scale(newcet_data[,1:2],scale=FALSE)
for (i in 1:9){
  for (j in 1:6){
    fit <- lm(newcet_data[,i+4] ~ newcet_data[,1]+newcet_data[,2]+newcet_data[,3])
    R <- resid(fit)
    res.aov <- kruskal.test(R ~ (newcet_data[,4]==j))
    P[i,j] <- res.aov$p.value
  }
}

# take -log10 of the p-values
logP <- -log10(P)

# add colnames and rownames to M and M_pval
colnames(logP)<-c("Aetiology 1", "Aetiology 2",
                  "Aetiology 3","Aetiology 4","Aetiology 5","Aetiology 6")
rownames(logP)<-colnames(newcet_data[,5:13])

# plot the result
logP <- round(logP,3)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 2, 0, 0)) 
image(1:ncol(logP), 1:nrow(logP), t(logP), 
      col = terrain.colors(60), axes = FALSE,ylab="",xlab="")
axis(1, 1:ncol(logP), colnames(logP))
axis(2, 1:nrow(logP), rownames(logP),las=1)

for (x in 1:ncol(logP))
  for (y in 1:nrow(logP))
    text(x, y, logP[y,x])

### CET B
# calculate p-values for nine items against six aetiologies
Q=matrix(0,nrow=9,ncol=6)
newcet_data <- na.omit(cet_data[c(2:5,17:25)])
newcet_data[,1:2] <- scale(newcet_data[,1:2],scale=FALSE)
for (i in 1:9){
  for (j in 1:6){
    fit <- lm(newcet_data[,i+4] ~ newcet_data[,1]+newcet_data[,2]+newcet_data[,3])
    R <- resid(fit)
    res.aov <- kruskal.test(R ~ (newcet_data[,4]==j))
    Q[i,j] <- res.aov$p.value
  }
}

# take -log10 of the p-values
logQ <- -log10(Q)

# add colnames and rownames to M and M_pval
colnames(logQ)<-c("Aetiology 1", "Aetiology 2","Aetiology 3","Aetiology 4","Aetiology 5","Aetiology 6")
rownames(logQ)<-colnames(newcet_data[,5:13])

# plot the result
logQ <- round(logQ,3)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 2, 0, 0)) 
image(1:ncol(logQ), 1:nrow(logQ), t(logQ), col = terrain.colors(60), axes = FALSE,ylab="",xlab="")
axis(1, 1:ncol(logQ), colnames(logQ))
axis(2, 1:nrow(logQ), rownames(logQ),las=1)

for (x in 1:ncol(logQ))
  for (y in 1:nrow(logQ))
    text(x, y, logQ[y,x])

############# Model Checking ##################
# obtain and clear data of age, education gender and items in CET-A and CET-B
l_total <- cet_data[c(2:5,8:25)]
l_total2 <- na.omit(l_total)
attach(l_total2)

# logistic model 1
Aetiology_cat=ifelse(Aetiology==5,1,0)
glm.fit_all <- glm(Aetiology_cat ~ Age + Years.of.Edu+ Gender+CET.A.Q5 + CET.A.Q6 + 
                     CET.A.Q9 + CET.B.Q4, 
                   data = l_total2, family = binomial)
summary(glm.fit_all)

# logistic model 2
glm.fit_oldall <- glm(Aetiology_cat ~.-Aetiology, 
                      data = l_total2, family = binomial)
summary(glm.fit_oldall)

# produce regression a table and export into latex form
install.packages("stargazer")
library("stargazer")
stargazer(glm.fit_all, glm.fit_oldall, title="Results", align=TRUE)

# logistic model 3
Aetiology_cat_4=ifelse(Aetiology==4,1,0)
glm.fit_all2 <- glm(Aetiology_cat_4 ~ Age + Years.of.Edu+ Gender+CET.A.Q5 + CET.A.Q6 + 
                      CET.A.Q9 + CET.B.Q4, 
                    data = l_total2, family = binomial)
summary(glm.fit_all2)

# logistic model 4
glm.fit_oldall <- glm(Aetiology_cat_4 ~.-Aetiology, 
                      data = l_total2, family = binomial)
summary(glm.fit_oldall)

# produce regression a table and export into latex form
install.packages("stargazer")
library("stargazer")
stargazer(glm.fit_all2 , glm.fit_oldall, title="Results", align=TRUE)