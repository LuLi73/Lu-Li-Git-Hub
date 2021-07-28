### 1. Modelling abundance of gulls

# (a) Perform some exploratory data analysis 
gull <- read.csv("/Users/lilu/Desktop/gulls_data.csv",header=TRUE)

plot(gull$year, gull$audouin,type="b",main="Temporal evolution of 
     the abundance of Audouin's and Yellowlegged's gulls",
     ylim = c(0,700),col="red",ylab="audouin and yellowlegged")
lines(gull$year, gull$yellowlegged,type="b",col="blue")
legend('topleft', c("Audouin", "Yellowlegged"), col=c('red', 'blue'), 
       lty=1,cex = 0.65)
plot(gull$yellowlegged,gull$audouin,main="Relation between the 
     abundance of the two species of gulls")
abline(lm(gull$yellowlegged[-c(9:11)]~gull$audouin[-c(9:11)]), col="red")

# (b) Fit a Bayesian Poisson model with a logarithm link function where the abundance of 
#     the Audouin’s gull couples is considered to be dependent on the year.
library(rjags)

# Data
n <- length(gull$year)
gull.data <- list(n=n,audouin=gull$audouin,year=gull$year)

# Inits
gull.inits <- list(list(beta0=-1,beta1=-1),
                    list(beta0=0, beta1=1),
                    list(beta0=3, beta1=2))

# Model
gull.model <-   "model {
# Hyperparameters
beta.mu.0   <- 0         
beta.tau.0  <- 0.01


# prior
beta0 ~ dnorm(beta.mu.0,beta.tau.0)
beta1 ~ dnorm(beta.mu.0,beta.tau.0)

#Likelihood
for(i in 1:n) {
log(mu[i])  <- beta0+beta1*(year[i]-mean(year[]))
audouin[i] ~ dpois(mu[i])
} 
}"



# Run JAGS to the completion of the "adaption" stage 
results.gull.A <- jags.model(file=textConnection(gull.model), 
                              data=gull.data, inits=gull.inits, 
                              n.chains=3)

# Burn-in of 5000 iterations
update(results.gull.A, n.iter=5000)

# Longer run for making inferences, assuming chains have converged
results.gull.B <- coda.samples(results.gull.A, 
                                variable.names=c("beta0","beta1"),n.iter=10000)

# Summary 
summary(results.gull.B)
  
## Check the chains
# Trace plots and density
plot(results.gull.B)  
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.gull.B)
gelman.diag(results.gull.B)
# Efective sample size
effectiveSize(results.gull.B[[1]][,"beta0"])   
effectiveSize(results.gull.B[[1]][,"beta1"]) 
# Autocorrelation plots
par(mfrow=c(2,1))
acf(results.gull.B[[1]][,"beta0"],main="Intercept")
acf(results.gull.B[[1]][,"beta1"],main="Slope")

# (c) Expand the previous model by including an extra variance term in the regressor. 

library(rjags)

# Data
n <- length(gull$year)
gull.data <- list(n=n,audouin=gull$audouin,year=gull$year)

# Inits
gull.inits <- list(list(beta0=-1,beta1=-1),
                   list(beta0=0, beta1=1),
                   list(beta0=3, beta1=2))

gull.model2 <-   "model {
# Hyperparameters
beta.mu.0   <- 0         
beta.tau.0  <- 0.01
sigma.epsilon.0 <-10


# prior
beta0 ~ dnorm(beta.mu.0,beta.tau.0)
beta1 ~ dnorm(beta.mu.0,beta.tau.0)
tau.epsilon <- pow(sigma.epsilon,-2)
sigma.epsilon ~ dunif(0,sigma.epsilon.0)

#Likelihood
for(i in 1:n) {
audouin[i] ~ dpois(mu[i])
estim[i]  <- beta0+beta1*(year[i]-mean(year[]))
log(mu[i]) <- estim.var[i]
estim.var[i] ~ dnorm(estim[i],tau.epsilon)
} 
#tracing the expected probability
E.surv <- exp(beta0)
}"
# Run JAGS to the completion of the "adaption" stage 
results.gull2.A <- jags.model(file=textConnection(gull.model2), 
                             data=gull.data, inits=gull.inits, 
                             n.chains=3)

# Burn-in of 5000 iterations
update(results.gull2.A, n.iter=5000)

# Longer run for making inferences, assuming chains have converged
results.gull2.B <- coda.samples(results.gull2.A, 
                               variable.names=c("beta0","beta1","sigma.epsilon"),n.iter=10000)

# Summary 
summary(results.gull2.B)
summary_model2 <- summary(results.gull2.B)

## Check the chains
# Trace plots and density
plot(results.gull2.B)  
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.gull2.B)
gelman.diag(results.gull2.B)
# Efective sample size
effectiveSize(results.gull2.B[[1]][,"beta0"])   
effectiveSize(results.gull2.B[[1]][,"beta1"]) 
effectiveSize(results.gull2.B[[1]][,"sigma.epsilon"]) 
# Autocorrelation plots
par(mfrow=c(3,1))
acf(results.gull2.B[[1]][,"beta0"],main="Intercept")
acf(results.gull2.B[[1]][,"beta1"],main="Slope")
acf(results.gull2.B[[1]][,"sigma.epsilon"],main="Variance")


# (d) Further expand the previous extra variance model by including the number of 
#     Yellow-legged gull couples as an explanatory variable.
# Data
n <- length(gull$year)
gull.data3 <- list(n=n,audouin=gull$audouin,year=gull$year,yellowlegged=gull$yellowlegged)

# Inits
gull.inits3 <- list(list(beta0=-1,beta1=-1,beta2=1),
                   list(beta0=0, beta1=1,beta2=2),
                   list(beta0=3, beta1=2,beta2=2))

gull.model3 <-   "model {
# Hyperparameters
beta.mu.0   <- 0         
beta.tau.0  <- 0.01
sigma.epsilon.0 <-10


# prior
beta0 ~ dnorm(beta.mu.0,beta.tau.0)
beta1 ~ dnorm(beta.mu.0,beta.tau.0)
beta2 ~ dnorm(beta.mu.0,beta.tau.0)
tau.epsilon <- pow(sigma.epsilon,-2)
sigma.epsilon ~ dunif(0,sigma.epsilon.0)

#Likelihood
for(i in 1:n) {
audouin[i] ~ dpois(mu[i])
estim[i]<-beta0+beta1*(year[i]-mean(year[]))+beta2*(yellowlegged[i]-mean(yellowlegged[]))
estim.var[i]~dnorm(estim[i],tau.epsilon)
log(mu[i])  <- estim.var[i]
}
#tracing expected probability
E.surv<-exp(beta0)
}"

# Run JAGS to the completion of the "adaption" stage 
results.gull3.A <- jags.model(file=textConnection(gull.model3), 
                             data=gull.data3, inits=gull.inits3, 
                             n.chains=3)

# Burn-in of 5000 iterations
update(results.gull3.A, n.iter=5000)

# Longer run for making inferences, assuming chains have converged
results.gull3.B <- coda.samples(results.gull3.A, 
                               variable.names=c("beta0","beta1","beta2","sigma.epsilon"),
                               n.iter=10000)

# Summary 
summary(results.gull3.B)
summary_model3 <- summary(results.gull3.B)

## Check the chains
# Trace plots and density
par(mfrow=c(2,2))
plot(results.gull3.B[,c("beta1","beta2")])
par(mfrow=c(2,2))
plot(results.gull3.B[,c("beta0","sigma.epsilon")])
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.gull3.B)
gelman.diag(results.gull3.B)
# Efective sample size
effectiveSize(results.gull3.B[[1]][,"beta0"])   
effectiveSize(results.gull3.B[[1]][,"beta1"]) 
effectiveSize(results.gull3.B[[1]][,"beta2"]) 
effectiveSize(results.gull3.B[[1]][,"sigma.epsilon"]) 
# Autocorrelation plots
par(mfrow=c(2,1))
acf(results.gull3.B[[1]][,"beta0"],main="Intercept")
acf(results.gull3.B[[1]][,"sigma.epsilon"],main="Variance")
par(mfrow=c(2,1))
acf(results.gull3.B[[1]][,"beta1"],main="Slope for Audouin's gull")
acf(results.gull3.B[[1]][,"beta2"],main="Slope for Yellow-legged gull")



#(e) Compute random samples of the predictive distributions for replicates
#    of the database (same years and abundance of Yellow-legged gull as covariates) for
#    the three models. 

# Model 1
gull.fit<-as.mcmc(do.call(rbind,results.gull.B))
logmu=as.matrix(gull.fit[,1])%*%t(rep(1,26))+as.matrix(gull.fit[,2])%*%t(gull$year-mean(gull$year))
mu=exp(logmu)
audouin.pred=apply(mu,2,function(x) rpois(150000,x))
audouin.pred.mean=apply(audouin.pred,2,mean)
audouin.pred.05=apply(audouin.pred,2,function(x) quantile(x,0.05))
audouin.pred.95=apply(audouin.pred,2,function(x) quantile(x,0.95))

plot(gull$audouin, type="l", col="dodgerblue1", lwd=2, ylim=c(0,700), main = "Model 1")
lines(audouin.pred.mean, col="forestgreen",lwd=2)
lines(audouin.pred.05, col="forestgreen",lwd=2, lty=2)
lines(audouin.pred.95, col="forestgreen",lwd=2, lty=2)

# Model 2
gull2.fit<-as.mcmc(do.call(rbind,results.gull2.B))
logmu2=as.matrix(gull2.fit[,1])%*%t(rep(1,26))+as.matrix(gull2.fit[,2])%*%t(gull$year-mean(gull$year)) 
mu2=exp(logmu2)
audouin2.pred=apply(mu2,2,function(x) rpois(150000,x))
audouin2.pred.mean=apply(audouin2.pred,2,mean)
audouin2.pred.05=apply(audouin2.pred,2,function(x) quantile(x,0.05))
audouin2.pred.95=apply(audouin2.pred,2,function(x) quantile(x,0.95))

plot(gull$audouin, type="l", col="dodgerblue1", lwd=2, ylim=c(0,700), main = "Model 2")
lines(audouin2.pred.mean, col="forestgreen",lwd=2)
lines(audouin2.pred.05, col="forestgreen",lwd=2, lty=2)
lines(audouin2.pred.95, col="forestgreen",lwd=2, lty=2)

# Model 3
gull3.fit<-as.mcmc(do.call(rbind,results.gull3.B))
logmu3=as.matrix(gull3.fit[,1])%*%t(rep(1,26))+as.matrix(gull3.fit[,2])%*%t(gull$year-mean(gull$year))+
  as.matrix(gull3.fit[,3])%*%t(gull$yellowlegged-mean(gull$yellowlegged))
mu3=exp(logmu3)
audouin3.pred=apply(mu3,2,function(x) rpois(150000,x))
audouin3.pred.mean=apply(audouin3.pred,2,mean)
audouin3.pred.05=apply(audouin3.pred,2,function(x) quantile(x,0.05))
audouin3.pred.95=apply(audouin3.pred,2,function(x) quantile(x,0.95))

plot(gull$audouin, type="l", col="dodgerblue1", lwd=2, ylim=c(0,900),, main = "Model 3")
lines(audouin3.pred.mean, col="forestgreen",lwd=2)
lines(audouin3.pred.05, col="forestgreen",lwd=2, lty=2)
lines(audouin3.pred.95, col="forestgreen",lwd=2, lty=2)


#(f) Compute the Deviance Information Criterion (DIC) of the three models and compare 
#    the performance of the three models (according to DIC and all the previous analyses). 

# DIC for model 1
results.gull.B.DIC <- dic.samples(model=results.gull.A, n.iter=10000, type="pD")
# DIC for model 2
results.gull2.B.DIC <- dic.samples(model=results.gull2.A, n.iter=10000, type="pD")
# DIC for model 3
results.gull3.B.DIC <- dic.samples(model=results.gull3.A, n.iter=10000, type="pD")
# compare model 1 and model 2
diffdic(results.gull.B.DIC, results.gull2.B.DIC)
# compare model 2 and model 3
diffdic( results.gull2.B.DIC, results.gull3.B.DIC)
# compare model 3 and model 1
diffdic(results.gull3.B.DIC, results.gull.B.DIC)


### 2. Modelling presence of parasites in cows

# (a) Perform some exploratory analysis of the database, including an analysis of the 
#     correlation between the variables of the environment of the farm
cow <- read.csv("/Users/lilu/Desktop/cows.csv",header=TRUE)
pair.cor<-cor(cbind(cow$temp,cow$rain,cow$permeab,cow$hight,cow$slope))
pair.cor
par(mfrow=c(3,1))
scatter.smooth(cow$temp~cow$rain,main="temp vs rain")
scatter.smooth(cow$temp~cow$hight,main="temp vs hight")
scatter.smooth(cow$permeab~cow$slope,main="permeab vs slope")

# (b) Fit a Bayesian hierarchical Bernoulli logistic model where the probability for each 
#     cow of having hairworm parasites is explained by their own covariates (age), 
#     the covariates of the environment of the farm (temperature, rain, permeability, 
#     height and slope) and a random effect on the farms themselves.
library(rjags)

cow.data <- list(n=nrow(cow),parasite=cow$parasite,age=cow$age,
                 temp=cow$temp,rain=cow$rain,permeab=cow$permeab,
                 hight=cow$hight,slope=cow$slope,farmID=cow$farmID,N=max(cow$farmID))
# Create initial values for JAGS
num.chains <- 3
# Create model block for JAGS
cow.model <- "model {

# hyperpriors for beta0
mu.beta0 ~ dnorm(0, 0.01)
tau.beta0 <- pow(sigma.beta0,-2)
sigma.beta0 ~ dunif(0, 20)

#priors
tau0    <- 0.00028  
beta1  ~ dnorm(0,tau0)
beta2  ~ dnorm(0,tau0)
beta3  ~ dnorm(0,tau0)
beta4  ~ dnorm(0,tau0)
beta5  ~ dnorm(0,tau0)
beta6  ~ dnorm(0,tau0)
#independent priors for intercepts per farm

for(j in 1:N){
beta0[j] ~ dnorm(mu.beta0,tau.beta0)
}
#Likelihood 
for(i in 1:n) {
logit(mu[i])   <- beta0[farmID[i]]+beta1*(age[i]-mean(age[]))/sd(age[])+
         beta2*(temp[i]-mean(temp[]))/sd(temp[])+
         beta3*(rain[i]-mean(rain[]))/sd(rain[])+
         beta4*(permeab[i]-mean(permeab[]))/sd(permeab[])+
         beta5*(hight[i]-mean(hight[]))/sd(hight[])+
         beta6*(slope[i]-mean(slope[]))/sd(slope[])
parasite[i]   ~ dbinom(mu[i],1) 
} 

estim[i]  <- beta0+beta1*(year[i]-mean(year[]))
log(mu[i]) <- estim.var[i]
estim.var[i] ~ dnorm(estim[i],tau.epsilon)

}"

# Run JAGS to the completion of the "adaption" stage
burnin           <- 5000
inference.length <- 50000
results.A <- jags.model(file=textConnection(cow.model),
                        data=cow.data,
                        n.chains=num.chains, quiet = TRUE)
update(results.A, n.iter=burnin)
results.B <- coda.samples(results.A,
                          variable.names=c("beta0","beta1","beta2","beta3",
                                           "beta4","beta5","beta6"),n.iter=inference.length)

# Summary 
summary(results.B)

# Check the chains
# Trace plots and density
plot(results.B) 
par(mfrow=c(2,2))
plot(results.B[,c("beta1","beta2")])
par(mfrow=c(2,2))
plot(results.B[,c("beta3","beta4")])
par(mfrow=c(2,2))
plot(results.B[,c("beta5","beta6")])
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.B)
gelman.diag(results.B)
# Effective sample size
effectiveSize(results.B[[1]])
effectiveSize(results.B[[1]][,"beta1"]) 
effectiveSize(results.B[[1]][,"beta2"]) 
effectiveSize(results.B[[1]][,"beta3"]) 
effectiveSize(results.B[[1]][,"beta4"]) 
effectiveSize(results.B[[1]][,"beta5"]) 
effectiveSize(results.B[[1]][,"beta6"]) 
# Autocorrelation plots
par(mfrow=c(3,3))
acf(results.B[[1]][,"beta0[1]"],main="Intercept")
acf(results.B[[1]][,"beta1"],main="Slope for age")
acf(results.B[[1]][,"beta2"],main="Slope for temp")
acf(results.B[[1]][,"beta3"],main="Slope for rain")
acf(results.B[[1]][,"beta4"],main="Slope for permeab")
acf(results.B[[1]][,"beta5"],main="Slope for hight")
acf(results.B[[1]][,"beta6"],main="Slope for slope")


# (c) Selecting a simpler model
# ************************without temp**********************************

library(rjags)

cow.data <- list(n=nrow(cow),parasite=cow$parasite,age=cow$age,rain=cow$rain,
                 permeab=cow$permeab,hight=cow$hight,slope=cow$slope,
                 farmID=cow$farmID,N=max(cow$farmID))
# Create initial values for JAGS
num.chains <- 3
# Create model block for JAGS
cow.model_no_temp <- "model {
# hyperpriors for beta0
mu.beta0 ~ dnorm(0, 0.01)
tau.beta0 <- pow(sigma.beta0,-2)
sigma.beta0 ~ dunif(0, 20)

#priors
tau0    <- 0.00028  
beta1  ~ dnorm(0,tau0)
beta2  ~ dnorm(0,tau0)
beta3  ~ dnorm(0,tau0)
beta4  ~ dnorm(0,tau0)
beta5  ~ dnorm(0,tau0)

#independent priors for intercepts per farm
for(j in 1:N){
beta0[j] ~ dnorm(mu.beta0,tau.beta0)
}

#Likelihood 
for(i in 1:n) {
logit(mu[i])   <- beta0[farmID[i]]+beta1*(age[i]-mean(age[]))/sd(age[])+
            beta2*(rain[i]-mean(rain[]))/sd(rain[])+
            beta3*(permeab[i]-mean(permeab[]))/sd(permeab[])+
            beta4*(hight[i]-mean(hight[]))/sd(hight[])+
            beta5*(slope[i]-mean(slope[]))/sd(slope[])
parasite[i]   ~ dbinom(mu[i],1)  
} 
}"

# Run JAGS to the completion of the "adaption" stage
burnin           <- 5000
inference.length <- 50000
results.A_no_temp <- jags.model(file=textConnection(cow.model_no_temp),
                        data=cow.data,
                        n.chains=num.chains, quiet = TRUE)
update(results.A_no_temp, n.iter=burnin)
results.B_no_temp <- coda.samples(results.A_no_temp,
                          variable.names=c("beta0","beta1","beta2","beta3","beta4","beta5"),
                          n.iter=inference.length)

summary(results.B_no_temp)
effectiveSize(results.B_no_temp[[1]][,"beta1"]) 
effectiveSize(results.B_no_temp[[1]][,"beta2"]) 
effectiveSize(results.B_no_temp[[1]][,"beta3"]) 
effectiveSize(results.B_no_temp[[1]][,"beta4"]) 
effectiveSize(results.B_no_temp[[1]][,"beta5"]) 

results.B.DIC <- dic.samples(model=results.A, n.iter=10000, type="pD")
results.B.DIC_no_temp <- dic.samples(model=results.A_no_temp, n.iter=10000, type="pD")
diffdic( results.B.DIC, results.B.DIC_no_temp)
## Difference: -0.8190394
## Sample standard error: 2.248158
# Hence, the first model is better

# ************************without rain**********************************
library(rjags)

cow.data <- list(n=nrow(cow),parasite=cow$parasite,age=cow$age,temp=cow$temp,
                 permeab=cow$permeab,hight=cow$hight,slope=cow$slope,
                 farmID=cow$farmID,N=max(cow$farmID))
# Create initial values for JAGS
num.chains <- 3
# Create model block for JAGS
cow.model_no_rain <- "model {
# hyperpriors for beta0
mu.beta0 ~ dnorm(0, 0.01)
tau.beta0 <- pow(sigma.beta0,-2)
sigma.beta0 ~ dunif(0, 20)

#priors
tau0    <- 0.00028  
beta1  ~ dnorm(0,tau0)
beta2  ~ dnorm(0,tau0)
beta3  ~ dnorm(0,tau0)
beta4  ~ dnorm(0,tau0)
beta5  ~ dnorm(0,tau0)

#independent priors for intercepts per farm
for(j in 1:N){
beta0[j] ~ dnorm(mu.beta0,tau.beta0)
}
#Likelihood 
for(i in 1:n) {
logit(mu[i])   <- beta0[farmID[i]]+beta1*(age[i]-mean(age[]))/sd(age[])+
                  beta2*(temp[i]-mean(temp[]))/sd(temp[])+
                  beta3*(permeab[i]-mean(permeab[]))/sd(permeab[])+
                  beta4*(hight[i]-mean(hight[]))/sd(hight[])+
                  beta5*(slope[i]-mean(slope[]))/sd(slope[])
parasite[i]   ~ dbinom(mu[i],1)  
} 
}"

# Run JAGS to the completion of the "adaption" stage
burnin           <- 5000
inference.length <- 50000
results.A_no_rain <- jags.model(file=textConnection(cow.model_no_rain),
                                data=cow.data,
                                n.chains=num.chains, quiet = TRUE)
update(results.A_no_rain, n.iter=burnin)
results.B_no_rain <- coda.samples(results.A_no_rain,
                                  variable.names=c("beta0","beta1","beta2",
                                                   "beta3","beta4","beta5"),
                                  n.iter=inference.length)

summary(results.B_no_rain)
effectiveSize(results.B_no_rain[[1]][,"beta1"]) 
effectiveSize(results.B_no_rain[[1]][,"beta2"]) 
effectiveSize(results.B_no_rain[[1]][,"beta3"]) 
effectiveSize(results.B_no_rain[[1]][,"beta4"]) 
effectiveSize(results.B_no_rain[[1]][,"beta5"]) 

results.B.DIC <- dic.samples(model=results.A, n.iter=10000, type="pD")
results.B.DIC_no_rain <- dic.samples(model=results.A_no_rain, n.iter=10000, type="pD")
diffdic(results.B.DIC, results.B.DIC_no_rain)

## Difference: -1.601041
## Sample standard error: 2.89821
# Hence, the first model is better

# ************************without permeab, slope and temp**********************************

library(rjags)

cow.data <- list(n=nrow(cow),parasite=cow$parasite,age=cow$age,
                 rain=cow$rain,hight=cow$hight,farmID=cow$farmID,N=max(cow$farmID))
# Create initial values for JAGS
num.chains <- 3
# Create model block for JAGS
cow.model_no_permeab_slope_temp <- "model {
#priors
tau0    <- 0.00028  
beta1  ~ dnorm(0,tau0)
beta2  ~ dnorm(0,tau0)
beta3  ~ dnorm(0,tau0)
#independent priors for intercepts per farm

for(j in 1:N){
beta0[j] ~ dnorm(mu.beta0,tau.beta0)
}
#Likelihood 
for(i in 1:n) {
logit(mu[i])   <- beta0[farmID[i]]+beta1*(age[i]-mean(age[]))/sd(age[])+
                  beta2*(rain[i]-mean(rain[]))/sd(rain[])
                  +beta3*(hight[i]-mean(hight[]))/sd(hight[])
parasite[i]   ~ dbinom(mu[i],1)  
} 


# hyperpriors for beta0
mu.beta0 ~ dnorm(0, 0.01)
tau.beta0 <- pow(sigma.beta0,-2)
sigma.beta0 ~ dunif(0, 20)
}"

# Run JAGS to the completion of the "adaption" stage
burnin           <- 5000
inference.length <- 50000
results.A_no_permeab_slope_temp <- jags.model(file=textConnection(cow.model_no_permeab_slope_temp),
                                data=cow.data,
                                n.chains=num.chains, quiet = TRUE)
update(results.A_no_permeab_slope_temp, n.iter=burnin)
results.B_no_permeab_slope_temp <- coda.samples(results.A_no_permeab_slope_temp,
                                  variable.names=c("beta0","beta1","beta2","beta3"),
                                  n.iter=inference.length)
summary(results.B_no_permeab_slope_temp)
results.B.DIC <- dic.samples(model=results.A, n.iter=10000, type="pD")
results.B.DIC_no_permeab_slope_temp <- dic.samples(model=results.A_no_permeab_slope_temp,
                                                   n.iter=10000, type="pD")
diffdic(results.B.DIC, results.B.DIC_no_permeab_slope_temp)
## Difference:  0.3653532
## Sample standard error: 3.409905
# Hence, the second model is better

# Check the chains
# Trace plots and density
par(mfrow=c(2,2))
plot(results.B_no_permeab_slope_temp[,c("beta0[1]","beta1")])
par(mfrow=c(2,2))
plot(results.B_no_permeab_slope_temp[,c("beta2","beta3")])
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.B_no_permeab_slope_temp[,c("beta0[1]","beta1","beta2","beta3")])
gelman.diag(results.B_no_permeab_slope_temp)
# Efective sample size
effectiveSize(results.B_no_permeab_slope_temp[[1]])
effectiveSize(results.B_no_permeab_slope_temp[[1]][,"beta1"]) 
effectiveSize(results.B_no_permeab_slope_temp[[1]][,"beta2"]) 
effectiveSize(results.B_no_permeab_slope_temp[[1]][,"beta3"]) 
# Autocorrelation plots
par(mfrow=c(2,2))
acf(results.B_no_permeab_slope_temp[[1]][,"beta0[1]"],main="Intercept")
acf(results.B_no_permeab_slope_temp[[1]][,"beta1"],main="Slope for age")
acf(results.B_no_permeab_slope_temp[[1]][,"beta2"],main="Slope for rain")
acf(results.B_no_permeab_slope_temp[[1]][,"beta3"],main="Slope for hight")

# (d) A farm is declared in epidemic state if the proportion of cows with the parasite in that farm is larger than 20%
# calculate the probability for farm 1 and farm 6 to be in the epidemic state
library(runjags)
cow.fit<-as.data.frame(combine.mcmc(results.B_no_permeab_slope_temp))
invlogit<-function(x){1/(1+exp(-x))}
cow.farm1<- invlogit(cow.fit[,1]+cow.fit$beta2*t((cow$rain-mean(cow$rain))/sd(cow$rain))[1]+
                       cow.fit$beta3*t((cow$hight-mean(cow$hight))/sd(cow$hight))[1])
cow.p.farm1<-rbinom(n=150000,size=36,prob=cow.farm1)
mean(cow.p.farm1)/36
quantile(cow.p.farm1,c(0.025,0.975))/36
mean(cow.p.farm1>7)

cow.farm6<- invlogit(cow.fit[,6]+cow.fit$beta2*t((cow$rain-mean(cow$rain))/sd(cow$rain))[141]+
                       cow.fit$beta3*t((cow$hight-mean(cow$hight))/sd(cow$hight))[141])
cow.p.farm6<-rbinom(n=150000,size=4,prob=cow.farm6)
mean(cow.p.farm6)/4
quantile(cow.p.farm6,c(0.025,0.975))/4
mean(cow.p.farm6>0)

# (e) Use hierarchical model this time and compare the results with the previous fixed effect model in part (d)
library(rjags)

cow.data <- list(n=nrow(cow),parasite=cow$parasite,age=cow$age,rain=cow$rain,hight=cow$hight)
# Create initial values for JAGS
num.chains <- 3
# Create model block for JAGS
cow.model_fix_no_permeab_slope_temp<- "model {
#priors
tau0    <- 0.00028  
beta0  ~ dnorm(0,tau0)
beta1  ~ dnorm(0,tau0)
beta2  ~ dnorm(0,tau0)
beta3  ~ dnorm(0,tau0)

#Likelihood 
for(i in 1:n) {
logit(mu[i])   <- beta0+beta1*(age[i]-mean(age[]))/sd(age[])+
beta2*(rain[i]-mean(rain[]))/sd(rain[])+
beta3*(hight[i]-mean(hight[]))/sd(hight[])
parasite[i]   ~ dbinom(mu[i],1) 
} 

# hyperpriors for beta0
mu.beta0 ~ dnorm(0, 0.01)
tau.beta0 <- pow(sigma.beta0,-2)
sigma.beta0 ~ dunif(0, 20)
}"

# Run JAGS to the completion of the "adaption" stage
burnin           <- 5000
inference.length <- 50000
results.A_fix_no_permeab_slope_temp <- jags.model(file=
                                                    textConnection(cow.model_fix_no_permeab_slope_temp),
                        data=cow.data,
                        n.chains=num.chains, quiet = TRUE)
update(results.A_fix_no_permeab_slope_temp, n.iter=burnin)
results.B_fix_no_permeab_slope_temp <- coda.samples(results.A_fix_no_permeab_slope_temp,
                          variable.names=c("beta0","beta1","beta2","beta3"),n.iter=inference.length)

# Summary 
summary(results.B_fix_no_permeab_slope_temp)

## Check the chains
# Trace plots and density
plot(results.B_fix_no_permeab_slope_temp[,c("beta1")])
par(mfrow=c(2,2))
plot(results.B_fix_no_permeab_slope_temp[,c("beta2","beta3")])
# Brooks-Gelman-Rubin statistic (want a value near 1)
gelman.plot(results.B_fix_no_permeab_slope_temp)
gelman.diag(results.B_fix_no_permeab_slope_temp)
# Effective sample size
effectiveSize(results.B_fix_no_permeab_slope_temp[[1]])
effectiveSize(results.B_fix_no_permeab_slope_temp[[1]][,"beta1"]) 
effectiveSize(results.B_fix_no_permeab_slope_temp[[1]][,"beta2"]) 
effectiveSize(results.B_fix_no_permeab_slope_temp[[1]][,"beta3"]) 
# Autocorrelation plots
par(mfrow=c(3,1))
acf(results.B_fix_no_permeab_slope_temp[[1]][,"beta1"],main="Slope for age")
acf(results.B_fix_no_permeab_slope_temp[[1]][,"beta2"],main="Slope for rain")
acf(results.B_fix_no_permeab_slope_temp[[1]][,"beta3"],main="Slope for hight")

# compare DIC
results.B.DIC_no_permeab_slope_temp <- dic.samples(model=results.A_no_permeab_slope_temp, 
                                                   n.iter=10000, type="pD")
results.B.DIC_fix_no_permeab_slope_temp <- dic.samples(model=results.A_fix_no_permeab_slope_temp, 
                                                       n.iter=10000, type="pD")
diffdic(results.B.DIC_no_permeab_slope_temp, results.B.DIC_fix_no_permeab_slope_temp)


library(runjags)
cow.fit<-as.data.frame(combine.mcmc(results.B_fix_no_permeab_slope_temp))
invlogit<-function(x){1/(1+exp(-x))}
cow.farm1<- invlogit(cow.fit[,1]+cow.fit$beta2*t((cow$rain-mean(cow$rain))/sd(cow$rain))[1]+
                       cow.fit$beta3*t((cow$hight-mean(cow$hight))/sd(cow$hight))[1])
cow.p.farm1<-rbinom(n=150000,size=36,prob=cow.farm1)
mean(cow.p.farm1)/36
quantile(cow.p.farm1,c(0.025,0.975))/36
mean(cow.p.farm1>7)

cow.farm6<- invlogit(cow.fit[,1]+cow.fit$beta2*t((cow$rain-mean(cow$rain))/sd(cow$rain))[141]+
                       cow.fit$beta3*t((cow$hight-mean(cow$hight))/sd(cow$hight))[141])
cow.p.farm6<-rbinom(n=150000,size=4,prob=cow.farm6)
mean(cow.p.farm6)/4
quantile(cow.p.farm6,c(0.025,0.975))/4
mean(cow.p.farm6>0)

