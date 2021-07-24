# Date: 20 June 2019
# Task: 1. Perform Kruskal-Wallis Test on nine items in each CET 
#          with age, education and gender respectively
#       2. Conduct multiple group estimation to detect DIF

##### CET A #####

### age
# obtain and clear data of age and 9 CET-A items
l_new_with_age <- cet_data[c(2,8:16)]
l_new_age_2 <- na.omit(l_new_with_age)
attach(l_new_age_2)

# conduct kruskal test
Agegroup=ifelse(Age>=60,1,0)
kruskal.test(CET.A.Q1~as.factor(Agegroup)) # p-value = 0.486
kruskal.test(CET.A.Q2~as.factor(Agegroup)) # p-value = 0.238
kruskal.test(CET.A.Q3~as.factor(Agegroup)) # p-value = 0.287
kruskal.test(CET.A.Q4~as.factor(Agegroup)) # p-value = 0.124
kruskal.test(CET.A.Q5~as.factor(Agegroup)) # p-value = 0.911
kruskal.test(CET.A.Q6~as.factor(Agegroup)) # p-value = 0.070
kruskal.test(CET.A.Q7~as.factor(Agegroup)) # p-value = 0.153
kruskal.test(CET.A.Q8~as.factor(Agegroup)) # p-value = 0.366
kruskal.test(CET.A.Q9~as.factor(Agegroup)) # p-value = 4.763e-05

# install and load the package
install.packages("mirt")
library("mirt")

# Multiple Group Estimation
models <- 'F1 = 1-9'
Agegroup_char<-ifelse(Agegroup==1,"old","young")
mod_configural <- multipleGroup(l_new_age_2[,2:10], models, 
                                group = as.factor(Agegroup_char))
par(mfrow=c(2,1))

# plot the result
plot(mod_configural)
plot(mod_configural, type = 'itemscore')

library(gridExtra)
grid.arrange(plot(mod_configural), plot(mod_configural, type = 'itemscore'),
             ncol = 2)

### education
# obtain and clear data of education and 9 CET-A items
l_new_with_edu <- cet_data[c(3,8:16)]
l_new_edu_2 <- na.omit(l_new_with_edu)
attach(l_new_edu_2)

# conduct Kruskal Test
Edugroup=ifelse(Years.of.Edu>=13,1,0)
kruskal.test(CET.A.Q1~as.factor(Edugroup)) # p-value = 0.523
kruskal.test(CET.A.Q2~as.factor(Edugroup)) # p-value = 0.102
kruskal.test(CET.A.Q3~as.factor(Edugroup)) # p-value = 0.580
kruskal.test(CET.A.Q4~as.factor(Edugroup)) # p-value = 0.014
kruskal.test(CET.A.Q5~as.factor(Edugroup)) # p-value = 0.655
kruskal.test(CET.A.Q6~as.factor(Edugroup)) # p-value = 0.681
kruskal.test(CET.A.Q7~as.factor(Edugroup)) # p-value = 0.711
kruskal.test(CET.A.Q8~as.factor(Edugroup)) # p-value = 0.136
kruskal.test(CET.A.Q9~as.factor(Edugroup)) # p-value = 0.004

# Multiple Group Estimation
models <- 'F1 = 1-9'
Edugroup_char<-ifelse(Edugroup==1,"High Education","Low Education")
mod_configural_edu <- multipleGroup(l_new_edu_2[,2:10], models, 
                                    group = as.factor(Edugroup_char))

# plot the result
plot(mod_configural_edu)
plot(mod_configural_edu, type = 'itemscore')

grid.arrange(plot(mod_configural_edu), 
             plot(mod_configural_edu, type = 'itemscore'),
             ncol = 2)

### gender
# obtain and clear data of gender and 9 CET-A items
l_new_with_gen <- cet_data[c(4,8:16)]
l_new_gen_2 <- na.omit(l_new_with_gen)
attach(l_new_gen_2)

# conduct Kruskal Test
kruskal.test(CET.A.Q1~as.factor(Gender)) # p-value = 0.003
kruskal.test(CET.A.Q2~as.factor(Gender)) # p-value = 0.495
kruskal.test(CET.A.Q3~as.factor(Gender)) # p-value = 0.033
kruskal.test(CET.A.Q4~as.factor(Gender)) # p-value = 0.33
kruskal.test(CET.A.Q5~as.factor(Gender)) # p-value = 0.886
kruskal.test(CET.A.Q6~as.factor(Gender)) # p-value = 0.032
kruskal.test(CET.A.Q7~as.factor(Gender)) # p-value = 0.032
kruskal.test(CET.A.Q8~as.factor(Gender)) # p-value = 0.848
kruskal.test(CET.A.Q9~as.factor(Gender)) # p-value = 0.293

# Multiple Group Estimation
models <- 'F1 = 1-9'
Gengroup_char<-ifelse(Gender==1,"Male","Female")
mod_configural_gen <- multipleGroup(l_new_gen_2[,2:10], models, 
                                    group = as.factor(Gengroup_char))

# plot the result
plot(mod_configural_gen)
plot(mod_configural_gen, type = 'itemscore')

grid.arrange(plot(mod_configural_gen), 
             plot(mod_configural_gen, type = 'itemscore'),
             ncol = 2)

##### CET B #####
### age
# obtain and clear data of age and 9 CET-B items
l_new_with_age_B <- cet_data[c(2,17:25)]
l_new_age_2_B <- na.omit(l_new_with_age_B)
attach(l_new_age_2_B)

# conduct Kruskal Test
Agegroup_B=ifelse(Age>=60,1,0)
kruskal.test(CET.B.Q1~as.factor(Agegroup_B)) # p-value = 0.727
kruskal.test(CET.B.Q2~as.factor(Agegroup_B)) # p-value = 0.186
kruskal.test(CET.B.Q3~as.factor(Agegroup_B)) # p-value = 1
kruskal.test(CET.B.Q4~as.factor(Agegroup_B)) # p-value = 0.991
kruskal.test(CET.B.Q5~as.factor(Agegroup_B)) # p-value = 0.652
kruskal.test(CET.B.Q6~as.factor(Agegroup_B)) # p-value = 0.869
kruskal.test(CET.B.Q7~as.factor(Agegroup_B)) # p-value = 0.806
kruskal.test(CET.B.Q8~as.factor(Agegroup_B)) # p-value = 0.306
kruskal.test(CET.B.Q9~as.factor(Agegroup_B)) # p-value = 0.294

# Multiple Group Estimation
models <- 'F1 = 1-9'
Agegroup_char_B<-ifelse(Agegroup_B==1,"old","young")
mod_configural_B <- multipleGroup(l_new_age_2_B[,2:10], models, 
                                  group = as.factor(Agegroup_char_B))

# plot the result
plot(mod_configural_B)
plot(mod_configural_B, type = 'itemscore')

grid.arrange(plot(mod_configural_B), plot(mod_configural_B, type = 'itemscore'),
             ncol = 2)

### education
# obtain and clear data of education and 9 CET-B items
l_new_with_edu_B <- cet_data[c(3,17:25)]
l_new_edu_2_B <- na.omit(l_new_with_edu_B)
attach(l_new_edu_2_B)

# conduct Kruskal Test
Edugroup_B=ifelse(Years.of.Edu>=13,1,0)
kruskal.test(CET.B.Q1~as.factor(Edugroup_B)) # p-value = 0.520
kruskal.test(CET.B.Q2~as.factor(Edugroup_B)) # p-value = 0.284
kruskal.test(CET.B.Q3~as.factor(Edugroup_B)) # p-value = 0.975
kruskal.test(CET.B.Q4~as.factor(Edugroup_B)) # p-value = 0.709
kruskal.test(CET.B.Q5~as.factor(Edugroup_B)) # p-value = 0.412
kruskal.test(CET.B.Q6~as.factor(Edugroup_B)) # p-value = 0.981
kruskal.test(CET.B.Q7~as.factor(Edugroup_B)) # p-value = 0.262
kruskal.test(CET.B.Q8~as.factor(Edugroup_B)) # p-value = 0.800
kruskal.test(CET.B.Q9~as.factor(Edugroup_B)) # p-value = 0.327

# Multiple Group Estimation
models <- 'F1 = 1-9'
Edugroup_char_B<-ifelse(Edugroup_B==1,"High Education","Low Education")
mod_configural_edu_B <- multipleGroup(l_new_edu_2_B[,2:10], models,
                                      group = as.factor(Edugroup_char_B))

# plot the result
plot(mod_configural_edu_B)
plot(mod_configural_edu_B, type = 'itemscore')

grid.arrange(plot(mod_configural_edu_B), plot(mod_configural_edu_B,
                                              type = 'itemscore'),
             ncol = 2)

### gender
# obtain and clear data of gender and 9 CET-B items
l_new_with_gen_B <- cet_data[c(4,17:25)]
l_new_gen_2_B <- na.omit(l_new_with_gen_B)
attach(l_new_gen_2_B)

# conduct Kruskal Test
kruskal.test(CET.B.Q1~as.factor(Gender)) # p-value = 0.049
kruskal.test(CET.B.Q2~as.factor(Gender)) # p-value = 0.006
kruskal.test(CET.B.Q3~as.factor(Gender)) # p-value = 0.122
kruskal.test(CET.B.Q4~as.factor(Gender)) # p-value = 0.247
kruskal.test(CET.B.Q5~as.factor(Gender)) # p-value = 0.371
kruskal.test(CET.B.Q6~as.factor(Gender)) # p-value = 0.353
kruskal.test(CET.B.Q7~as.factor(Gender)) # p-value = 0.256
kruskal.test(CET.B.Q8~as.factor(Gender)) # p-value = 0.400
kruskal.test(CET.B.Q9~as.factor(Gender)) # p-value = 0.150

# multiple group estimation
models <- 'F1 = 1-9'
Gengroup_char_B<-ifelse(Gender==1,"Male","Female")
mod_configural_gen_B <- multipleGroup(l_new_gen_2_B[,2:10], 
                                      models, 
                                      group = as.factor(Gengroup_char_B))

# plot the result
plot(mod_configural_gen_B)
plot(mod_configural_gen_B, type = 'itemscore')

grid.arrange(plot(mod_configural_gen_B), 
             plot(mod_configural_gen_B, type = 'itemscore'),
             ncol = 2)

# Results show that age, gender and years of education did have effects on their performances on the CET.
#We should control for such confounding effects before performing further analysis.


