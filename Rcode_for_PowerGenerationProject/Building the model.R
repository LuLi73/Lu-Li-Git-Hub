# Date: 2 Aug 2019
# Task: 1. Build and check the model to predict power output using river flow
#       2. Find environmental factors that influence the scheme
#       3. Find other viable locations of the scheme


# From the exploratory data analysis, we know that the relationship between the flow 
# and power generation is approximately log-linear. But we are not sure if log-linear 
# is really the best. We therefore do the following model testing.

# Model 1 linear
fit <- lm(merge_gen_flow$t1_avg_power~merge_gen_flow$flow)
summary(fit)

# Model 2 square model
fit_squar <- lm((merge_gen_flow$t1_avg_power)^2~merge_gen_flow$flow)
summary(fit_squar)

# Model 3 square root model
fit_squarroot <- lm((merge_gen_flow$t1_avg_power)^0.5~merge_gen_flow$flow)
summary(fit_squarroot)

# Model 4 log linear model
log2_fit <- lm(log(merge_gen_flow$t1_avg_power)~merge_gen_flow$flow)
summary(log2_fit)

# Model 5 interaction model
inter_model <- lm(merge_gen_flow_level$t1_avg_power~
                    (merge_gen_flow_level$flow)*(merge_gen_flow_level$min_level))
summary(inter_model)

# Check AIC of these models
AIC(fit, fit_squar, fit_squarroot,log2_fit,inter_model)

# Load the package
install.packages("stargazer")
library("stargazer")

# Present the result of these models 
stargazer(fit , fit_squar, fit_squarroot, log2_fit, title="Results", align=TRUE)
stargazer(inter_model)

# Merge power generation, flow and level data
setDT(level)
level$date <- as.Date(level$date)
merge_gen_flow_level  <- merge(merge_gen_flow,level,by="date",allow.cartesian=T)

# Checking the relationship between power generation and other variables
# Plot average river level vs power generation
j <- ggplot(merge_gen_flow_level, aes(x=merge_gen_flow_level$avg_level, 
                                      y= merge_gen_flow_level$t1_avg_power)) + 
  geom_point(color="blue")+
  geom_smooth(linetype="dashed",color="red")+
  labs(title="Average River Level vs Generation Power",
       x="Average River Level", y = "Generation Power")

# Plot minimum river level vs power generation
jj <- ggplot(merge_gen_flow_level, aes(x=merge_gen_flow_level$min_level, 
                                       y= merge_gen_flow_level$t1_avg_power)) + 
  geom_point(color="blue")+
  geom_smooth(linetype="dashed",color="red")+
  labs(title="Minimum River Level vs Generation Power",
       x="Minimum River Level", y = "Generation Power")

# Plot maximum river level vs power generation
jjj <- ggplot(merge_gen_flow_level, aes(x=merge_gen_flow_level$max_level, 
                                        y= merge_gen_flow_level$t1_avg_power)) + 
  geom_point(color="blue")+
  geom_smooth(linetype="dashed",color="red")+
  labs(title="Maximum River Level vs Generation Power",
       x="Maximum River Level", y = "Generation Power")

# Combine the above pictures
library(gridExtra)
grid.arrange(j,jj,jjj,
             ncol = 1)

##################### Environmental Factors 
# Find max level of river of these years
max_of_min <- max(merge_gen_flow_level$min_level, na.rm = TRUE) #1.533
max_of_avg <- max(merge_gen_flow_level$avg_level, na.rm = TRUE)  #1.746
max_of_max <- max(merge_gen_flow_level$max_level, na.rm = TRUE)  #1.913
merge_gen_flow_level[merge_gen_flow_level$min_level == max_of_min, ]
merge_gen_flow_level[merge_gen_flow_level$avg_level == max_of_avg, ]
merge_gen_flow_level[merge_gen_flow_level$max_level == max_of_max, ]

# Find average, maximum and minimum level 
avg <- mean(level$avg_level, na.rm = TRUE)  # 1.095024
max <- max(level$max_level, na.rm = TRUE)  #2.848
min <- min(level$min_level, na.rm = TRUE)  #0.784

########################### Find other viable locations
# Import information of river levels of stations in Scotland
RiverScot <- read.csv('/Users/lilu/Desktop/dissertation 2/data/SEPA_River_Levels_Web.csv')

# Specify the conditions which limit the differences in 30%
df1 <- RiverScot[avg*0.7 <= RiverScot$MEAN & RiverScot$MEAN<= avg*1.3, ]   
df2 <- RiverScot[min*0.7 <= RiverScot$LOWEST_VALUE & RiverScot$MEAN<= min*1.3, ] 
df3<-RiverScot[max*0.7 <= RiverScot$MAX_VALUE & RiverScot$MEAN<= max*1.3, ] 

# Load the package
library(dplyr)

# inner join the information and find the locations we want
newjoin <- inner_join(df1, df2)
ne2<-inner_join(df2, df3)
innjoin <- inner_join(newjoin, ne2)
innjoin$NATIONAL_GRID_REFERENCE <-NULL
innjoin$CATCHMENT_AREA <- NULL
innjoin$CATCHMENT_NAME <- NULL
innjoin$GAUGE_DATUM <- NULL
innjoin$SYSTEM_ID<-NULL
innjoin$WEB_MESSAGE <- NULL
innjoin$NRFA_LINK <- NULL
innjoin$LOW<-NULL
innjoin$HIGH<-NULL
innjoin$MAX_DISPLAY<-NULL
colnames(innjoin) <- c("District", "Station", "Location Code","River Name","Start Date","End Date","Minimum","Highest","Average","Unit")
innjoin$`End Date` <- c("2019-08-02","2019-08-02","2019-08-02","2019-08-02","2019-08-02")

# Install and load the package
install.packages("Hmisc")
library(Hmisc)

# Transfer table to latex form
latex(innjoin, file="")   