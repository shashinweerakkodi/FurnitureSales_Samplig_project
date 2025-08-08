## Simple Random Sampling 

# Load required packages
library(survey)
library(sampler)

# Load the Furniture dataset (assuming it is already loaded)
attach(Furniture)

# Set a seed for reproducibility
set.seed(123)

# Sample size calculation for demonstration (you can adjust based on your needs)
a = rsampcalc(nrow(Furniture), e = 3, ci = 95, p = 0.5)
a

# Create a survey design object using Simple Random Sampling (SRS)
srs_design <- svydesign(id = ~1, data = Furniture)

# Select two samples using SRS design
srs_sample1 <- svydesign(id = ~1, data = Furniture[sample(nrow(Furniture), a), ])
srs_sample2 <- svydesign(id = ~1, data = Furniture[sample(nrow(Furniture), a), ])

# Mean of Quantitative Variables with standard error
means_se <- svymean(~price + cost, srs_design, se = TRUE)
means_se

# Total of Quantitative Variables with standard error
totals_se <- svytotal(~price + cost, srs_design, se = TRUE)
totals_se

# Proportion Tables for Qualitative Variables with standard error
proportions_with_se <- svymean(~factor(location) + factor(store_type), srs_design, na.rm = TRUE)
proportions_with_se 

# Sample 1 Summary Statistics: Mean and Total with standard error
means_se_sample1 <- svymean(~price + cost, srs_sample1, se = TRUE)
totals_se_sample1 <- svytotal(~price + cost, srs_sample1, se = TRUE)
means_se_sample1
totals_se_sample1

# Sample 2 Summary Statistics: Mean and Total with standard error
means_se_sample2 <- svymean(~price + cost, srs_sample2, se = TRUE)
totals_se_sample2 <- svytotal(~price + cost, srs_sample2, se = TRUE)
means_se_sample2
totals_se_sample2

# Calculate proportions and standard errors for store_type in Sample 1
proportions_with_se_store_type_sample1 <- svymean(~factor(store_type), srs_sample1, na.rm = TRUE)
proportions_with_se_store_type_sample1
proportions_with_se_location_sample1 <- svymean(~factor(location), srs_sample1, na.rm = TRUE)
proportions_with_se_location_sample1

# Calculate proportions and standard errors for store_type in Sample 2
proportions_with_se_store_type_sample2 <- svymean(~factor(store_type), srs_sample2, na.rm = TRUE)
proportions_with_se_store_type_sample2
proportions_with_se_location_sample2 <- svymean(~factor(location), srs_sample2, na.rm = TRUE)
proportions_with_se_location_sample2 

# Correlation Coefficient between cost and price in samples
cor_sample1 <- cor(srs_sample1$variables$price, srs_sample1$variables$cost, use = "complete.obs")
cor_sample1
cor_sample2 <- cor(srs_sample2$variables$price, srs_sample2$variables$cost, use = "complete.obs")
cor_sample2

# Regression Analysis for Sample 1
plot(srs_sample1$variables$cost, srs_sample1$variables$price, main = "Cost vs Price (Sample 1)", xlab = "Cost", ylab = "Price")
model1 <- lm(price ~ cost, data = srs_sample1$variables)
model1

# Regression Analysis for Sample 2
plot(srs_sample2$variables$cost, srs_sample2$variables$price, main = "Cost vs Price (Sample 2)", xlab = "Cost", ylab = "Price")
model2 <- lm(price ~ cost, data = srs_sample2$variables)
model2

# Ratio Estimation for Sample 1
mean_Y1 <- as.numeric(svymean(~price, srs_sample1))
mean_X1 <- as.numeric(svymean(~cost, srs_sample1))
ratio_estimate1 <- mean_Y1 / mean_X1
ratio_estimate1

# Ratio Estimation for Sample 2
mean_Y2 <- as.numeric(svymean(~price, srs_sample2))
mean_X2 <- as.numeric(svymean(~cost, srs_sample2))
ratio_estimate2 <- mean_Y2 / mean_X2
ratio_estimate2

# Graphical Representation

# Boxplots for cost and price
boxplot(srs_sample1$variables$cost, srs_sample2$variables$cost, main = "Box plot of Cost (Sample 1 vs Sample 2)", xlab = "Sample", ylab = "Cost")
boxplot(srs_sample1$variables$price, srs_sample2$variables$price, main = "Box plot of Price (Sample 1 vs Sample 2)", xlab = "Sample", ylab = "Price")

# Histograms
hist(srs_sample1$variables$cost, main = "Histogram of Cost (Sample 1)", xlab = "Cost", ylab = "Frequency")
hist(srs_sample2$variables$cost, main = "Histogram of Cost (Sample 2)", xlab = "Cost", ylab = "Frequency")

# Pie chart 
pie(table(srs_sample1$variables$color), main = "Pie Chart of color of sample 1")
pie(table(srs_sample2$variables$color), main = "Pie Chart of color of sample 2")


## stratified Random Sampling

library(survey)
library(sampler)

attach(Furniture)

se_by_stratum <- tapply(Furniture$price,Furniture$cost, function(x) sd(x))
print(se_by_stratum)

unique(category)
unique(material)
unique(Furniture$color)
unique(Furniture$location)
unique(Furniture$season)
unique(Furniture$store_type)
unique(Furniture$brand)

#population parameters
pop_mean_cost=mean(cost)
pop_mean_cost

pop_mean_price=mean(price)
pop_mean_price

pop_total_cost=sum(cost)
pop_total_cost

pop_total_price=sum(price)
pop_total_price

pop_proportion_storetype=table(store_type)/length(store_type)
pop_proportion_storetype

pop_proportion_season = table(season)/length(season)
pop_proportion_season

pop_proportion_location = table(location)/length(location)
pop_proportion_location

## STRATIFIED RANDOM SAMPLING ##
## ESTIMATIONS sample 01
set.seed(1637)
Samp_size=rsampcalc(nrow(Furniture),e=3,ci=95,0.5) 
Samp_size

#variable is category for stratas
#sample size for stratified sampling

strata_size=ssampcalc(Furniture,Samp_size,category)
strata_size

## getting stratified sampling1
strat_samp1=ssamp(Furniture,Samp_size,category)
strat_samp1

category_freq <- table(strat_samp1$category)
print(category_freq)
#stratified
attach(strat_samp1)
# sample weight for Bed     = 481/144=3.34
# sample weight for Chair   = 497/149=3.34
# sample weight for Desk    = 501/150=3.34
# sample weight for Sofa    = 488/146=3.34
# sample weight for Table   = 533/159=3.34
strat_samp1$w=3.34

#Define survey design object
strat_design1=svydesign(id=~1,strata =category,data = strat_samp1,weights=~w)
summary(strat_design1)

#Sample mean for cost
strata_1_mean_for_cost=svymean(~cost,strat_design1)
strata_1_mean_for_cost

#Sample mean for price
strata_1_mean_for_price=svymean(~price,strat_design1)
strata_1_mean_for_price

#Sample total for cost
strata_1_total_for_cost=svytotal(~cost,strat_design1)
strata_1_total_for_cost

estimated_tot_cost = 2500*strata_1_mean_for_cost
estimated_tot_cost

#Sample total for price
strata_1_total_for_price=svytotal(~price,strat_design1)
strata_1_total_for_price

estimated_tot_price = 2500*strata_1_mean_for_price
estimated_tot_price

# sample proportions
sample_prop_storetype=svymean(~store_type,strat_design1)
sample_prop_storetype

sample_prop_season=svymean(~season,strat_design1)
sample_prop_season

sample_prop_location=svymean(~location,strat_design1)
sample_prop_location

#Ratio Estimation(Combined)
r1=svyratio(~strat_samp1$price,~strat_samp1$cost, strat_design1)
predict(r1,total=pop_total_cost)

#Estimated Total = 692028.3
Estimated_mean = 692028.3/2500
Estimated_mean
SE(r1)

#Regression estimation
#fitting linear regression model

plot(strat_samp1$cost,strat_samp1$price)
cor(strat_samp1$cost,strat_samp1$price)
lm1=lm(strat_samp1$price~strat_samp1$cost,strat_samp1)
lm1
summary(lm1)
anova(lm1)

# mean for cost in population=191.9301
#then calculate expected mean for price using regression model

mean_price= 36.30445+ 1.25756*191.9301
mean_price

Estimated_Total=2500*mean_price
Estimated_Total

##############################################
#ESTIMATIONS sample 2
set.seed(3761)

Samp_size=rsampcalc(nrow(Furniture),e=3,ci=95,0.5) 
Samp_size

#variable is category for stratas
#sample size for stratified sampling

strata_size=ssampcalc(Furniture,Samp_size,category)
strata_size

## getting stratified sampling2
strat_samp2=ssamp(Furniture,Samp_size,category)
strat_samp2

category_freq <- table(strat_samp2$category)
print(category_freq)
#stratified
attach(strat_samp2)
# sample weight for Bed     = 481/144=3.34
# sample weight for Chair   = 497/149=3.34
# sample weight for Desk    = 501/150=3.34
# sample weight for Sofa    = 488/146=3.34
# sample weight for Table   = 533/159=3.34
strat_samp1$w=3.34

#Define survey design object
strat_design2=svydesign(id=~1,strata =category,data = strat_samp2,weights=~w)
summary(strat_design2)

#Sample mean for cost
strata_2_mean_for_cost=svymean(~cost,strat_design2)
strata_2_mean_for_cost

#Sample mean for price
strata_2_mean_for_price=svymean(~price,strat_design2)
strata_2_mean_for_price

#Sample total for cost
strata_2_total_for_cost=svytotal(~cost,strat_design2)
strata_2_total_for_cost

estimated_tot_cost = 2500*strata_2_mean_for_cost
estimated_tot_cost

#Sample total for price
strata_2_total_for_price=svytotal(~price,strat_design2)
strata_2_total_for_price

estimated_tot_price = 2500*strata_2_mean_for_price
estimated_tot_price

# sample proportions
sample_prop_storetype=svymean(~store_type,strat_design2)
sample_prop_storetype

sample_prop_season=svymean(~season,strat_design2)
sample_prop_season

sample_prop_location=svymean(~location,strat_design2)
sample_prop_location

#Ratio Estimation(Combined)
r2=svyratio(~strat_samp2$price,~strat_samp2$cost, strat_design2)
predict(r2,total=pop_total_cost)

#Estimated Total = 691839.5
Estimated_mean = 691839.5/2500
Estimated_mean
SE(r1)

#Regression estimation
#fitting linear regression model

plot(strat_samp2$cost,strat_samp2$price)
cor(strat_samp2$cost,strat_samp2$price)
lm2=lm(strat_samp2$price~strat_samp2$cost,strat_samp2)
lm2
summary(lm2)
anova(lm2)

# mean for cost in population=196.5638
#then calculate expected mean for price using regression model

mean_price= 38.7939+ 1.2415*196.5638
mean_price

Estimated_Total=2500*mean_price
Estimated_Total

#Graphical Analysis Using Sample 01

svyhist(~price,strat_design1, main="Histogram of Price", 
        col="blue",probability = FALSE)

svyboxplot(~price~store_type,strat_design1, 
           main="Boxplot of Price with store_type ", col="brown" )

svyboxplot(~price~season,strat_design1, 
           main="Boxplot of Price with season ", col="yellow" )

svyboxplot(~price~location,strat_design1, 
           main="Boxplot of Price with location ", col="cyan" )

svyplot(cost~price, design=strat_design1, style="bubble")

cor(cost,price)

## Two – Stage Cluster Sampling

attach(Furniture)
library(survey)
library(sampler)

###Population Parameters###
pop_mean_price=mean(price)
pop_mean_price

pop_mean_cost=mean(cost)
pop_mean_cost

pop_sum_price=sum(price)
pop_sum_price

pop_sum_cost=sum(cost)
pop_sum_cost

pop_proportion_storetype=table(store_type)/length(store_type)
pop_proportion_storetype

pop_proportion_location=table(location)/length(location)
pop_proportion_location

#clustering
N=length(unique(color))
N

color=table(color)
color

#we arbitrary select 4 clusters
n=4

#####################################################################################################
################# sample 1 ################# 

set.seed(2109) 
clusters1 = sample(x = unique(Furniture$color),size = n,replace = F) 
clusters1

#variables to save data
Cluster1 = c() 
Cluster1 
m=numeric(n) 
m 

ClusterSize = numeric(n) 
ClusterSize

for (i in 1:n){ 
  
  dat =  Furniture[Furniture$color==clusters1[i],] 
  ClusterSize[i] = nrow(dat) 
  
  #Selecting sample sizes for each cluster 
  m[i] = rsampcalc(N = nrow(dat),e = 3,ci = 95) 
  
  #selecting a sample from each cluster and saving it 
  Cluster1=rbind(Cluster1,rsamp(df = dat,n = m[i],rep = F)) 
}

ClusterDetails=data.frame(clusters1,ClusterSize,m) 
ClusterDetails

colnames(ClusterDetails) = c("color","Population Size","Sample Size") 
ClusterDetails 

Cluster1
unique(ClusterDetails$color)
unique(Cluster1$color)
ClusterDetails[ClusterDetails[1,]$color==Cluster1[1,]$color,]$Sample Size

#calculating sample weights
S_weight=numeric(nrow(Cluster1))

for (i in 1:nrow(Cluster1)){ 
  S_weight[i] =  (N*ClusterDetails[ClusterDetails$color==Cluster1[i,]$color,]$Population Size)/ 
    (n*ClusterDetails[ClusterDetails$color==Cluster1[i,]$color,]$Sample Size) 
}
S_weight

Cluster1=cbind(Cluster1,S_weight) 
Cluster1

#Survey Design 
Cluster_Design1 = svydesign(ids=~color, weights =~S_weight, data = Cluster1)

#ESTIMATION  

sample1_mean_for_price=svymean(~price,Cluster_Design1) 
sample1_mean_for_price

sample1_mean_for_cost=svymean(~cost,Cluster_Design1) 
sample1_mean_for_cost

sample1_tot_for_price=svytotal(~price,Cluster_Design1) 
sample1_tot_for_price

sample1_tot_for_cost=svytotal(~cost,Cluster_Design1) 
sample1_tot_for_cost

sample1_prop1=svymean(~store_type,Cluster_Design1) 
sample1_prop1

sample1_prop2=svymean(~location,Cluster_Design1) 
sample1_prop2

r1=svyratio(~Cluster1$price,~Cluster1$cost,Cluster_Design1) 
r1
predict(r1,total=pop_sum_cost) 
SE(r1)

#regression estimation
plot(Cluster1$cost,Cluster1$price, xlab="Cost",ylab="Price") 
cor(Cluster1$cost,Cluster1$price) 
lm1=lm(Cluster1$price~Cluster1$cost,Cluster1) 
lm1 
summary(lm1) 
anova(lm1)

#Population mean cost = 191.9301
#calculating expected mean value using regression model
mean_price = 34.92207 +  1.25957*191.9301
mean_price

#calculating estimated total 
estimated_tot=mean_price*2500
estimated_tot


#####################################################################################################
################# sample 2 ################# 
set.seed(555) 
clusters1 = sample(x = unique(Furniture$color),size = n,replace = F) 
clusters1

#variables to save data
Cluster1 = c() 
Cluster1 

m=numeric(n) 
m 

ClusterSize = numeric(n) 
ClusterSize

for (i in 1:n){ 
  
  dat =  Furniture[Furniture$color==clusters1[i],] 
  ClusterSize[i] = nrow(dat) 
  
  #Selecting sample sizes for each cluster 
  m[i] = rsampcalc(N = nrow(dat),e = 3,ci = 95) 
  
  #selecting a sample from each cluster and saving it 
  Cluster1=rbind(Cluster1,rsamp(df = dat,n = m[i],rep = F)) 
  
}

ClusterDetails=data.frame(clusters1,ClusterSize,m) 
ClusterDetails

colnames(ClusterDetails) = c("color","Population Size","Sample Size") 
ClusterDetails 

Cluster1
unique(ClusterDetails$color)
unique(Cluster1$color)
ClusterDetails[ClusterDetails[1,]$color==Cluster1[1,]$color,]$Sample Size

#calculating sample weights
S_weight=numeric(nrow(Cluster1))

for (i in 1:nrow(Cluster1)){ 
  S_weight[i] =  (N*ClusterDetails[ClusterDetails$color==Cluster1[i,]$color,]$Population Size)/ 
    (n*ClusterDetails[ClusterDetails$color==Cluster1[i,]$color,]$Sample Size) 
}
S_weight

Cluster1=cbind(Cluster1,S_weight) 
Cluster1

#Survey Design 
Cluster_Design1 = svydesign(ids=~color, weights =~S_weight, data = Cluster1)

#ESTIMATION  

sample1_mean_for_price=svymean(~price,Cluster_Design1) 
sample1_mean_for_price

sample1_mean_for_cost=svymean(~cost,Cluster_Design1) 
sample1_mean_for_cost

sample1_tot_for_price=svytotal(~price,Cluster_Design1) 
sample1_tot_for_price

sample1_tot_for_cost=svytotal(~cost,Cluster_Design1) 
sample1_tot_for_cost

sample1_prop1=svymean(~store_type,Cluster_Design1) 
sample1_prop1

sample1_prop2=svymean(~location,Cluster_Design1) 
sample1_prop2

r1=svyratio(~Cluster1$price,~Cluster1$cost,Cluster_Design1) 
predict(r1,total=pop_sum_cost) 
SE(r1)

#regression estimation
plot(Cluster1$cost,Cluster1$price) 
cor(Cluster1$cost,Cluster1$price) 
lm2=lm(Cluster1$price~Cluster1$cost,Cluster1) 
lm2 
summary(lm2) 
anova(lm2)

#Population mean cost = 191.9301
#calculating expected mean value using regression model
mean_price = 35.29215 + 1.24417*191.9301
mean_price
#calculating estimated total 
estimated_tot=mean_price*2500
estimated_tot

#####################################################################################################
################# Graphical Analysis ################# 

#sample 1
svyhist(~price,Cluster_Design1, main="Histogram of Price",col="purple",probability = FALSE) 

svyboxplot(~price~store_type,Cluster_Design1,main="Boxplot of Price with Store_type ", col="purple" ) 

svyboxplot(~price~location,Cluster_Design1,main="Boxplot of Price with Location ", col="purple" )

svyplot(cost~price,design=Cluster_Design1, style="bubble",xlab="Price", ylab="Cost", main="Cost Vs Price")
