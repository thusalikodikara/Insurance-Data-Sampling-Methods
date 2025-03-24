##============IS 3001 - Group Project===========##
##============Group 23===========##


#required libraries

# install.packages("survey")
# install.packages("sampler")
# install.packages("readx1")
# install.packages("plotrix")
# install.packages("dplyr")

library(survey)
library(sampler)
library(readxl)
library(plotrix)
library(dplyr)

#importing dataset
attach(insurance)
insurance$children = as.factor(insurance$children)

#Checking for missing values
sum(is.na(insurance))

#summary
summary(insurance)



##============Population===========##

mean(insurance$age)
mean(insurance$bmi)
mean(insurance$charges)

std.error(insurance$age)
std.error(insurance$bmi)
std.error(insurance$charges)

prop_table = table(sex)/length(sex)
prop_table

prop_table = table(children)/length(children)
prop_table

prop_table = table(smoker)/length(smoker)
prop_table

prop_table = table(region)/length(region)
prop_table

table(sex)
table(children)
table(smoker)
table(region)

sum(insurance$age)
sum(bmi)
sum(charges)



##============Simple Random Sampling===========##

#SAMPLE 1
n1=rsampcalc(nrow(insurance),e=3,ci=95)
set.seed(123456)
SRS_Sample1 = rsamp(df = insurance,n1, rep = FALSE)

#CALCULATING ESTIMATES 
SRS1=svydesign(id=~1,data=SRS_Sample1)

#estimation of mean of age
svymean(~`age`,design=SRS1,na.rm=TRUE)

#estimation of mean of bmi
svymean(~`bmi`,design=SRS1,na.rm=TRUE)

#estimation of mean of Insuarance Charges
svymean(~`charges`,design=SRS1,na.rm=TRUE)

#_____

#estimation of proportion of no of Children
svymean(~`children`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of smokers
svymean(~`smoker`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of gender
svymean(~`sex`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of regeion
svymean(~`region`,design=SRS1,na.rm=TRUE)

#_____

#estimation of total of age
svytotal(~`age`,design=SRS1,na.rm=TRUE)

#estimation of total of bmi
svytotal(~`bmi`,design=SRS1,na.rm=TRUE)

#estimation of total of Insuarance Charges
svytotal(~`charges`,design=SRS1,na.rm=TRUE)

#estimation of total of no of Children
svytotal(~`children`,design=SRS1,na.rm=TRUE)

#estimation of total of no of smokers
svytotal(~`smoker`,design=SRS1,na.rm=TRUE)

#estimation of total of no of gender
svytotal(~`sex`,design=SRS1,na.rm=TRUE)

#estimation of total of no of regeion
svytotal(~`region`,design=SRS1,na.rm=TRUE)


#ratio estimates for charges
ratio_charges1 = svyratio(SRS_Sample1$charges, SRS_Sample1$age, design = SRS1)

# ration estimates for bmi
ratio_bmi1 = svyratio(SRS_Sample1$bmi, SRS_Sample1$age, design = SRS1)



#SAMPLE 2
n2=rsampcalc(nrow(insurance),e=3,ci=95)
set.seed(234567)
SRS_Sample2 = rsamp(df = insurance,n2, rep = FALSE)

#CALCULATING ESTIMATES 
SRS2=svydesign(id=~1,data=SRS_Sample2)

#estimation of mean of age
svymean(~`age`,design=SRS2,na.rm=TRUE)

#estimation of mean of bmi
svymean(~`bmi`,design=SRS2,na.rm=TRUE)

#estimation of mean of Insuarance Charges
svymean(~`charges`,design=SRS2,na.rm=TRUE)

#_____

#estimation of proportion of no of Children
svymean(~`children`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of smokers
svymean(~`smoker`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of gender
svymean(~`sex`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of regeion
svymean(~`region`,design=SRS2,na.rm=TRUE)

#_____

#estimation of total of age
svytotal(~`age`,design=SRS2,na.rm=TRUE)

#estimation of total of bmi
svytotal(~`bmi`,design=SRS2,na.rm=TRUE)

#estimation of total of Insuarance Charges
svytotal(~`charges`,design=SRS2,na.rm=TRUE)

#estimation of total of no of Children
svytotal(~`children`,design=SRS2,na.rm=TRUE)

#estimation of total of no of smokers
svytotal(~`smoker`,design=SRS2,na.rm=TRUE)

#estimation of total of no of gender
svytotal(~`sex`,design=SRS2,na.rm=TRUE)

#estimation of total of no of regeion
svytotal(~`region`,design=SRS2,na.rm=TRUE)


#ratio estimates for charges
ratio_charges2 = svyratio(SRS_Sample1$charges, SRS_Sample1$age, design = SRS1)

# ration estimates for bmi
ratio_bmi2 = svyratio(SRS_Sample1$bmi, SRS_Sample1$age, design = SRS1)



##==================Graphical Analysis=================##

svyhist(~charges, SRS1, main="Histogram of Charges of Sample 1", col="#5780EF",probability = FALSE) 
svyhist(~charges, SRS2, main="Histogram of Charges of Sample 2", col="#57EAEF",probability = FALSE)

svyplot(charges~age,design = SRS1,style = "bubble",main="Plot of Charges vs Age of sample 1",xlab="Age",ylab="Charges") 
svyplot(charges~age,design = SRS2,style = "bubble",main="Plot of Charges vs Age of sample 2",xlab="Age",ylab="Charges")

svyplot(charges~bmi,design = SRS1,style = "bubble",main="Plot of Charges vs BMI of sample 1",xlab="BMI",ylab="Charges")
svyplot(charges~bmi,design = SRS2,style = "bubble",main="Plot of Charges vs BMI of sample 2",xlab="BMI",ylab="Charges")



##============Stratified Sampling===========##


#SAMPLE 1

#Sample size Calculation
size=rsampcalc(nrow(insurance),2,95,0.03)
sample1_size=ssampcalc(df=insurance,n=size,strata = sex)

set.seed(123456)
strata1=stratsample(insurance$sex,c("female"=294,"male"=300))
strata_table=insurance[strata1,]
data_2=data.frame(strata_table)

#weights
###---calculating weights---###
weight1=round(662/294,3)
weight2=round(676/300,3)
data_3=data.frame(c("female","male"),c(weight1,weight2))
colnames(data_3)=c("sex","WEIGHT")

stratified_sample1=merge(data_2,data_3,by="sex")

Stratified1=svydesign(id=~1,strata = ~sex,weights = ~WEIGHT,data = stratified_sample1)


#CALCULATING ESTIMATES

#estimate for mean of charges
svymean(~charges,design = Stratified1)

#estimate for mean of age
svymean(~age,design = Stratified1)

#estimate for mean of BMI
svymean(~bmi,design = Stratified1)

#estimate for proportion of Gender
svymean(~sex,design = Stratified1)

#estimate for proportion of smoker
svymean(~smoker,design = Stratified1)

#estimate for proportion of region
svymean(~region,design = Stratified1)

#estimate for proportion of children
svymean(~children,design = Stratified1)

#estimate for total of age
svytotal(~age,design = Stratified1)

#estimate for total of BMI
svytotal(~bmi,design = Stratified1)

#estimate for total of charges
svytotal(~charges,design = Stratified1)

#estimate for total of sex
svytotal(~sex,design = Stratified1)

#estimate for total of smoker
svytotal(~smoker,design = Stratified1)

#estimate for total of region
svytotal(~region,design = Stratified1)

#estimate for total of children
svytotal(~children,design = Stratified1)

#ratio estimation
ratio_1=svyratio(~charges,~age,design = Stratified1)
ratio_2=svyratio(~bmi,~age,design = Stratified1)



#SAMPLE 2

#Sample size Calculation
size2=rsampcalc(nrow(insurance),2,95,0.03)
sample2_size=ssampcalc(df=insurance,n=size2,strata = sex)

set.seed(888456)
strata2=stratsample(insurance$sex,c("female"=294,"male"=300))
strata_table2=insurance[strata2,]
data_7=data.frame(strata_table2)

#weights
###---calculating weights---###
weight3=round(662/294,3)
weight4=round(676/300,3)
data_8=data.frame(c("female","male"),c(weight3,weight4))
colnames(data_8)=c("sex","WEIGHT")

stratified_sample2=merge(data_7,data_8,by="sex")

Stratified2=svydesign(id=~1,strata = ~sex,weights = ~WEIGHT,data = stratified_sample2)


#CALCULATING ESTIMATES

#estimate for mean of charges
svymean(~charges,design = Stratified2)

#estimate for mean of age
svymean(~age,design = Stratified2)

#estimate for mean of BMI
svymean(~bmi,design = Stratified2)

#estimate for proportion of Gender
svymean(~sex,design = Stratified2)

#estimate for proportion of smoker
svymean(~smoker,design = Stratified2)

#estimate for proportion of region
svymean(~region,design = Stratified2)

#estimate for proportion of children
svymean(~children,design = Stratified2)

#estimate for total of age
svytotal(~age,design = Stratified2)

#estimate for total of BMI
svytotal(~bmi,design = Stratified2)

#estimate for total of charges
svytotal(~charges,design = Stratified2)

#estimate for total of sex
svytotal(~sex,design = Stratified2)

#estimate for total of smoker
svytotal(~smoker,design = Stratified2)

#estimate for total of region
svytotal(~region,design = Stratified2)

#estimate for total of children
svytotal(~children,design = Stratified2)

#ratio estimation
ratio_3=svyratio(~charges,~age,design = Stratified2)
ratio_4=svyratio(~bmi,~age,design = Stratified2)



##====================Graphical Analysis===================##

#SAMPLE 1

boxplot(stratified_sample1$charges~stratified_sample1$sex, col="#5780EF", ylab="Charges",xlab = "Gender", main = "The Boxplot of Charges vs Gender of stratified sample 1")
boxplot(stratified_sample1$charges~stratified_sample1$smoker, col="#5780EF", ylab="Charges",xlab = "Smoking status", main = "The Boxplot of Charges vs Smoking status of stratified sample 1")
boxplot(stratified_sample1$age~stratified_sample1$sex, col="#5780EF", ylab="Age",xlab = "Gender", main = "The Boxplot of Age vs Gender of stratified sample 1")
boxplot(stratified_sample1$age~stratified_sample1$smoker, col="#5780EF", ylab="Age",xlab = "Smoking status", main = "The Boxplot of Age vs Smoking status of stratified sample 1")
plot(stratified_sample1$age,stratified_sample1$bmi,col="#5780EF", ylab="BMI",xlab = "Age",main = "BMI value vs Age")

table1=table(stratified_sample1$sex)
pie(table1,main = "Medical cost distribution in Gender", col = c("#5780EF","#16357E"))

table3=table(stratified_sample1$smoker)
barplot(table3, col="#5780EF", main = "Bar chart of Smoking status")

#SAMPLE 2

boxplot(stratified_sample2$charges~stratified_sample2$sex, col="#57EAEF", ylab="Charges",xlab = "Gender", main = "The Boxplot of Charges vs Gender of stratified sample 2")
boxplot(stratified_sample2$charges~stratified_sample2$smoker, col="#57EAEF", ylab="Charges",xlab = "Smoking status", main = "The Boxplot of Charges vs Smoking status of stratified sample 2")
boxplot(stratified_sample2$age~stratified_sample2$sex, col="#57EAEF",ylab="Age",xlab = "Gender", main = "The Boxplot of Age vs Gender of stratified sample 2")
boxplot(stratified_sample2$age~stratified_sample2$smoker, col="#57EAEF", ylab="Age",xlab = "Smoking status", main = "The Boxplot of Age vs Smoking status of stratified sample 2")
plot(stratified_sample2$age,stratified_sample2$bmi, col="#57EAEF",ylab="BMI",xlab = "Age",main = "BMI value vs Age")

table2=table(stratified_sample2$sex)
pie(table2,main = "Medical cost distribution in Gender", col = c("#57EAEF","#12A59B"))

table4=table(stratified_sample2$smoker)
barplot(table4,main = "Bar chart of Smoking status", col="#57EAEF")



##==============Cluster Sampling==============##

#Stage 1

set.seed(678910)
sample(c("northeast","northwest", "southeast", "southwest"), 1)
target <- c("southwest")

cluster1<-filter(insurance, region %in% target)

#Stage 2

n1=rsampcalc(nrow(cluster1),e=3,ci=95)
set.seed(246810)
cluster1_SRS1 = rsamp(df = cluster1, n1, rep = FALSE)

n2=rsampcalc(nrow(cluster1),e=3,ci=95)
set.seed(13579)
cluster1_SRS2 = rsamp(df = cluster1, n2, rep = FALSE)

SRS1=svydesign(id=~1,data=cluster1_SRS1)

SRS2=svydesign(id=~1,data=cluster1_SRS2)


#SAMPLE 1

#estimation of mean of age
svymean(~`age`,design=SRS1,na.rm=TRUE)

#estimation of mean of bmi
svymean(~`bmi`,design=SRS1,na.rm=TRUE)

#estimation of mean of Insuarance Charges
svymean(~`charges`,design=SRS1,na.rm=TRUE)

#_____

#estimation of proportion of no of Children
svymean(~`children`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of smokers
svymean(~`smoker`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of gender
svymean(~`sex`,design=SRS1,na.rm=TRUE)

#estimation of proportion of no of regeion
svymean(~`region`,design=SRS1,na.rm=TRUE)

#_____

#estimation of total of age
svytotal(~`age`,design=SRS1,na.rm=TRUE)

#estimation of total of bmi
svytotal(~`bmi`,design=SRS1,na.rm=TRUE)

#estimation of total of Insuarance Charges
svytotal(~`charges`,design=SRS1,na.rm=TRUE)

#estimation of total of no of Children
svytotal(~`children`,design=SRS1,na.rm=TRUE)

#estimation of total of no of smokers
svytotal(~`smoker`,design=SRS1,na.rm=TRUE)

#estimation of total of no of gender
svytotal(~`sex`,design=SRS1,na.rm=TRUE)

#estimation of total of no of regeion
svytotal(~`region`,design=SRS1,na.rm=TRUE)



#SAMPLE 2

#estimation of mean of age
svymean(~`age`,design=SRS2,na.rm=TRUE)

#estimation of mean of bmi
svymean(~`bmi`,design=SRS2,na.rm=TRUE)

#estimation of mean of Insuarance Charges
svymean(~`charges`,design=SRS2,na.rm=TRUE)

#_____

#estimation of proportion of no of Children
svymean(~`children`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of smokers
svymean(~`smoker`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of gender
svymean(~`sex`,design=SRS2,na.rm=TRUE)

#estimation of proportion of no of regeion
svymean(~`region`,design=SRS2,na.rm=TRUE)

#_____

#estimation of total of age
svytotal(~`age`,design=SRS2,na.rm=TRUE)

#estimation of total of bmi
svytotal(~`bmi`,design=SRS2,na.rm=TRUE)

#estimation of total of Insuarance Charges
svytotal(~`charges`,design=SRS2,na.rm=TRUE)

#estimation of total of no of Children
svytotal(~`children`,design=SRS2,na.rm=TRUE)

#estimation of total of no of smokers
svytotal(~`smoker`,design=SRS2,na.rm=TRUE)

#estimation of total of no of gender
svytotal(~`sex`,design=SRS2,na.rm=TRUE)

#estimation of total of no of regeion
svytotal(~`region`,design=SRS2,na.rm=TRUE)



##==============Graphical Analysis==============##

hist(cluster1_SRS1$charges, col="#5780EF", xlab="Charges", main="Charges Histogram of Sample 1")
hist(cluster1_SRS2$charges, col="#57EAEF", xlab="Charges", main="Charges Histogram of Sample 2")

boxplot(cluster1_SRS1$charges ~ cluster1_SRS1$smoker, ylab="Charges", xlab="Smoker",col="#5780EF", main = "Boxplot of Charges vs Smoker of Sample 1") 
boxplot(cluster1_SRS2$charges ~ cluster1_SRS2$smoker, ylab="Charges", xlab="Smoker", col="#57EAEF", main = "Boxplot of Charges vs Smoker of Sample 2") 

