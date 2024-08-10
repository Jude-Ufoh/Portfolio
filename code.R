setwd("C:/Users/JUDE/Documents/Assign")
#Loading in the dataset
main_data=read.csv('main_data.csv')
main_data
#Statistical Description of the Data
summary(main_data)
#mean for each country for the 10 years period
main_data_mean<-aggregate(cbind(GDP,Tax_rate,Population,Mobile_Telephone,Fixed_Telephone,Internet_Users,Teenage_popu,Youth_popu,Adult_Popu)~Country+Continent+Status, data=main_data, FUN=mean, na.rm=TRUE)
main_data_mode<-aggregate(cbind(GDP,Tax_rate,Population,Mobile_Telephone,Fixed_Telephone,Internet_Users,Teenage_popu,Youth_popu,Adult_Popu)~Country+Continent+Status, data=main_data, FUN=mode)
#median for each country for the 10 years period
main_data_median<-aggregate(cbind(GDP,Tax_rate,Population,Mobile_Telephone,Fixed_Telephone,Internet_Users,Teenage_popu,Youth_popu,Adult_Popu)~Country+Continent+Status, data=main_data, FUN=median, na.rm =TRUE)
#Standard Deviation for each of the country for the 10 years period
main_data_sd<-aggregate(cbind(GDP,Tax_rate,Population,Mobile_Telephone,Fixed_Telephone,Internet_Users,Teenage_popu,Youth_popu,Adult_Popu)~Country+Continent+Status, data=main_data, FUN=sd, na.rm =TRUE)
#Skewness and kurtosis of each of the indicators in the dataset
install.packages("moments")
library(moments)
skewness(main_data$GDP)
kurtosis(main_data$GDP)
sd(main_data$GDP)
skewness(main_data$Tax_rate)
kurtosis(main_data$Tax_rate)
sd(main_data$Tax_rate)
skewness(main_data$Population)
kurtosis(main_data$Population)
sd(main_data$Population)
skewness(main_data$Mobile_Telephone)
kurtosis(main_data$Mobile_Telephone)
sd(main_data$Mobile_Telephone)
skewness(main_data$Fixed_Telephone)
kurtosis(main_data$Fixed_Telephone)
sd(main_data$Fixed_Telephone)
skewness(main_data$Internet_Users)
kurtosis(main_data$Internet_Users)
sd(main_data$Internet_Users)
skewness(main_data$Teenage_popu)
kurtosis(main_data$Teenage_popu)
sd(main_data$Teenage_popu)
skewness(main_data$Youth_popu)
kurtosis(main_data$Youth_popu)
sd(main_data$Youth_popu)
skewness(main_data$Adult_Popu)
kurtosis(main_data$Adult_Popu)
sd(main_data$Adult_Popu)

#CORRELATION ANALYSIS
#Importing all the necessary library
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("corrplot")
install.packages("tidyverse")
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)
library(ggplot2)
#Loading and viewing the first 5 rows of our data set
head(main_data, 3)
#removing the categorical variables
main_data_pruned<-main_data %>%select(-Country, -Year,-Continent,-Status )
main_data_pruned
#Building a correlatin matrix
corrplot(cor(main_data_pruned), method ='number', type = 'upper')
#Plotting a scatter plot to visualise the seemingly correlated data
pairs(main_data_pruned[, c("Mobile_Telephone", "Fixed_Telephone")])
plot(main_data_pruned$Mobile_Telephone, main_data_pruned$Fixed_Telephone)
#REGRESSION ANALYSIS
#extracting the numerical data
main_data_reduced<-main_data[ ,c("GDP", "Tax_rate","Population", "Mobile_Telephone", "Fixed_Telephone","Internet_Users", "Teenage_popu", "Youth_popu","Adult_Popu")]
cor(main_data_reduced)
corrplot(cor(main_data_reduced))
#USING FORWARD STEPWISE METHOD TO FIT SLR model between Mobile_Telephone and Internet_Users
model_1<-lm(Mobile_Telephone~Internet_Users,main_data_reduced )
summary.lm(model_1)
#Visualizing the fitted regression
plot(Mobile_Telephone~Internet_Users,main_data_reduced,
     col="blue",
     main="Regression:Mobile_Telephone and Internet Users",
     xlab ="Internet Users",
     ylab ="Mobile_Telephone")
abline(model_1, col="red")
plot(model_1,1)
plot(model_1,2)
plot(model_1,3)
model_2<-lm(Mobile_Telephone~GDP + Adult_Popu + Teenage_popu, main_data_reduced)
summary.lm(model_2)
#TIME SERIES ANALYSIS


#HYPOTHETICAL ANALYSIS
#Finding the ten years average for Mobile_Telephone for each of the countries

main_data_hyp=aggregate(cbind(Mobile_Telephone) ~Country+Continent+Status, data = main_data, FUN = mean, na.rm = TRUE)
main_data_hyp
#Checking for the normality of the data
ggplot(mapping=aes(sample=main_data_hyp$Mobile_Telephone))+stat_qq_point(size=2,color="Blue")+stat_qq_line(color="orange")+xlab("Theoretical")+ylab("sample")
#Conducting a shapiro-wilk test to test the normality of the distribution
shapiro.test(main_data_hyp$Mobile_Telephone)
#Log Transforming the Mobile_Telephone Column
main_data_hyp_log<-main_data_hyp
main_data_hyp_log[,4:4]<-log(main_data_hyp_log[4:4],10)
main_data_hyp_log
#Visualising the Log10 transformed data in a ggplot
ggplot(mapping=aes(sample=main_data_hyp_log$Mobile_Telephone))+stat_qq_point(size=2,color="Blue")+stat_qq_line(color="orange")+xlab("Theoretical")+ylab("sample")
#Conducting a shapiro-wilk test to test the normality of the distribution
shapiro.test(main_data_hyp_log$Mobile_Telephone)
#converting the Status column into factor
main_data_hyp_log$Status<-as.factor(main_data_hyp_log$Status)
#Using a boxplot to compare the developed and undeveloped countries
boxplot(Mobile_Telephone~Status, data=main_data_hyp_log,  names=c("Developed","Undeveloped"),
         xlab="Developed or Undeveloped",ylab="Mobile_Telephone",main="Mobile Telephone Users for Developed and Undeveloped Countries")
#Performing an independent two sample t-test
t.test(Mobile_Telephone~Status,main_data_hyp_log)
#ANOVA TEST
#Extracting the required data
Anova_test_data<-select(main_data_hyp_log,Country, Mobile_Telephone, Continent)
Anova_test_data
#USing a boxplot to compare the different coontinents to check Anova A4
boxplot(Mobile_Telephone~Continent,data=Anova_test_data, names=c("Africa","Asia","Europe","N.America"),
        xlab ="Continent",ylab="Mobile_Telephone", main="Mobile Telephone by Continent")
#Using Shapiro-Wilk Test to test for Anova A5
byf.shapiro(Mobile_Telephone~Continent, data = Anova_test_data)
#Checking for Anova A6, the homogeneity of variances in the different continents
bartlett.test(Mobile_Telephone~Continent, data=Anova_test_data)
#Performing the One way Anova Test
oneway.test(Mobile_Telephone~Continent, data =Anova_test_data,var.equal=TRUE)
######PERFORMING TIME SERIES#########
library("TTR")
library("forecast")
library(dplyr)
Time_series_data=aggregate(cbind(Mobile_Telephone) ~Status+Year, data = main_data, FUN = mean, na.rm = TRUE)
main_data_time_series=Time_series_data
Developed<-filter(Time_series_data, Status=='Developed')
Undeveloped<-filter(Time_series_data, Status=='Undeveloped')
Undeveloped_country_Data=Undeveloped['Mobile_Telephone']
Developed_country_Data=Developed['Mobile_Telephone']
Developed_time_series=ts(Developed_country_Data,frequency=1,start = c(2005,1))
Undeveloped_time_series=ts(Undeveloped_country_Data,frequency=1,start = c(2005,1))
#Because the values are big we reduce them using log transform
Undeveloped_time_series_log=log(Undeveloped_time_series)
plot.ts(Undeveloped_time_series_log)

Developed_time_series_log=log(Developed_time_series)
plot.ts(Developed_time_series_log)
#for the undeveloped values
#checking for auto regression model
acf(Undeveloped_time_series_log)
pacf(Undeveloped_time_series_log)
#Fitting the AR Model to the time series
AR_undeveloped <- arima(Undeveloped_time_series_log, order = c(1,0,0))
print(AR_undeveloped)
#plotting the series along with the fitted values
ts.plot(Undeveloped_time_series_log)
AR_fit_undeveloped <- Undeveloped_time_series_log - residuals(AR_undeveloped)
points(AR_fit_undeveloped, type = "l", col = 2, lty = 2)
#Using predict()function in R to make a 1-step forecast
predict_AR_undeveloped <- predict(AR_undeveloped)
#Obtaining the 1-step forecast using $pred[1]
predict_AR_undeveloped$pred[1]

######Values for the Developed Countries #####
#checking for auto regression model
acf(Developed_time_series_log)
pacf(Developed_time_series_log)
#Fitting the AR Model to the time series
AR_Developed <- arima(Developed_time_series_log, order = c(1,0,0))
print(AR_Developed)
#plotting the series along with the fitted values
ts.plot(Developed_time_series_log)
AR_fit_Developed <- Developed_time_series_log - residuals(AR_Developed)
points(AR_fit_Developed, type = "l", col = 2, lty = 2)
#Using predict()function in R to make a 1-step forecast
predict_AR_Developed <- predict(AR_Developed)
#Obtaining the 1-step forecast using $pred[1]
predict_AR_Developed$pred[1]
predict(AR_Developed, n.ahead = 10)
predict(AR_undeveloped, n.ahead = 10)
