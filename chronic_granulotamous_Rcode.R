#Read the data
chronic_gran <- read.csv("C:/Users/sitan/OneDrive/smbaproject/chromic_granulotamous/chronic_granulotamous_csv..csv")

#Attach the data frame
attach(chronic_gran)
head(chronic_gran)
str(chronic_gran)

#Factorize the categorical variables
chronic_gran$Treatment <- ifelse(test=chronic_gran$Treatment==0, yes="Placebo",no="GammaInterferon")
chronic_gran$Sex <- ifelse(test=chronic_gran$Sex==0, yes="Female",no="Male")
chronic_gran$Inherit <- ifelse(test=chronic_gran$Inherit==0, yes="X-Linked",no="Autosomal")
chronic_gran$Steroids <- ifelse(test=chronic_gran$Steroids==0, yes="No",no="Yes")
chronic_gran$Prophylac <- ifelse(test=chronic_gran$Prophylac==0, yes="No",no="Yes")
chronic_gran$Sex <- as.factor(chronic_gran$Sex)
chronic_gran$Inherit <- as.factor(chronic_gran$Inherit)
chronic_gran$Steroids <- as.factor(chronic_gran$Steroids)
chronic_gran$Prophylac <- as.factor(chronic_gran$Prophylac)
chronic_gran$status_Detected <- as.factor(chronic_gran$status_Detected)

#Check whether changes comitted
str(chronic_gran)

#Check whether male and female are sufficiently significant for the dependent variable.
xtabs(~ status_Detected + Sex, data=chronic_gran)

#Model 1: Predicting detection based only on the gender
logistic <- glm(status_Detected ~ Sex, data = chronic_gran, family = "binomial")
summary(logistic)

#Result: status_Detected = 0.7802 + (-0.3198)*Male
#Log (odds) of female having heart disease is 0.7802
#Log (odds) of male having heart disease is 0.4604
#In logistic regression, we estimate the mean of the data and the variance is 'derived' from the mean. 
#Since the variance is not estimated from the mean, it is possible that the variance is underestimated
#AIC: Akaike Information Criterion is the residual deviance adjusted for the number of paramenters in the model. 
#AIC can be used to compare one model to another

#Model2: Predicting detection using ALL the variables
logistic <- glm(status_Detected ~ Sex + Inherit + Steroids + Prophylac, data = chronic_gran, family = "binomial")

