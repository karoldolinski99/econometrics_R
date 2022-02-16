library(ade4)
library(car)
library(corrplot)
library(dplyr)
library(gtools)
library(lmtest)
library(outliers)
library(psych)
library(randtests)
library(readxl)
library(readr)
library(strucchange)
library(skedastic)
library(tidyr)

set.seed(307)
alpha <- 0.05
dataset <- read.csv('bmw1.csv')

# --- Missing values ----------------------------------
colSums(is.na(dataset))
dataset <- missing_values(dataset)
colSums(is.na(dataset))
dataset <- as.data.frame(dataset)

stats <- describe(dataset)
table(dataset$model)
table(dataset$fuelType)
table(dataset$transmission)

# --- cor ---------------------------------------------
corr <- cor(cbind.data.frame(dataset$price, dataset$year, dataset$mileage, dataset$tax,
                             dataset$mpg, dataset$engineSize))
colnames(corr) <- rownames(corr) <- c('price', 'year', 'mileage', 'tax', 'mpg', 'engineSize')
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(corr, method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black",
                   tl.col="black", tl.srt=45,
                   diag=FALSE 
)

# --- Outliers ----------------------------------------
dataset <- outliers_grubbs(dataset, 0.05)

# --- categorical variables to binary variables -------
dataset <- cbind(dataset$price,
                 as.data.frame(acm.disjonctif(as.data.frame(dataset[,1]))), dataset$year, 
                 as.data.frame(acm.disjonctif(as.data.frame(dataset[,4]))), dataset$mileage,
                 as.data.frame(acm.disjonctif(as.data.frame(dataset[,6]))), dataset$tax, 
                 dataset$mpg, dataset$engineSize)
colSums(dataset)

#removing binary variables which has no observations
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. i3")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. i8")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. M5")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. M6")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. Z3")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 6].Electric")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 6].Other")]

# removing one binary variable for each category (model, transmission, fuelType)
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 1]. Z4")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 4].Semi-Auto")]
dataset <- dataset[, -which(colnames(dataset)=="dataset[, 6].Hybrid")]

# --- colnames -----------------------------------------
colnames(dataset) <- c('price', 'model_1', 'model_2',
                       'model_3', 'model_4', 'model_5',
                       'model_6', 'model_7', 'model_8',
                       'model_M2', 'model_M3', 'model_M4',
                       'model_X1', 'model_X2', 'model_X3',
                       'model_X4', 'model_X5', 'model_X6', 'model_X7',
                       'year', 'transmission_Automatic', 'transmission_Manual', 
                       'mileage', 'fuelType_Diesel', 'fuelType_Petrol', 'tax',
                       'mpg', 'engineSize')

# variable: age and mpy ----------------------
dataset$age <- 2021 - dataset$year
dataset$mpy <- dataset$mileage/dataset$age

# removing variable year
dataset <- dataset[, !names(dataset) %in% c("year")]

# --- training and testing dataset ---------------------
dt = sort(sample(nrow(dataset), nrow(dataset)*.8))
training_set <- dataset[dt,]
testing_set <- dataset[-dt,]

# step method 
model <- lm(price~., data = training_set)
summary(model)
s <- step(model, direction = 'backward')$model
model <- lm(price ~ ., data = s)
summary(model)

# choosing variables based on correlations, then step method 
corr <- cor(dataset)
corr[1,]

training_set <- training_set[, names(training_set) %in% c('price', 'model_X5', 
                                                          'transmission_Manual', 'mileage', 'tax', 'mpg', 'engineSize', 'age', 'mpy')]

model <- lm(price~., data = training_set)
summary(model)
s <- step(model, direction = 'backward')$model
model <- lm(price ~ ., data = s)
summary(model)

training_set <- training_set[, !names(training_set) %in% c('engineSize', 'mpy')]

model <- lm(price~., data = training_set)
summary(model)

# four possible models

model1 <- lm(price~., data = training_set)
summary(model1)

model2 <- lm(log(price)~., data = training_set)
summary(model2)

model3 <- lm(log(price) ~ model_X5 + transmission_Manual + log(mileage) + tax + log(mpg) + log(age), data = training_set)
summary(model3)

model4 <- lm(price ~ model_X5 + transmission_Manual + log(mileage) + tax + log(mpg) + log(age), data = training_set)
summary(model)

AIC(model1)
BIC(model1)
AIC(model2)
BIC(model2)
AIC(model3)
BIC(model3)
AIC(model4)
BIC(model4)

# -- model I -------
shapiro.test(model2$residuals)

bptest(model2)
skedastic::white_lm(model2, interactions = T)

for(i in 2:ncol(training_set))
  print(gqtest(model2, alternative = 'two.sided', 
               order.by = ~ training_set[,i], data = training_set)$p.value)

# WLS

ln_e2 <- log(model2$residuals^2)
resid_model <- lm(ln_e2 ~ training_set$mileage +
                    training_set$tax + training_set$mpg + training_set$age)
w <- exp(resid_model$fitted.values)
model_wmnk <- lm(log(price)~., training_set, weights = 1/w)
summary(model_wmnk)
e <- (1/sqrt(w)*model_wmnk$residuals)

shapiro.test(e)
tseries::jarque.bera.test(e)

ncvTest(model_wmnk, var.formula = ~ model_X5 + transmission_Manual + training_set$mileage +
          training_set$tax + training_set$mpg + training_set$age)

corr <- cor(training_set)
corr[1,]

sctest(model_wmnk, data = training_set,type="Chow", point=floor(dim(training_set)[1]/2))
runs.test(e)
car::vif(model_wmnk)

# prediction
prediction <- exp(predict(model_wmnk, testing_set))
expost_error <- as.data.frame(testing_set$price - prediction)
ME <- mean(expost_error)
MAE <- mean(abs(expost_error))
MSE <- mean(expost_error^2)
RMSE <- MSE ^ 0.5
pred_y <- cbind(as.data.frame(prediction), testing_set$price)

