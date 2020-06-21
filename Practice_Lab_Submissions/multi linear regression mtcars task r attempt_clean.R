mtcars
#data preprocessing
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

ggplot(data=mtcars, aes(x=mpg)) + geom_histogram(binwidth=.5)

#create an index column
mtcars$id <- 1:nrow(mtcars)


#Split the data into train and test samples
library(dplyr)
train <- mtcars %>% dplyr::sample_frac(.70)
test<- dplyr::anti_join(mtcars, train, by='id')


#Build a multivariate model containing all feature variables
multivariate_model <- lm(train$mpg ~ train$cyl+train$disp+train$hp+train$drat+train$wt+train$qsec+train$vs+train$am+train$gear+train$carb)

MV_Training_MSE <- mean(multivariate_model$residuals^2)
MV_Test_MSE <- mean((test$mpg - predict.lm(multivariate_model, test))^2)
#Training MSE is 3.819504 while Test MSE is 45.83205 this shows a bit of overfitting as training accuracy does not totally transfer to test accuracy, but the test error is not too large, and the performance  is slightly acceptable.


#Try out various linear models for different feature variables
cyl_model <- lm(train$mpg ~ train$cyl)

cyl_Training_MSE <- mean(cyl_model$residuals^2)
cyl_Test_MSE <- mean((test$mpg - predict.lm(cyl_model, test))^2)
#Training MSE 8.764684; Test MSE 42.90403

disp_model <- lm(train$mpg ~ train$disp)

disp_Training_MSE <- mean(disp_model$residuals^2)
disp_Test_MSE <- mean((test$mpg - predict.lm(disp_model, test))^2)
#Training MSE=7.859935; Test MSE=38.72789

hp_model <- lm(train$mpg ~ train$hp)

hp_Training_MSE <- mean(hp_model$residuals^2)
hp_Test_MSE <- mean((test$mpg - predict.lm(hp_model, test))^2)
#Training MSE=11.68481; Test MSE=34.34934