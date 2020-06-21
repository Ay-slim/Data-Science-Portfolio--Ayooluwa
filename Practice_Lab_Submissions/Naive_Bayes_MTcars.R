#install.packages("gmodels")

library(e1071)
library(gmodels)
library(ggplot)


mtcars
#data preprocessing
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

ggplot(data=mtcars, aes(x=mpg)) + geom_histogram(binwidth=.5)
summary(mtcars$mpg)

#create an index column
mtcars$id <- 1:nrow(mtcars)

#Split the data into train and test samples
library(dplyr)
train <- mtcars %>% dplyr::sample_frac(.80)
test<- dplyr::anti_join(mtcars, train, by='id')





# Model
nb = naiveBayes(train,train$vs)

## Evaluation
test_pred=predict(nb,test)
CrossTable(test_pred,test$vs,prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('predicted', 'actual'))

write.csv(mtcars, "mtcars.csv")
