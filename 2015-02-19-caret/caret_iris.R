library(caret)
set.seed(42)

# The iris dataset
data(iris)

head(iris)

summary(iris)

# look at the data
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

# seperate train and test
trainIndex <- createDataPartition(iris$Species,   # data labels
                                  p = .7,         # percentage used for training
                                  list = FALSE,   # return matrix instead of list
                                  times = 1)      # how many slices?
head(trainIndex)

# training data
train_data <- iris[trainIndex,1:4]
train_labels <- iris[trainIndex,5]

# test data
test_data <- iris[-trainIndex,1:4]
test_labels <- iris[-trainIndex,5]

table(train_labels)

table(test_labels)# pre process the data

preprocess_methods <- c("center", "scale")

# determine transformation values
preprocess <- preProcess(train_data,
                         method=preprocess_methods)
help(preProcess)

# apply transformation values
train_data.pre <- predict(preprocess, train_data)
test_data.pre <- predict(preprocess, test_data)

summary(train_data)
summary(train_data.pre)

featurePlot(x = train_data.pre,
            y = train_labels,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

# train the model

# resampling methods / counts
train_control <- trainControl(method = "repeatedcv", # type of resampling
                              number = 10,           # number of folds
                              repeats = 2)           # repeats of whole process

# train a random forest
train_model.rf <- train(x=train_data.pre,      # data
                     y=train_labels,           # labels
                     method="rf",              # classification method
                     trControl=train_control,  # train control
                     metric="Accuracy",        # metric to determine best model
                     tuneLength=3)             # how many tuning parameters to try

train_model.rf

names(train_model.rf)

# train penalized logistic regression
train_model.plr <- train(x=train_data.pre,
                     y=train_labels,
                     method="plr",
                     trControl=train_control,
                     metric="Accuracy",
                     tuneLength=3)

train_model.plr

# results of the "best" model
train_model.rf$finalModel
train_model.plr$finalModel

# dive into the final model
class(train_model.rf$finalModel)
names(train_model.rf$finalModel)

plot(train_model.rf$finalModel)

class(train_model.plr$finalModel)
names(train_model.plr$finalModel)

# apply to test data
results.rf <- predict(train_model.rf, test_data.pre)
results.plr <- predict(train_model.plr, test_data.pre)

# side by side results
View(data.frame(test_labels,
           results.rf,
           results.plr))

# stats for the results
confusionMatrix(data=results.rf,
                reference=test_labels)

confusionMatrix(data=results.plr,
                reference=test_labels)

# variable importance
varImp(train_model.rf)

train_model.rf$finalModel$importance




