load('service_model_dat.rdata')

#separate analyses for cooling service and coil service, one row per each week.
#column for e.g. cooling service occurred in this ~week. Additional columns for:
    #QA metrics that week, previous week, 2ago, etc. Some slope measures too across X weeks.

#figure out how to do some cross-fold validation.

#random forest predictive model
#https://www.guru99.com/r-random-forest-tutorial.html
#

library(dplyr)
library(randomForest)
library(caret)
library(e1071)

#RandomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
#Arguments:
#  - Formula: Formula of the fitted model
#- ntree: number of trees in the forest
#- mtry: Number of candidates draw to feed the algorithm. By default, it is the square of the number of columns.
#- maxnodes: Set the maximum amount of terminal nodes in the forest
#- importance=TRUE: Whether independent variables importance in the random forest be assessed

#trainControl(method = "cv", number = n, search ="grid")
#arguments
#- method = "cv": The method used to resample the dataset.
#- number = n: Number of folders to create
#- search = "grid": Use the search grid method. For randomized method, use "grid"
#Note: You can refer to the vignette to see the other arguments of the function.

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

#train(formula, df, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)
#argument
#- `formula`: Define the formula of the algorithm
#- `method`: Define which model to train. Note, at the end of the tutorial, there is a list of all the models that can be trained
#- `metric` = "Accuracy": Define how to select the optimal model
#- `trControl = trainControl()`: Define the control parameters
#- `tuneGrid = NULL`: Return a data frame with all the possible combination

