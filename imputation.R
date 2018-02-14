#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Random Forest Imputation
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Cleans data for survival analyses and imputes total number of people affected 
#by th breach
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, randomForest, caret, readr, perturb, corrplot, 
               pROC)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
load("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_miss.Rda")

rbs2 <- rbs %>%
          mutate(y = factor(ifelse(person.cost >= 2000, "1", "0"))) %>%
          dplyr::select(-Incident.occurred, -status, -time, -person.cost, -Id)
#------------------------------------------------------------------------------

#Model Fitting-----------------------------------------------------------------
set.seed(12345)

#Restrict to complete cases
rbs.complete <- rbs2[complete.cases(rbs), ] #%>%
                  #Getting less accurate results including the handful of 
                  #records with more than 100 000 000 affected
                  #filter(person.cost < 100000000)

#Splits data into two equal folds
folds = createFolds(rbs.complete$y, k = 3, list = TRUE, 
                    returnTrain = FALSE)

#How does the linear model do?
fit <- glm(y ~ ., rbs.complete[folds$Fold1, ], family = binomial)
pred <- predict(fit, rbs.complete[folds$Fold2, ], type = "response")
obs <- rbs.complete[folds$Fold2, ]$y


#Fits the training model
rf.train <- randomForest(y ~ ., data = rbs.complete[-folds$Fold3, ], 
                         ntrees = 1000)
pred <- predict(rf.train, type = "class")
obs <- rbs.complete[-folds$Fold3, ]$y
forest.confusion <- table(obs, pred)
forest.confusion
forest.misclass <- (forest.confusion[1, 2] + forest.confusion[2, 1])/sum(forest.confusion)
forest.misclass
forest.specificity <- forest.confusion[1, 1]/(forest.confusion[1, 1] + forest.confusion[1, 2])
forest.specificity
forest.sensitivity <- forest.confusion[2, 2]/(forest.confusion[2, 1] + forest.confusion[2, 2])
forest.sensitivity

pred <- predict(rf.train, type = "response")

model.roc <- roc(obs, as.numeric(pred))
plot.roc(model.roc)
print(model.roc$auc)

#Evaluates how the model performs on the training set
pred <- predict(rf.train, rbs.complete[folds$Fold3, ], type = "class")
obs <- rbs.complete[folds$Fold3, ]$y
forest.confusion <- table(obs, pred)
forest.confusion
forest.misclass <- (forest.confusion[1, 2] + forest.confusion[2, 1])/sum(forest.confusion)
forest.misclass
forest.specificity <- forest.confusion[1, 1]/(forest.confusion[1, 1] + forest.confusion[1, 2])
forest.specificity
forest.sensitivity <- forest.confusion[2, 2]/(forest.confusion[2, 1] + forest.confusion[2, 2])
forest.sensitivity


pred <- predict(rf.train, rbs.complete[folds$Fold3, ], type = "response")

model.roc <- roc(obs, as.numeric(pred))
plot.roc(model.roc)
print(model.roc$auc)


#Final model
system.time(
rf <- randomForest(y ~ ., data = rbs.complete, ntrees = 1000)
)

save(rf, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rf.Rda")

# load("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rf.Rda")
#------------------------------------------------------------------------------

#Final Imputations-------------------------------------------------------------
rbs.imp <- rbs %>%
  mutate(y = factor(ifelse(person.cost >= 2000, "1", "0")),
         `Total Affected` = factor(coalesce(y, 
                                     predict(rf, rbs, type = "class")), 
                                   levels = c("0", "1"), 
                                   label = c("<2000", ">=2000"))) %>%
  dplyr::select(Id, `Total Affected`)

save(rbs.imp, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_imp.Rda")
write_csv(rbs.imp, "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_imp.csv")
#------------------------------------------------------------------------------