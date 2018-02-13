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
               stringr, randomForest, caret, readr, perturb, corrplot)
d#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
load("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_miss.Rda")

rbs <- dplyr::select(rbs, -Incident.occurred, -status, -time)
#------------------------------------------------------------------------------

#Model Fitting-----------------------------------------------------------------
set.seed(12345)

#Restrict to complete cases
rbs.complete <- rbs[complete.cases(rbs), ] #%>%
                  #Getting less accurate results including the handful of 
                  #records with more than 100 000 000 affected
                  #filter(person.cost < 100000000)

#There's some colliniearity problems going on
fit <- lm(sqrt(person.cost) ~ ., rbs.complete, x = TRUE)
mc <- colldiag(fit$x)

# View(mc$pi[mc$condindx > 30, ])

problem <- abs(cor(fit$x, method = "spearman")) > 0.75
problem[unlist(lapply(1:ncol(problem), function (x) {sum(problem[ ,x], na.rm = TRUE)})), ]

#Splits data into two equal folds
folds = createFolds(rbs.complete$person.cost, k = 2, list = TRUE, 
                    returnTrain = FALSE)

#How does the linear model do?
fit <- lm(log(person.cost) ~ ., rbs.complete[folds$Fold1, ])
summ <- summary(fit)
mse <- sum(resid^2)/nrow(rbs.complete[folds$Fold1, ])
sqrt(mse)

pred <- predict(fit, rbs.complete[folds$Fold2, ])
obs <- rbs.complete[folds$Fold2, ]$person.cost
resid <- obs - rf.test
mse <- sum(resid^2)/nrow(rbs.complete[folds$Fold2, ])
sqrt(mse)


#Fits the training model
rf.train <- randomForest(person.cost ~ ., data = rbs.complete[folds$Fold1, ], 
                         ntrees = 1000)
pred <- predict(rf.train)
obs <- rbs.complete[folds$Fold1, ]$person.cost
resid <- obs - pred
mse <- sum(resid^2)/nrow(rbs.complete[folds$Fold1, ])
sqrt(mse)

#Evaluates how the model performs on the training set
rf.test <- predict(rf.train, rbs.complete[folds$Fold2, ])
obs <- rbs.complete[folds$Fold2, ]$person.cost
resid <- obs - rf.test
mse <- sum(resid^2)/nrow(rbs.complete[folds$Fold2, ])
sqrt(mse)

#Final model
system.time(
rf <- randomForest(person.cost ~ ., data = rbs.complete, ntrees = 1000)
)

save(rf, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rf.Rda")

load("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rf.Rda")
#------------------------------------------------------------------------------

#Confidence Intervals----------------------------------------------------------
# Function that returns MAPE
mape <- function( actualValues  , predictedValues )
{
  # actualValues = NewData$Sepal.Length
  # predictedValues = as.numeric(predictions_for_seed)
  mean(abs((actualValues-predictedValues)/ actualValues ) * 100)
}

#make model object
MODEL = lm(sqrt(person.cost) ~ ., rbs.complete, x = TRUE)
DF = rbs.complete
#you wil be storing predictions on this list
list_of_prediction = list()

for(RandomGenNumber in 1:100){
  NewData = DF[sample(1:nrow(DF)),]
  NewData = NewData[1:round(.25*nrow(NewData)),]
  
  #this is where you predict
  predictions_for_seed = predict(MODEL, NewData)
  MAPE_Score = mape(NewData$person.cost ,as.numeric(predictions_for_seed))
  list_of_prediction = c(list_of_prediction,MAPE_Score)
  print( MAPE_Score )
}


#now you can do all sorts of stuff here to get ranges

All_Mapes = unlist( list_of_prediction )
#------------------------------------------------------------------------------

#Final Imputations-------------------------------------------------------------
rbs.imp <- raw %>%
  mutate(`Total Affected` = coalesce(as.numeric(Total.affected), 
                                     predict(rf, rbs))) %>%
  dplyr::select(Id, `Total Affected`)

save(rbs.imp, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_imp.Rda")
write_csv(rbs.imp, "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_imp.csv")
#------------------------------------------------------------------------------