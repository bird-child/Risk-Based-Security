#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Random Forest Imputation
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Imputations of # Affected and > 2,000 affected 
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, randomForest, caret, readr, perturb, pROC)

options(scipen = 9999)
set.seed(12345)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
#Change to location where data are stored.
data.path <- "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/"

load(paste0(data.path, "rbs_miss.Rda"))

#Copies rbs and removes/preps fields for imputation
rbs2 <- rbs %>%
  #Removes character fields with too many levels to be factors or are identical to 
  #other fields in the data set. Including them in the imputation would create 
  #colinearty and singularity issues.
  dplyr::select(-Urls, -`Organization - address 1`, - `Organization - address 2`, 
                -`Exploit cve`, -References, -Summary, -`Breach location - address`, 
                -Latitude, -Longitude, -Gmaps, -`Organization address`, -`Naics code`, 
                -`Actor - person`, -`Actor - group`,
                -Name, -`Organization - city`, -`Organization - postcode`, 
                -`Data type`, -`Third party name`, -`Stock symbol`, -`Court costs`, 
                -`Non court costs`, -`Breach location - country`, 
                -`Breach location - state`, -`Related incidents`, 
                -`Total affected`, -`Economic sector`) 
#------------------------------------------------------------------------------

#Exploration of Patterns of missingness----------------------------------------
#Determines % of records with missing values for each field 
miss_summ <- data.frame(name = names(rbs2), 
                        prop.miss = map_dbl(seq_along(rbs2), 
                                            function (x) {
                                              sum(is.na(rbs2[, x]))/nrow(rbs2)
                                              }), 
                        index = 1:ncol(rbs2)) %>%
                        arrange(desc(prop.miss)) 

miss_summ

#Target variables have ~33% missing - other variables with more missings won't 
#aid the imputation, so remove them
enough <- miss.summ %>%
            filter(prop.miss < 0.5)

#Data set to for imputations
for_imps <- rbs2 %>% 
              select(enough$index) %>%
              #remaining variables with missings are factors where missing is 
              #a meaningful category. Forces NAs to "Missing"
              mutate_if(is.factor, fct_explicit_na) %>%
              #Status is constant, and Incident occured likely doesn't have
              #much information that we dont have in other date fields
              #time is a transformation of Date reported
              select(-`Incident occurred`, -status, -time) %>%
              mutate(y.bin = factor(`> 2,000 affected`))

names(for_imps) <- gsub(" ",".", names(for_imps))

#For continuous imputation
for_cont_imp <- for_imps %>%
                  select(-`>.2,000.affected`, -y.bin)

#For binary imputation
for_bin_imp <- for_imps %>%
                select(-`#.Affected`, -`>.2,000.affected`)

complete <- for_imps[complete.cases(for_imps), ]

#Splits data into three equal folds
folds <- createFolds(rbs.complete$y.bin, k = 3, list = TRUE, 
                    returnTrain = FALSE)
#------------------------------------------------------------------------------

#Continuous Imputation---------------------------------------------------------
#Restricts to complete cases
rbs.complete <- for_cont_imp[complete.cases(for_cont_imp), ] 

#Fits model with training data
rf.train <- randomForest(`#.Affected` ~ ., data = rbs.complete[-folds$Fold3, ], 
                         ntrees = 1000)

#Evaluates predictions on test data
pred <- predict(rf.train, rbs.complete[folds$Fold3, ])
obs <- rbs.complete[folds$Fold3, ]$`#.Affected`
resid <- obs - pred
mse <- sum(resid^2)/nrow(rbs.complete[folds$Fold3, ])
sqrt(mse)

#Final model
system.time(
  rf_cont <- randomForest(`#.Affected` ~ ., data = rbs.complete, ntrees = 1000)
)

save(rf_cont, file = paste0(data.path, "rf_cont.Rda"))

# load(paste0(data.path, "rf_cont.Rda"))
#------------------------------------------------------------------------------

#Binary Imputation-------------------------------------------------------------
#Restricts to complete cases
rbs.complete <- for_bin_imp[complete.cases(for_bin_imp), ] 

#Fits the training model
rf.train <- randomForest(y.bin ~ ., data = rbs.complete[-folds$Fold3, ], 
                         ntrees = 1000)

#Evaluates prediction on test data
pred <- predict(rf.train, rbs.complete[folds$Fold3, ], type = "class")
obs <- rbs.complete[folds$Fold3, ]$y.bin
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
rf_bin <- randomForest(y.bin ~ ., data = rbs.complete, ntrees = 1000)
)

save(rf_bin, file = paste0(data.path, "rf_class.Rda"))

# load(paste0(data.path, "rf_bin.Rda"))
#------------------------------------------------------------------------------

#Final Imputations-------------------------------------------------------------
rbs.imp <- rbs %>%
            mutate(`> 2,000 affected (imp)` = factor(coalesce(factor(`> 2,000 affected`), 
                                                       predict(rf_bin, for_imps, 
                                                               type = "class")),
                                                       levels = c("0", "1"), 
                                                       label = c(">2000", "<=2000")), 
                   `# Affected (imp)` = coalesce(`# Affected`, 
                                                 predict(rf_cont, for_imps))) 

save(rbs.imp, file = paste0(data.path, "rbs_imp.Rda"))
write_csv(rbs.imp, paste0(data.path, "rbs_imp.csv"))
#------------------------------------------------------------------------------