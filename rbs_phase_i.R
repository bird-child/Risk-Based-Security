#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, readr, randomForest, caret, rms)

options(scipen = 9999)
set.seed(12345)

#Change to path where data is stored
setwd("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/")
#------------------------------------------------------------------------------

#Read--------------------------------------------------------------------------
#Two rows were not quite right, so I removed them and resaved the data set.
# rbs <- 
#   read_csv(paste0(data.path, "CRA Export (2015-2017).csv")
# 
# rbs <- rbs %>%
#         filter(!is.na(Id), Id != 18588)
# 
# write_csv(rbs, paste0(data.path, "CRA_Export.csv"))

#Uncleaned data
raw <- read_csv("CRA_Export.csv")

names(raw) <- tolower(gsub(" ", "_", gsub("- ", "", 
                                          gsub("/", "_", names(raw)))))
rbs <- raw %>%
  #Regex to extract court costs. Pulls all numeric characters out of the string and
  #sums them.
  mutate(court_costs = map_dbl(map(str_extract_all(court_costs, "[0-9]+"), 
                                  as.numeric), sum),
         #Regex to extract non-court costs 
         non_court_costs = map_dbl(map(str_extract_all(non_court_costs, "[0-9]+"), 
                                      as.numeric), sum),
         #Forces Total affected from character to numeric
         total_affected = as.numeric(total_affected),
         #Days from 1/1/2015 to breach
         time = as.numeric(as.Date(date_reported, format = "%m/%d/%y") - as.Date('2015-01-01')), 
         #All records are breaches
         status = 1, 
         #Formats date fields. Different formats were used for differet fields
         date_reported = as.Date(date_reported, format = "%m/%d/%y"), 
         date_discovered_by_organization = as.Date(date_discovered_by_organization), 
         date_organization_mails_notifications = as.Date(date_organization_mails_notifications), 
         date_records_recovered = as.Date(date_records_recovered), 
         date_lawsuit_filed = as.Date(date_lawsuit_filed), 
         date_arrest_made = as.Date(date_arrest_made), 
         date_of_revenue = as.Date(date_of_revenue, format = "%m/%d/%y"), 
         date_of_employee_count = as.Date(date_of_employee_count, format = "%m/%d/%y"), 
         created_at = as.Date(created_at),
         regulatory_action_taken = as.Date(regulatory_action_taken), 
         incident_occurred = as.Date(incident_occurred), 
         updated_at = as.Date(updated_at), 
         #Binary target indicator variable for breaches where > 2 000 affected
         over_2000_records_breached = ifelse(total_affected > 2000, 1, 0),
         #Groupings for preliminary survival analyses
         organization_rating = factor(floor(2*organization_rating)/2), 
         severity_score = factor(floor(2*severity_score)/2)) %>%
  #Forces character fields to factors
  mutate_if(is.character, factor) %>%
  #Forces date fields to numeric
  mutate_if(is.Date, as.numeric) %>%
  mutate_if(is.logical, factor) %>%
  mutate(data_type_raw = str_replace_all(data_type, 
                                         c("NUM|NAA|ADD" = "G1", 
                                           "USR|PWD|MISC|EMA" = "G2", 
                                           "UNK|MED" = "G3", 
                                           "FIN|CCN|ACC" = "G4", 
                                           "SSN|DOB" = "G5", 
                                           "IP" = "G6"))) %>%
  rowwise() %>%
  mutate(data_type = paste0(sort(unique(str_split(data_type_raw,"/")[[1]])),
                            collapse="/")) 
  
save(rbs, file = "rbs_miss.Rda")
#------------------------------------------------------------------------------------

#Imputation--------------------------------------------------------------------------
#Copies rbs and removes/preps fields for imputation
rbs <- rbs %>%
  data.frame() %>%
  mutate(data_type = factor(data_type)) %>%
  #Removes character fields with too many levels to be factors or are identical to 
  #other fields in the data set. Including them in the imputation would create 
  #colinearty and singularity issues.
  dplyr::select(-urls, -organization_address_1, -organization_address_2, 
                -exploit_cve, -references, -summary, -breach_location_address, 
                -latitude, -longitude, -gmaps, -organization_address,
                -naics_code,
                -actor_person, -actor_group,
                -name, -organization_city, -organization_postcode, 
                -data_type_raw,
                -third_party_name, -stock_symbol, -court_costs, 
                # -non_court_costs, 
                -breach_location_country, 
                -breach_location_state, -related_incidents, 
                -economic_sector, -id, -third_party, -data_recovered, 
                -consumer_lawsuit, -arrest_prosecution, -data_family, 
                -from_primary_source, -regulatory_action_indicator, 
                -related_incidents_indicator) 

#Determines % of records with missing values for each field 
miss_summ <- data.frame(name = names(rbs), 
                        prop.miss = map_dbl(seq_along(rbs), 
                                            function (x) {
                                              sum(is.na(rbs[, x]))/nrow(rbs)
                                            }), 
                        index = 1:ncol(rbs)) %>%
  arrange(desc(prop.miss)) 

#Target variables have ~33% missing - other variables with more missings won't 
#aid the imputation, so remove them
enough <- miss_summ %>%
  filter(prop.miss < 0.5)

#Data set to for imputations
for_imps <- rbs %>% 
  select(enough$index, non_court_costs) %>%
  #remaining variables with missings are factors where missing is 
  #a meaningful category. Forces NAs to "Missing"
  mutate_if(is.factor, fct_explicit_na) %>%
  #Status is constant, and Incident occured likely doesn't have
  #much information that we dont have in other date fields
  #time is a transformation of Date reported
  select(-incident_occurred, -status, -time) %>%
  mutate(over_2000_records_breached = factor(over_2000_records_breached))

#For continuous imputation
for_cont_imp <- for_imps %>%
  select(-over_2000_records_breached, -non_court_costs)

#For binary imputation
for_bin_imp <- for_imps %>%
  select(-total_affected, -non_court_costs)

#For cost imputation
for_cost_imp <- for_imps %>%
  select(-over_2000_records_breached, -total_affected)

#Continuous Imputation
#Restricts to complete cases
rbs.complete <- for_cont_imp[complete.cases(for_cont_imp), ] 

rf_cont <- randomForest(total_affected ~ ., data = rbs.complete, ntrees = 1000)

save(rf_cont, file = "rf_cont.Rda")

# load("rf_cont.Rda")

#Binary Imputation
#Restricts to complete cases
rbs.complete <- for_bin_imp[complete.cases(for_bin_imp), ] 

rf_bin <- randomForest(over_2000_records_breached ~ ., data = rbs.complete,
                       ntrees = 1000)

save(rf_bin, file = "rf_class.Rda")

# load("rf_class.Rda")

#Cost Imputation
#Restricts to complete cases
rbs.complete <- for_cost_imp[complete.cases(for_cost_imp), ] 

rf_cost <- randomForest(non_court_costs ~ ., data = rbs.complete, ntrees = 1000)

save(rf_cost, file = "rf_cost.Rda")

# load("rf_cost.Rda")


rbs.imp <- rbs %>%
  mutate(over_2000_records_breached_imputed = factor(coalesce(factor(over_2000_records_breached), 
                                                    predict(rf_bin, for_imps, 
                                                            type = "class"))), 
         total_affected_imputed = coalesce(total_affected, 
                                       predict(rf_cont, for_imps)), 
         cost_imputed = coalesce(non_court_costs, predict(rf_cost, for_imps)), 
         economic_sector = factor(raw$economic_sector_name), 
         naics_code = factor(raw$naics_code))

save(rbs.imp, file = "rbs_imp.Rda")
write_csv(rbs.imp, "rbs_imp.csv")
#------------------------------------------------------------------------------

#Survival----------------------------------------------------------------------
#Fits time to breach model 
fit <- cph(Surv(time, status) ~ 
             # naics_code  + 
             economic_sector
           ,
               data = rbs.imp, x = TRUE, y = TRUE)

# combos <- expand.grid(naics_code = levels(rbs.imp$naics_code),
#                       economic_sector = levels(factor(rbs.imp$economic_sector)))

combos <- data.frame(economic_sector = levels(rbs.imp$economic_sector))

estimates.60 <- survest(fit, combos, times = 60)

#Calculates risk of breach within 60 days and 95% CI
risk.60 <- combos %>%
                mutate(time_horizon = 60, 
                       risk = 1 - estimates.60$surv, 
                       lcl = 1 - estimates.60$upper, 
                       ucl = 1 - estimates.60$lower)


estimates.120 <- survest(fit, combos, times = 120)

#Calculates risk of breach within 120 days and 95% CI
risk.120 <- combos %>%
                mutate(time_horizon = 120, 
                       risk = 1 - estimates.120$surv, 
                       lcl = 1 - estimates.120$upper, 
                       ucl = 1 - estimates.120$lower)

risk <- risk.60 %>%
          union_all(risk.120) 

save(risk, file = "risk.Rda")
write_csv(risk, "risk.csv")
#-----------------------------------------------------------------------------------

#Aggregations-----------------------------------------------------------------------
data_type_rates <- data.frame(data_type = paste0("G", 1:6), 
                              ratio = c(5, 10, 40, 50, 65, 10)) %>%
                    mutate(pct = ratio/sum(ratio))

surv_results <-  rbs.imp %>%
              group_by(economic_sector) %>%
              summarise(RowsBreached = mean(total_affected_imputed), 
                       CostOfBreach = mean(cost_imputed)) %>%
              right_join(risk, by = c("economic_sector" = "economic_sector"))

data_ty_cols <- do.call(rbind, replicate(2*nlevels(rbs.imp$economic_sector), 
                                         data_type_rates, simplify = FALSE))

Results <- do.call(rbind, replicate(nlevels(factor(data_type_rates$data_type)), 
                                         surv_results, simplify = FALSE))%>%
              arrange(economic_sector, time_horizon) %>%
              cbind(data_ty_cols) %>%
              mutate(RowsBreached_data_type = pct*RowsBreached, 
                     CostOfBreach_data_type = pct*CostOfBreach)


write.csv(Results,"ZackAllCombos3.csv")
#-----------------------------------------------------------------------------------