#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Survival Analyses
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Investigation of risk of breach within 60 and 120 days by industry and company
#rating.
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, Hmisc, rms, survminer, survMisc)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
#Change to location where data are stored.
data.path <- "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/"

load(paste0(data.path, "rbs_imp.Rda"))
#------------------------------------------------------------------------------

#Survival Calculations---------------------------------------------------------
#Looks at disttribution of observations across categories
table(rbs.imp$comp_rate, rbs.imp$`Business type`)

names(rbs.imp) <- gsub(" ", "_", names(rbs.imp))

#Fits time to breach model 
fit <- survfit(Surv(time, status) ~ comp_rate + Business_type, data = rbs.imp)

#Calculates risk of breach within 60 days and 95% CI
risk.60 <- rbs.imp %>%
  group_by(comp_rate, Business_type) %>%
  summarise(time = max(time)) %>%
  filter(time >= 60) %>%
  ungroup() %>%
  mutate(risk = 1 - summary(fit, 60)$surv, 
         lcl = 1 - summary(fit, 60)$upper, 
         ucl = 1 - summary(fit, 60)$lower, 
         time_horizon = 60) %>%
  dplyr::select(-time)

#Calculates risk of breach within 120 days and 95% CI
risk.120 <- rbs.imp %>%
  group_by(comp_rate, Business_type) %>%
  summarise(time = max(time)) %>%
  filter(time >= 120) %>%
  ungroup() %>%
  mutate(risk = 1 - summary(fit, 120)$surv, 
         lcl = 1 - summary(fit, 120)$upper, 
         ucl = 1 - summary(fit, 120)$lower, 
         time_horizon = 120) %>%
  dplyr::select(-time)

#Combines data sets
risk <- risk.60 %>%
  union_all(risk.120) %>%
  rename(Company_rating = comp_rate) %>%
  mutate(Business_type = factor(Business_type), 
         Company_rating = factor(Company_rating)) %>%
  complete(Business_type, Company_rating, time_horizon)

save(risk, file = paste0(data.path, "risk.Rda"))
write_csv(risk, paste0(data.path, "risk.csv"))
#-----------------------------------------------------------------------------------