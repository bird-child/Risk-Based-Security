#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Data Cleaning and Imputation
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Cleans data for survival analyses and imputes total number of people affected 
#by th breach
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, readr, Hmisc, rms, survminer, randomForest, 
               missForest)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
#Two rows were not quite right, so I removed them and resaved the data set.
# rbs <- 
#   read_csv("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA Export (2015-2017).csv")
# 
# rbs <- rbs %>%
#         filter(!is.na(Id), Id != 18588)
# 
# write_csv(rbs, 
#           "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA_Export.csv")

#Uncleaned data
raw <- read_csv("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA_Export.csv")
#------------------------------------------------------------------------------

#Data Exploration--------------------------------------------------------------
# describe(rbs)
names(raw) <- gsub(" ", ".", names(raw))

rbs <- raw %>%
              #Regex to extract court costs
        mutate(court.cost = map_dbl(map(str_extract_all(Court.costs, "[0-9]+"), 
                                        as.numeric), sum),
               #Regex to extract non-court costs
               non.court.cost = map_dbl(map(str_extract_all(Non.court.costs, "[0-9]+"), 
                                            as.numeric), sum),
               #Forces Total.affected from character to numeric
               person.cost = as.numeric(Total.affected),
               #Days from 1/1/2015 to breach
               time = as.numeric(as.Date(Date.reported, format = "%m/%d/%y") - as.Date('2015-01-01')), 
               #All records are breaches
               status = 1, 
               #Formats data fields
               Date.reported = as.Date(Date.reported, format = "%m/%d/%y"), 
               Date.discovered.by.organization = as.Date(Date.discovered.by.organization), 
               Date.organization.mails.notifications = as.Date(Date.organization.mails.notifications), 
               Date.records.recovered = as.Date(Date.records.recovered), 
               Date.lawsuit.filed = as.Date(Date.lawsuit.filed), 
               Date.arrest.made = as.Date(Date.arrest.made), 
               Date.of.revenue = as.Date(Date.of.revenue, format = "%m/%d/%y"), 
               Date.of.employee.count = as.Date(Date.of.employee.count, format = "%m/%d/%y"), 
               Created.at = as.Date(Created.at),
               Regulatory.action.taken = as.Date(Regulatory.action.taken), 
               Incident.occurred = as.Date(Incident.occurred), 
               Updated.at = as.Date(Updated.at)) %>%
        #Removes character fields with too many levels to be factors
        dplyr::select(-Urls, -`Organization.-.address.1`, - `Organization.-.address.2`, 
                      -Exploit.cve, -References, -Summary, -`Breach.location.-.address`, 
                      -Latitude, -Longitude, -Gmaps, -Organization.address, -Naics.code, 
                      -`Actor.-.person`, -`Actor.-.group`, -Id, 
                      -Name, -`Organization.-.city`, -`Organization.-.postcode`, 
                      -Data.type, -Third.party.name, -Stock.symbol, -Court.costs, 
                      -Non.court.costs, -`Breach.location.-.country`, 
                      -`Breach.location.-.state`, -Related.incidents, 
                      -Total.affected, -Economic.sector) %>%
        #Forces character fields to factors
        mutate_if(is.character, factor) %>%
        #Forces date fields to numeric
        mutate_if(is.Date, as.numeric) %>%
        mutate_if(is.logical, factor)

#Determines which fields have enough values to impute
enough <- data.frame(name = names(rbs), 
           prop.miss = unlist(lapply(seq_along(rbs), 
                                     function (x) {
                                       sum(is.na(rbs[, x]))/nrow(rbs)
                                       })), 
           index = 1:ncol(rbs)) %>%
           arrange(desc(prop.miss)) %>%
           filter(prop.miss < 0.5)

rbs <- subset(rbs, select = enough$index)

rbs <- rbs %>%
        mutate_if(is.factor, fct_explicit_na)

save(rbs, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_miss.Rda")

rbs <- dplyr::select(rbs, -Incident.occurred)

1 - nrow(rbs[complete.cases(rbs), ])/nrow(rbs)

system.time(
rf <- randomForest(person.cost ~ ., data = rbs[complete.cases(rbs), ], ntrees = 1000)
)

save(rf, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rf.Rda")

lin <- lm(person.cost ~ ., data = dplyr::select(rbs, -time, -status))
poi <- glm(person.cost ~ ., data = dplyr::select(rbs, -time, -status), family = poisson)

# rbs.imputed <- missForest(as.data.frame(rbs), ntree = 300, verbose = TRUE)
#------------------------------------------------------------------------------