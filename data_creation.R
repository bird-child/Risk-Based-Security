#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Data Cleaning
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Cleans data for survival analyses and imputations. Creates new response field
#that takes 1 if the breach impacted more than 2 000 people. 
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, readr, Hmisc, rms, survminer, randomForest)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
#Change to path where data is stored
data.path <- "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/"
#Two rows were not quite right, so I removed them and resaved the data set.
# rbs <- 
#   read_csv(paste0(data.path, "CRA Export (2015-2017).csv")
# 
# rbs <- rbs %>%
#         filter(!is.na(Id), Id != 18588)
# 
# write_csv(rbs, paste0(data.path, "CRA_Export.csv"))

#Uncleaned data
raw <- read_csv(paste0(data.path, "CRA_Export.csv"))
#------------------------------------------------------------------------------

#Data Exploration--------------------------------------------------------------
# describe(rbs)
rbs <- raw %>%
              #Regex to extract court costs. Pulls all numeric characters out of the string and
              #sums them.
        mutate(court_cost = map_dbl(map(str_extract_all(`Court costs`, "[0-9]+"), 
                                        as.numeric), sum),
               #Regex to extract non-court costs
               non_court_cost = map_dbl(map(str_extract_all(`Non court costs`, "[0-9]+"), 
                                            as.numeric), sum),
               #Forces Total affected from character to numeric
               `# Affected` = as.numeric(`Total affected`),
               #Days from 1/1/2015 to breach
               time = as.numeric(as.Date(`Date reported`, format = "%m/%d/%y") - as.Date('2015-01-01')), 
               #All records are breaches
               status = 1, 
               #Formats date fields. Different formats were used for differet fields
               `Date reported` = as.Date(`Date reported`, format = "%m/%d/%y"), 
               `Date discovered by organization` = as.Date(`Date discovered by organization`), 
               `Date organization mails notifications` = as.Date(`Date organization mails notifications`), 
               `Date records recovered` = as.Date(`Date records recovered`), 
               `Date lawsuit filed` = as.Date(`Date lawsuit filed`), 
               `Date arrest made` = as.Date(`Date arrest made`), 
               `Date of revenue` = as.Date(`Date of revenue`, format = "%m/%d/%y"), 
               `Date of employee count` = as.Date(`Date of employee count`, format = "%m/%d/%y"), 
               `Created at` = as.Date(`Created at`),
               `Regulatory action taken` = as.Date(`Regulatory action taken`), 
               `Incident occurred` = as.Date(`Incident occurred`), 
               `Updated at` = as.Date(`Updated at`), 
               #Binary target indicator variable for breaches where > 2 000 affected
               `> 2,000 affected` = ifelse(`# Affected` > 2000, 1, 0),
               #Groupings for preliminary survival analyses
               company_rating = cut(`Organization rating`, 
                                    breaks = seq(0.5, 5.5, 0.5),
                                    right = FALSE,
                                    labels = seq(0.5, 5, 0.5)), 
               severity_score = cut(`Severity score`, 
                                    breaks = seq(0, 10.5, 0.5),
                                    right = FALSE,
                                    labels = seq(0, 10, 0.5)),
               comp_rate = factor(round(`Organization rating`)), 
               sev_score = factor(round(`Severity score`))) %>%
        #Forces character fields to factors
        mutate_if(is.character, factor) %>%
        #Forces date fields to numeric
        mutate_if(is.Date, as.numeric) %>%
        mutate_if(is.logical, factor)

save(rbs, file = paste0(data.path, "rbs_miss.Rda"))
#------------------------------------------------------------------------------