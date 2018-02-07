#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, readr, Hmisc, rms, survminer, randomForest, 
               missForest)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
# rbs <- 
#   read_csv("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA Export (2015-2017).csv")
# 
# rbs <- rbs %>%
#         filter(!is.na(Id), Id != 18588)
# 
# write_csv(rbs, 
#           "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA_Export.csv")

rbs <- read_csv("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/CRA_Export.csv")
#------------------------------------------------------------------------------

#Data Exploration--------------------------------------------------------------
# describe(rbs)
names(rbs) <- gsub(" ", ".", names(rbs))

rbs <- rbs %>%
        mutate(court.cost = sum(as.numeric(str_extract_all(Court.costs, "[0-9]+")[[1]]), 
                                na.rm = TRUE), 
               non.court.cost = sum(as.numeric(str_extract_all(Non.court.costs, "[0-9]+")[[1]])), 
               person.cost = as.numeric(Total.affected), 
               time = as.numeric(as.Date(Date.reported, format = "%m/%d/%y") - as.Date('2015-01-01')), 
               status = 1, 
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
        dplyr::select(-Urls, -`Organization.-.address.1`, - `Organization.-.address.2`, 
                      -Exploit.cve, -References, -Summary, -`Breach.location.-.address`, 
                      -Latitude, -Longitude, -Gmaps, -Organization.address, -Naics.code, 
                      -`Actor.-.person`, -`Actor.-.group`, -Id, 
                      -Name, -`Organization.-.city`, -`Organization.-.postcode`, 
                      -Data.type, -Third.party.name, -Stock.symbol, -Court.costs, 
                      -Non.court.costs, -`Breach.location.-.country`, 
                      -`Breach.location.-.state`, -Related.incidents, 
                      -Total.affected) %>%
        mutate_if(is.character, factor) %>%
        mutate_if(is.Date, as.numeric)

rbs.imputed <- missForest(as.data.frame(rbs), ntree = 300, verbose = TRUE)
#------------------------------------------------------------------------------

mod.data <- rbs 

hist(mod.data$time)

fit <- survfit(Surv(time, status)~1, data = mod.data)
plot.data <- tidy(fit)

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate), size = 1) +
  ggtitle("Overall") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw()

fig.path <- "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/graphics/"

ggsave(paste0(fig.path, "overall.png"))

fit <- survfit(Surv(time, status) ~ Data.family, data = mod.data)
plot.data <- tidy(fit) %>%
              rowwise() %>%
              mutate(`Data Family` = str_split(strata, "=")[[1]][2])

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate, color = `Data Family`), 
            size = 1) +
  ggtitle("By Data Family") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw()

ggsave(paste0(fig.path, "Data_Family.png"))


fit <- survfit(Surv(time, status) ~ Business.type, data = mod.data)
plot.data <- tidy(fit) %>%
  rowwise() %>%
  mutate(`Business Type` = str_split(strata, "=")[[1]][2])

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate, color = `Business Type`), 
            size = 1) +
  ggtitle("By Business Type") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw()

ggsave(paste0(fig.path, "Business_Type.png"))


fit <- survfit(Surv(time, status) ~ Business.type + Data.family, data = mod.data)
plot.data <- tidy(fit) %>%
  rowwise() %>%
  mutate(strata1 = gsub(", Data.family=", " ", strata), 
         strata2 = gsub("Business.type=", "", strata1), 
         `Business Type` = str_split(strata2, " ")[[1]][1], 
         `Data Family` = str_split(strata2, " ")[[1]][2])

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate, color = `Data Family`), 
            size = 1) +
  ggtitle("By Business Type") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw() +
  facet_wrap(~`Business Type`)


ggplot(rbs) +
  geom_histogram(aes(x = Severity.score))

ggplot(rbs) +
  geom_boxplot(aes(y = Severity.score, x = Data.family)) 

ggplot(rbs) +
  geom_boxplot(aes(x = Business.type, y = Severity.score)) 

ggplot(rbs) +
  geom_boxplot(aes(x = paste(Business.type, Data.family), y = Severity.score)) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(rbs) + 
  geom_point(aes(x = Severity.score, y = as.numeric(Total.affected), color = Data.family), 
             alpha = 0.5, size = 2) +
  coord_cartesian(ylim = c(0, 1000000)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Business.type)
