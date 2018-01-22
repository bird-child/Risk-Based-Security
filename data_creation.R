#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, readr, Hmisc, rms, survminer)
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


nms <- names(rbs)
missing <- unlist(lapply(nms, function (x) {sum(is.na(rbs[,x]))/nrow(rbs)}))
names(missing) <- nms

1 - nrow(rbs[complete.cases(rbs),])/nrow(rbs)

rbs <- rbs %>%
        mutate(court_cost = sum(as.numeric(str_extract_all(`Court costs`, "[0-9]+")[[1]])), 
               non_court_cost = sum(as.numeric(str_extract_all(`Non court costs`, "[0-9]+")[[1]])))

#------------------------------------------------------------------------------

names(rbs) <- gsub(" ", ".", names(rbs))

mod.data <- rbs %>%
        mutate(time = as.numeric(as.Date(Incident.occurred) - as.Date('2015-01-01')), 
               status = 1) %>%
        filter(time >= 0, !is.na(time))

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
