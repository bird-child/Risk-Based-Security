#------------------------------------------------------------------------------
#Risk Based Security
#Phase I
#Survival Graphics
#------------------------------------------------------------------------------

#Purpose-----------------------------------------------------------------------
#Pretty pretty time to event plots.
#------------------------------------------------------------------------------

#Packages----------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, forcats, ggplot2, lubridate, purrr, 
               stringr, survival, Hmisc, rms, survminer, survMisc)
#------------------------------------------------------------------------------

#Data Importation--------------------------------------------------------------
load("C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/rbs_miss.Rda")
#------------------------------------------------------------------------------

#Graphs------------------------------------------------------------------------
mod.data <- rbs 

hist(mod.data$time)

fit <- survfit(Surv(time, status)~1, data = mod.data)
plot.data <- tidy(fit)

summary(fit)$table["median"]

ggplot(plot.data) +
  geom_segment(aes(x = summary(fit)$table["median"]/365.25, 
                   xend = summary(fit)$table["median"]/365.25, 
                   y = 0, yend = 0.5), size = 1, linetype = "dashed") +
  geom_segment(aes(x = 0, 
                   xend = summary(fit)$table["median"]/365.25, 
                   y = 0.5, yend = 0.5), size = 1, linetype = "dashed") +
  geom_step(aes(x = time/365.25, y = 1 - estimate), size = 1) +
  geom_label(aes(x = 0.7, y = 0.9,
  label = "Half of all companies \n experienced a breach \n within 1.5 years of 1/1/2015" )) +
  ggtitle("Overall Risk of Breach") +
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
  ggtitle("Risk of Breach by Data Family") +
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
  ggtitle("Risk of Breach by Business Type") +
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


mod.data$breach.ty <- fct_lump(mod.data$Breach.type, prop = 0.04)

fit <- survfit(Surv(time, status) ~ breach.ty, data = mod.data)
plot.data <- tidy(fit) %>%
  rowwise() %>%
  mutate(`Breach Type` = str_split(strata, "=")[[1]][2])

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate, color = `Breach Type`), 
            size = 1) +
  ggtitle("Risk of Breach by Breach Type") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw()

ggsave(paste0(fig.path, "Breach_Type.png"))


fit <- survfit(Surv(time, status) ~ Targeted, data = mod.data)
plot.data <- tidy(fit) %>%
  rowwise() %>%
  mutate(Targeted = str_split(strata, "=")[[1]][2]) %>%
  filter(Targeted != "(Missing)")

ggplot(plot.data) +
  geom_step(aes(x = time/365.25, y = 1 - estimate, color = Targeted), 
            size = 1) +
  ggtitle("Risk of Breach by Target Type") +
  xlab("Years") +
  ylab("Probability of Breach") +
  theme_bw()

ggsave(paste0(fig.path, "Targeted.png"))

lapply(levels(rbs$Business.type, function (x) {suvexp}))

#Misc
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
#------------------------------------------------------------------------------

#Survival Table
mod.data <- mod.data %>%
              mutate(company.rating = cut(Organization.rating, 
                                          breaks = seq(0.5, 5.5, 0.5),
                                          right = FALSE,
                                          labels = seq(0.5, 5, 0.5)), 
                     severity.score = cut(Severity.score, 
                                          breaks = seq(0, 10.5, 0.5),
                                          right = FALSE,
                                          labels = seq(0, 10, 0.5)),
                     comp.rate = factor(round(Organization.rating)), 
                     sev.score = factor(round(Severity.score)))

table(mod.data$comp.rate, mod.data$Business.type)

fit <- survfit(Surv(time, status) ~ comp.rate + Business.type, data = mod.data)

risk.60 <- mod.data %>%
    group_by(comp.rate, Business.type) %>%
    summarise(time = max(time)) %>%
    filter(time >= 60) %>%
    ungroup() %>%
    mutate(risk_60 = 1 - summary(fit, 60)$surv, 
           lcl_60 = 1 - summary(fit, 60)$upper, 
           ucl_60 = 1 - summary(fit, 60)$lower) %>%
    dplyr::select(-time)

risk.120 <- mod.data %>%
  group_by(comp.rate, Business.type) %>%
  summarise(time = max(time)) %>%
  filter(time >= 120) %>%
  ungroup() %>%
  mutate(risk_120 = 1 - summary(fit, 120)$surv, 
         lcl_120 = 1 - summary(fit, 120)$upper, 
         ucl_120 = 1 - summary(fit, 120)$lower) %>%
  dplyr::select(-time)

risk <- full_join(risk.60, risk.120, by = c("comp.rate" = "comp.rate", "Business.type" = "Business.type")) %>%
          rename(Business_type = Business.type, Company_rating = comp.rate) %>%
          mutate(Business_type = factor(Business_type), 
                 Company_rating = factor(Company_rating)) %>%
          complete(Business_type, Company_rating)

save(risk, file = "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/risk.Rda")
write_csv(risk, "C:/Users/Amanda/Documents/Documents/Analytics Adventures/Risk_Based_Security/data/risk.csv")
