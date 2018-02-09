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

summary(fit)$table[, "median"]

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