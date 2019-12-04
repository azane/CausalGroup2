# packages
library(tidyverse)
library(lubridate)

# dataset
dat <- readRDS("data/MAStatePatrol.rds")
colnames(dat)
table(year(dat$date))

datBos <-  dat %>% 
  filter(location == 'BOSTON') %>% 
  filter(year(date) == 2015 ) %>% 
  filter(subject_race != 'unknown' & subject_race != 'other') %>% # positivity assumption
  filter(vehicle_type != 'Motorcycle' & vehicle_type != 'Trailer') %>% # positicity assumption
  filter(!is.na(subject_age) & !is.na(subject_sex)) %>% 
  select(subject_age,
         subject_race,
         subject_sex,
         arrest_made,
         citation_issued,
         warning_issued,
         outcome,
         contraband_found,
         frisk_performed,
         search_conducted,
         search_basis,
         reason_for_stop,
         vehicle_type,
         raw_Race)

summary(datBos$search_conducted)
summary(datBos$contraband_found)
summary(datBos$subject_age)
summary(datBos$subject_race)
summary(datBos$subject_sex)
table(datBos$vehicle_type)
sum(is.na(datBos$vehicle_type))

# D : search_conductred
# Y* : Underlying contraband status
# Y : contraband_found
# W1 : age
# W2 : race
# W3 : sex
# W4 : vehicle_type

# Assume all bakground variables are independent

# Structural equations:
# W1 = fw1(Uw1)
# W2 = fw2(Uw2)
# W3 = fw3(Uw3)
# W4 = fw4(W1,W2, W3, Uw4)
# D = fd(W1, W2, W3, W4, Ud)
# Y* = fy*(W1, W2, W3, W4, Uy*)
# Y = D*(Y*)

# Causal Paramter : P*(Y* = 1) = P*(Y = 1|D = 1)

# Identifiability : Y indeoendent of D given W's

# Positivity assumptions maybe violated (to be check)

# G-computation
# (1) NPMLE
# (2) logistic model : E(Y|D, W) ~ all Ws
# (3) may inlcude some interaction.
datBos.searched <-  datBos %>% filter(search_conducted == TRUE)
fit.gcomp <- glm(contraband_found ~ 
                   subject_age + 
                   as.factor(subject_race) + 
                   subject_sex + 
                   as.factor(vehicle_type),
                 family = 'binomial', data = datBos.searched)
datBos.intervene <- datBos %>% mutate(search_conducted = TRUE)
EY.gcomp <- predict(fit.gcomp, newdata = datBos.intervene, type = 'response')
est.gcomp <- mean(EY.gcomp)
est.gcomp

# IPTW
# P0(A = 1|W) = logit^(-1){B0 + B1W1 + B2W2 + B2W3 + B4W4}
fit.prob.D <- glm(search_conducted ~ 
                    subject_age + 
                    as.factor(subject_race) + 
                    subject_sex + 
                    as.factor(vehicle_type),
                  family = 'binomial', data = datBos)
prob.D1 <- predict(fit.prob.D, type = 'response')
summary(prob.D1)
# calculate weights
wt1 <- as.numeric(datBos$search_conducted == 1)/prob.D1
summary(wt1)
est.IPTW <- mean(wt1*datBos$contraband_found, na.rm = TRUE)
est.IPTW # too large

# Stabelized IPTW
wt.mean <- mean(wt1[!is.na(datBos$contraband_found)])
est.sIPTW <- mean(wt1*datBos$contraband_found, na.rm = TRUE)/wt.mean

# Super learner

# TMLE