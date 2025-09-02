#...............................................................................
# Outsourcing and time-related dimensions of job quality
# Álvaro F. Junquera
#...............................................................................

library(readxl)

library(tidyverse)
library(survey)
library(janitor)

library(openxlsx)

library(pmsampsize)
library(marginaleffects)
library(modelsummary)

# 0. Uploading ------------
epa_18 <- readRDS("data/processed/epa_18.RDS")
epa_20 <- readRDS("data/processed/epa_20.RDS")

epa_18 <- epa_18 %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), haven::zap_labels))

epa_20 <- epa_20 %>%
  mutate(across(where(~ inherits(.x, "haven_labelled")), haven::zap_labels))

macrocen21 <- read_excel("data/raw/Censo_2021.xlsx")
macrocen21 <- macrocen21[-1, -2]

macrocen11fila <- read_excel("data/raw/Censo_2011_pfila.xlsx", na = "*")

ecv17 <- read_delim("data/raw/ECV_Tp_2017.csv")

# 1. Declaring sampling weights (not the complex design) --------
e18design <- svydesign(ids = ~1,
                       weights = ~sampweight,
                       data = epa_18)

e20design <- svydesign(ids = ~1,
                       weights = ~sampweight,
                       data = epa_20)

# 2. Univariate and bivariate analyses ---------------
## 2.1. Prevalence with LFS (Table 1) ---------
# This is known as "ratios for subpopulation estimates" in the survey literature

### Cleaners -------
### 2018
svyratio(numerator = ~ I(ACT == "812" & OCUP == "921"), # num: "outsourced" & cleaners
         denominator = ~ I(OCUP == "921"), # den: cleaners
         design = e18design) # Remember: Pr[A|B] = Pr[A & B] / Pr[B]

### 2020
svyratio(numerator = ~ I(ACT == "812" & OCUP == "921"), # num: "outsourced" & cleaners
         denominator = ~ I(OCUP == "921"), # den: cleaners
         design = e20design) # Remember: Pr[A|B] = Pr[A & B] / Pr[B]

### Security guards ---------
### 2018
svyratio(numerator = ~ I(ACT %in% c("801", "802", "803") & OCUP == "594"), # num: "outsourced" & security workers
         denominator = ~ I(OCUP == "594"), # den: security workers
         design = e18design)

### 2020
svyratio(numerator = ~ I(ACT %in% c("801", "802", "803") & OCUP == "594"), # num: "outsourced" & security workers
         denominator = ~ I(OCUP == "594"), # den: security workers
         design = e20design)

### Gardeners ------------
### 2018
svyratio(numerator = ~ I(ACT == "813" & OCUP == "612"), # num: "outsourced" & gardening workers
         denominator = ~ I(OCUP == "612"), # den: gardening workers
         design = e18design)

### 2020
svyratio(numerator = ~ I(ACT == "813" & OCUP == "612"), # num: "outsourced" & gardening workers
         denominator = ~ I(OCUP == "612"), # den: gardening workers
         design = e20design)


## 2.2. Prevalence with census data (Table 1) --------
### Census 2011
colnames(macrocen11fila)[1] <- "ocu3d"

# Cleaners
macrocen11fila$`812 - Actividades de limpieza`[which(macrocen11fila$ocu3d == "921 - Personal de limpieza de oficinas, hoteles y otros establecimientos similares")]

# Security guards
macrocen11fila$`801 - Actividades de seguridad privada`[which(macrocen11fila$ocu3d == "594 - Personal de seguridad privado")]
macrocen11fila$`802 - Servicios de sistemas de seguridad`[which(macrocen11fila$ocu3d == "594 - Personal de seguridad privado")]
macrocen11fila$`803 - Actividades de investigación`[which(macrocen11fila$ocu3d == "594 - Personal de seguridad privado")]

27.62 + 0.71 + 0.2

## Gardeners
macrocen11fila$`813 - Actividades de jardinería`[which(macrocen11fila$ocu3d == "612 - Trabajadores cualificados en huertas, invernaderos, viveros y jardines")]


### Census 2021
macrocen21_n <- map_df(macrocen21[, 2:274], .f = ~ as.numeric(.x)) %>% as.matrix()

frcen21 <- prop.table(macrocen21_n, margin = 1)

frcen21n <- as.data.frame(frcen21) %>% mutate(ocu3d = macrocen21$`Actividad establecimiento a 3 dígitos CNAE-09`) %>%
  relocate(ocu3d)

## Cleaning
frcen21n$`812 - Actividades de limpieza`[which(frcen21n$ocu3d == "921 - Personal de limpieza de oficinas, hoteles y otros establecimientos similares")] * 100

## Gardening
frcen21n$`813 - Actividades de jardinería`[which(frcen21n$ocu3d == "612 - Trabajadores cualificados en huertas, invernaderos, viveros y jardines")] * 100

## 2.3. Contingency tables (Table 2) ----------
## Props by row
psex <- round(prop.table(svytable(~sex + outsourced, e18design), margin = 1) * 100, 2)
svychisq(~sex + outsourced, e18design)

pciti <- round(prop.table(svytable(~citizenship + outsourced, e18design), margin = 1) * 100, 2)
svychisq(~citizenship + outsourced, e18design)

pedu <- round(prop.table(svytable(~leducation + outsourced, e18design), margin = 1) * 100, 2)
svychisq(~leducation + outsourced, e18design)

pbigregion <- round(prop.table(svytable(~bigregion + outsourced, e18design), margin = 1) * 100, 2)
svychisq(~bigregion + outsourced, e18design)

## Export
wb_contigency <- createWorkbook()

addWorksheet(wb_contigency, "sex")
writeData(wb_contigency, sheet = 1, x = psex, startCol = 1, startRow = 1)

addWorksheet(wb_contigency, "citizenship")
writeData(wb_contigency, sheet = 2, x = pciti, startCol = 1, startRow = 1)

addWorksheet(wb_contigency, "leducation")
writeData(wb_contigency, sheet = 3, x = pedu, startCol = 1, startRow = 1)

addWorksheet(wb_contigency, "bigregion")
writeData(wb_contigency, sheet = 4, x = pbigregion, startCol = 1, startRow = 1)

saveWorkbook(wb_contigency, "results/table2.xlsx")

# 3. Multivariate analyses ----------

## 3.1. Modelling for cleaners --------------
### Available degrees of freedom ---------

### Candidate covariates

# We depart from a basic set of 5 regression parameters from
# being outsourced (1), sex (1), nationality (2), age (1)
# and we would like to add...

epa_18 %>% filter(OCUP == "921") %>%
  tabyl(leducation) # 5 regression parameters

epa_18 %>% filter(OCUP == "921") %>%
  tabyl(bigregion) # 6 regression parameters

### Outcomes
epa_18 %>% filter(OCUP == "921") %>%
  tabyl(tempcontract)

# 1- Estimate log-likelihood for the null model
nullmodel_tc <- svyglm(tempcontract ~ 1,
                       design = e20design, family = quasibinomial(),
                       na.action = na.exclude,
                       subset = OCUP == "921")

nullmodel_pt <- svyglm(parttime ~ 1,
                       design = e20design, family = quasibinomial(),
                       na.action = na.exclude,
                       subset = OCUP == "921")

ecv17_cno92 <- subset(ecv17, ecv17$PL051 == "91" & ecv17$PL031 %in% c(" 1", " 2", " 3", " 4")) # ISCO08 code. Subsample of (currently) employed as cleaners

ecv17design_cl <- svydesign(ids = ~1,
                            weights = ~PB040,
                            data = ecv17_cno92)

nullmodel_pt_ecv <- svyglm(I(PL031 == " 2") ~ 1,
                           design = ecv17design_cl, family = quasibinomial(),
                           na.action = na.exclude)

# 2- Estimate apparent R^2 of Cox-Snell from a R^2 of Nagelkerke = 0.15
rsq_csapp_tc <- 0.15 * (1 - exp(-nullmodel_tc$null.deviance / sum(epa_20$OCUP == "921")))
#svh <- 1 + (24 / sum(epa_18$OCUP == "921") * log(1 - rsq_csapp_tc)) #svh
#rsq_csadj_tc <- rsq_csapp_tc*svh

rsq_csapp_pt <- 0.15 * (1 - exp(-nullmodel_pt$null.deviance / sum(epa_20$OCUP == "921")))
rsq_csapp_pt_ecv <- 0.15 * (1 - exp(-nullmodel_pt_ecv$null.deviance / nrow(ecv17_cno92)))



# 3- Estimate prevalence (or features of the population) in other sample
epa_20 %>% filter(OCUP == "921") %>%
  tabyl(tempcontract)

epa_20 %>% filter(OCUP == "921") %>%
  tabyl(parttime) # ERTES?

prop.table(xtabs(PB040 ~ I(PL031 == " 2"), data = ecv17_cno92)) * 100 # PB040 is the sampling weight

epa_20 %>% filter(OCUP == "921") %>%
  summarise(sd_gs = sd(goodschedule, na.rm = T),
            mean_gs = mean(goodschedule, na.rm = T))

# 4- Calculate required sample size
# temporary
pmsampsize(type = "b", csrsquared = rsq_csapp_tc, parameters = 16, prevalence = 0.2766) #!!!!
pmsampsize(type = "b", csrsquared = rsq_csapp_tc, parameters = 15, prevalence = 0.2766) #!!!!
pmsampsize(type = "b", nagrsquared = 0.15, parameters = 16, prevalence = 0.2766) #!!!!

epa_18 %>% filter(OCUP == "921") %>%
  tabyl(tempcontract) # number of events approx >= minimum size of required events

# parttime
## census
pmsampsize(type = "b", csrsquared = rsq_csapp_pt, parameters = 16, prevalence = 0.458) # parttime
pmsampsize(type = "b", csrsquared = rsq_csapp_pt, parameters = 15, prevalence = 0.458) # parttime

## ecv
pmsampsize(type = "b", csrsquared = rsq_csapp_pt_ecv, parameters = 16, prevalence = 0.3837) # parttime
pmsampsize(type = "b", csrsquared = rsq_csapp_pt_ecv, parameters = 15, prevalence = 0.3837) # parttime

epa_18 %>% filter(OCUP == "921") %>%
  tabyl(parttime) # number of events > minimum size of required events

# good schedule
pmsampsize(type="c", rsquared = 0.15, intercept = 3.33, sd = 0.944, parameters = 16) #!!!!
pmsampsize(type="c", rsquared = 0.15, intercept = 3.33, sd = 0.944, parameters = 15) #!!!!

epa_18 %>%
  filter(if_all(c(outsourced, sex, citizenship, leducation, bigregion, age), ~ !is.na(.))) %>%
  tally() # number of observations > minimum size of required observations


### Estimation ------------
### A. Temporary contract
m1 <- svyglm(tempcontract ~ outsourced + sex + citizenship + leducation + bigregion + age,
             design = e18design, family = quasibinomial(),
             subset = OCUP == "921")

a_mfx_m1 <- avg_slopes(m1)
psrsq(m1, method = "Nagelkerke")

### B. Part-time
m_pt_1 <- svyglm(parttime ~ outsourced + sex + citizenship + leducation + bigregion + age,
                 design = e18design, family = quasibinomial(),
                 subset = OCUP == "921")

a_mfx_m_pt_1 <- avg_slopes(m_pt_1)
psrsq(m_pt_1, method = "Nagelkerke")


### C. Good schedule
m_gs_1 <- svyglm(goodschedule ~ outsourced + sex + citizenship + leducation + bigregion + age,
                 design = e18design, family = gaussian(),
                 subset = OCUP == "921")

summary(m_gs_1)

m_gs_null <- svyglm(goodschedule ~ 1,
                    design = e18design, family = gaussian(),
                    subset = OCUP == "921")

summary(m_gs_null)

round(1 - 0.8883011/0.9836956, 3) # r-squared


m_gsA_1 <- svyglm(night ~ outsourced + sex + citizenship + leducation + bigregion + age,
                  design = e18design, family = gaussian(),
                  subset = OCUP == "921")

summary(m_gsA_1)

m_gsB_1 <- svyglm(evening ~ outsourced + sex + citizenship + leducation + bigregion + age,
                  design = e18design, family = gaussian(),
                  subset = OCUP == "921")

summary(m_gsB_1)

m_gsC_1 <- svyglm(saturdays ~ outsourced + sex + citizenship + leducation + bigregion + age,
                  design = e18design, family = gaussian(),
                  subset = OCUP == "921")

summary(m_gsC_1)

m_gsD_1 <- svyglm(sundays ~ outsourced + sex + citizenship + leducation + bigregion + age,
                  design = e18design, family = gaussian(),
                  subset = OCUP == "921")

summary(m_gsD_1)


### Exporting ------
options(modelsummary_factory_default = "gt")

modelsummary(list("Temporary" = a_mfx_m1,
                  "Part-time" = a_mfx_m_pt_1,
                  "Good schedule" = avg_slopes(m_gs_1)),
             shape = term + contrast ~ model,
             stars = c('*' = .1, '**' = .05, '***' = 0.01),
             gof_omit = "R2|R2 Adj|Log.Lik.|F|RMSE",
             coef_map = c("outsourced" = "Outsourced",
                          "sex" = "Sex",
                          "citizenship" = "Citizenship",
                          "age" = "Age",
                          "leducation" = "Level of education",
                          "bigregion" = "NUTS-1 region"),
             output = "results/table3.xlsx")

### Relative importance ------------
#### Through Cramér's V
svycramerV <- function(formula,design,...){
  tbl<-svytable(formula,design,...)
  chisq<-chisq.test(tbl, correct=FALSE)$statistic
  N<-sum(tbl)
  V<-chisq/N/min(dim(tbl)-1)
  names(V)<-"V"
  V
}

# Temporary
temp_cramerCL <- c(
  svycramerV(~ tempcontract + outsourced, design = subset(e18design, OCUP == "921")),
  svycramerV(~ tempcontract + sex, design = subset(e18design, OCUP == "921")),
  svycramerV(~ tempcontract + citizenship, design = subset(e18design, OCUP == "921")),
  svycramerV(~ tempcontract + age10, design = subset(e18design, OCUP == "921")),
  svycramerV(~ tempcontract + leducation, design = subset(e18design, OCUP == "921")),
  svycramerV(~ tempcontract + bigregion, design = subset(e18design, OCUP == "921"))
)

# Part-time
part_cramerCL <- c(
  svycramerV(~ parttime + outsourced, design = subset(e18design, OCUP == "921")),
  svycramerV(~ parttime + sex, design = subset(e18design, OCUP == "921")),
  svycramerV(~ parttime + citizenship, design = subset(e18design, OCUP == "921")),
  svycramerV(~ parttime + age10, design = subset(e18design, OCUP == "921")),
  svycramerV(~ parttime + leducation, design = subset(e18design, OCUP == "921")),
  svycramerV(~ parttime + bigregion, design = subset(e18design, OCUP == "921"))
)


# Good schedule
good_cramerCL <- c(
  svycramerV(~ goodscheduleF + outsourced, design = subset(e18design, OCUP == "921")),
  svycramerV(~ goodscheduleF + sex, design = subset(e18design, OCUP == "921")),
  svycramerV(~ goodscheduleF + citizenship, design = subset(e18design, OCUP == "921")),
  svycramerV(~ goodscheduleF + age10, design = subset(e18design, OCUP == "921")),
  svycramerV(~ goodscheduleF + leducation, design = subset(e18design, OCUP == "921")),
  svycramerV(~ goodscheduleF + bigregion, design = subset(e18design, OCUP == "921"))
)

cramerCL <- data.frame(covariate = c("Outsourced", "Sex", "Nationality", "Age", "Level of education", "NUTS-1 region"),
                       temporary = round(temp_cramerCL, 3),
                       parttime = round(part_cramerCL, 3),
                       good_schedule = round(good_cramerCL, 3))

openxlsx::write.xlsx(cramerCL, 'results/table4.xlsx')


## 3.2. Modelling for security guards AND gardeners --------------
epa_18 %>%
  filter(OCUP %in% c("594", "612")) %>%
  tabyl(tempcontract)

epa_18 %>%
  filter(OCUP %in% c("594", "612")) %>%
  tabyl(parttime)

epa_18 %>%
  filter(OCUP %in% c("594", "612")) %>%
  tabyl(goodschedule)

### Available degrees of freedom ---------

# The low number of events for this sample allows a smaller number of
# regression parameters. We will try with 4 parameters: outourced (1),
# sex (1), nationality (1), and age (1)

# 1- Estimate log-likelihood for the null model
nullmodel_tc_sec <- svyglm(tempcontract ~ 1,
                           design = e20design, family = quasibinomial(),
                           na.action = na.exclude,
                           subset = OCUP %in% c("594", "612"))

nullmodel_pt_sec <- svyglm(parttime ~ 1,
                           design = e20design, family = quasibinomial(),
                           na.action = na.exclude,
                           subset = OCUP %in% c("594", "612"))

nullmodel_gs_sec <- svyglm(goodschedule ~ 1,
                           design = e20design, family = gaussian(),
                           na.action = na.exclude,
                           subset = OCUP %in% c("594", "612"))

# 2- Estimate apparent R^2 of Cox-Snell from a R^2 of Nagelkerke = 0.15
rsq_csapp_tc_sec <- 0.15 * (1 - exp(-nullmodel_tc_sec$null.deviance / sum(epa_20$OCUP %in% c("594", "612"))))
#svh <- 1 + (24 / sum(epa_18$OCUP == "921") * log(1 - rsq_csapp_tc)) #svh
#rsq_csadj_tc <- rsq_csapp_tc*svh

rsq_csapp_pt_sec <- 0.15 * (1 - exp(-nullmodel_pt_sec$null.deviance / sum(epa_20$OCUP %in% c("594", "612"))))


# 3- Estimate prevalence (or features of the population) in other sample
epa_20 %>% filter(OCUP %in% c("594", "612")) %>%
  tabyl(tempcontract)

epa_20 %>% filter(OCUP %in% c("594", "612")) %>%
  tabyl(parttime)

epa_20 %>% filter(OCUP %in% c("594", "612")) %>%
  summarise(sd_gs = sd(goodschedule, na.rm = T),
            mean_gs = mean(goodschedule, na.rm = T))

# 4- Calculate required sample size
pmsampsize(type="b", csrsquared = rsq_csapp_tc_sec, parameters = 4, prevalence = 0.1862) # temporary

epa_18 %>% filter(OCUP %in% c("594", "612")) %>%
  tabyl(tempcontract) # number of events >= minimum size of required events

pmsampsize(type="b", csrsquared = rsq_csapp_pt_sec, parameters = 4, prevalence = 0.0714) # part-time

epa_18 %>% filter(OCUP %in% c("594", "612")) %>%
  tabyl(parttime) # number of events approx >= minimum size of required events

pmsampsize(type="c", rsquared = 0.15, intercept = 2.45, sd = 1.44, parameters = 6) # good schedule

epa_20 %>%
  filter(if_all(c(outsourced, sex, spanish, age), ~ !is.na(.))) %>%
  tally() # number of observations > minimum size of required observations


### Estimation ---------

### A. Temporary contract
m1_gg <- svyglm(tempcontract ~ outsourced + sex + spanish + age,
                design = e18design, family = quasibinomial(),
                subset = OCUP %in% c("594", "612"))

a_mfx_m1gg <- avg_slopes(m1_gg)

psrsq(m1_gg, method = "Nagelkerke")

### B. Part-time
m_pt_1gg <- svyglm(parttime ~ outsourced + sex + spanish + age,
                   design = e18design, family = quasibinomial(),
                   subset = OCUP %in% c("594", "612"))

a_mfx_m_pt_1gg <- avg_slopes(m_pt_1gg)
psrsq(m_pt_1gg, method = "Nagelkerke")

### C. Good schedule
m_gs_1gg <- svyglm(goodschedule ~ outsourced + OCUP + spanish + sex + age,
                   design = e18design, family = gaussian(),
                   subset = OCUP %in% c("594", "612"))

summary(m_gs_1gg)
avg_slopes(m_gs_1gg)

m_gs_1gg_null <- svyglm(goodschedule ~ 1,
                        design = e18design, family = gaussian(),
                        subset = OCUP %in% c("594", "612"))

summary(m_gs_1gg_null)

round(1 - 1.210201/1.770033, 3)

### Exporting ------
options(modelsummary_factory_default = "gt")

modelsummary(list("Temporary" = a_mfx_m1gg,
                  "Part-time" = a_mfx_m_pt_1gg,
                  "Good schedule" = avg_slopes(m_gs_1gg)),
             shape = term + contrast ~ model,
             stars = c('*' = .1, '**' = .05, '***' = 0.01),
             gof_omit = "R2|R2 Adj|Log.Lik.|F|RMSE",
             coef_map = c("outsourced" = "Outsourced",
                          "sex" = "Sex",
                          "spanish" = "Spanish",
                          "age" = "Age"),
             output = "results/table5.xlsx")

### Relative importance -----------

# Temporary
temp_cramerGG <- c(
  svycramerV(~ tempcontract + outsourced, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ tempcontract + sex, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ tempcontract + citizenship, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ tempcontract + age10, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ tempcontract + leducation, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ tempcontract + bigregion, design = subset(e18design, OCUP %in% c("594", "612")))
)

# Part-time
part_cramerGG <- c(
  svycramerV(~ parttime + outsourced, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ parttime + sex, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ parttime + citizenship, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ parttime + age10, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ parttime + leducation, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ parttime + bigregion, design = subset(e18design, OCUP %in% c("594", "612")))
)


# Good schedule
good_cramerGG <- c(
  svycramerV(~ goodscheduleF + outsourced, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ goodscheduleF + sex, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ goodscheduleF + citizenship, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ goodscheduleF + age10, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ goodscheduleF + leducation, design = subset(e18design, OCUP %in% c("594", "612"))),
  svycramerV(~ goodscheduleF + bigregion, design = subset(e18design, OCUP %in% c("594", "612")))
)

cramerGG <- data.frame(covariate = c("Outsourced", "Sex", "Nationality", "Age", "Level of education", "NUTS-1 region"),
                       temporary = round(temp_cramerGG, 3),
                       parttime = round(part_cramerGG, 3),
                       good_schedule = round(good_cramerGG, 3))

openxlsx::write.xlsx(cramerGG, 'results/table6.xlsx')
