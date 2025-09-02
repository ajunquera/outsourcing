#...............................................................................
# Outsourcing and time-related dimensions of job quality
# Álvaro F. Junquera
#...............................................................................

library(haven)
library(tidyverse)

# 1. Uploading ---------
epa18 <- read_sav("data/raw/epa2018.sav")
epa20 <- read_sav("data/raw/epa2020.sav")

# 2. Constructing variables -----------
## Correct reading of sampling weights and CNO at 2 digits ------
epa18 <- epa18 %>%
  mutate(sampweight = FACTOREL / 100)

epa20 <- epa20 %>%
  mutate(sampweight = FACTOREL / 100)

## CNO 2-digits
epa18 <- epa18 %>%
  mutate(OCUP2 = str_sub(OCUP, 1, 2))

epa20 <- epa20 %>%
  mutate(OCUP2 = str_sub(OCUP, 1, 2))

## Outcomes: Temporary contract, part-time job and scheduling index ----------
### 2018
epa18 <- epa18 %>%
  mutate(tempcontract = case_when(DUCON1 == "1" ~ "No",
                                  DUCON1 == "6" ~ "Yes",
                                  TRUE ~ NA),
         parttime = case_when(PARCO1 == "1" ~ "No",
                              PARCO1 == "6" ~ "Yes",
                              TRUE ~ NA),
         night = case_when(NOCHE == "3" ~ 1,
                           NOCHE == "1" ~ 0.5,
                           NOCHE == "2" ~ 0,
                           is.na(NOCHE) ~ NA),
         evening = case_when(TARDE == "3" ~ 1,
                             TARDE == "1" ~ 0.5,
                             TARDE == "2" ~ 0,
                             is.na(TARDE) ~ NA),
         sundays = case_when(DOMING == "3" ~ 1,
                             DOMING == "1" ~ 0.5,
                             DOMING == "2" ~ 0,
                             is.na(DOMING) ~ NA),
         saturdays = case_when(SABAD == "3" ~ 1,
                               SABAD == "1" ~ 0.5,
                               SABAD == "2" ~ 0,
                               is.na(SABAD) ~ NA))

epa18$tempcontract <- as.factor(epa18$tempcontract)
epa18$parttime <- as.factor(epa18$parttime)
epa18$goodschedule <- epa18$night + epa18$evening + epa18$saturdays + epa18$sundays

epa18 <- epa18 %>%
  mutate(goodscheduleF = case_when(goodschedule %in% c(0, 0.5, 1) ~ "0to1",
                                   goodschedule %in% c(1.5, 2) ~ "1.5to2",
                                   goodschedule %in% c(2.5, 3) ~ "2.5to3",
                                   goodschedule %in% c(3.5, 4) ~ "3.5to4",
                                   T ~ NA))

epa18$goodscheduleF <- as.factor(epa18$goodscheduleF)

### 2020
epa20 <- epa20 %>%
  mutate(tempcontract = case_when(DUCON1 == "1" ~ "No",
                                  DUCON1 == "6" ~ "Yes",
                                  TRUE ~ NA),
         parttime = case_when(PARCO1 == "1" ~ "No",
                              PARCO1 == "6" ~ "Yes",
                              TRUE ~ NA),
         night = case_when(NOCHE == "3" ~ 1,
                           NOCHE == "1" ~ 0.5,
                           NOCHE == "2" ~ 0,
                           is.na(NOCHE) ~ NA),
         evening = case_when(TARDE == "3" ~ 1,
                             TARDE == "1" ~ 0.5,
                             TARDE == "2" ~ 0,
                             is.na(TARDE) ~ NA),
         sundays = case_when(DOMING == "3" ~ 1,
                             DOMING == "1" ~ 0.5,
                             DOMING == "2" ~ 0,
                             is.na(DOMING) ~ NA),
         saturdays = case_when(SABAD == "3" ~ 1,
                               SABAD == "1" ~ 0.5,
                               SABAD == "2" ~ 0,
                               is.na(SABAD) ~ NA))

epa20$tempcontract <- as.factor(epa20$tempcontract)
epa20$parttime <- as.factor(epa20$parttime)
epa20$goodschedule <- epa20$night + epa20$evening + epa20$saturdays + epa20$sundays

epa20 <- epa20 %>%
  mutate(goodscheduleF = case_when(goodschedule %in% c(0, 0.5, 1) ~ "0to1",
                                   goodschedule %in% c(1.5, 2) ~ "1.5to2",
                                   goodschedule %in% c(2.5, 3) ~ "2.5to3",
                                   goodschedule %in% c(3.5, 4) ~ "3.5to4",
                                   T ~ NA))

epa20$goodscheduleF <- as.factor(epa20$goodscheduleF)

## Predictors -------------
EUwoES <- c("102", "105", "107", "110", "111", "112", "113", "114", "115", "116", "117",
            "119", "120", "121", "123", "124", "126", "127", "129", "133", "134", "135",
            "136", "137", "138", "139", "143")

### 2018
epa18 <- epa18 %>%
  mutate(sex = case_when(SEXO1 == 1 ~ "Man",
                         SEXO1 == 6 ~ "Woman",
                         T ~ NA),
         age = EDAD1,
         citizenship = case_when(EXTNA1 == "NA" ~ "Spanish",
                                 EXTNA1 %in% EUwoES ~ "EU",
                                 T ~ "Foreign"),
         spanish = if_else(citizenship == "Spanish", T, F),
         leducation = case_when(NFORMA %in% c("01", "02", "10") ~ "1_or_less",
                                NFORMA %in% c("21", "22", "23", "38") ~ "2_lower",
                                NFORMA %in% c("32") ~ "2_upper_general",
                                NFORMA %in% c("24", "33", "34", "35") ~ "2_upper_vocational",
                                NFORMA %in% c("41", "51", "52") ~ "3_vocational",
                                NFORMA %in% c("61", "62", "63", "71", "72", "73", "74", "75", "81") ~ "3_general",
                                T ~ NA),
         bigregion = case_when(CCAA %in% c("12", "3", "6") ~ "North_West",
                               CCAA %in% c("16", "15", "17", "2") ~ "North_East",
                               CCAA %in% c("13") ~ "Madrid",
                               CCAA %in% c("7", "8", "11") ~ "Centre",
                               CCAA %in% c("9", "10", "4") ~ "East",
                               CCAA %in% c("1", "14", "51", "52") ~ "South",
                               CCAA %in% c("5") ~ "Canarias"),
         outsourced = case_when(OCUP == "921" & ACT == "812" ~ "Yes",
                                OCUP == "921" ~ "No",
                                ACT %in% c("801", "802", "803") & OCUP == "594" ~ "Yes",
                                OCUP == "594" ~ "No",
                                ACT == "813" & OCUP == "612" ~ "Yes",
                                OCUP == "612" ~ "No",
                                TRUE ~ NA),
         age10 = case_when(EDAD1 %in% 16:20 ~ "16to20",
                           EDAD1 %in% 21:30 ~ "21to30",
                           EDAD1 %in% 31:40 ~ "31to40",
                           EDAD1 %in% 41:50 ~ "41to50",
                           EDAD1 %in% 51:60 ~ "51to60",
                           EDAD1 %in% 61:100 ~ "61to100",
                           T ~ NA)) ## 0-1 (primaria o menos), 2 (secundaria), 32 (pos-secundaria generalista), 24+33[rest] (pos-secundaria técnica), 5 (terciaria técnica), 6-7 (terciaria generalista)


epa18$id <- epa18$V1
epa18$sex <- as.factor(epa18$sex)
epa18$citizenship <- as.factor(epa18$citizenship)
epa18$spanish <- as.factor(epa18$spanish)
epa18$leducation <- as.factor(epa18$leducation)
epa18$bigregion <- as.factor(epa18$bigregion)
epa18$bigregion <- factor(epa18$bigregion,
                          levels = c("Madrid", "Canarias", "Centre", "East", "North_East", "North_West", "South"))


epa18$outsourced <- as.factor(epa18$outsourced)
epa18$canum <- as.factor(epa18$CCAA)
epa18$age10 <- as.factor(epa18$age10)


### 2020
epa20 <- epa20 %>%
  mutate(sex = case_when(SEXO1 == 1 ~ "Man",
                         SEXO1 == 6 ~ "Woman",
                         T ~ NA),
         age = EDAD1,
         citizenship = case_when(EXTNA1 == "NA" ~ "Spanish",
                                 EXTNA1 %in% EUwoES ~ "EU",
                                 T ~ "Foreign"),
         spanish = if_else(citizenship == "Spanish", T, F),
         leducation = case_when(NFORMA %in% c("01", "02", "10") ~ "1_or_less",
                                NFORMA %in% c("21", "22", "23", "38") ~ "2_lower",
                                NFORMA %in% c("32") ~ "2_upper_general",
                                NFORMA %in% c("24", "33", "34", "35") ~ "2_upper_vocational",
                                NFORMA %in% c("41", "51", "52") ~ "3_vocational",
                                NFORMA %in% c("61", "62", "63", "71", "72", "73", "74", "75", "81") ~ "3_general",
                                T ~ NA),
         bigregion = case_when(CCAA %in% c("12", "3", "6") ~ "North_West",
                               CCAA %in% c("16", "15", "17", "2") ~ "North_East",
                               CCAA %in% c("13") ~ "Madrid",
                               CCAA %in% c("7", "8", "11") ~ "Centre",
                               CCAA %in% c("9", "10", "4") ~ "East",
                               CCAA %in% c("1", "14", "51", "52") ~ "South",
                               CCAA %in% c("5") ~ "Canarias"),
         outsourced = case_when(OCUP == "921" & ACT == "812" ~ "Yes",
                                OCUP == "921" ~ "No",
                                ACT %in% c("801", "802", "803") & OCUP == "594" ~ "Yes",
                                OCUP == "594" ~ "No",
                                ACT == "813" & OCUP == "612" ~ "Yes",
                                OCUP == "612" ~ "No",
                                TRUE ~ NA),
         age10 = case_when(EDAD1 %in% 16:20 ~ "16to20",
                           EDAD1 %in% 21:30 ~ "21to30",
                           EDAD1 %in% 31:40 ~ "31to40",
                           EDAD1 %in% 41:50 ~ "41to50",
                           EDAD1 %in% 51:60 ~ "51to60",
                           EDAD1 %in% 61:100 ~ "61to100",
                           T ~ NA)) ## 0-1 (primaria o menos), 2 (secundaria), 32 (pos-secundaria generalista), 24+33[rest] (pos-secundaria técnica), 5 (terciaria técnica), 6-7 (terciaria generalista)

epa20$id <- epa20$V1
epa20$sex <- as.factor(epa20$sex)
epa20$citizenship <- as.factor(epa20$citizenship)
epa20$spanish <- as.factor(epa20$spanish)
epa20$leducation <- as.factor(epa20$leducation)
epa20$bigregion <- as.factor(epa20$bigregion)
epa20$bigregion <- factor(epa20$bigregion,
                          levels = c("Madrid", "Canarias", "Centre", "East", "North_East", "North_West", "South"))

epa20$outsourced <- as.factor(epa20$outsourced)
epa20$canum <- as.factor(epa20$CCAA)
epa20$age10 <- as.factor(epa20$age10)


# 3. Saving ----------
epa_18 <- epa18 %>%
  select(id, sampweight, AOI, OCUP, OCUP2, ACT,
         outsourced, tempcontract, parttime, goodschedule, goodscheduleF,
         night, evening, saturdays, sundays,
         sex, citizenship, spanish, leducation, bigregion, age, age10)

epa_20 <- epa20 %>%
  select(id, sampweight, AOI, OCUP, OCUP2, ACT,
         outsourced, tempcontract, parttime, goodschedule, goodscheduleF,
         night, evening, saturdays, sundays,
         sex, citizenship, spanish, leducation, bigregion, age, age10)

saveRDS(epa_18, "data/processed/epa_18.RDS")
saveRDS(epa_20, "data/processed/epa_20.RDS")
