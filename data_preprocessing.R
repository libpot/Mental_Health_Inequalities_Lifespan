###################################################################################################################
### Social gradient in mental health from adolescence to midlife and the mediating role of parental practices: ###
############################# a population-based 28-year longitudinal study ######################################
##################################################################################################################



#################################
##### DATA PRE-PROCESSING  #####
################################

# loading the packages
library(haven)
library(tidyverse)

# loading the data
UiN_data_orig <- read_sav("N:/durable/Data_analyses/Libor/data/UiN_regfile_030124.sav") 

# dataset with original variables
UiN_data_T1_sample <- UiN_data_orig %>% 
  filter(!is.na(id), # 0 observations without ID
         partici1 == 1,
         Type2_1 %in% c(1:8)) %>%  
  mutate(participated = ifelse(partici2 == 1 & (partici3 == 1 | partici4 == 1), 1, 0)) %>% # participated in at least three data collection waves: 1, 2 and 3 or 4 -> n = 3086
  select(id, # identification number
         participated,
         Type2_1,
         School1, Class1,
         Gende1_1, gende, # gender (self-reported and register-based)
         Age1_1, Age2_1, Birth9_3, # age 
         House1_1, # living situation
         ParEd1_1:ParEd1_9, SOSBAK, # parents' education (self-reported and register-based))
         ParJo1_1, ParJo1_2,  # father's and mother's work status
         ParJo1_3, ParJo1_4,  # father's and mother's occupation
         Origi1p1, Origi1p2, Origi1p3, Origi1p4, # mother's and father's ethnic background
         (starts_with("SCL") & !ends_with(c("13", "tot"))), # depression (items 7-12) and anxiety (items 1-6)
         starts_with("CP") & ends_with(c("04","05","08","31")), # conduct problems
         starts_with("Subst") & ends_with("1"), # alcohol intoxication frequency
         starts_with("SPPA") & contains("sw") & !ends_with(c("n", "m")), # global self-esteem
         starts_with("UCLA") & !ends_with("5") & !ends_with(c("n", "m")), # loneliness
         PM1_1:PM1_6, # behavioral_monitoring
         PBI1_01:PBI1_10, # items 1-5 psychological over-protection/over-control and item 6-10 warmth/care
         SS1b02,SS1b03,SS1b04, SS1c02,SS1c03,SS1c04, SS1d02,SS1d03,SS1d04, # social support: mother/father/both
         FLES1_1:FLES1_6) %>%  # educational investment/involvement
  mutate(across(c(starts_with("UCLA") & ends_with(c("1", "2")),
                  ends_with(c("sw1", "sw2")),
                  PBI1_03, PBI1_04, PBI1_05, PBI1_07, PBI1_08),
                ~ 5 - .x),   # re-coding reverse items
         across(c(FLES1_3, FLES1_5),
                ~ 6 - .x),
         across(starts_with("SS1"),
                ~ ifelse(.x %in% c(2, 3, 4), 1, .x))) %>% 
  mutate(SSb = ifelse(rowSums(select(., SS1b02:SS1b04)) >= 1, 1, 0), # 1 if any of the caregivers available, 0 if none available
         SSc = ifelse(rowSums(select(., SS1c02:SS1c04)) >= 1, 1, 0),
         SSd = ifelse(rowSums(select(., SS1d02:SS1d04)) >= 1, 1, 0))


# country codes of non-Western and Western migrants 
non_west <- c(27, 92, 212, 63, 90, 997, 502, 56, 55, 234, 58, 961, 62, 82, 809, 98, 94, 591, 91, 251, 252, 66, 852, 998, 81)
western <- as.numeric(na.omit(setdiff(c(unique(UiN_data_T1_sample$Origi1p4), unique(UiN_data_T1_sample$Origi1p3)), non_west)))

# dataset with newly created variables
UiN_data_T1_sample <- UiN_data_T1_sample %>%
  mutate(ethnic_background = case_when(Origi1p1 == 1 | Origi1p2 == 1 ~ "Norwegian",
                                      (Origi1p1 == 2 | Origi1p2 == 2) & (Origi1p3 %in% non_west & Origi1p4 %in% non_west) ~ "Non-Western",
                                      (Origi1p1 == 2 | Origi1p2 == 2) & (Origi1p3 %in% western & Origi1p4 %in% western) ~ "Western"),
         gender = ifelse(is.na(Gende1_1), gende, Gende1_1),
         gender = ifelse(gender == 1, "Girl/woman", "Boy/man"),
         age_birth = 92 - Birth9_3,
         age = ifelse(is.na(Age1_1), Age2_1 - 1, Age1_1),
         age = ifelse(is.na(age), age_birth, age),
         isco.m = ParJo1_4, 
         isco.f = ParJo1_3,
         work_status.m = ParJo1_2,
         work_status.f = ParJo1_1,
         education_sr = case_when(ParEd1_7 %in% c(1,2,3) ~ 3, # hoye universitetsutdannelse
                                  ParEd1_6 %in% c(1,2,3) ~ 2, # lavere universitetsutdanning (3-årig høyskole)
                                  ParEd1_3 %in% c(1,2,3) | ParEd1_4 %in% c(1,2,3) | ParEd1_5 %in% c(1,2,3) ~ 1, # videregaende skole (gymnas/yrkesskole/fagopplæring)
                                  ParEd1_1 %in% c(1,2,3) | ParEd1_2 %in% c(1,2,3) ~ 0, # grunnskoleutdanning (grunnskole/ungdomsskole/realskole)
                                  TRUE ~ NA),
         education_reg = case_when(SOSBAK == 4 ~ 3,
                                   SOSBAK == 3 ~ 2,
                                   SOSBAK == 2 ~ 1,
                                   SOSBAK == 1 ~ 0,
                                   SOSBAK == 9 ~ NA),
         education_cat = ifelse(is.na(education_reg), education_sr, education_reg),
         education = (education_cat - 0)/(3 - 0), # normalizing the exposure
         living_situation = ifelse(House1_1 == 1, 1, 0), # 0 = other , 1 = biological parents
         anxiety_1 = rowMeans(select(., SCL1_01:SCL1_06), na.rm = T), 
         anxiety_2 = rowMeans(select(., SCL2_01:SCL2_06), na.rm = T),
         anxiety_3 = rowMeans(select(., SCL3_01:SCL3_06), na.rm = T), 
         anxiety_4 = rowMeans(select(., SCL4_01:SCL4_06), na.rm = T),
         anxiety_5 = rowMeans(select(., SCL5_01:SCL5_06), na.rm = T),
         depression_1 = rowMeans(select(., SCL1_07:SCL1_12), na.rm = T),
         depression_2 = rowMeans(select(., SCL2_07:SCL2_12), na.rm = T),
         depression_3 = rowMeans(select(., SCL3_07:SCL3_12), na.rm = T),
         depression_4 = rowMeans(select(., SCL4_07:SCL4_12), na.rm = T),
         depression_5 = rowMeans(select(., SCL5_07:SCL5_12), na.rm = T),
         loneliness_1 = rowMeans(select(., UCLA1_1:UCLA1_4), na.rm = T),
         loneliness_2 = rowMeans(select(., UCLA2_1:UCLA2_4), na.rm = T),
         loneliness_3 = rowMeans(select(., UCLA3_1:UCLA3_4), na.rm = T),
         loneliness_4 = rowMeans(select(., UCLA4_1:UCLA4_4), na.rm = T),
         loneliness_5 = rowMeans(select(., UCLA5_1:UCLA5_4), na.rm = T),
         selfesteem_1 = rowMeans(select(., SPPA1sw1:SPPA1sw5), na.rm = T),
         selfesteem_2 = rowMeans(select(., SPPA2sw1:SPPA2sw5), na.rm = T),
         selfesteem_3 = rowMeans(select(., SPPA3sw1:SPPA3sw5), na.rm = T),
         selfesteem_4 = rowMeans(select(., SPPA4sw1:SPPA4sw5), na.rm = T),
         selfesteem_5 = rowMeans(select(., SPPA5sw1:SPPA5sw5), na.rm = T),
         Subst4_1 = ifelse(is.na(Subst4_1) & Subst4e1 == 1, 1, Subst4_1), # if NA, replace with a response "no/never" from binary item
         Subst3_1 = ifelse(is.na(Subst3_1) & Subst3e1 == 1, 1, Subst3_1),
         alcoholuse_1 = Subst1_1,
         alcoholuse_2 = Subst2_1,
         alcoholuse_3 = Subst3_1,
         alcoholuse_4 = Subst4_1,
         alcoholuse_5 = Subst5_1,
         CP3_04 = ifelse(is.na(CP3_04) & CP3e04 == 1, 1, CP3_04), # if NA, replace with a response "no/never" from binary item
         CP4_04 = ifelse(is.na(CP4_04) & CP4e04 == 1, 1, CP4_04),
         CP3_05 = ifelse(is.na(CP3_05) & CP3e05 == 1, 1, CP3_05),
         CP4_05 = ifelse(is.na(CP4_05) & CP4e05 == 1, 1, CP4_05),
         CP3_08 = ifelse(is.na(CP3_08) & CP3e08 == 1, 1, CP3_08),
         CP4_08 = ifelse(is.na(CP4_08) & CP4e08 == 1, 1, CP4_08),
         CP3_31 = ifelse(is.na(CP3_31) & CP3e31 == 1, 1, CP3_31),
         CP4_31 = ifelse(is.na(CP4_31) & CP4e31 == 1, 1, CP4_31),
         conduct_1 = rowMeans(select(., CP1_04:CP1_08), na.rm = T),
         conduct_2 = rowMeans(select(., CP2_04:CP2_31), na.rm = T),
         conduct_3 = rowMeans(select(., CP3_04:CP3_31), na.rm = T),
         conduct_4 = rowMeans(select(., CP4_04:CP4_31), na.rm = T),
         conduct_5 = rowMeans(select(., CP5_04:CP5_31), na.rm = T),
         behavioral_monitoring = rowMeans(select(., PM1_1:PM1_6), na.rm = T),
         psych_overcontrol = rowMeans(select(., PBI1_01:PBI1_05), na.rm = T), 
         warmth_care = rowMeans(select(., PBI1_06:PBI1_10), na.rm = T),
         edu_investment =  rowMeans(select(., FLES1_1:FLES1_6), na.rm = T),
         social_support = rowSums(select(., SSb:SSd))) %>%
  mutate(across(c(starts_with(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct")),
                behavioral_monitoring, psych_overcontrol, warmth_care, edu_investment, social_support),
                ~ ifelse(is.nan(.x), NA, as.numeric(.x))),
# calculating a POMP (percentage of maximum possible) score
         across(c(starts_with(c("depression", "anxiety", "loneliness", "selfesteem")), psych_overcontrol, warmth_care),
                ~ (.x - 1)/(4 - 1) * 100), # a 4-point Likert scale
         across(c(starts_with(c("conduct", "alcoholuse")), behavioral_monitoring),
                ~ (.x - 1)/(6 - 1) * 100), # a 6-point Likert scale
         edu_investment = (edu_investment - 1)/(5 - 1) * 100, # a 5-point Likert scale
         social_support = (social_support - 0)/(3 - 0) * 100) %>%  # a scale from 0 to 3
  select(id, participated,
         School1, Class1, Type2_1,
         gender, ethnic_background, education_cat, education, living_situation, age, 
         work_status.f, work_status.m, isco.m, isco.f,
         behavioral_monitoring, psych_overcontrol, warmth_care, edu_investment, social_support, 
         starts_with(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct")))


# deriving international socioeconomic index of occupational status (isei)
yin.isco88 <- read_table("N:/no-backup/Libor/isei_isco88_new.csv") %>% 
  transmute(isco.m = isco.yin,
            isco.f = isco.yin,
            isco.m.int = isco2, 
            isco.f.int = isco2,
            isei.m = isei882,
            isei.f = isei882)

UiN_data_isei <- as_tibble(merge(UiN_data_T1_sample, yin.isco88[,c("isco.m", "isco.m.int", "isei.m")], by = "isco.m", all.x = T)) 
UiN_data_isei <- as_tibble(merge(UiN_data_isei, yin.isco88[,c("isco.f", "isco.m.int", "isei.f")], by = "isco.f", all.x = T))
UiN_data_isei <- UiN_data_isei %>% 
  mutate(isei.f = ifelse(work_status.f >= 3, 15, isei.f), # if economically inactive (unemployed/in household/in education), then the lowest ISEI code (15)
         isei.m = ifelse(work_status.m >= 3, 15, isei.m), 
         isei_orig = case_when(isei.f >= isei.m ~ isei.f, 
                               isei.m >= isei.f ~ isei.m,
                               is.na(isei.f) ~ isei.m, 
                               is.na(isei.m) ~ isei.f),
         isei = (isei_orig - 15)/(90 - 15)) # normalizing the exposure

UiN_data <- UiN_data_isei %>% filter(participated == 1)
UiN_data_T1_sample <- UiN_data_isei
UiN_data_isei <- UiN_data_isei %>% filter(participated == 1)
         

# saving new datasets
write.csv(UiN_data, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_data")
write.csv(UiN_data_T1_sample, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_data_T1_sample")
write.csv(UiN_data_isei, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_data_isei")


UiN_very_long <- UiN_data_isei %>% 
  select(id,
         gender, age, ethnic_background,
         isei, isei_orig, education_cat, education, living_situation, 
         starts_with(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct")),
         behavioral_monitoring, psych_overcontrol, warmth_care, edu_investment, social_support) %>% 
  pivot_longer(cols = depression_1:conduct_5,
               names_to = c("mental_health", "time_wave"),
               names_pattern = "(.*)_(.)",
               values_to = "symptoms")
write.csv(UiN_very_long, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_very_long")

UiN_long_mental_health <- UiN_data_isei %>% 
  select(id, starts_with(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct"))) %>% 
  pivot_longer(cols = depression_1:conduct_5,
               names_to = c("domain", "time"),
               names_pattern = "(.*)_(.)",
               values_to = "value") %>% 
  pivot_wider(names_from = domain,
              values_from = value)
write.csv(UiN_long_mental_health, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_long_mental_health")

UiN_model_data <- UiN_very_long %>% 
  pivot_wider(names_from = time_wave,
              values_from = "symptoms") %>% 
  pivot_longer(cols = c("isei", "education", "living_situation"),
               names_to = "ses_indicator",
               values_to = "ses_value") %>% 
  mutate(age = age - mean(age, na.rm = T),
         western_ethn = case_when(ethnic_background == "Western" ~ 1,
                                  is.na(ethnic_background) ~ NA,
                                  TRUE ~ 0),
         nonwestern_ethn = case_when(ethnic_background == "Non-Western" ~ 1,
                                     is.na(ethnic_background) ~ NA,
                                     TRUE ~ 0),
         gender = ifelse(gender == "Girl/woman", 1, 0),
         T1 = `1`, T2 = `2`, T3 = `3`, T4 = `4`, T5 = `5`,
         ses_indicator = case_when(ses_indicator == "living_situation" ~ "Living situation",
                                   ses_indicator == "isei" ~ "Occupation",
                                   ses_indicator == "education" ~ "Education"),
         mental_health = factor(case_when(mental_health == "alcoholuse" ~ "Alcohol use",
                                   mental_health == "depression" ~ "Depression",
                                   mental_health == "anxiety" ~ "Anxiety",
                                   mental_health == "loneliness" ~ "Loneliness",
                                   mental_health == "conduct" ~ "Conduct problems",
                                   mental_health == "selfesteem" ~ "Self-esteem"),
                                levels = c("Depression", "Anxiety", "Alcohol use", "Conduct problems","Loneliness", "Self-esteem"),
                                ordered = TRUE)) %>%
  select(-c(`2`:`5`))
write.csv(UiN_model_data, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_model_data")

