###################################################################################################################
### Social gradient in mental health from adolescence to midlife and the mediating role of parental practices: ###
############################# a population-based 28-year longitudinal study ######################################
##################################################################################################################



###############################
### DESCRIPTIVE STATISTICS ###
##############################

#loading necessary packages
library(tidyverse)
library(readr)
library(haven)
library(RColorBrewer)


# loading data
UiN_data_orig <- read_sav("N:/durable/Data_analyses/Libor/data/UiN_regfile_030124.sav") 
UiN_data_isei <- read_csv("data/UiN_data_isei")
UiN_very_long <- read_csv("data/UiN_long_mental_health")
UiN_long_mental_health <- read_csv("data/UiN_long_mental_health")
UiN_data_T1_sample <- read_csv("data/UiN_data_T1_sample")

# sample characteristics by socio-demographics: means (SD) and counts (%)
levels_all <- c("gender", "age", "ethnic_background", "education_cat", "isei_orig", "living_situation", 
                "depression_1", "anxiety_1", "loneliness_1", "selfesteem_1", "alcoholuse_1", "conduct_1", 
                "behavioral_monitoring", "psych_overcontrol", "warmth_care", "edu_investment", "social_support")
levels_attrition <- c("gender", "ethnic_background", "education_cat", "isei_orig", "living_situation", 
                      "depression_1", "anxiety_1", "loneliness_1", "selfesteem_1", "alcoholuse_1", "conduct_1", 
                      "behavioral_monitoring", "psych_overcontrol", "warmth_care", "social_support")
labels_all <- c("Gender", "Age", "Ethnicity", "Parental education", "Parental ISEI", "Living situation",
                "Depression", "Anxiety", "Loneliness", "Self-esteem", "Alcohol use", "Conduct problems",
                "Behavioral monitoring", "Psychological overcontrol", "Warmth/care", "Educational investment", "Social support")

sample_characteristics_factor <- UiN_data_isei %>% 
  select(gender, ethnic_background, education_cat, living_situation) %>% 
  mutate(across(gender:living_situation, as.factor)) %>% 
  pivot_longer(everything()) %>% 
  filter(!is.na(value)) %>% 
  count(name, value) %>% 
  group_by(name) %>% 
  mutate(prop = formatC(round(n/sum(n)*100, 2), format = "f", digits = 2),
         stat = paste(n, paste0("(", prop, ")"))) %>% 
  select(name, value, stat) 

sample_characteristics_conti <- UiN_data_isei %>% 
  select(age, isei_orig, 
         behavioral_monitoring, psych_overcontrol, warmth_care, edu_investment, social_support, 
         depression_1, anxiety_1, loneliness_1, selfesteem_1, alcoholuse_1, conduct_1) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(stat = paste0(formatC(round(mean(value, na.rm = TRUE), 2), format = "f", digits = 2), " (", 
                          formatC(round(sd(value, na.rm = TRUE), 2), format = "f", digits = 2), ")")) 

sample_characteristics <- rbind(sample_characteristics_factor, sample_characteristics_conti) %>% 
  mutate(name = factor(name, levels = levels_all, labels = labels_all, ordered = TRUE)) %>% 
  arrange(name) 

write.csv(sample_characteristics, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/sample_characteristics")

# descriptives for mental health outcomes (T2 to T5): means (sd) 
descriptives_outcomes <- UiN_long_mental_health %>% 
  group_by(time) %>% 
  summarise(across(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct"),
                   ~ paste0(formatC(round(mean(.x, na.rm = TRUE), 2), format = "f", digits = 2), " (", 
                            formatC(round(sd(.x, na.rm = TRUE), 2), format = "f", digits = 2), ")")))
write.csv(descriptives_outcomes, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/descriptives_outcomes")

# missing data in mental health outcomes (T2 to T5): counts (%)
missing_outcomes <- UiN_long_mental_health %>% 
  group_by(time) %>% 
  summarise(across(c("depression", "anxiety", "loneliness", "selfesteem", "alcoholuse", "conduct"),
                   ~ paste0(sum(is.na(.x), na.rm = T), " (", formatC(round(sum(is.na(.x), na.rm = T)/length(.x)*100, 2), format = "f", digits = 2), ")")))
write.csv(missing_outcomes, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/missing_outcomes")

# missing data at the baseline (T1): counts (%)
missing_baseline <- UiN_data_isei %>% 
  select(gender, ethnic_background, education_cat, living_situation,
         age, isei_orig, 
         behavioral_monitoring, psych_overcontrol, warmth_care, edu_investment, social_support, 
         depression_1, anxiety_1, loneliness_1, selfesteem_1, alcoholuse_1, conduct_1) %>%
  mutate(across(c(gender, ethnic_background), ~ ifelse(!is.na(.x), 1, NA)),
         across(everything(), as.numeric)) %>% 
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise(n = sum(is.na(value)),
            prop = formatC(round(n/length(UiN_data_isei$id)*100, 2), format = "f", digits = 2),
            stat = paste(n, paste0("(", prop, ")"))) %>% 
  select(name, stat) %>% 
  mutate(name = factor(name, levels = levels_all, labels = labels_all, ordered = TRUE)) %>% 
  arrange(name) 
write.csv(missing_baseline, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/missing_baseline")




##########################
### ATTRITION ANALYSIS ###
##########################

schools_followed <- unique(UiN_data_isei$School1)
classes_followed <- unique(UiN_data_isei$Class1)

UiN_data_attrition <- UiN_data_T1_sample %>% 
  filter((School1 %in% schools_followed & Class1 %in% classes_followed)) %>%
  select(c(participated, isei_orig, gender, ethnic_background, education_cat, living_situation,
           behavioral_monitoring, psych_overcontrol, warmth_care, social_support, 
           depression_1, anxiety_1, loneliness_1, selfesteem_1, alcoholuse_1, conduct_1)) %>% 
  mutate(across(c(isei_orig, 
                  behavioral_monitoring, psych_overcontrol, warmth_care, social_support, 
                  depression_1, anxiety_1, loneliness_1, selfesteem_1, alcoholuse_1, conduct_1),
                ~ ntile(.x, 5)),
         gender = ifelse(gender == "Boy/man", 0, 1),
         ethnic_background = case_when(ethnic_background == "Norwegian" ~ 0,
                                       ethnic_background == "Western" ~ 1,
                                       ethnic_background == "Non-Western" ~ 2,
                                       TRUE ~ NA),
         across(everything(),
                ~ as.factor(ifelse(is.na(.x), "X", .x)))) %>% 
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = levels_attrition)

attrition_models <- UiN_data_attrition %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(attrition_model = map(.x = data,
                               ~ glm(participated ~ value, 
                                     data = .x, 
                                     family = "binomial")),
         results = map(.x = attrition_model,
                       ~ as.data.frame(summary(.x)$coefficients) %>% 
                         rownames_to_column(var = "predictor") %>% 
                         filter(predictor != "(Intercept)",
                                `Std. Error` < 100) %>% # dropping excessively uncertain estimates (categories for small N)
                         select(predictor, Estimate, 'Std. Error', 'Pr(>|z|)'))) %>% 
  select(results) %>% 
  unnest() %>% 
  ungroup()

attrition <- attrition_models %>% 
  mutate(level = substr(predictor, nchar(predictor), nchar(predictor)),
         level = ifelse(level == "X", "NA", level),
         variable = factor(variable, levels = levels_all, labels = labels_all, ordered = TRUE),
         Estimate = exp(Estimate),
         across(c(Estimate, `Std. Error`, `Pr(>|z|)`),
         ~ formatC(round(.x, 2), format = "f", digits = 2)),
         p_value = ifelse(`Pr(>|z|)` == "0.00", "<0.01", `Pr(>|z|)`)) %>% 
  select(variable, level, Estimate, `Std. Error`, p_value)
write.csv(attrition, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/attrition")





