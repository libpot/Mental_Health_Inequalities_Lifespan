###################################################################################################################
### Social gradient in mental health from adolescence to midlife and the mediating role of parental practices: ###
############################# a population-based 28-year longitudinal study ######################################
##################################################################################################################

##################################
##### SUPPLEMENTARY ANALYSES #####
##################################



# loading necessary packages
library(tidyverse)
library(lavaan)
library(semTools)



#################################
### CROSS-SECTIONAL ANALYSES ###
################################

cross_section <- '
symptoms ~ ses_value + age + gender + western_ethn + nonwestern_ethn
'

cross_sect_analysis <- UiN_model_data %>% 
  select(ses_indicator, mental_health, ses_value, age, gender, western_ethn, nonwestern_ethn, T2:T5) %>% 
  filter(mental_health %in% c("Alcohol use", "Conduct problems")) %>% 
  pivot_longer(cols = T2:T5,
               names_to = "wave",
               values_to = "symptoms") %>% 
  group_by(mental_health, ses_indicator, wave) %>% 
  nest() %>% 
  filter(wave %in% c("T2", "T5")) %>% 
  mutate(regress = map(.x = data,
                       ~ sem(model = cross_section,
                                estimator = "MLR",
                                missing = "ML",
                                fixed.x = FALSE,
                                data = .x)),
         soc_gradient = map(.x = regress,
                            ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                              filter(lhs %in% c("symptoms"), rhs == "ses_value") %>% 
                              select(est, ci.lower, ci.upper))) %>% 
  select(soc_gradient) %>% 
  unnest() %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                                        " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                                        formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  select(mental_health, ses_indicator, wave, result_unstd) %>% 
  pivot_wider(names_from = "wave",
              values_from = "result_unstd")
write.csv(cross_sect_analysis, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/cross_sect_results")





################################
### MEASUREMEMNT INVARIANCE ###
###############################

#### preparing the data ###
MI_data <- UiN_data_orig %>% 
  filter(!is.na(id), # 0 observations without ID
         partici1 == 1,
         Type2_1 %in% c(1:8),
         partici2 == 1 & (partici3 == 1 | partici4 == 1)) %>% 
  select(id, # selecting id and mental health outcomes
         starts_with("SCL") & !ends_with(c("13", "tot")), # depression and anxiety
         starts_with("CP") & ends_with(c("04","05","08","31")) & !contains("e"), # conduct problems
         starts_with("SPPA") & contains("sw") & !ends_with(c("n", "m")), # global self-esteem
         starts_with("UCLA") & !ends_with("5") & !ends_with(c("n", "m")), # loneliness
         PM1_1:PM1_6, # behavioral_monitoring
         PBI1_01:PBI1_10, # items 1-5 over-protection/over-control and item 6-10 warmth/care
         FLES1_1:FLES1_6) %>% 
# reversing items
  mutate(across(c(starts_with("UCLA") & ends_with(c("1", "2")),
                  ends_with(c("sw1", "sw2")),
                  PBI1_03, PBI1_04, PBI1_05, PBI1_07, PBI1_08),
                ~ 5 - .x),   # re-coding reverse items
         across(c(FLES1_3, FLES1_5),
                ~ 6 - .x)) %>% 
  rename_with(.cols = starts_with("SPPA"),
              ~ gsub("sw", "_", .x)) %>% # reformatting self-worth to the same format as remaining mental health variables
  rename_with(.cols = (starts_with("SCL") & ends_with(c("01", "02", "03", "04", "05", "06"))),
              ~ gsub("SCL", "ANX", .x)) %>% 
  rename_with(.cols = (starts_with("SCL") & ends_with(c("07", "08", "09", "10", "11", "12"))),
              ~ gsub("SCL", "DEP", .x)) %>%
  rename_with(.cols = (starts_with("PBI") & ends_with(c("01", "02", "03", "04", "05"))),
              ~ gsub("PBI", "CONT", .x)) %>%
  rename_with(.cols = (starts_with("PBI") & ends_with(c("06", "07", "08", "09", "10"))),
              ~ gsub("PBI", "WARM", .x)) %>%
  pivot_longer(cols = -id,
               names_to = c("time_wave", "item"),
               names_sep = "_",
               values_to = "symptoms") %>% # reshaping to a long format
  mutate(outcome = substr(time_wave, 1, nchar(time_wave) - 1),
         time_wave = substr(time_wave, nchar(time_wave), nchar(time_wave)),
         item = str_remove(item, "0"),
         symptoms = labelled::remove_labels(symptoms)) %>% 
  group_by(id, outcome, time_wave) %>% 
  mutate(item = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "item",
              values_from = "symptoms") %>% 
  mutate(outcome = case_when(outcome == "SPPA" ~ "Self-esteem",
                             outcome == "CP" ~ "Conduct problems",
                             outcome == "UCLA" ~ "Loneliness",
                             outcome == "ANX" ~ "Anxiety",
                             outcome == "DEP" ~ "Depression",
                             outcome == "PM" ~ "Behavioral monitoring",
                             outcome == "FLES" ~ "Educational investment",
                             outcome == "CONT" ~ "Psychological overcontrol",
                             outcome == "WARM" ~ "Warmth/care"),
         variable_type = case_when(outcome %in% c("Self-esteem", "Conduct problems", "Loneliness", "Anxiety", "Depression") ~ "mental health",
                                   outcome %in% c("Psychological overcontrol", "Warmth/care", "Educational investment", "Behavioral monitoring") ~ "parental practice"))

# specifying the measurement models for 4-/5-/6-item scales
outcome_4i <- ' outcome =~ `1` + `2` + `3` + `4` '
outcome_5i <- ' outcome =~ `1` + `2` + `3` + `4` + `5` '
outcome_6i <- ' outcome =~ `1` + `2` + `3` + `4` + `5` + `6` '




##########################################
### CONSTRUCT VALIDITY AND RELIABILITY ###
##########################################

my_fit_indices <- c("df", "chisq.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "srmr_bentler", "rmsea.robust")
my_fit_indices_conti <- c("chisq.scaled", "cfi.robust", "tli.robust", "srmr_bentler", "rmsea.robust")

CFA_rel_models <- MI_data %>% 
  filter(!(variable_type == "mental health" &  time_wave == "1")) %>% 
  group_by(outcome, variable_type, time_wave) %>% 
  nest() %>% 
  mutate(model = map(.x = data,
                     ~ cfa(model = case_when(outcome %in% c("Self-esteem", "Psychological overcontrol", "Warmth/care") ~ outcome_5i, # specifying measurement model depending on the number of items
                                             outcome %in% c("Conduct problems", "Loneliness") ~ outcome_4i,
                                             outcome %in% c("Anxiety", "Depression", "Educational investment", "Behavioral monitoring") ~ outcome_6i),
                           data = .x,
                           estimator = "MLR", # estimator: robust maximum likelihood
                           missing = "ML")), # full-information likelihood for missing values
         fit_indices = map_dfr(.x = model,
                               ~ fitmeasures(.x)[my_fit_indices]), # global fit indices
         reliability = map_dfr(.x = model,
                               ~ semTools::reliability(.x)[c("alpha", "omega"),])) # Cronbach’s alpha (α) and McDonald’s omega (ω) 

  
# confirmatory factor analyses (CFA) results 
CFA_results <- CFA_rel_models %>% 
  select(fit_indices) %>% 
  unnest() 

CFA_results <- CFA_results %>% 
  mutate(across(my_fit_indices_conti,
                ~ formatC(round(.x, 2), format = "f", digits = 2)),
         across(my_fit_indices_conti,
                ~ case_when(.x == "0.00|0" ~ "<0.01",
                            .x == "-0.00" ~ "<-0.01",
                            TRUE ~ .x)),
         pvalue.scaled = as.character(ifelse(pvalue.scaled < 0.01, "<0.01", round(pvalue.scaled, 2)))) %>% 
  arrange(variable_type, outcome) %>% 
  ungroup() %>% 
  select(-variable_type)
write.csv(CFA_results, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/CFA")


# reliability coefficients: McDonald’s omega (ω) and Cronbach’s alpha (α)
reliability_results <- CFA_rel_models %>% 
  select(reliability) %>% 
  unnest() 

reliability_results %>% 
  filter(variable_type == "mental health") %>% 
  group_by(outcome) %>% 
  summarise(across(c("alpha", "omega"), ~ round(mean(.x), 2))) 

reliability_results <- reliability_results %>% 
  mutate(across(c("alpha", "omega"),
                ~ formatC(round(.x, 2), format = "f", digits = 2))) %>% 
  arrange(variable_type, outcome) %>% 
  ungroup() %>% 
  select(-variable_type)
write.csv(reliability_results, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/reliability")



################################################
### LONGITUDINAL MEASUREMENT INVARIANCE (MI) ###
################################################

MI_models <- MI_data %>% 
  filter(time_wave != "1") %>% 
  group_by(outcome) %>% 
  nest() %>% 
  mutate(config = map(.x = data,
                      ~ cfa(model = case_when(outcome == "Self-esteem" ~ outcome_5i,
                                              outcome %in% c("Conduct problems", "Loneliness") ~ outcome_4i,
                                              outcome %in% c("Anxiety", "Depression") ~ outcome_6i), 
                            data = .x, estimator = "MLR", missing = "ML",
                            group = "time_wave")),                               # configural invariance
         metric = map(.x = data,
                      ~ cfa(model = case_when(outcome == "Self-esteem" ~ outcome_5i,
                                              outcome %in% c("Conduct problems", "Loneliness") ~ outcome_4i,
                                              outcome %in% c("Anxiety", "Depression") ~ outcome_6i), 
                            data = .x, estimator = "MLR", missing = "ML",
                            group = "time_wave", group.equal = "loadings")),       # metric invariance
         scalar = map(.x = data,
                      ~ cfa(model = case_when(outcome == "Self-esteem" ~ outcome_5i,
                                              outcome %in% c("Conduct problems", "Loneliness") ~ outcome_4i,
                                              outcome %in% c("Anxiety", "Depression") ~ outcome_6i), 
                            data = .x, estimator = "MLR", missing = "ML",
                            group = "time_wave", group.equal = c("intercepts", "loadings")))) # scalar invariance

MI_results <- MI_models %>% 
  pivot_longer(cols = c("config", "metric", "scalar"),
               names_to = "invariance",
               values_to = "model") %>% 
  mutate(fit_indices = map_dfr(.x = model, # global fit indices
                               ~ fitmeasures(.x)[my_fit_indices])) %>%  
  select(invariance, fit_indices) %>% 
  unnest() 

# overall model fit
MI_results_overall <- MI_results %>% 
  mutate(across(my_fit_indices_conti,
                ~ formatC(round(.x, 2), format = "f", digits = 2)),
         across(my_fit_indices_conti,
                ~ case_when(.x == "0.00|0" ~ "<0.01",
                            .x == "-0.00" ~ "<-0.01",
                            TRUE ~ .x)))
write.csv(MI_results_overall, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/invariance_overall")

# relative model fit (delta)
MI_results_delta <- MI_results %>% 
  group_by(outcome) %>%  
  mutate(across(df:rmsea.robust,
                ~ .x - lag(.x)))

MI_results_delta %>% 
  filter(invariance != "config",
         outcome != "conduct problems") %>% 
  group_by(invariance) %>% 
  summarise(across(my_fit_indices_conti, mean))

MI_results_delta <- MI_results_delta %>% 
  mutate(across(my_fit_indices_conti,
                ~ formatC(round(.x, 2), format = "f", digits = 2)),
         across(my_fit_indices_conti,
                ~ case_when(.x == "0.00|0" ~ "<0.01",
                            .x == "-0.00" ~ "<-0.01",
                            TRUE ~ .x))) %>%  
  filter(invariance != "config")
write.csv(MI_results_delta, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/invariance_delta")
