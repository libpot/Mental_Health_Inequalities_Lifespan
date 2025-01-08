###################################################################################################################
### Social gradient in mental health from adolescence to midlife and the mediating role of parental practices: ###
############################# a population-based 28-year longitudinal study ######################################
##################################################################################################################


##########################
##### MAIN ANALYSES #####
#########################

# loading the packages
library(tidyverse)
library(lavaan)
library(semTools)
library(semPlot)
library(lemon)
library(RColorBrewer)
library(Cairo)

# loading data
UiN_data_isei <- read_csv("data/UiN_data_isei")
UiN_very_long <- read_csv("data/UiN_very_long")
UiN_model_data <- read_csv("data/UiN_model_data")




###################################
### LATENT GROWTH CURVE MODELS ###
##################################

growth_model_uncond_linear <-  '  
# intercept and slope growth factors with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5   
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
# covariance among growth factors
    i ~~ s'

growth_model_uncond_quadr <-  '  
# intercept, slope and quadratic growth factors with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q'

growth_model_uncond_cubic <-  '  
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
    c =~ 0*T2 + 0.125*T3 + 1.331*T4 + 17.576*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q'

growth_model_cond_ses <-  '  
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    s ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    q ~ ses_value + age + gender + western_ethn + nonwestern_ethn'

growth_model_cond_ses_intT5_inv <-  '  
# intercept and slope with fixed coefficients
    i =~ 1*T5 + 1*T4 + 1*T3 + 1*T2
    s =~ 0*T5 + 1.5*T4 + 2.1*T3 + 2.6*T2
    q =~ 0*T5 + 2.25*T4 + 4.41*T3 + 6.76*T2 
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    s ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    q ~ ses_value + age + gender + western_ethn + nonwestern_ethn'

growth_model_parental_practices <-  '  
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ practices_value + age + gender + western_ethn + nonwestern_ethn
    s ~ practices_value + age + gender + western_ethn + nonwestern_ethn
    q ~ practices_value + age + gender + western_ethn + nonwestern_ethn'

mediation_growth_model_sq <- '
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    s ~ Cs*ses_value + Bs*practices_value + age + gender + western_ethn + nonwestern_ethn
    q ~ Cq*ses_value + Bq*practices_value + age + gender + western_ethn + nonwestern_ethn
    practices_value ~ 1 + A*ses_value + age + gender + western_ethn + nonwestern_ethn
# indirect effect: a*b
    indirect_s := A*Bs
    indirect_q := A*Bq
# total effect: c + (a*b)
    total_s := Cs + (A*Bs)
    total_q := Cq  + (A*Bq)'

mediation_growth_model_q <- '
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    s ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    q ~ Cq*ses_value + Bq*practices_value + age + gender + western_ethn + nonwestern_ethn
    practices_value ~ 1 + A*ses_value + age + gender + western_ethn + nonwestern_ethn
# indirect effect: a*b
    indirect_q := A*Bq
# total effect: c + (a*b)
    total_q := Cq  + (A*Bq)'

mediation_growth_model_i <- '
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ Ci*ses_value + Bi*practices_value + age + gender + western_ethn + nonwestern_ethn
    s ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    q ~ ses_value + age + gender + western_ethn + nonwestern_ethn
    practices_value ~ 1 + A*ses_value + age + gender + western_ethn + nonwestern_ethn
# indirect effect: a*b
    indirect_i := A*Bi
# total effect: c + (a*b)
    total_i := Ci + (A*Bi)'

mediation_growth_model_all <- '
# intercept and slope with fixed coefficients
    i =~ 1*T2 + 1*T3 + 1*T4 + 1*T5
    s =~ 0*T2 + 0.5*T3 + 1.1*T4 + 2.6*T5
    q =~ 0*T2 + 0.25*T3 + 1.21*T4 + 6.76*T5
# covariance among growth factors
    i ~~ s
    i ~~ q
    s ~~ q
# regressions
    i ~ Ci*ses_value + Bi*practices_value + age + gender + western_ethn + nonwestern_ethn
    s ~ Cs*ses_value + Bs*practices_value + age + gender + western_ethn + nonwestern_ethn
    q ~ Cq*ses_value + Bq*practices_value + age + gender + western_ethn + nonwestern_ethn
    practices_value ~ 1 + A*ses_value + age + gender + western_ethn + nonwestern_ethn
# indirect effect: a*b
    indirect_i := A*Bi
    indirect_s := A*Bs
    indirect_q := A*Bq
# total effect: c + (a*b)
    total_i := Ci + (A*Bi)
    total_s := Cs + (A*Bs)
    total_q := Cq  + (A*Bq)'




#####################################
### (UN-)CONDITIONAL GROWTH MODEL ###
#####################################

# running unconditional and conditional latent growth models
LGC_models <- UiN_model_data %>% 
  group_by(mental_health, ses_indicator) %>% 
  nest() %>% 
 mutate(uncond_linear = map(.x = data,
                             ~ growth(model = growth_model_uncond_linear,
                                      data = .x,
                                      estimator = "MLR", # maximum likelihood estimation with robust (Huber-White/sandwich) standard errors 
                                      se = "robust",
                                      missing = "fiml.x")), # full-information maximum likelihood for all variables, incl. exogenous (x)
         uncond_quadr = map(.x = data,
                            ~ growth(model = growth_model_uncond_quadr,
                                     data = .x,
                                     estimator = "MLR",
                                     se = "robust",
                                     missing = "fiml.x")),
         quadr_vs_linear_chi = map2_lgl(.x = uncond_linear,
                                        .y = uncond_quadr,
                                        ~ anova(.x, .y)$`Pr(>Chisq)`[[2]] < 0.05),
         quadr_vs_linear_bic = map2_lgl(.x = uncond_linear,
                                        .y = uncond_quadr,
                                         ~ BIC(.y) < BIC(.x)),
         cond_ses = map(.x = data,
                        ~ growth(model = growth_model_cond_ses,
                                 data = .x,
                                 estimator = "MLR",
                                 se = "robust",
                                 missing = "fiml.x")),
         cond_ses_intT5_inv = map(.x = data,
                               ~ growth(model = growth_model_cond_ses_intT5_inv,
                                        data = .x,
                                        estimator = "MLR",
                                        se = "robust",
                                        missing = "fiml.x"))) 
 

LGC_models <- LGC_models %>% 
# sanity check: any negative variances or latent correlations out of bounds?
  mutate(neg_var = map_dbl(.x = uncond_quadr,
                           ~ nrow(filter(parameterestimates(.x), lhs == rhs, op == "~~", est < 0))), # yes, in conduct problems and loneliness 
         neg_var_sign = map_lgl(.x = uncond_quadr,
                                ~ nrow(filter(parameterestimates(.x), lhs == rhs, op == "~~", est < 0, pvalue < 0.05))), # none
         latent_corr = map_dbl(.x = uncond_quadr,
                              ~ filter(parameterestimates(.x, standardized = T), lhs == "s", op == "~~", rhs == "q")$std.all)) %>% # (cor. > 1 in alcohol)
# significant variance in growth parameters (intercepts, linear and quadratic trends)?
 mutate(i_var_sign = map_lgl(.x = uncond_quadr,
                                  ~ filter(parameterestimates(.x), lhs == "i", op == "~~", rhs == "i")$pvalue < 0.05), # all
        s_var_sign = map_lgl(.x = uncond_quadr,
                                  ~ filter(parameterestimates(.x), lhs == "s", op == "~~", rhs == "s")$pvalue < 0.05), # all excl. conduct problems
        q_var_sign = map_lgl(.x = uncond_quadr,
                                  ~ filter(parameterestimates(.x), lhs == "i", op == "~~", rhs == "i")$pvalue < 0.05)) %>% # all excl. conduct problems
# significant regression of growth parameters on SES?
 mutate(i_regr_sign = map2_lgl(.x = cond_ses, .y = i_var_sign,
                                    ~ ifelse(.y == FALSE, FALSE, filter(parameterestimates(.x), lhs == "i", op == "~", rhs == "ses_value")$pvalue < 0.05)),
        s_regr_sign = map2_lgl(.x = cond_ses, .y = s_var_sign, 
                                    ~ ifelse(.y == FALSE, FALSE, filter(parameterestimates(.x), lhs == "s", op == "~", rhs == "ses_value")$pvalue < 0.05)),
        q_regr_sign = map_lgl(.x = cond_ses, .y = q_var_sign, 
                                   ~ ifelse(.y == FALSE, FALSE, filter(parameterestimates(.x), lhs == "q", op == "~", rhs == "ses_value")$pvalue < 0.05)))
 
         

# extracting model estimates for association between ses (T1) and mental health (T2 to T5)
model_estimates_cond <- LGC_models %>%    
  mutate(estimates = map(.x = cond_ses,
                         ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                              filter(lhs %in% c("i", "s", "q"), rhs == "ses_value") %>% 
                              select(lhs, est, ci.lower, ci.upper))) %>% 
  select(estimates, i_var_sign:q_var_sign) %>% 
  unnest() %>% 
  pivot_longer(names_to = "parameter",
               values_to = "var_sign",
               cols = i_var_sign:q_var_sign,
               names_pattern = "(.)_var_sign") %>% 
  filter(lhs == parameter) %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                                " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                                formatC(round(ci.upper, 2), format = "f", digits = 2), ")"),
         across(est:result_unstd,
                ~ ifelse(var_sign != TRUE, NA, .x))) %>% 
  select(-c(parameter, var_sign))

# a re-parametrized model with intercept at T5
intercept_T5 <- LGC_models %>% 
  mutate(intercept_T5 = map(.x = cond_ses_intT5_inv,
                            ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                              filter(lhs %in% c("i"), rhs == "ses_value") %>% 
                              select(lhs, est, ci.lower, ci.upper))) %>% 
  select(intercept_T5) %>% 
  unnest() %>% 
  mutate(lhs = "T5") %>% 
   mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                     " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                     formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) 
   
# bringing together original LGC model estimates (int = T1, linear and quadratic trends) 
# and re-parametrized model intercept estimate (= T5)
model_estimates_cond_ext <- rbind(model_estimates_cond, intercept_T5) 
write.csv(model_estimates_cond_ext, "output/Tables/model_estimates_cond")

results_model_estimates_cond_ext <- model_estimates_cond_ext %>% 
  select(lhs, result_unstd) %>% 
  pivot_wider(names_from = "lhs",
              values_from = "result_unstd") %>% 
  mutate(ses_indicator = factor(ses_indicator, levels = c("Education", "Occupation", "Living situation"), ordered = T)) %>% 
  arrange(mental_health, ses_indicator)
write.csv(results_model_estimates_cond_ext, "output/Tables/results_model_estimates_cond_ext")




########################
### MEDIATION MODELS ###
########################

# 1) parental practices by SES: path a
new_data <- data.frame(ses_value = c(0,1,0,1),
                       gender = factor(c("Girl/woman", "Boy/man", "Boy/man", "Girl/woman")),
                       western_ethn = 0,
                       nonwestern_ethn = 0,
                       age = 0)

data_practices <- UiN_data_isei %>% 
  pivot_longer(cols = c(living_situation, education, isei),
               names_to = "ses_indicator",
               values_to = "ses_value") %>% 
  pivot_longer(cols = c(behavioral_monitoring:social_support),
               names_to = "parental_practices",
               values_to = "practices_value") %>% 
  mutate(western_ethn = case_when(ethnic_background == "Western" ~ 1,
                                  is.na(ethnic_background) ~ NA,
                                  TRUE ~ 0),
         nonwestern_ethn = case_when(ethnic_background == "Non-Western" ~ 1,
                                     is.na(ethnic_background) ~ NA,
                                     TRUE ~ 0),
         age = age - mean(age, na.rm = T)) %>% 
  group_by(ses_indicator, parental_practices) %>% 
  nest() %>% 
  mutate(practices_adj = map(.x = data,
                       ~ sem(model = 'practices_value ~ 1 + ses_value + age + gender + western_ethn + nonwestern_ethn',
                             estimator = "MLR",
                             missing = "ML",
                             fixed.x = FALSE,
                             data = .x)),
         estimates = map(.x = practices_adj,
                            ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                              filter(lhs %in% c("practices_value"), rhs == "ses_value") %>% 
                              select(est, ci.lower, ci.upper))) %>% 
  select(estimates) %>% 
  unnest() %>% 
  mutate(ses_indicator = factor(case_when(ses_indicator == "living_situation" ~ "Living situation",
                                          ses_indicator == "isei" ~ "Occupation",
                                          ses_indicator == "education" ~ "Education"),
                                levels = c("Education", "Occupation", "Living situation")),
         parental_practices = factor(case_when(parental_practices == "behavioral_monitoring" ~ "Behavioral monitoring",
                                               parental_practices == "psych_overcontrol" ~ "Psychological overcontrol",
                                               parental_practices == "warmth_care" ~ "Warmth",
                                               parental_practices == "edu_investment" ~ "Educational investment",
                                               parental_practices == "social_support" ~ "Social support"),
                                     levels = c("Behavioral monitoring", "Warmth", "Educational investment", "Social support", "Psychological overcontrol"))) %>% 
  arrange(ses_indicator, parental_practices)

results_practices <- data_practices %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                               " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  select(ses_indicator, parental_practices, result_unstd) %>% 
  pivot_wider(names_from = "parental_practices",
              values_from = "result_unstd") 
write.csv(results_practices, "output/Tables/results_practices")


# 2) mental health by parenting practices (pp): path b
LGC_pp_models <- UiN_model_data %>%
  pivot_wider(names_from = "ses_indicator",
              values_from = "ses_value") %>% 
  pivot_longer(cols = c(behavioral_monitoring:social_support),
               names_to = "parental_practices",
               values_to = "practices_value") %>% 
  group_by(parental_practices, mental_health) %>% 
  nest() %>% 
  mutate(cond_pp = map(.x = data,
                      ~ growth(model = growth_model_parental_practices,
                               data = .x,
                               estimator = "MLR",
                               se = "robust",
                               missing = "fiml.x")))

model_estimates_pp <- LGC_pp_models %>%    
  mutate(estimates = map(.x = cond_pp,
                         ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                           filter(lhs %in% c("i", "s", "q"), rhs == "practices_value") %>% 
                           select(lhs, est, ci.lower, ci.upper))) %>% 
  select(estimates) %>% 
  unnest() %>% 
  mutate(result = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                               " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  select(lhs, result) %>% 
  ungroup() %>% 
  pivot_wider(names_from = parental_practices,
              values_from = result) %>% 
  mutate(lhs = factor(lhs, levels = c("i", "s", "q"))) %>% 
  arrange(lhs)
write.csv(model_estimates_pp, "output/Tables/model_estimates_pp")



### MEDIATION MODELS ###

# 3) indirect effect of family disadvantage (exposure) on mental health (outcome) via parental practice (mediator)
n_cores <- parallel::detectCores() - 1

mediation_models <- LGC_models %>% 
  filter(if_any(i_regr_sign:q_regr_sign, ~ .x == TRUE)) %>% 
  select(data, i_regr_sign:q_regr_sign) %>% 
  unnest() %>% 
  pivot_longer(cols = behavioral_monitoring:social_support,
               names_to = "parental_practices",
               values_to = "practices_value") %>% 
  group_by(mental_health, ses_indicator, parental_practices, i_regr_sign, s_regr_sign, q_regr_sign) %>% 
  nest() %>% 
  mutate(model_type = case_when(i_regr_sign == TRUE & s_regr_sign == TRUE & q_regr_sign == TRUE ~ mediation_growth_model_all,
                                i_regr_sign == TRUE & s_regr_sign == FALSE & q_regr_sign == FALSE ~ mediation_growth_model_i,
                                i_regr_sign == FALSE & s_regr_sign == FALSE & q_regr_sign == TRUE ~ mediation_growth_model_q,
                                i_regr_sign == FALSE & s_regr_sign == TRUE  & q_regr_sign == TRUE ~ mediation_growth_model_sq),
         mediation_models = map2(.x = data, .y = model_type,
                                ~ growth(model = .y,
                                         data = .x,
                                         estimator = "MLR",
                                         se = "robust", 
                                         missing = "fiml.x")),
         mediation_models_boot = map2(.x = data, .y = model_type,
                                       ~ growth(model = .y,
                                                data = .x,
                                                estimator = "ML",
                                                se = "boot", # bootstrapping method
                                                bootstrap = 10000, 
                                                parallel= "snow", # parallel computation 
                                                ncpus= n_cores,
                                                iseed = 112233, # seed for reproducibility
                                                missing = "fiml.x"))) # FIML for all variables, incl. exogenous (x)

# extracting model estimates for association between ses and mental health
model_estimates_mediation_boot <- mediation_models %>%    
  mutate(estimates = map(.x = mediation_models_boot,
                         ~ parameterEstimates(.x, boot.ci.type = "norm", standardized = T, rsquare = T) %>% 
                           
                           filter(label %in% c("indirect_i", "indirect_s", "indirect_q")) %>% 
                           select(lhs, est, ci.lower, ci.upper))) %>% 
  select(estimates) %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                               " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  select(-c(i_regr_sign, s_regr_sign, q_regr_sign)) %>% 
  mutate(lhs = str_remove(lhs, "indirect_"))

total_effects <- model_estimates_cond %>% 
  mutate(total = est) %>% 
  select(mental_health, ses_indicator, lhs, total)

model_estimates_mediation_boot <- merge(model_estimates_mediation_boot, total_effects, 
                                        by = c("mental_health", "ses_indicator", "lhs")) %>% 
  mutate(prop_mediated = est/total*100,
         prop_mediated.lower = ifelse(total > 0, ci.lower/total*100, ci.upper/total*100),
         prop_mediated.upper = ifelse(total > 0, ci.upper/total*100, ci.lower/total*100),
         prop_mediated_tidy = paste0(formatC(round(prop_mediated, 2), format = "f", digits = 2), 
                               " (", formatC(round(prop_mediated.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(prop_mediated.upper, 2), format = "f", digits = 2), ")"),
         prop_mediated_tidy = ifelse(prop_mediated_tidy < 0, "NA", prop_mediated_tidy))

results_model_estimates_mediation_boot <- model_estimates_mediation_boot %>% 
  mutate(parental_practices = factor(case_when(parental_practices == "behavioral_monitoring" ~ "Behavioral monitoring",
                                        parental_practices == "psych_overcontrol" ~ "Psychological overcontrol",
                                        parental_practices == "warmth_care" ~ "Warmth",
                                        parental_practices == "edu_investment" ~ "Educational investment",
                                        parental_practices == "social_support" ~ "Social support"),
                                     levels = c("Behavioral monitoring", "Warmth", "Educational investment", "Social support", "Psychological overcontrol")),
         mental_health = factor(mental_health, levels = c("Depression", "Anxiety", "Alcohol use", "Conduct problems", "Loneliness", "Self-esteem")),
         lhs = factor(case_when(lhs == "i" ~ "Intercept",
                         lhs == "s" ~ "Slope",
                         lhs == "q" ~ "Quadratic"), levels = c("Intercept", "Slope", "Quadratic")),
         ses_indicator = factor(ses_indicator, levels = c("Education", "Occupation", "Living situation"), ordered = T)) %>% 
  select(mental_health, ses_indicator, parental_practices, lhs, result_unstd, prop_mediated_tidy) %>% 
  arrange(mental_health, parental_practices, lhs)

results_mediation_proportions_boot <- results_model_estimates_mediation_boot %>% 
  select(-result_unstd) %>% 
  pivot_wider(names_from = parental_practices,
              values_from = prop_mediated_tidy) %>% 
  arrange(mental_health, ses_indicator, lhs)
write.csv(results_mediation_proportions_boot, "output/Tables/results_mediation_proportions_boot")

results_mediation_estimates_boot <- results_model_estimates_mediation_boot %>% 
  select(-prop_mediated_tidy) %>% 
  pivot_wider(names_from = parental_practices,
              values_from = result_unstd)
write.csv(results_mediation_estimates_boot, "output/Tables/results_mediation_estimates_boot")




##########################
### MODEL DIAGNOSTICS ###
#########################

my_fit_indices <- c("df", "chisq.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "srmr_bentler", "rmsea.robust")

### global fit: fit indices ###
global_fit_uncond_models <- LGC_models[c(1,4,7,10,13,16),] %>% 
  mutate(global_fit_measures = map(.x = uncond_quadr,
                                   ~ rownames_to_column(data.frame(fit = fitmeasures(.x, my_fit_indices, output = "vector"))))) %>% 
  select(global_fit_measures) %>% 
  unnest() %>% 
  transmute(fit_index = rowname,
            fit_value = fit) %>% 
  pivot_wider(names_from = fit_index,
              values_from = fit_value) %>% 
  mutate(across(chisq.scaled:rmsea.robust, ~ formatC(round(.x, 2), format = "f", digits = 2)),
         pvalue.scaled = as.character(ifelse(pvalue.scaled == "0.00", "< 0.01", pvalue.scaled))) %>% 
  ungroup() %>% 
  select(-ses_indicator)
write.csv(global_fit_uncond_models, "output/Tables/global_fit_uncond_models")

local_fit_uncond_models <- LGC_models[c(1,4,7,10,13,16),] %>% 
  mutate(local_fit_measures = map(.x = uncond_quadr,
                                   ~ as.data.frame(as.matrix(lavResiduals(.x, type = "cor.bollen")$mean)) %>% 
                                    rownames_to_column)) %>% 
  select(local_fit_measures) %>% 
  unnest() %>% 
  pivot_wider(names_from = rowname,
              values_from = V1) %>% 
  mutate(across(T2:T5, ~ formatC(round(.x, 2), format = "f", digits = 2))) %>% 
  ungroup %>% 
  select(-ses_indicator)
write.csv(local_fit_uncond_models, "output/Tables/local_fit_uncond_models")

global_fit_mediation <- mediation_model %>%                                
  mutate(global_fit_measures = map(.x = mediation_models,
                                   ~ rownames_to_column(data.frame(fit = fitmeasures(.x, my_fit_indices, output = "vector"))))) %>% 
  select(global_fit_measures) %>% 
  unnest() %>% 
  transmute(fit_index = rowname,
            fit_value = fit) %>% 
  pivot_wider(names_from = fit_index,
              values_from = fit_value) %>% 
  mutate(across(chisq.scaled:rmsea.robust, ~ formatC(round(.x, 2), format = "f", digits = 2)),
         pvalue.scaled = as.character(ifelse(pvalue.scaled == "0.00", "< 0.01", pvalue.scaled)),
         parental_practices = case_when(parental_practices == "behavioral_monitoring" ~ "Behavioral monitoring",
                                        parental_practices == "psych_overcontrol" ~ "Psychological overcontrol",
                                        parental_practices == "warmth_care" ~ "Warmth/care",
                                        parental_practices == "edu_investment" ~ "Educational investment",
                                        parental_practices == "social_support" ~ "Social support")) %>% 
  ungroup() %>% 
  select(-c(i_regr_sign:q_regr_sign))
write.csv(global_fit_mediation, "output/Tables/global_fit_mediation")



        
#################
### PLOTTING ####
#################

# setting up a font style
windowsFonts(Times = windowsFont("Times New Roman"))

# color palette
color_gradient_2 <- brewer.pal(n = 10, name = 'PRGn')[c(7,9)]
my_colors <- brewer.pal(n = 6, name = 'Dark2')
my_colors2 <- c("Advantaged" = my_colors[[1]],
                "Disadvantaged" = my_colors[[2]])
my_colors3 <- c("Intercept" = my_colors[[1]],
                "Slope" = my_colors[[2]],
                "Quadratic" = my_colors[[3]])

### latent growth curves ###

# new dataset with all combinations of predictors (SES, gender etc.) 
new_data <- data.frame(ses_value = c(0,1,0,1),
                       gender = c(0,1,1,0),
                       western_ethn = 0,
                       nonwestern_ethn = 0,
                       age = 0,
                       T2 = NA,
                       T3 = NA,
                       T4 = NA,
                       T5 = NA)

plot_data <- data.frame(age = rep(17:43, 4),
                        gender = c(rep(0, 54), rep(1, 54)),
                        ses_value = c(rep(0, 27), rep(1, 27), rep(0, 27), rep(1, 27)),
                        decade = rep(0:26/10, 4))

prop_women <- table(UiN_data_isei$gender)[["Girl/woman"]]/length(UiN_data_isei$id)

# model-based predicted values of mental health trajectories
predicted <- LGC_models %>% 
  mutate(standard_errors = map(.x = cond_ses,
                              ~ data.frame(parameterEstimates(.x)[,c("lhs", "op", "se")] %>% 
                                             filter(lhs %in% c("i", "s", "q") & op == "~1") %>% 
                                             select(lhs, se) %>% 
                                             pivot_wider(names_from = lhs, 
                                                         values_from = se, 
                                                         names_glue = "{lhs}_{.value}"))),
         model_prediction = map(.x = cond_ses,
                         ~ data.frame(lavPredict(.x, type = "lv", newdata = new_data, append.data = T)) %>% 
                           select(i, s, q, ses_value, gender) %>% 
                           merge(., plot_data, by = c("ses_value", "gender")))) %>% 
  select(model_prediction, standard_errors) %>% 
  unnest(cols = c(model_prediction, standard_errors))
  

predicted_overall <- predicted %>% 
  mutate(outcome = i + decade*s + decade^2*q,
         gender = ifelse(gender == 0, "men", "women"),
         weight = ifelse(gender == "women", prop_women, 1-prop_women),
         weighted_outcome = outcome*weight,
         ses_value = as.factor(ifelse(ses_value == 1, "Advantaged", "Disadvantaged"))) %>% 
  group_by(mental_health, ses_indicator, ses_value, age, decade, i_se, s_se, q_se) %>% 
  summarise(weighted_outcome = sum(weighted_outcome)) %>% 
  mutate(upper_ci = (weighted_outcome + 1.96*i_se) + (decade*1.96*s_se) + (decade^2*1.96*q_se),
         lower_ci = (weighted_outcome - 1.96*i_se) - (decade*1.96*s_se) - (decade^2*1.96*q_se),
         lower_ci = ifelse(lower_ci < 0, 0, lower_ci))

plot_curves <- predicted_overall %>% 
  mutate(mental_health = factor(mental_health, levels = c("Depression", "Anxiety", "Alcohol use", "Conduct problems", "Loneliness", "Self-esteem")),
         ses_indicator = factor(ses_indicator, levels = c("Education", "Occupation", "Living situation"))) %>% 
  arrange(mental_health, ses_indicator) %>% 
  select(age, mental_health, ses_indicator, ses_value, weighted_outcome, lower_ci, upper_ci) %>% 
  ggplot(aes(x = age, y = weighted_outcome, color = ses_value, group = ses_value)) +
  geom_line(linewidth = 1) +
#  geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, fill = ses_value), 
#              alpha=0.1, color = NA) +
  facet_rep_grid(mental_health ~ ses_indicator, 
             labeller = labeller(ses_indicator=label_value, mental_health=label_value),
             repeat.tick.labels = FALSE,
             scales = "free") +
  scale_x_continuous(breaks=c(17,22,28,43)) +
  ggh4x::facetted_pos_scales(y = list(
    mental_health == "Depression" ~ scale_y_continuous(breaks = c(20, 25, 30), limits = c(18, 30)),
    mental_health == "Anxiety" ~ scale_y_continuous(breaks = c(10, 15, 20), limits = c(10, 20)),
    mental_health == "Loneliness" ~ scale_y_continuous(breaks = c(20, 25, 30), limits = c(20, 30)),
    mental_health == "Alcohol use" ~ scale_y_continuous(breaks = c(30, 40, 50, 60), limits = c(28, 60)),
    mental_health == "Conduct problems" ~ scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 14)),
    mental_health == "Self-esteem" ~ scale_y_continuous(breaks = c(60, 65, 70), limits = c(60, 70)))) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  theme_classic() + 
  theme(strip.text = element_text(face="bold"),
        text = element_text(size=26,family = "Times")) +
  theme(legend.position = c(0.93, 0.45),
        legend.title = element_text(size = 22), 
        legend.text = element_text(size = 20)) +
  guides(fill="none") +
  labs(x = "Age (in years)",
       y = "POMP score (0 - 100)",
       color = "Family disadvantage")

ggsave(filename = "plot_curves.jpg",
       path = "output/Graphs", 
       width = 27, 
       height = 18,  
       bg="white",
       dpi=700)

ggsave(filename = "plot_curves.eps",
       path = "output/Graphs", 
       width = 27, 
       height = 18,  
       device = cairo_ps,
       bg="white",
       dpi=700)



# Total effect: ses -> mental health
plot_total_effect <- model_estimates_cond %>%
  mutate(mental_health = factor(mental_health, levels = c("Depression", "Anxiety", "Loneliness", "Alcohol use", "Conduct problems", "Self-esteem")),
         ses_indicator = factor(ses_indicator, levels = c("Education", "Occupation", "Living situation")),
         Parameter = factor(case_when(lhs == "i" ~ "Intercept",
                                      lhs == "s" ~ "Slope",
                                      lhs == "q" ~ "Quadratic"),
                            levels = c( "Quadratic", "Slope", "Intercept"),
                            ordered = TRUE)) %>%
  arrange(mental_health) %>% 
  ggplot(aes(x = est,
             y = Parameter,
             color = Parameter,
             group = Parameter)) +
  geom_point(size = 3.5,
             position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = ci.lower,
                    xmax = ci.upper),
                linewidth = 1,
                width = 0.5,
                position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = my_colors3, 
                     breaks = c("Intercept", "Slope", "Quadratic")) +
  facet_rep_grid(mental_health ~ ses_indicator, 
                 labeller = labeller(ses_indicator=label_value, mental_health=label_value),
                 repeat.tick.labels = FALSE) +
  labs(x = "Effect estimate (95% CI)", 
       y = "") +
  theme_classic() + 
  theme(strip.text = element_text(face="bold"),
        text = element_text(size=24,family = "Times"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.position = c(0.90, 0.26),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18))

ggsave(filename = "plot_total_effect.jpg",
       path = "output/Graphs", 
       width = 24, 
       height = 20,  
       bg="white",
       dpi=700)


# Indirect effect: ses -> parental practices -> mental health
plot_indirect_effect <- model_estimates_mediation_boot %>%
  mutate(Parameter = factor(case_when(lhs == "i" ~ "Intercept",
                                      lhs == "s" ~ "Slope",
                                      lhs == "q" ~ "Quadratic"),
                            levels = c("Quadratic", "Slope", "Intercept"),
                            ordered = TRUE),
         ses_indicator = ifelse(ses_indicator == "ISEI", "Occupation", ses_indicator),
         ses_indicator = factor(ses_indicator, levels = c("Education", "Occupation", "Living situation")),
         mental_health = factor(mental_health, levels = c("Depression", "Anxiety", "Loneliness", "Alcohol use", "Conduct problems", "Self-esteem")),
         parental_practices = case_when(parental_practices == "behavioral_monitoring" ~ "Behavioral monitoring",
                                        parental_practices == "psych_overcontrol" ~ "Psychological overcontrol",
                                        parental_practices == "warmth_care" ~ "Warmth/care",
                                        parental_practices == "edu_investment" ~ "Educational investment",
                                        parental_practices == "social_support" ~ "Social support")) %>% 
  arrange(mental_health) %>% 
  ggplot(aes(x = est,
             y = Parameter,
             color = parental_practices,
             group = parental_practices)) +
  geom_point(size = 2.5,
             position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = ci.lower,
                    xmax = ci.upper),
                linewidth = 0.8,
                width = 0.5,
                position = position_dodge(width = 0.8)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_rep_grid(mental_health ~ ses_indicator, 
                 labeller = labeller(ses_indicator=label_value, mental_health=label_value),
                 repeat.tick.labels = FALSE) +
  #ggh4x::facetted_pos_scales(x = list(
    #ses_indicator == "Education" ~ scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), limits = c(-2.3, 3.1)),
    #ses_indicator == "ISEI" ~ scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), limits = c(-3.3, 2.4)),
    #ses_indicator == "Living situation" ~ scale_x_continuous(breaks = c(-2, -1, 0), limits = c(-2.05, 0.7)))) +
  labs(x = "Indirect effect (95% CI)", 
       y = "Parameter",
       color = "Parenting practices") +
  scale_color_manual(values = my_colors[1:5]) +
  guides(colour = guide_legend(reverse=T)) +
  theme_classic() + 
  theme(strip.text = element_text(face="bold"),
        panel.spacing.x = unit(1, "lines"),
        text = element_text(size = 24, family = "Times")) +
  theme(legend.position = c(0.92, 0.24),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

ggsave(filename = "plot_indirect_effect.jpg",
       path = "output/Graphs", 
       width = 24, 
       height = 20,  
       bg="white",
       dpi=700)



# Parental practices by SES
plot_practices_diff <- data_practices %>% 
  ggplot(aes(y = est, x = parental_practices, group = parental_practices, color = parental_practices)) +
  geom_point(size = 3) +
  labs(x = "", 
       y = "Mean difference (95% CI)", 
       color = "Parenting practices") +
  scale_color_manual(values = my_colors[1:5]) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), linewidth = 1, width = 0.4) +
  facet_rep_wrap(~ ses_indicator, 
                  ncol = 3) +
  scale_y_continuous(limits = c(-8, 15.1), breaks = c(-5, 0, 5, 10, 15)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() + 
  theme(strip.text = element_text(size = 20, face="bold"),
        axis.text.x = element_blank(),
        panel.border=element_blank(),
        axis.ticks = element_blank(),
        panel.spacing.x = unit(5, "lines"),
        text = element_text(size=20,family = "Times")) +
  theme(legend.position = c(0.88, 0.16),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18))

ggsave(filename = "plot_practices_diff.jpg",
       path = "output/Graphs", 
       width = 20, 
       height = 8,  
       bg="white",
       dpi=700)

ggsave(filename = "plot_practices_diff.eps",
       path = "output/Graphs", 
       width = 20, 
       height = 8,   
       device = cairo_ps,
       bg="white",
       dpi=700)

  


         
         
