###################################################################################################################
### Social gradient in mental health from adolescence to midlife and the mediating role of parental practices: ###
############################# a population-based 28-year longitudinal study ######################################
##################################################################################################################

#########################################################
##### INVERSE PROBABILITY ATTRITION WEIGHTS (IPAW) #####
########################################################


library(haven) # file reading
library(tidyverse) # data pre-processing
library(glmnet) # LASSO regression
library(WeightIt) # inverse probability weights
library(cobalt) # covariate balance assessment
library(lavaan) #structural equation modelling

# id number of schools and classes that were followed-up
schools_followed <- unique(UiN_data_isei$School1)
classes_followed <- unique(UiN_data_isei$Class1)

# loading the data
UiN_data_orig <- read_sav("N:/durable/Data_analyses/Libor/data/UiN_regfile_030124.sav") 
UiN_data_T1_sample <- as_tibble(read.csv("N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_data_T1_sample")) %>% 
  filter((School1 %in% schools_followed & Class1 %in% classes_followed))




##### 1.) IDENTIFICATION OF BASELINE (T1) VARIABLES ##### 

UiN_data_T1_sample <- as_tibble(merge(UiN_data_orig, UiN_data_T1_sample, by = "id")) 

UiN_data_IPAW_id <- UiN_data_T1_sample %>% 
# identifying relevant baseline variables by context theme
  select(participated, id,
         # Personal characteristics
         gende, Origi1_1, Origi1_2, Relig1_1, Urban1_1, Disa1_01, Disa1_02, Disa1_03, Disa1_04, Disa1_05, Sibli1_1, 
         # Parental background 
         ParJo1_7, ParJo1_8, ParJo1_1, ParJo1_2, education_cat, ethnic_background, living_situation, Books1_1, Subst1p1, Subst1p2, Subst1p3,
         # Education and jobs,
         Grade1s1, Grade1j1, Grade1s2, Grade1j2, Grade1s3, Grade1j3, Homew1_1, Futur1i1, Skip1_3, Job1_1, Job1s1, Allow1_1, Allow1_2, Bank1_1,
         # Biological development and sexuality,
         bmi1, bass1_m, Looks1_1, Looks1_2, SexEx1_1, SexEx1_2, SexEx1_3, SexEx1_4, 
         # Personality, 
         bsri1m_m, bsri1f_m, MCSD1_m,  ycs1_m, rsss1_m, sppa1scm, sppa1sam, sppa1acm, sppa1pam, sppa1ram, sppa1cfm,
         # Mental health,
         depression_1, anxiety_1, loneliness_1, selfesteem_1, eat1_m, cp_crim1, cp_cov1, cp_scho1, Subst1_1, Smoke1_1, Subst1_2, Subst1_3, SCL1_13, Para1_1,
         # Social arenas, 
         behavioral_monitoring, psych_overcontrol, warmth_care, social_support, Frien1_1, Frien1_3, BFCP1_1a, BFCP1_2a, BFCP1_3a, BFCP1_4a, Girlf1_1, 
        # Leisure time
         LeAc1_01, LeAc1_03, LeAc1_06, LeAc1_10, LeAc1_12, LeAc1_22, LeAc1_04, LeAc1_05, LeAc1_07, LeAc1_13, LeAc1_14, LeAc1_20, TV1_02, TV1_03, TV1_05, TV1_09) %>% 
# creating new categorical variables
  mutate(Sibli1_1 = case_when(Sibli1_1 == 0 ~ "0",
                              Sibli1_1 == 1 ~ "1",
                              Sibli1_1 == 2 ~ "2",
                              Sibli1_1 == 3 ~ "3",
                              Sibli1_1 > 3 ~ "4+"),
         ethnic_background = case_when(ethnic_background == "Norwegian" ~ 0,
                                       ethnic_background %in% c("Non-Western", "Western") ~ 1),
         grades_Norw = ifelse(is.na(Grade1j1), Grade1s1, Grade1j1), 
         grades_Math = ifelse(is.na(Grade1j2), Grade1s2, Grade1j2),
         grades_Engl = ifelse(is.na(Grade1j3), Grade1s3, Grade1j3),
         across(grades_Norw:grades_Engl, ~ ifelse(.x == 6, NA, .x)),
         across(grades_Norw:grades_Math, ~ ifelse(.x == 1, 2, .x)),
         social_support = case_when(round(social_support) == 33 ~ 1,
                                    round(social_support) == 67 ~ 2,
                                    social_support == 100 ~ 3,
                                    social_support == 0 ~ 0),
         Skip1_3 = case_when(Skip1_3 == 1 ~ "0",
                             Skip1_3 == 2 ~ "1",
                             Skip1_3 %in% c(3,4,5) ~ "2"),
         across(c(cp_scho1, cp_cov1), ~ ifelse(.x > 2.5, "1", ifelse(is.na(.x), NA, "0"))),
         cp_crim1 = ifelse(cp_crim1 > 1.2, "1", ifelse(is.na(cp_crim1), NA, "0")),
         Subst1_2 = ifelse(Subst1_2 > 1, "1", ifelse(is.na(Subst1_2), NA, "0")),
         Subst1_3 = ifelse(Subst1_3 > 1, "1", ifelse(is.na(Subst1_3), NA, "0")),
         Smoke1_1 = ifelse(Smoke1_1 %in% c(1,2), 0, ifelse(Smoke1_1 %in% c(3,4,5), 1, NA)),
         TV1_progr = rowMeans(select(., c(TV1_03, TV1_05, TV1_09)), na.rm = T),
         across(LeAc1_04:LeAc1_20, ~ ifelse(.x == 0, "0", ifelse(is.na(.x), NA, "1"))),
         gende = ifelse(is.na(gende), 0, gende), # just 0 missing -> recoding to man/boy
         across(c(Subst1p1, Subst1p2, Subst1p3), ~ ifelse(.x == 5, 4, .x))) %>%
# creating quantile-based variables out of continuous ones and introducing a category for missing values
  mutate(across(c(Books1_1, Homew1_1, Bank1_1, Allow1_2, bmi1, bass1_m, bsri1m_m:eat1_m,behavioral_monitoring:warmth_care, LeAc1_01:LeAc1_22, TV1_progr),
         ~ ntile(.x, 5)),
# coding all variables as factors (i.e., categorical variables)
         across(everything(),
                ~ as.factor(ifelse(is.na(.x), "99", .x)))) %>%
# selecting final set of variables
  select(-c(starts_with("Grade1"), TV1_03, TV1_05, TV1_09))

UiN_data_IPAW <- UiN_data_IPAW_id %>% 
  select(-id)

# dataset in matrix-form with dummy variables
IPAW_data <- cbind(UiN_data_IPAW[,1], as.data.frame(model.matrix(~ . -1, data = UiN_data_IPAW[,-1])))[,-2] 

# checking the variables
UiN_data_IPAW %>% map(table)
nr_dummies <- length(colnames(IPAW_data)) - 1

UiN_data_IPAW_long <- UiN_data_IPAW %>% 
  pivot_longer(names_to = "variable", 
               values_to = "value", 
               cols = -participated) %>% 
  mutate(context_theme = case_when(str_detect(variable, "gende|Origi1|Relig1|Urban1|Disa1|Sibli1") ~ "Personal characteristics",
                                   str_detect(variable, "ParJo1|education_cat|ethnic_back|living_sit|Books1|Subst1p") ~ "Parental characteristics",
                                   str_detect(variable, "grades|Homew1|Futur1i|Skip1|Job1|Allow1|Bank1") ~ "Education and jobs",
                                   str_detect(variable, "bmi1|bass1|Looks1|SexEx1") ~ "Biological development and sexuality", 
                                   str_detect(variable, "bsri1|MCSD1|ycs1|rsss1|sppa1") ~ "Personality",
                                   str_detect(variable, "depression|anxiety|loneliness|selfesteem|eat1|cp_crim|cp_cov|cp_scho|Subst1_1|Smoke1_1|Subst1_2|Subst1_3|SCL1_13|Para1") ~ "Mental health",
                                   str_detect(variable, "behavioral_mon|psych_over|warmth_care|social_supp|Frien1|Frien1|BFCP1|Girlf1") ~ "Social arenas",
                                   str_detect(variable, "LeAc1|TV1") ~ "Leisure time"))

# checking the variables and context themes
UiN_data_IPAW_long %>% 
  distinct(context_theme, variable) %>% 
  arrange(context_theme) %>% 
  print(n = 100)




##### 2.) VARIABLE SELECTION via least absolute shrinkage and selection operator (LASSO) regression ######
set.seed(100) 
LASSO <- UiN_data_IPAW_long %>% 
  group_by(context_theme) %>% 
  nest() %>% 
  mutate(data = map(.x = data,
                    ~ .x %>% group_by(variable) %>% 
                      mutate(row = row_number(),
                             value = as.character(value)) %>% 
                      pivot_wider(names_from = "variable",values_from = "value") %>% 
                      select(-row) %>% 
                      mutate(across(everything(), ~ as.factor(.x)))),
         matrix = map(.x = data,
                      ~ cbind(.x[,"participated"], as.data.frame(model.matrix(~ ., data = .x[,-1])))[,-2]),
         # covariate balance before using IPAWs
         balance_bef = map(.x = matrix,
                           ~ bal.tab(participated ~ .,
                                     data = .x, 
                                     stats = "mean.diffs", # difference in proportion
                                     estimand = "ATE", 
                                     treat = "participated",
                                     thresholds = c(m = .05))),
         # selecting unbalanced covariates
         pretest_unbalanced = map(.x = balance_bef,
                          ~ .x$Balance[.x$Balance$M.Threshold.Un == "Not Balanced, >0.05", c("Type", "Diff.Un")] %>% 
                            mutate(Diff.Un = formatC(round(Diff.Un, 2), format = "f", digits = 2))),
         # running LASSO regressions
         lasso = map(.x = matrix,
                     ~ cv.glmnet(x = model.matrix(~ . -1, data = .x[,-1]), 
                                 y = .x[,"participated"],  
                                 family = "binomial", # binary outcome: dropped out (=1) OR followed-up (=1)
                                 alpha = 1, # lasso penalty
                                 nfolds = 10)), # 10-fold cross-validation
         predictors = map(.x = lasso,
                          ~ as.data.frame(as.matrix(coef(.x))) %>% 
                            rownames_to_column() %>% 
                            filter(rowname != "(Intercept)",
                                   s1 != 0) %>% 
                            transmute(rowname = rowname,
                                      `exp(b)` = formatC(round(exp(s1), 2), format = "f", digits = 2))))

pretest_unbalanced <- rownames_to_column(do.call(rbind, LASSO$pretest_unbalanced))[,c("rowname", "Diff.Un")]
lasso_predictors <- do.call(rbind, LASSO$predictors)
extra_covs_pretest <- setdiff(pretest_unbalanced$rowname, lasso_predictors$rowname)
extra_covs_lasso <- setdiff(lasso_predictors$rowname, pretest_unbalanced$rowname)




##### 3) CONSTRUCTION OF INVERSE PROBABILITY ATTRITION WEIGHTS (IPAW) using logistic regression #####

# dataset with all LASSO-selected baseline predictors
IPAW_data_selected <- IPAW_data %>% 
  select(participated, lasso_predictors$rowname)

IPAW <- weightit(participated ~ .,
                 data = IPAW_data_selected, 
                 method = "glm", 
                 estimand = "ATE")

# trimming large weights
IPAW.trim <- trim(IPAW, at = .99)

# inspecting weights and effective sample size (ESS): size of an un-weighted sample that contains the same precision as a weighted sample
summary(IPAW.trim)
ESS <- summary(IPAW.trim)$effective.sample.size %>% rownames_to_column()
precision_decrease <- round(100-(ESS[ESS$rowname == "Weighted",3]/ESS[ESS$rowname == "Unweighted",3]*100), 2)

# checking the covariate balance after weighting using all previously non-balanced covariates
IPAW_data_selected <- IPAW_data %>% 
  select(participated, c(pretest_unbalanced$rowname, extra_covs_lasso))

balance_aft <- bal.tab(IPAW.trim, 
                       data = IPAW_data_selected,
                       addl = extra_covs_pretest,
                       stats = "mean.diffs",
                       thresholds = c(m = .05))

# mean differences in covariates before and after weighting
posttest_balance <- rownames_to_column(balance_aft$Balance)[, c("rowname", "Diff.Adj")] %>% 
  mutate(Diff.Adj = formatC(round(Diff.Adj, 2), format = "f", digits = 2))

balancing_results <- merge(pretest_unbalanced, lasso_predictors, by = "rowname", all = TRUE) %>% 
                           merge(., posttest_balance, by = "rowname", all = TRUE) %>% 
                          arrange(`exp(b)`)
write.csv(balancing_results, "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/output/Tables/balancing_results")

mean_diff_before <- round(mean(abs(as.numeric(balancing_results$Diff.Un)), na.rm = T), 3)*100
mean_diff_after <- round(mean(abs(as.numeric(balancing_results$Diff.Adj)), na.rm = T), 3)*100
bias_reduction <- round(100-mean_diff_after/mean_diff_before*100, 1)




##### 4.) WEIGHTED ANALYSES: re-running conditional LGC models using IPAWs #####

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


# bringing weights into the dataset
UiN_data_IPAW_id$weights <- as.numeric(IPAW.trim$weights)
UiN_data_IPAW_id <- filter(UiN_data_IPAW_id, participated == 1)
IPAW_data$weights <- as.numeric(IPAW.trim$weights)
write.csv(UiN_data_IPAW_id[, c("id", "weights")], "N:/durable/Data_analyses/Libor/Social_Gradient_Mental_Health_Lifespan/data/UiN_data_IPAW")


# quality check: inspecting the weights by LASSO-selected variables
IPAW_data %>% 
  filter(participated == 1) %>% 
  select(weights, lasso_predictors$rowname) %>% 
  filter(weights > 4)
sum(UiN_data_IPAW_id$weights - mean(UiN_data_IPAW_id$weights) + 1) 


# Latent growth curve models
LGC_models_weighted <- UiN_model_data %>% 
  merge(., UiN_data_IPAW_id[, c("id", "weights")], by = "id") %>% 
  group_by(mental_health, ses_indicator) %>% 
  nest() %>% 
  mutate(cond_ses = map(.x = data,
                        ~ growth(model = growth_model_cond_ses,
                          data = .x,
                          estimator = "MLR",
                          se = "robust",
                          sampling.weights = "weights", # implementing IPAWs
                          missing = "fiml.x")),
         cond_ses_intT5_inv = map(.x = data,
                                  ~ growth(model = growth_model_cond_ses_intT5_inv,
                                           data = .x,
                                           estimator = "MLR",
                                           se = "robust",
                                           sampling.weights = "weights",
                                           missing = "fiml.x")))

# extracting model estimates
model_estimates_cond_weighted <- LGC_models_weighted %>%    
  mutate(estimates = map(.x = cond_ses,
                         ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                           filter(lhs %in% c("i", "s", "q"), rhs == "ses_value") %>% 
                           select(lhs, est, ci.lower, ci.upper))) %>% 
  select(estimates) %>% 
  unnest() %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                               " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  rename_with(.cols = est:result_unstd, ~ paste(., "w", sep = "_"))

# a re-parametrized model with intercept at T5
intercept_T5_weighted <- LGC_models_weighted %>% 
  mutate(intercept_T5 = map(.x = cond_ses_intT5_inv,
                            ~ parameterEstimates(.x, standardized = T, rsquare = T) %>% 
                              filter(lhs %in% c("i"), rhs == "ses_value") %>% 
                              select(lhs, est, ci.lower, ci.upper))) %>% 
  select(intercept_T5) %>% 
  unnest() %>% 
  mutate(lhs = "T5") %>% 
  mutate(result_unstd = paste0(formatC(round(est, 2), format = "f", digits = 2), 
                               " (", formatC(round(ci.lower, 2), format = "f", digits = 2), ", ", 
                               formatC(round(ci.upper, 2), format = "f", digits = 2), ")")) %>% 
  rename_with(.cols = est:result_unstd, ~ paste(., "w", sep = "_"))


# bringing together original LGC model estimates (int = T1, linear and quadratic trends) 
# and re-parametrized model intercept estimate (= T5)
model_estimates_cond_ext_weighted <- rbind(model_estimates_cond_weighted, intercept_T5_weighted)

results_model_estimates_cond_ext_weighted <- model_estimates_cond_ext_weighted %>% 
  select(lhs, result_unstd_w) %>% 
  pivot_wider(names_from = "lhs",
              values_from = "result_unstd_w")
write.csv(results_model_estimates_cond_ext_weighted, "output/Tables/results_model_estimates_cond_ext_weighted")

# comparing with the original un-weighted analyses
model_estimates_cond <- read.csv("output/Tables/model_estimates_cond")

weighted_results_equal_sign <- cbind(model_estimates_cond_ext[,-7], model_estimates_cond_ext_weighted[,4:6]) %>% 
  mutate(sign = ifelse(est < 0 & ci.lower < 0 & ci.upper < 0 | est > 0 & ci.lower > 0 & ci.upper > 0, TRUE, FALSE),
         sign_w = ifelse(est_w < 0 & ci.lower_w < 0 & ci.upper_w < 0 | est_w > 0 & ci.lower_w > 0 & ci.upper_w > 0, TRUE, FALSE),
         sign_equal = sign == sign_w) %>% 
  select(mental_health, ses_indicator, lhs, sign_equal) %>% 
  pivot_wider(names_from = "lhs",
              values_from = "sign_equal")
  
weighted_results_diff <- cbind(model_estimates_cond_ext[,-7], model_estimates_cond_ext_weighted[,4:6]) %>%
  select(mental_health, ses_indicator, lhs, est, est_w) %>% 
  mutate(diff = est_w-est) %>% 
  select(mental_health, ses_indicator, lhs, diff) %>% 
  pivot_wider(names_from = "lhs",
              values_from = "diff") %>% 
  mutate(across(i:`T5`, ~ formatC(round(.x, 2), format = "f", digits = 2)))
write.csv(weighted_results_diff, "output/Tables/weighted_results_diff")

mean_diffs_by_ses <- weighted_results_diff %>% group_by(ses_indicator) %>% summarise(across(i:`T5`, ~ round(mean(.x, na.rm = T), 2))) 
mean_diffs_by_mental_health <- weighted_results_diff %>% group_by(mental_health) %>% summarise(across(i:T5, ~ round(mean(.x, na.rm = T), 2)))
  



### plotting ###

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
predicted_w <- LGC_models_weighted %>% 
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


predicted_overall_w <- predicted %>% 
  mutate(outcome = i + decade*s + decade^2*q,
         gender = ifelse(gender == 0, "men", "women"),
         weight = ifelse(gender == "women", prop_women, 1-prop_women),
         weighted_outcome = outcome*weight,
         ses_value = as.factor(ifelse(ses_value == 1, "High SES", "Low SES"))) %>% 
  group_by(mental_health, ses_indicator, ses_value, age, decade, i_se, s_se, q_se) %>% 
  summarise(weighted_outcome = sum(weighted_outcome)) %>% 
  mutate(upper_ci = (weighted_outcome + 1.96*i_se) + (decade*1.96*s_se) + (decade^2*1.96*q_se),
         lower_ci = (weighted_outcome - 1.96*i_se) - (decade*1.96*s_se) - (decade^2*1.96*q_se),
         lower_ci = ifelse(lower_ci < 0, 0, lower_ci))

plot_curves_w <- predicted_overall_w %>% 
  mutate(mental_health = factor(mental_health, levels = c("Depression", "Anxiety", "Loneliness", "Alcohol use", "Conduct problems", "Self-esteem"))) %>% 
  arrange(mental_health) %>% 
  select(age, mental_health, ses_indicator, ses_value, weighted_outcome, lower_ci, upper_ci) %>% 
  ggplot(aes(x = age, y = weighted_outcome, color = ses_value, group = ses_value)) +
  geom_line(linewidth = 1) +
  #  geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, fill = ses_value), 
  #              alpha=0.1, color = NA) +
  facet_rep_grid(mental_health ~ ses_indicator, 
                 labeller = labeller(ses_indicator=label_value, mental_health=label_value),
                 repeat.tick.labels = TRUE,
                 scales = "free") +
  scale_x_continuous(breaks=c(17,22,28,43)) +
  ggh4x::facetted_pos_scales(y = list(
    mental_health == "Depression" ~ scale_y_continuous(breaks = c(20, 25, 30), limits = c(20, 30)),
    mental_health == "Anxiety" ~ scale_y_continuous(breaks = c(10, 15, 20), limits = c(10, 20)),
    mental_health == "Loneliness" ~ scale_y_continuous(breaks = c(20, 25, 30), limits = c(20, 30)),
    mental_health == "Alcohol use" ~ scale_y_continuous(breaks = c(30, 40, 50, 60), limits = c(30, 60)),
    mental_health == "Conduct problems" ~ scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 14)),
    mental_health == "Self-esteem" ~ scale_y_continuous(breaks = c(60, 65, 70), limits = c(60, 70)))) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  theme_classic() + 
  theme(strip.text = element_text(face="bold"),
        text = element_text(size=20,family = "Times")) +
  guides(fill="none") +
  labs(x = "Age (in years)",
       y = "POMP score",
       color = "Group")

ggsave(filename = "plot_curves_w.jpg",
       path = "output/Graphs", 
       width = 32, 
       height = 16,  
       bg="white",
       dpi=700)




mean_comparisons <- UiN_model_data %>% 
  merge(., UiN_data_IPAW_id[, c("id", "weights")], by = "id") %>% 
  filter(ses_indicator == "ISEI") %>% 
  group_by(mental_health) %>% 
  summarise(unweighted = across(T2:T5, ~ mean(.x, na.rm = T)),
            weighted = across(T2:T5, ~ weighted.mean(.x, weights, na.rm = T)),
            diff = weighted - unweighted)



