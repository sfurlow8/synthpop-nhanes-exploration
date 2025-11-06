## ---------------------------------------------
## 
## Script: analysis.R
## 
## Author: Sophie Furlow
## Date created: October 22, 2025
##
## Description: Demonstration of synthpop package on national health data.
## Contents: Setup & Data Handling, Train / Hold Split, Synthesis Recipes,
##           Synthesis, Utility Evaluation, Privacy Evaluation, Train on 
##           Synthetic Test on Real (TSTR), Model Coefficients Comparison
##
## ---------------------------------------------


#----------------------------------------------------------------
# Setup & Data Handling
#----------------------------------------------------------------

set.seed(102025)

# load packages
library(synthpop)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(tidyverse)
library(ggplot2)
library(pROC)
library(randomForest)
library(nnet)

# source scripts
source('lookups.R') # lookup lists to recode data
source('nn_synth.R') # custom neural network function and helpers for synthesis

# read data
data <- read.csv('nhanes_mort.csv')

# data recoding and filtering
data <- data %>%
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% # filter out self-reported pregnancy
  rename(age = RIDAGEYR) %>%
  rename(bmi = BMXBMI) %>%
  mutate(ethnicity = recode(RIDRETH2, !!!eth_lookup)) %>%
  mutate(education = recode(DMDEDUC2, !!!educ_lookup)) %>%
  mutate(marital = recode(DMDMARTL, !!!mar_lookup)) %>%
  mutate(income = recode(INDHHINC, !!!inc_lookup)) %>%
  mutate(sex = case_when(RIAGENDR == 1 ~ 'Male', RIAGENDR == 2 ~ 'Female')) %>%
  mutate(smoking = case_when(SMQ040 == 3 ~ 'Non-smoker', TRUE ~ 'Smoker')) %>%
  mutate(
    diabetes = case_when(
      DIQ010 == 1 ~ 'Diabetic',
      DIQ010 == 2 ~ 'Non-diabetic',
      DIQ010 == 3 ~ 'Borderline'
    )
  ) %>%
  mutate(
    hypertension = case_when(
      BPXDI1 >= 90 ~ 'Hypertension',
      BPXSY1 >= 140 & age < 60 ~ 'Hypertension',
      BPXSY1 >= 150 & age >= 60 ~ 'Hypertension',
      TRUE ~ 'No hypertension'
    )
  ) %>%
  rename(ntprobnp = SSBNP) %>%
  filter(eligstat == 1) %>% # must be eligible for mortality follow up screening
  mutate(
    mortality = case_when(mortstat == 0 ~ 'Alive', mortstat == 1 ~ 'Dead')
  ) %>%
  mutate(cod = recode(ucod_leading, !!!cod_lookup)) %>%
  rowwise() %>%
  mutate(
    gfr = case_when(
      sex == 'Male' ~ (141 *
        (min((LBXSCR / 0.9), 1)^(-0.411)) *
        (max((LBXSCR / 0.9), 1)^(-1.209)) *
        (0.993^age)),
      TRUE ~ ((141 *
        (min((LBXSCR / 0.7), 1)^(-0.329)) *
        (max((LBXSCR / 0.7), 1)^(-1.209)) *
        (0.993^age)) *
        1.018)
    )
  ) %>%
  mutate(gfr2 = case_when(RIDRETH2 == 2 ~ gfr * 1.159, TRUE ~ gfr)) %>%
  mutate(egfr = case_when(gfr2 >= 60 ~ 'Normal', gfr2 < 60 ~ '<60')) %>%
  ungroup()

data_inc <- data %>%
  select(
    'SEQN',
    'age',
    'bmi',
    'ethnicity',
    'education',
    'marital',
    'income',
    'sex',
    'smoking',
    'diabetes',
    'hypertension',
    'ntprobnp',
    'egfr',
    'mortality',
    'cod'
  ) %>%
  mutate(across(where(is.character), factor)) # factor all character vars


#----------------------------------------------------------------
# Train / Hold Split
#----------------------------------------------------------------
# real holdout set remains completely untouched, used only for performance evaluation

data_to_synth <- data_inc %>% select(-c('SEQN'))
n <- nrow(data_to_synth)
id_hold <- sample.int(n, size = floor(0.30 * n))
real_holdout <- data_to_synth[id_hold, , drop = FALSE]
real_train <- data_to_synth[-id_hold, , drop = FALSE]

#----------------------------------------------------------------
# Synthesis Recipes
#----------------------------------------------------------------

# visit sequence: synthesize predictors first, outcome last (helps relationships)
visit_seq <- c(
  'age',
  'sex',
  'ethnicity',
  'bmi',
  'education',
  'marital',
  'income',
  'smoking',
  'diabetes',
  'hypertension',
  'ntprobnp',
  'egfr',
  'mortality',
  'cod'
)

# cart method
method_cart <- 'cart'
smoothing_cart <- c(
  age = 'density',
  ntprobnp = 'density',
  bmi = 'density'
)

# parametric method (lets synthpop choose defaults per type: normrank/logreg/polyreg/polr)
method_parametric <- 'parametric' # syn() will map to defaults internally

# random forest method
method_rf <- 'rf'

# neural network method
method_nn <- rep('cart', ncol(real_train))
names(method_nn) <- names(real_train)
method_nn[c('bmi', 'smoking', 'diabetes', 'mortality')] <- 'nn'

# predictor matrix: use all other vars as predictors
pred_mat <- matrix(
  1,
  ncol = ncol(real_train),
  nrow = ncol(real_train),
  dimnames = list(names(real_train), names(real_train))
)
diag(pred_mat) <- 0 # variable does not predict itself


#----------------------------------------------------------------
# Synthesis
#----------------------------------------------------------------

# wrapper for syn function
# takes method name and label as input (with option smoothing)
# outputs syn object from synthpop::syn
synthesize <- function(method, label, smoothing = NULL) {
  syn(
    data = real_train,
    method = method,
    visit.sequence = visit_seq,
    predictor.matrix = pred_mat,
    m = 3, # 3 synthetic copies
    proper = TRUE, # bootstrap fits for added variation
    smoothing = smoothing, # optional smoothing for numeric vars
    print.flag = FALSE,
    seed = 100 + which(c('cart', 'parametric', 'rf', 'nn') == label),
    polyreg.maxit = 5000
  )
}

# synthesize m=3 datasets for each of 4 methods
syn_cart <- synthesize(method_cart, 'cart', smoothing_cart)
syn_param <- synthesize(method_parametric, 'parametric', NULL)
syn_rf <- synthesize(method_rf, 'rf', NULL)
syn_nn <- synthesize(method_nn, 'nn', NULL)


#----------------------------------------------------------------
# Utility Evaluation
#----------------------------------------------------------------

# univariate comparisons (quick visual + tabular)
cmp_cart <- compare(
  syn_cart,
  data = real_train,
  vars = names(real_train),
  plot = TRUE,
  table = TRUE
)
cmp_param <- compare(
  syn_param,
  data = real_train,
  vars = names(real_train),
  plot = TRUE,
  table = TRUE
)
cmp_rf <- compare(
  syn_rf,
  data = real_train,
  vars = names(real_train),
  plot = TRUE,
  table = TRUE
)
cmp_nn <- compare(
  syn_nn,
  data = real_train,
  vars = names(real_train),
  plot = TRUE,
  table = TRUE
)

# global propensity-based utility (pMSE, S_pMSE)
util_cart <- utility.gen(
  syn_cart,
  data = as.data.frame(real_train),
  print.flag = FALSE
)
util_param <- utility.gen(
  syn_param,
  data = as.data.frame(real_train),
  print.flag = FALSE
)
util_rf <- utility.gen(
  syn_rf,
  data = as.data.frame(real_train),
  print.flag = FALSE
)
util_nn <- utility.gen(
  syn_nn,
  data = as.data.frame(real_train),
  print.flag = FALSE
)

# utility summaries
utility_long <- bind_rows(
  tibble(
    method = 'CART',
    syn_id = seq_along(util_cart$pMSE),
    pMSE = as.numeric(util_cart$pMSE),
    S_pMSE = as.numeric(util_cart$S_pMSE)
  ),
  tibble(
    method = 'Parametric',
    syn_id = seq_along(util_param$pMSE),
    pMSE = as.numeric(util_param$pMSE),
    S_pMSE = as.numeric(util_param$S_pMSE)
  ),
  tibble(
    method = 'RandomForest',
    syn_id = seq_along(util_rf$pMSE),
    pMSE = as.numeric(util_rf$pMSE),
    S_pMSE = as.numeric(util_rf$S_pMSE)
  ),
  tibble(
    method = 'NeuralNet',
    syn_id = seq_along(util_nn$pMSE),
    pMSE = as.numeric(util_nn$pMSE),
    S_pMSE = as.numeric(util_nn$S_pMSE)
  )
)

print(utility_long)

# averages by method
utility_avg <- utility_long %>%
  group_by(method) %>%
  summarise(
    pMSE_mean = mean(pMSE, na.rm = TRUE),
    S_pMSE_mean = mean(S_pMSE, na.rm = TRUE),
    .groups = 'drop'
  )

print(utility_avg)

# lower pMSE / S_pMSE typically indicates better similarity between synthetic and real


#----------------------------------------------------------------
# Privacy Evaluation
#----------------------------------------------------------------

# keys are quasi-identifiers that could be known externally
keys <- c("age", "sex", "education", "ethnicity", "marital")

# targets are potentially sensitive variables we do not want to be disclosed
targets <- c("mortality", "income", "hypertension", "diabetes") # attribute disclosure on mortality

# disclosure measures for each method
disc_cart <- multi.disclosure(
  syn_cart,
  data = real_train,
  keys = keys,
  targets = targets,
  print.flag = FALSE
)
disc_param <- multi.disclosure(
  syn_param,
  data = real_train,
  keys = keys,
  targets = targets,
  print.flag = FALSE
)
disc_rf <- multi.disclosure(
  syn_rf,
  data = real_train,
  keys = keys,
  targets = targets,
  print.flag = FALSE
)
disc_nn <- multi.disclosure(
  syn_nn,
  data = real_train,
  keys = keys,
  targets = targets,
  print.flag = FALSE
)

# lower is better for both RepU (identity) and DiSCO (attribute) risks


#----------------------------------------------------------------
# Train on Synthetic Test on Real (TSTR)
#----------------------------------------------------------------

# evaluate_model function
evaluate_models <- function(
  syn_obj,
  method_label,
  positive_class = NULL,
  print_plot = TRUE
) {
  # helper: get positive/negative order for pROC when response is factor
  .levels_pos_last <- function(y, positive_class = NULL) {
    y <- droplevels(y)
    if (!is.factor(y)) {
      return(NULL)
    }
    if (is.null(positive_class)) {
      positive_class <- tail(levels(y), 1)
    }
    c(setdiff(levels(y), positive_class), positive_class)
  }

  # helper: align selected factor vars to reference levels (data_inc)
  .align_levels_to_inc <- function(df) {
    df %>%
      mutate(
        sex = factor(sex, levels = levels(data_inc$sex)),
        smoking = factor(smoking, levels = levels(data_inc$smoking)),
        hypertension = factor(
          hypertension,
          levels = levels(data_inc$hypertension)
        ),
        mortality = factor(mortality, levels = levels(data_inc$mortality))
      )
  }

  # safe ROC (drops cases with NA preds/labels)
  .make_roc <- function(y, p, positive_class = NULL) {
    ok <- !(is.na(y) | is.na(p))
    y <- droplevels(y[ok])
    p <- p[ok]
    if (is.factor(y)) {
      lv <- .levels_pos_last(y, positive_class)
      roc(
        response = y,
        predictor = p,
        levels = lv,
        direction = "<",
        quiet = TRUE
      )
    } else {
      roc(response = y, predictor = p, direction = "<", quiet = TRUE)
    }
  }

  syn_list <- syn_obj$syn
  if (!is.list(syn_list)) {
    syn_list <- list(syn_list)
  }

  f <- mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr
  mod_vars <- c(
    "age",
    "sex",
    "ntprobnp",
    "bmi",
    "hypertension",
    "smoking",
    "egfr"
  )

  # align holdout to real levels, restrict to common vars
  common_vars <- intersect(names(syn_list[[1]]), names(real_holdout))
  hold <- real_holdout[, common_vars, drop = FALSE] %>% .align_levels_to_inc()

  roc_list <- list()

  # loop over synthetic copies: train GLM & RF and make ROC
  metrics_and_roc <- map_dfr(seq_along(syn_list), function(i) {
    print('Running map function')
    # synthetic copy i
    sdat <- syn_list[[i]] %>%
      .align_levels_to_inc()


    common_vars_i <- intersect(names(sdat), names(hold))
    sdat <- sdat[, common_vars_i, drop = FALSE]
    hold_i <- hold[, common_vars_i, drop = FALSE]

    # ---------- GLM ----------
    glm_fit <- glm(f, data = sdat, family = binomial())
    glm_prob <- predict(glm_fit, newdata = hold_i, type = "response")
    glm_auc <- as.numeric(pROC::auc(hold_i$mortality, glm_prob))

    roc_glm <- .make_roc(hold_i$mortality, glm_prob, positive_class)
    roc_list[[sprintf("%s (syn %d)", method_label, i)]] <- roc_glm

    # # ---------- RF with rfImpute ----------
    # # rfImpute is costly time-wise: uncomment if you want to run
    # dat_rf <- sdat %>%
    #   select(all_of(mod_vars)) %>%
    #   mutate(mortality = sdat$mortality) %>%
    #   mutate(mortality = factor(mortality)) %>%       # ensure classification
    #   select(mortality, all_of(mod_vars))

    # imputed <- rfImpute(
    #   mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
    #   data = dat_rf,
    #   iter = 5, ntree = 300
    # )

    # rf_fit <- randomForest(
    #   mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
    #   data = imputed, ntree = 300
    # )

    # holdout_imputed <- na.roughfix(hold_i)  # simple rough-fix for holdout
    # rf_prob <- predict(rf_fit,
    #                    newdata = holdout_imputed %>% select(-mortality),
    #                    type = "prob")[, 2]
    # rf_auc  <- as.numeric(pROC::auc(holdout_imputed$mortality, rf_prob))

    # roc_rf <- .make_roc(holdout_imputed$mortality, rf_prob, positive_class)
    # roc_list[[sprintf("%s-RF (syn %d)", method_label, i)]] <- roc_rf

    return(list(
      metrics = tibble(method = method_label, syn_copy = i, glm_auc = glm_auc),
      roc_list = roc_list
    ))
  })

  metrics <- metrics_and_roc$metrics
  roc_list <- metrics_and_roc$roc_list

  # models trained on real data (real_train)

  # GLM (real)
  glm_real <- glm(
    f,
    data = .align_levels_to_inc(real_train),
    family = binomial()
  )
  glm_real_prob <- predict(glm_real, newdata = hold, type = "response")
  roc_list[["Real-GLM"]] <- .make_roc(
    hold$mortality,
    glm_real_prob,
    positive_class
  )

  # # RF (real) with the same rfImpute pipeline for consistency
  # # uncomment if you want to run with random forest
  # dat_real_rf <- .align_levels_to_inc(real_train) %>%
  #   select(all_of(mod_vars)) %>%
  #   mutate(mortality = real_train$mortality) %>%
  #   mutate(mortality = factor(mortality)) %>%
  #   select(mortality, all_of(mod_vars))

  # imputed_real <- rfImpute(
  #   mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
  #   data = dat_real_rf,
  #   iter = 5, ntree = 300
  # )

  # rf_real <- randomForest(
  #   mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
  #   data = imputed_real, ntree = 300
  # )

  # hold_real_imputed <- na.roughfix(hold)
  # rf_real_prob <- predict(rf_real,
  #                         newdata = hold_real_imputed %>% select(-mortality),
  #                         type = "prob")[, 2]
  # roc_list[["Real-RF"]] <- .make_roc(hold_real_imputed$mortality, rf_real_prob, positive_class)


  # ggroc plot for each method
  auc_vals <- sapply(roc_list, pROC::auc)
  legend_labels <- paste0(
    names(auc_vals),
    " (AUC = ",
    sprintf("%.3f", as.numeric(auc_vals)),
    ")"
  )

  # palette from roc_list
  pal <- scales::hue_pal()(length(roc_list))
  names(pal) <- names(roc_list)

  # create plot
  roc_plot <- ggroc(roc_list, size = 1.1) +
    aes(color = name) +
    scale_color_manual(values = pal, labels = legend_labels) +
    coord_equal() +
    labs(
      title = sprintf(
        'ROC — %s vs. Real (evaluation on real holdout)',
        method_label
      ),
      x = '1 - Specificity',
      y = 'Sensitivity',
      color = 'Training source / model'
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'right')

  if (print_plot) {
    print(roc_plot)
  }

  return(list(
    metrics = metrics,
    roc_plot = roc_plot,
    roc_list = roc_list,
    auc = auc_vals
  ))
}


# one plot per synth object (method)
out_cart <- evaluate_models(syn_cart, method_label = 'CART', positive_class = 'Dead')
out_param <- evaluate_models(syn_param, method_label = 'Parametric', positive_class = 'Dead')
out_rf <- evaluate_models(syn_rf, method_label = 'RF', positive_class = 'Dead')
out_nn <- evaluate_models(syn_nn, method_label = 'NN', positive_class = 'Dead')

# construct summary ROC

# only looking at syn 1 from each method
roc_list_full <- c(out_cart$roc_list, 
                    out_param$roc_list,
                    out_rf$roc_list,
                    out_nn$roc_list)
roc_list_full <- roc_list_full[!duplicated(names(roc_list_full))]
real_roc <- roc_list_full[
  names(roc_list_full) == grep('Real', names(roc_list_full), value = TRUE)
]
names_to_keep <- grep('syn 1', names(roc_list_full), value = TRUE)
roc_list_full <- c(
  roc_list_full[names(roc_list_full) %in% names_to_keep],
  real_roc
)

auc_vals <- sapply(roc_list_full, pROC::auc)
legend_labels <- paste0(
  names(auc_vals),
  " (AUC = ",
  sprintf("%.3f", as.numeric(auc_vals)),
  ")"
)

# palette from roc_list
pal <- scales::hue_pal()(length(roc_list_full))
names(pal) <- names(roc_list_full)

roc_plot <- ggroc(roc_list_full, linetype = 'dashed', size = 1) +
  aes(color = name) +
  scale_color_manual(values = pal, labels = legend_labels) +
  coord_equal() +
  labs(
    title = sprintf('ROC Evaluation on Real Holdout'),
    x = '1 - Specificity',
    y = 'Sensitivity',
    color = 'Training source'
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'right')

print(roc_plot)


#----------------------------------------------------------------
# Model Coefficients Comparison
#----------------------------------------------------------------

# compare z-values of glm models between original and synthetic cart
fit_syn_glm <- glm.synds(
  mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
  data = syn_cart,
  family = binomial()
)
fit_real_glm <- glm(
  mortality ~ age + sex + ntprobnp + bmi + hypertension + smoking + egfr,
  data = real_train,
  family = binomial()
)

cmp_fit <- compare.fit.synds(fit_syn_glm, real_train)
print(cmp_fit)

