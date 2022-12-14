# DiD Course Pedro H. C. Sant'Anna Problem Set 2
# Student: PEDRO ROVERI SCATIMBURGO

# ---- Preamble ----

rm(list=ls())

library(tidyverse)


# ---- Question 1 ----

data <- haven::read_dta("data/ehec_data.dta")

# ----- Question 2 ----

data <- data %>%
  filter(year %in% c("2013","2014"))

# ----- Question 3 -----

data <- data %>%
  mutate(
    year = as.numeric(as.character(year)),
    stfips = as.numeric(as.character(stfips))
  )

# ----- Question 4 ----

data <- data %>%
  mutate(
    treatment = replace_na(ifelse(yexp2 == 2014, 1, 0), 0),
    post_treatment = ifelse(year == 2014, 1, 0),
    product = treatment*post_treatment
  )

# ----- Question 5 -----

# Without using W as weights:

first_diff <- mean(data %>% filter(treatment == 1 & post_treatment == 1) %>% pull(dins)) - mean(data %>% filter(treatment == 1 & post_treatment == 0) %>% pull(dins))

second_diff <- mean(data %>% filter(treatment == 0 & post_treatment == 1) %>% pull(dins)) - mean(data %>% filter(treatment == 0 & post_treatment == 0) %>% pull(dins))

(did_estimator <- first_diff - second_diff)

# Using W as weights:

first_diff_W <- weighted.mean(data %>% filter(treatment == 1 & post_treatment == 1) %>% pull(dins), data %>% filter(treatment == 1 & post_treatment == 1) %>% pull(W)) - weighted.mean(data %>% filter(treatment == 1 & post_treatment == 0) %>% pull(dins), data %>% filter(treatment == 1 & post_treatment == 0) %>% pull(W))

second_diff_W <- weighted.mean(data %>% filter(treatment == 0 & post_treatment == 1) %>% pull(dins), data %>% filter(treatment == 0 & post_treatment == 1) %>% pull(W)) - weighted.mean(data %>% filter(treatment == 0 & post_treatment == 0) %>% pull(dins), data %>% filter(treatment == 0 & post_treatment == 0) %>% pull(W))

(did_estimator_W <- first_diff_W - second_diff_W)

# ----- Question 6 -----

# Without using W as weights:

regression <- data %>%
  estimatr::lm_robust(
    formula = dins ~ treatment + post_treatment + treatment*post_treatment
  )

(regression$coefficients[4])

# Using W as weights:

regression_W <- data %>%
  estimatr::lm_robust(
    formula = dins ~ treatment + post_treatment + treatment*post_treatment,
    weights = W
  )

(regression_W$coefficients[4])

# ---- Question 7 ----

# Without using W as weights:

regression_FE <- data %>%
  estimatr::lm_robust(
    formula = dins ~ product,
    fixed_effects = ~ stfips + year
  )

(regression_FE$coefficients[1])

# Using W as weights:

regression_FE_W <- data %>%
  estimatr::lm_robust(
    formula = dins ~ product,
    fixed_effects = ~ stfips + year,
    weights = W
  )

(regression_FE_W$coefficients[1])

# ----- Question 8 -----

# Specification from Question 6:

regression_clustered <- data %>%
  estimatr::lm_robust(
    formula = dins ~ treatment + post_treatment + treatment*post_treatment,
    cluster = stfips
  )

(regression_clustered$p.value[4])

# Specification from Question 7:

regression_FE_clustered <- data %>%
  estimatr::lm_robust(
    formula = dins ~ product,
    fixed_effects = ~ stfips + year,
    cluster = stfips
  )

(regression_FE_clustered$p.value[1])

# Both p-values are the same, as we expected. We reject the null hypothesis
# of no treatment effect at the 1% significance level.