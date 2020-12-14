
# Load ----
library(here)
source(here('Load.R'))
source(here('RunTests.R'))

library(purrr)
library(tidyr)
library(DoseFinding)
library(brms)

# Functions and objects ----
expand_binary_outcomes <- function(n, n_events) {
  tibble(Event = rep(c(0, 1), times = c(n - n_events, n_events)))
}
# expand_binary_outcomes(2, 1)
expand_binary_outcomes_df <- function(data) {
  data %>% 
    mutate(
      X = map2(n, Events, ~ expand_binary_outcomes(n = .x, n_events = .y))
    ) %>% 
    select(Dose, DoseLevel, DoseLevelN, X) %>% 
    unnest(X)
}
# dlt_analyses %>% 
#   head(1) %>% 
#   pull(data) %>% .[[1]] -> df; df
# expand_binary_outcomes_df(df)

fit_sig_df_model <- function(df) {
  if(sum(df$Event) > 0)
  {
    fitMod(DoseLevelN, Event, data = df,  model = "sigEmax",
           bnds = defBnds(mD = max(df$DoseLevelN))$sigEmax)
  } else {
    NULL
  }
}

# Parametisation as published
.fit_type1_brms_model <- function(df, prior) {
  
  if(sum(df$Event) > 0)
  {
    model <- bf(
      Event ~ E0 + Emax * DoseLevelN^N / (ED50^N + DoseLevelN^N),
      E0 + N + Emax + ED50 ~ 1,
      nl = TRUE
    )
    
    brm(model, prior = prior, data = df, cores = 4, 
        control = list(adapt_delta = 0.99), seed = 123)
  } else {
    NULL
  }
}

fit_type1_brms_model_p1 <- function(df) {
  prior_mean_ed50 <- max(df$DoseLevelN) / 2
  prior_sd_ed50 <- (max(df$DoseLevelN) - min(df$DoseLevelN)) / 2
  ed50_prior_str <- paste0('normal(', prior_mean_ed50, ', ', prior_sd_ed50, ')')
  prior <- set_prior('beta(1, 1)', nlpar = "E0", lb = 0, ub = 1) + 
    set_prior('beta(1, 1)', nlpar = "Emax", lb = 0, ub = 1) + 
    set_prior(ed50_prior_str, nlpar = "ED50", lb = 0) +
    set_prior('normal(1, 3)', nlpar = "N", lb = 0)
  .fit_type1_brms_model(df, prior)
}

fit_type1_brms_model_p2 <- function(df) {
  prior_mean_ed50 <- max(df$DoseLevelN) / 2
  prior_sd_ed50 <- (max(df$DoseLevelN) - min(df$DoseLevelN))
  ed50_prior_str <- paste0('normal(', prior_mean_ed50, ', ', prior_sd_ed50, ')')
  prior <- set_prior('beta(1, 1)', nlpar = "E0", lb = 0, ub = 1) + 
    set_prior('beta(1, 1)', nlpar = "Emax", lb = 0, ub = 1) + 
    set_prior(ed50_prior_str, nlpar = "ED50", lb = 0) +
    set_prior('normal(1, 5)', nlpar = "N", lb = 0)
  .fit_type1_brms_model(df, prior)
}

# Adjusted parametisation for easier prior specifiation
.fit_type2_brms_model <- function(df, prior) {
  
  if(sum(df$Event) > 0)
  {
    model <- bf(
      Event ~ (lambda + E0 * (ED50 / DoseLevelN)^N) / (1 + (ED50 / DoseLevelN)^N),
      E0 + N + lambda + ED50 ~ 1,
      nl = TRUE
    )
    
    brm(model, prior = prior, data = df, cores = 4, 
        control = list(adapt_delta = 0.99), seed = 123)
  } else {
    NULL
  }
}

fit_type2_brms_model_p1 <- function(df) {
  prior_mean_ed50 <- max(df$DoseLevelN) / 2
  prior_sd_ed50 <- (max(df$DoseLevelN) - min(df$DoseLevelN)) / 2
  ed50_prior_str <- paste0('normal(', prior_mean_ed50, ', ', prior_sd_ed50, ')')
  prior <- set_prior('beta(1, 1)', nlpar = "E0", lb = 0, ub = 1) + 
    set_prior('beta(1, 1)', nlpar = "lambda", lb = 0, ub = 1) + 
    set_prior(ed50_prior_str, nlpar = "ED50", lb = 0) +
    set_prior('normal(1, 3)', nlpar = "N", lb = 0)
  .fit_type2_brms_model(df, prior)
}

fit_type2_brms_model_p2 <- function(df) {
  prior_mean_ed50 <- max(df$DoseLevelN) / 2
  prior_sd_ed50 <- (max(df$DoseLevelN) - min(df$DoseLevelN))
  ed50_prior_str <- paste0('normal(', prior_mean_ed50, ', ', prior_sd_ed50, ')')
  prior <- set_prior('beta(1, 1)', nlpar = "E0", lb = 0, ub = 1) + 
    set_prior('beta(1, 1)', nlpar = "lambda", lb = 0, ub = 1) + 
    set_prior(ed50_prior_str, nlpar = "ED50", lb = 0) +
    set_prior('normal(1, 5)', nlpar = "N", lb = 0)
  .fit_type2_brms_model(df, prior)
}

tidy.DRMod <- function(obj, ...) {
  tibble(
    term = names(obj$coefs),
    estimate = obj$coefs,
    std.error = sqrt(diag(vcov(obj))),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
  )
}

augment.DRMod <- function(obj, predType = 'full-model', ...) {
  
  # predict(obj, predType = predType, se.fit = TRUE)
  # yields:
  # Warning message:
  # In predict.DRMod(obj, predType = predType, se.fit = TRUE) :
  #   Cannot cannot calculate standard deviation for sigEmax model.
  modelframe <- attr(obj, "data")
  tibble(
    modelframe,
    .fitted = predict(obj, predType = predType, se.fit = FALSE),
    .se.fit = NA
  )
}

variable_lookup <- bind_rows(
  
  list(term = 'e0', Variable = 'E0'),
  list(term = 'E0_(Intercept)', Variable = 'E0'),
  list(term = 'b_E0_Intercept', Variable = 'E0'),
  
  list(term = 'ed50', Variable = 'ED50'),
  list(term = 'ED50_(Intercept)', Variable = 'ED50'),
  list(term = 'b_ED50_Intercept', Variable = 'ED50'),
  
  list(term = 'eMax', Variable = 'EMax'),
  list(term = 'Emax_(Intercept)', Variable = 'EMax'),
  list(term = 'b_Emax_Intercept', Variable = 'EMax'),
  
  list(term = 'h', Variable = 'N'),
  list(term = 'N_(Intercept)', Variable = 'N'),
  list(term = 'b_N_Intercept', Variable = 'N'),
  
  list(term = 'b_lambda_Intercept', Variable = 'E0 + EMax'),
  list(term = 'lambda_(Intercept)', Variable = 'E0 + EMax'),
  
  list(term = 'sigma', Variable = 'sigma'),
  list(term = 'sd__Observation', Variable = 'sigma'),
  
  list(term = 'lp__', Variable = 'LP'),
)

# Fit ----
dlt_analyses <- dlt %>% 
  filter(AnalysisSeriesId <= 3) %>% 
  group_by(Study, AnalysisSeriesId) %>% 
  nest() %>% 
  head(30) %>%
  mutate(
    num_dose = map_int(data, nrow),
    num_pat = map_dbl(data, .f = function(df) sum(df$n)),
    num_event = map_dbl(data, .f = function(df) sum(df$Events)),
    has_events = num_event > 0,
    expanded_data = map(data, expand_binary_outcomes_df),
    model_sig_df = map(expanded_data, fit_sig_df_model),
    model_type1_brms_p1 = map(expanded_data, fit_type1_brms_model_p1),
    model_type1_brms_p2 = map(expanded_data, fit_type1_brms_model_p2),
    model_type2_brms_p1 = map(expanded_data, fit_type2_brms_model_p1),
    model_type2_brms_p2 = map(expanded_data, fit_type2_brms_model_p2)
  ) %>% 
  ungroup()
# saveRDS(dlt_analyses, 'Scratch/lab/dlt_analyses.Rds')

# One row per analysis series, included aggregated dose-level data (called data)
# and expanded patient-level data (called expanded_data):
dlt_analyses

# How many series see events, i.e. are analysable
dlt_analyses %>% count(has_events) %>% mutate('%' = n / sum(n))


# Estimates ----

#  Parameters
dlt_sig_df_params <- dlt_analyses %>% 
  mutate(ABC = map(model_sig_df, broom::tidy)) %>% 
  # mutate(ABC = map(ABC, ~ .x %>% mutate(Model = 'DF-Sig'))) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  left_join(variable_lookup, by = 'term') %>% 
  mutate(Model = 'DF-Sig')
expect_equal(dlt_sig_df_params %>% filter(is.na(Variable)) %>% nrow, 0)

dlt_type1_brms_p1_params <- dlt_analyses %>% 
  mutate(ABC = map(model_type1_brms_p1, broom::tidy)) %>% 
  # mutate(ABC = map(ABC, ~ .x %>% mutate(Model = 'Bayes1P1'))) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  left_join(variable_lookup, by = 'term') %>% 
  mutate(Model = 'Bayes1P1')
expect_equal(dlt_type1_brms_p1_params %>% filter(is.na(Variable)) %>% nrow, 0)

dlt_type1_brms_p2_params <- dlt_analyses %>% 
  mutate(ABC = map(model_type1_brms_p2, broom::tidy)) %>% 
  # mutate(ABC = map(ABC, ~ .x %>% mutate(Model = 'Bayes1P2'))) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  left_join(variable_lookup, by = 'term') %>% 
  mutate(Model = 'Bayes1P2')
expect_equal(dlt_type1_brms_p2_params %>% filter(is.na(Variable)) %>% nrow, 0)

dlt_type2_brms_p1_params <- dlt_analyses %>% 
  mutate(ABC = map(model_type2_brms_p1, broom::tidy)) %>% 
  # mutate(ABC = map(ABC, ~ .x %>% mutate(Model = 'Bayes2P1'))) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  left_join(variable_lookup, by = 'term') %>% 
  mutate(Model = 'Bayes2P1')
expect_equal(dlt_type2_brms_p1_params %>% filter(is.na(Variable)) %>% nrow, 0)

dlt_type2_brms_p2_params <- dlt_analyses %>% 
  mutate(ABC = map(model_type2_brms_p2, broom::tidy)) %>% 
  # mutate(ABC = map(ABC, ~ .x %>% mutate(Model = 'Bayes2P2'))) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  left_join(variable_lookup, by = 'term') %>% 
  mutate(Model = 'Bayes2P2')
expect_equal(dlt_type2_brms_p2_params %>% filter(is.na(Variable)) %>% nrow, 0)

dlt_model_params <- bind_rows(
  dlt_sig_df_params, 
  dlt_type1_brms_p1_params,
  dlt_type1_brms_p2_params,
  dlt_type2_brms_p1_params,
  dlt_type2_brms_p2_params,
)

#  Fitted values
dlt_sig_df_fitted <- dlt_analyses %>% 
  mutate(ABC = map(model_sig_df, broom.mixed::augment)) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  group_by(Study, AnalysisSeriesId, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  mutate(Model = 'DF-Sig')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type1_brms_p1_fitted <- dlt_analyses %>% 
  mutate(ABC = map(model_type1_brms_p1, broom.mixed::augment)) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  group_by(Study, AnalysisSeriesId, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  mutate(Model = 'Bayes1P1')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type1_brms_p2_fitted <- dlt_analyses %>% 
  mutate(ABC = map(model_type1_brms_p2, broom.mixed::augment)) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  group_by(Study, AnalysisSeriesId, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  mutate(Model = 'Bayes1P2')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type2_brms_p1_fitted <- dlt_analyses %>% 
  mutate(ABC = map(model_type2_brms_p1, broom.mixed::augment)) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  group_by(Study, AnalysisSeriesId, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  mutate(Model = 'Bayes2P1')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type2_brms_p2_fitted <- dlt_analyses %>% 
  mutate(ABC = map(model_type2_brms_p2, broom.mixed::augment)) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  group_by(Study, AnalysisSeriesId, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  mutate(Model = 'Bayes2P2')

dlt_model_fitted <- bind_rows(
  dlt_sig_df_fitted, 
  dlt_type1_brms_p1_fitted,
  dlt_type1_brms_p2_fitted,
  dlt_type2_brms_p1_fitted,
  dlt_type2_brms_p2_fitted,
)


# Samples ----

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type1_brms_p1_samples <- dlt_analyses %>% 
  filter(has_events) %>% 
  mutate(ABC = map(
    model_type1_brms_p1, 
    ~ tidybayes::add_fitted_draws(
      model = .x, 
      newdata = tibble(DoseLevelN = .x$data$DoseLevelN %>% unique() %>% sort)
    )
  )) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  mutate(Model = 'Bayes1P1')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type1_brms_p2_samples <- dlt_analyses %>% 
  filter(has_events) %>% 
  mutate(ABC = map(
    model_type1_brms_p2, 
    ~ tidybayes::add_fitted_draws(
      model = .x, 
      newdata = tibble(DoseLevelN = .x$data$DoseLevelN %>% unique() %>% sort)
    )
  )) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  mutate(Model = 'Bayes1P2')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type2_brms_p1_samples <- dlt_analyses %>% 
  filter(has_events) %>% 
  mutate(ABC = map(
    model_type2_brms_p1, 
    ~ tidybayes::add_fitted_draws(
      model = .x, 
      newdata = tibble(DoseLevelN = .x$data$DoseLevelN %>% unique() %>% sort)
    )
  )) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  mutate(Model = 'Bayes2P1')

set.seed(123) # Seed needed because this involves sampling for Bayes models
dlt_type2_brms_p2_samples <- dlt_analyses %>% 
  filter(has_events) %>% 
  mutate(ABC = map(
    model_type2_brms_p2, 
    ~ tidybayes::add_fitted_draws(
      model = .x, 
      newdata = tibble(DoseLevelN = .x$data$DoseLevelN %>% unique() %>% sort)
    )
  )) %>% 
  select(Study, AnalysisSeriesId, ABC) %>% 
  unnest(c(ABC)) %>% 
  mutate(Model = 'Bayes2P2')

dlt_model_samples <- bind_rows(
  dlt_type1_brms_p1_samples,
  dlt_type1_brms_p2_samples,
  dlt_type2_brms_p1_samples,
  dlt_type2_brms_p2_samples,
)

dlt_curve_heights <- bind_rows(
  # Samples on bottom dose for each study:
  dlt_model_samples %>% 
    semi_join(
      dlt_model_samples %>% 
        group_by(Model, AnalysisSeriesId) %>% 
        summarise(Dose1 = min(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'Dose1')
    ) %>% mutate(DoseLabel = 'Bottom'),
  # Samples on top dose for each study:
  dlt_model_samples %>% 
    semi_join(
      dlt_model_samples %>% 
        group_by(Model, AnalysisSeriesId) %>% 
        summarise(DoseN = max(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'DoseN')
    ) %>% mutate(DoseLabel = 'Top')
) %>% 
  # ungroup() %>% 
  select(Model, AnalysisSeriesId, .draw, DoseLabel, .value) %>% 
  spread(DoseLabel, .value) %>% 
  mutate(CurveHeight = Top - Bottom)

dlt_curve_height_summaries <- dlt_curve_heights %>% 
  group_by(AnalysisSeriesId, Model) %>% 
  summarise(
    mu = mean(CurveHeight),
    sigma = sd(CurveHeight),
    n = n(),
    se = sigma / sqrt(n),
    prob_pos = mean(CurveHeight > 0)
  )



# Plots ----
library(ggplot2)

# Params
dlt_model_params %>% 
  filter(Variable == 'ED50' | Variable == 'N') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(-5, 10)
# Generally ED50 and N increase when they can.

dlt_model_params %>% 
  filter(Variable == 'E0' | Variable == 'EMax') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(0, 1)
# Under the looser priors, E0 decreases and lambda increases
# SEs are huge. 
# Some are sensible for size.
# Some are so huge as to represent model-fitting failure.

# Fitted
dlt_type1_brms_p1_fitted %>% 
  filter(AnalysisSeriesId == 21) %>%
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_line(col = 'blue') + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT')

dlt_model_fitted %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  # geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_line(aes(group = Model, col = Model)) + 
  facet_grid(AnalysisSeriesId ~ Model) + 
  ylim(0, 1) + 
  labs(x = 'Dose', y = 'ProbDLT')

# Samples

# Wiggly lines by series
dlt_model_samples %>% 
  # filter(AnalysisSeriesId == 3) %>% 
  filter(.draw <= 1000) %>%
  ggplot(aes(x = DoseLevelN, y = .value)) + 
  geom_line(aes(group = .draw), alpha = 0.01) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  facet_grid(AnalysisSeriesId ~ Model)

# Heights of dose-DLT curves
dlt_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = mu)) + 
  geom_pointrange(aes(ymin = mu - 1.96 * sigma, ymax = mu + 1.96 * sigma), 
                  size = 0.2) +
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  labs(y = 'Height of dose-DLT curve') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# Chances of positive curve height
dlt_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = prob_pos)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  ylim(0, 1) +
  labs(y = 'Certainty that dose-DLT curve has positive gradient') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of curve heights
dlt_curve_heights %>% 
  ggplot(aes(x = CurveHeight)) + 
  geom_density() + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_grid(AnalysisSeriesId ~ Model)
# Or
library(ggridges)  
dlt_curve_heights %>% 
  ggplot(aes(x = CurveHeight, y = Model)) + 
  geom_density_ridges() +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_wrap( ~ AnalysisSeriesId, ncol = 3)





# Investigations ----
# Outcomes for a study:  
dlt_analyses %>% 
  head(1) %>% 
  select(-expanded_data) %>% 
  unnest(cols = c(data)) %>% 
  print(n = 100)

# Patient-level outcomes for a study:  
dlt_analyses %>% 
  head(1) %>% 
  select(-data) %>% 
  unnest(cols = c(expanded_data)) %>% 
  print(n = 100)

i <- 1

dlt_analyses %>% 
  filter(has_events) %>% 
  slice(i)

dlt_analyses %>% 
  filter(has_events) %>% 
  slice(i) %>% 
  pull(expanded_data) %>% .[[1]] -> expanded_data

dlt_analyses %>% 
  filter(has_events) %>% 
  slice(i) %>% 
  pull(model_sig_df) %>% .[[1]] -> df_mod
summary(df_mod)
df_mod %>% plot()

dlt_analyses %>% 
  filter(has_events) %>% 
  slice(i) %>% 
  pull(model_type1_brms_p1) %>% .[[1]] -> brms_mod
brms_mod
brms_mod %>% 
  conditional_effects(.) %>% 
  plot(points = TRUE)

# Look at one model
dlt_model_params %>% 
  filter(AnalysisSeriesId == 21) %>% 
  filter(Model == 'DF-Sig')

dlt_model_fitted %>% 
  filter(AnalysisSeriesId == 21) %>% 
  filter(Model == 'DF-Sig')
dlt_model_fitted %>% 
  filter(AnalysisSeriesId == 21) %>% 
  filter(Model == 'Bayes1P1')


# Manual check
an_s <- 3
dlt_analyses %>% 
  filter(AnalysisSeriesId == an_s) %>% 
  pull(data) %>% .[[1]] -> df; df
dlt_analyses %>% 
  filter(AnalysisSeriesId == an_s) %>% 
  pull(expanded_data) %>% .[[1]] -> expanded_df; expanded_df

testthat::expect_equal(
  df %>% select(Dose, DoseLevelN, ProbEvent),
  expanded_df %>% 
    group_by(Dose, DoseLevelN) %>% 
    summarise(ProbEvent = mean(Event))
)

fitMod(DoseLevelN, Event, data = expanded_df, model = "emax") %>% plot
fitMod(DoseLevelN, Event, data = expanded_df, model = "sigEmax") %>% plot

