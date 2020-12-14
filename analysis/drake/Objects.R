
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

fit_sig_df_model <- function(df) {
  if(sum(df$Event) > 0)
  {
    fitMod(DoseLevelN, Event, data = df,  model = "sigEmax",
           bnds = defBnds(mD = max(df$DoseLevelN))$sigEmax)
  } else {
    NULL
  }
}

# Standard parametisation
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
    set_prior('uniform(-1, 1)', nlpar = "Emax", lb = -1, ub = 1) + 
    set_prior(ed50_prior_str, nlpar = "ED50", lb = 0) +
    set_prior('normal(1, 3)', nlpar = "N", lb = 0)
  .fit_type1_brms_model(df, prior)
}

fit_type1_brms_model_p2 <- function(df) {
  prior_mean_ed50 <- max(df$DoseLevelN) / 2
  prior_sd_ed50 <- (max(df$DoseLevelN) - min(df$DoseLevelN))
  ed50_prior_str <- paste0('normal(', prior_mean_ed50, ', ', prior_sd_ed50, ')')
  prior <- set_prior('beta(1, 1)', nlpar = "E0", lb = 0, ub = 1) + 
    set_prior('uniform(-1, 1)', nlpar = "Emax", lb = -1, ub = 1) + 
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

get_expanded_dlt_data <- function(series_id) {
  
  data <- dlt %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    select(Dose, DoseLevel, DoseLevelN, Events, n)
  
  expand_binary_outcomes_df(data)
}

get_expanded_obj_resp_data <- function(series_id) {
  
  data <- obj_resp %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    select(Dose, DoseLevel, DoseLevelN, Events, n)
  
  expand_binary_outcomes_df(data)
}

get_fitted_draws <- function(bayes_model, seed = 123) {
  if(is.null(bayes_model)) {
    tibble()
  } else {
    set.seed(seed); # Seed needed because this involves sampling for Bayes models
    tidybayes::add_fitted_draws(
      newdata = bayes_model$data %>% 
        select(DoseLevelN) %>% 
        distinct() %>% 
        arrange(DoseLevelN), 
      model = bayes_model
    )
  }
}
