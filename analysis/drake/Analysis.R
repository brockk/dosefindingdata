
# Load ----
library(here)
source(here('Load.R'))
source(here('RunTests.R'))

library(purrr)
library(tidyr)
library(DoseFinding)
library(brms)
library(drake)
library(ggplot2)

source(here('analysis', 'drake', 'Objects.R'))

if(FALSE) {
  # drake creates the .drake cache in the working directory.
  getwd()
  setwd('analysis/drake/')
}


# Drake plan ----
dlt_series = dlt %>% pull(AnalysisSeriesId) %>% unique()
or_series = obj_resp %>% pull(AnalysisSeriesId) %>% unique()
{plan <- drake_plan(
  
  #  Expanded outcomes (i.e. by patient) for each DLT series
  expanded_dlt_data = target(
    get_expanded_dlt_data(series),
    transform = cross(series = !!dlt_series)
  ),
  
  # And objective response series
  expanded_obj_resp_data = target(
    get_expanded_obj_resp_data(series),
    transform = cross(series = !!or_series)
  ),

  #  Models
  dlt_model_sig_df = target(
    fit_sig_df_model(expanded_dlt_data),
    transform = map(expanded_dlt_data, .id = series)
  ),
  
  obj_resp_model_sig_df = target(
    fit_sig_df_model(expanded_obj_resp_data),
    transform = map(expanded_obj_resp_data, .id = series)
  ),
  
  dlt_model_type1_brms_p1 = target(
    fit_type1_brms_model_p1(expanded_dlt_data),
    transform = map(expanded_dlt_data, .id = series)
  ),
  
  obj_resp_model_type1_brms_p1 = target(
    fit_type1_brms_model_p1(expanded_obj_resp_data),
    transform = map(expanded_obj_resp_data, .id = series)
  ),
  
  dlt_model_type1_brms_p2 = target(
    fit_type1_brms_model_p2(expanded_dlt_data),
    transform = map(expanded_dlt_data, .id = series)
  ), 
  
  obj_resp_model_type1_brms_p2 = target(
    fit_type1_brms_model_p2(expanded_obj_resp_data),
    transform = map(expanded_obj_resp_data, .id = series)
  ), 
  
  dlt_model_type2_brms_p1 = target(
    fit_type2_brms_model_p1(expanded_dlt_data),
    transform = map(expanded_dlt_data, .id = series)
  ),
  
  obj_resp_model_type2_brms_p1 = target(
    fit_type2_brms_model_p1(expanded_obj_resp_data),
    transform = map(expanded_obj_resp_data, .id = series)
  ),
  
  dlt_model_type2_brms_p2 = target(
    fit_type2_brms_model_p2(expanded_dlt_data),
    transform = map(expanded_dlt_data, .id = series)
  ),
  
  obj_resp_model_type2_brms_p2 = target(
    fit_type2_brms_model_p2(expanded_obj_resp_data),
    transform = map(expanded_obj_resp_data, .id = series)
  ),
  
  
  #  Parameter estimates
  temp1 = target(
    broom::tidy(dlt_model_sig_df) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'DF-Sig'),
    transform = map(dlt_model_sig_df, .id = series)
  ),
  
  dlt_sig_df_params = target(
    dplyr::bind_rows(temp1),
    transform = combine(temp1) 
  ),
  
  temp1b = target(
    broom::tidy(obj_resp_model_sig_df) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'DF-Sig'),
    transform = map(obj_resp_model_sig_df, .id = series)
  ),
  
  obj_resp_sig_df_params = target(
    dplyr::bind_rows(temp1b),
    transform = combine(temp1b) 
  ),
  
  temp2 = target(
    broom::tidy(dlt_model_type1_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes1P1'),
    transform = map(dlt_model_type1_brms_p1, .id = series)
  ),
  
  dlt_type1_brms_p1_params = target(
    dplyr::bind_rows(temp2),
    transform = combine(temp2) 
  ),
  
  temp2b = target(
    broom::tidy(obj_resp_model_type1_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes1P1'),
    transform = map(obj_resp_model_type1_brms_p1, .id = series)
  ),
  
  obj_resp_type1_brms_p1_params = target(
    dplyr::bind_rows(temp2b),
    transform = combine(temp2b) 
  ),
  
  temp3 = target(
    broom::tidy(dlt_model_type1_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes1P2'),
    transform = map(dlt_model_type1_brms_p2, .id = series)
  ),
  
  dlt_type1_brms_p2_params = target(
    dplyr::bind_rows(temp3),
    transform = combine(temp3) 
  ),
  
  temp3b = target(
    broom::tidy(obj_resp_model_type1_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes1P2'),
    transform = map(obj_resp_model_type1_brms_p2, .id = series)
  ),
  
  obj_resp_type1_brms_p2_params = target(
    dplyr::bind_rows(temp3b),
    transform = combine(temp3b) 
  ),
  
  temp4 = target(
    broom::tidy(dlt_model_type2_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes2P1'),
    transform = map(dlt_model_type2_brms_p1, .id = series)
  ),
  
  dlt_type2_brms_p1_params = target(
    dplyr::bind_rows(temp4),
    transform = combine(temp4) 
  ),
  
  temp4b = target(
    broom::tidy(obj_resp_model_type2_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes2P1'),
    transform = map(obj_resp_model_type2_brms_p1, .id = series)
  ),
  
  obj_resp_type2_brms_p1_params = target(
    dplyr::bind_rows(temp4b),
    transform = combine(temp4b) 
  ),
  
  temp5 = target(
    broom::tidy(dlt_model_type2_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes2P2'),
    transform = map(dlt_model_type2_brms_p2, .id = series)
  ),
  
  dlt_type2_brms_p2_params = target(
    dplyr::bind_rows(temp5),
    transform = combine(temp5) 
  ),
  
  temp5b = target(
    broom::tidy(obj_resp_model_type2_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series) %>% 
      mutate(Model = 'Bayes2P2'),
    transform = map(obj_resp_model_type2_brms_p2, .id = series)
  ),
  
  obj_resp_type2_brms_p2_params = target(
    dplyr::bind_rows(temp5b),
    transform = combine(temp5b) 
  ),
  
  dlt_model_params = bind_rows(
    dlt_sig_df_params, 
    dlt_type1_brms_p1_params,
    dlt_type1_brms_p2_params,
    dlt_type2_brms_p1_params,
    dlt_type2_brms_p2_params,
  ) %>% left_join(variable_lookup, by = 'term'),
  
  obj_resp_model_params = bind_rows(
    obj_resp_sig_df_params, 
    obj_resp_type1_brms_p1_params,
    obj_resp_type1_brms_p2_params,
    obj_resp_type2_brms_p1_params,
    obj_resp_type2_brms_p2_params,
  ) %>% left_join(variable_lookup, by = 'term'),

  
  #  Fitted values
  temp6 = target(
    broom::augment(dlt_model_sig_df) %>% 
      ungroup() %>% 
      mutate(AnalysisSeriesId = !!series, 
             Model = 'DF-Sig'),
    transform = map(dlt_model_sig_df, .id = series)
  ),
  
  dlt_sig_df_fitted = target(
    dplyr::bind_rows(temp6) %>% 
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>% 
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
      ungroup(),
    transform = combine(temp6)
  ),
  
  temp6b = target(
    broom::augment(obj_resp_model_sig_df) %>% 
      ungroup() %>% 
      mutate(AnalysisSeriesId = !!series, 
             Model = 'DF-Sig'),
    transform = map(obj_resp_model_sig_df, .id = series)
  ),
  
  obj_resp_sig_df_fitted = target(
    dplyr::bind_rows(temp6b) %>% 
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>% 
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
      ungroup(),
    transform = combine(temp6b)
  ),
  
  temp7 = target(
    broom.mixed::augment(dlt_model_type1_brms_p1) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P1'),
    transform = map(dlt_model_type1_brms_p1, .id = series)
  ),

  dlt_type1_brms_p1_fitted = target(
    dplyr::bind_rows(temp7) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp7)
  ),
  
  temp7b = target(
    broom.mixed::augment(obj_resp_model_type1_brms_p1) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P1'),
    transform = map(obj_resp_model_type1_brms_p1, .id = series)
  ),
  
  obj_resp_type1_brms_p1_fitted = target(
    dplyr::bind_rows(temp7b) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp7b)
  ),
  
  temp8 = target(
    broom.mixed::augment(dlt_model_type1_brms_p2) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P2'),
    transform = map(dlt_model_type1_brms_p2, .id = series)
  ),
  
  dlt_type1_brms_p2_fitted = target(
    dplyr::bind_rows(temp8) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp8)
  ),
  
  temp8b = target(
    broom.mixed::augment(obj_resp_model_type1_brms_p2) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P2'),
    transform = map(obj_resp_model_type1_brms_p2, .id = series)
  ),
  
  obj_resp_type1_brms_p2_fitted = target(
    dplyr::bind_rows(temp8b) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp8b)
  ),
  
  temp9 = target(
    broom.mixed::augment(dlt_model_type2_brms_p1) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P1'),
    transform = map(dlt_model_type2_brms_p1, .id = series)
  ),
  
  dlt_type2_brms_p1_fitted = target(
    dplyr::bind_rows(temp9) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp9)
  ),
  
  temp9b = target(
    broom.mixed::augment(obj_resp_model_type2_brms_p1) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P1'),
    transform = map(obj_resp_model_type2_brms_p1, .id = series)
  ),
  
  obj_resp_type2_brms_p1_fitted = target(
    dplyr::bind_rows(temp9b) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp9b)
  ),
  
  temp10 = target(
    broom.mixed::augment(dlt_model_type2_brms_p2) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P2'),
    transform = map(dlt_model_type2_brms_p2, .id = series)
  ),
  
  dlt_type2_brms_p2_fitted = target(
    dplyr::bind_rows(temp10) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp10)
  ),
  
  temp10b = target(
    broom.mixed::augment(obj_resp_model_type2_brms_p2) %>%
      ungroup() %>%
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P2'),
    transform = map(obj_resp_model_type2_brms_p2, .id = series)
  ),
  
  obj_resp_type2_brms_p2_fitted = target(
    dplyr::bind_rows(temp10b) %>%
      group_by(AnalysisSeriesId, Model, DoseLevelN) %>%
      summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>%
      mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
             conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>%
      ungroup(),
    transform = combine(temp10b)
  ),
  
  dlt_model_fitted = bind_rows(
    dlt_sig_df_fitted, 
    dlt_type1_brms_p1_fitted,
    dlt_type1_brms_p2_fitted,
    dlt_type2_brms_p1_fitted,
    dlt_type2_brms_p2_fitted,
  ),
  
  obj_resp_model_fitted = bind_rows(
    obj_resp_sig_df_fitted, 
    obj_resp_type1_brms_p1_fitted,
    obj_resp_type1_brms_p2_fitted,
    obj_resp_type2_brms_p1_fitted,
    obj_resp_type2_brms_p2_fitted,
  ),
  
  # Sampled series
  temp11 = target(
    get_fitted_draws(dlt_model_type1_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P1'),
    transform = map(dlt_model_type1_brms_p1, .id = series)
  ),
  
  dlt_type1_brms_p1_samples = target(
    dplyr::bind_rows(temp11),
    transform = combine(temp11)
  ),
  
  temp11b = target(
    get_fitted_draws(obj_resp_model_type1_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P1'),
    transform = map(obj_resp_model_type1_brms_p1, .id = series)
  ),
  
  obj_resp_type1_brms_p1_samples = target(
    dplyr::bind_rows(temp11b),
    transform = combine(temp11b)
  ),
  
  temp12 = target(
    get_fitted_draws(dlt_model_type1_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P2'),
    transform = map(dlt_model_type1_brms_p2, .id = series)
  ),
  
  dlt_type1_brms_p2_samples = target(
    dplyr::bind_rows(temp12),
    transform = combine(temp12)
  ),
  
  temp12b = target(
    get_fitted_draws(obj_resp_model_type1_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes1P2'),
    transform = map(obj_resp_model_type1_brms_p2, .id = series)
  ),
  
  obj_resp_type1_brms_p2_samples = target(
    dplyr::bind_rows(temp12b),
    transform = combine(temp12b)
  ),
  
  temp13 = target(
    get_fitted_draws(dlt_model_type2_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P1'),
    transform = map(dlt_model_type2_brms_p1, .id = series)
  ),
  
  dlt_type2_brms_p1_samples = target(
    dplyr::bind_rows(temp13),
    transform = combine(temp13)
  ),
  
  temp13b = target(
    get_fitted_draws(obj_resp_model_type2_brms_p1) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P1'),
    transform = map(obj_resp_model_type2_brms_p1, .id = series)
  ),
  
  obj_resp_type2_brms_p1_samples = target(
    dplyr::bind_rows(temp13b),
    transform = combine(temp13b)
  ),
  
  temp14 = target(
    get_fitted_draws(dlt_model_type2_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P2'),
    transform = map(dlt_model_type2_brms_p2, .id = series)
  ),
  
  dlt_type2_brms_p2_samples = target(
    dplyr::bind_rows(temp14),
    transform = combine(temp14)
  ),
  
  temp14b = target(
    get_fitted_draws(obj_resp_model_type2_brms_p2) %>% 
      mutate(AnalysisSeriesId = !!series,
             Model = 'Bayes2P2'),
    transform = map(obj_resp_model_type2_brms_p2, .id = series)
  ),
  
  obj_resp_type2_brms_p2_samples = target(
    dplyr::bind_rows(temp14b),
    transform = combine(temp14b)
  ),
  
  dlt_model_samples = bind_rows(
    dlt_type1_brms_p1_samples,
    dlt_type1_brms_p2_samples,
    dlt_type2_brms_p1_samples,
    dlt_type2_brms_p2_samples,
  ),
  
  obj_resp_model_samples = bind_rows(
    obj_resp_type1_brms_p1_samples,
    obj_resp_type1_brms_p2_samples,
    obj_resp_type2_brms_p1_samples,
    obj_resp_type2_brms_p2_samples,
  ),
  
  dlt_curve_heights = bind_rows(
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
    ungroup() %>%
    select(Model, AnalysisSeriesId, .draw, DoseLabel, .value) %>% 
    spread(DoseLabel, .value) %>% 
    mutate(CurveHeight = Top - Bottom),
  
  dlt_curve_height_summaries = dlt_curve_heights %>% 
    group_by(AnalysisSeriesId, Model) %>% 
    summarise(
      mu = mean(CurveHeight),
      sigma = sd(CurveHeight),
      n = n(),
      prob_pos = mean(CurveHeight > 0)
    ),
  
  obj_resp_curve_heights = bind_rows(
    # Samples on bottom dose for each study:
    obj_resp_model_samples %>% 
      semi_join(
        obj_resp_model_samples %>% 
          group_by(Model, AnalysisSeriesId) %>% 
          summarise(Dose1 = min(DoseLevelN)),
        by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
               'DoseLevelN' = 'Dose1')
      ) %>% mutate(DoseLabel = 'Bottom'),
    # Samples on top dose for each study:
    obj_resp_model_samples %>% 
      semi_join(
        obj_resp_model_samples %>% 
          group_by(Model, AnalysisSeriesId) %>% 
          summarise(DoseN = max(DoseLevelN)),
        by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
               'DoseLevelN' = 'DoseN')
      ) %>% mutate(DoseLabel = 'Top')
  ) %>% 
    ungroup() %>%
    select(Model, AnalysisSeriesId, .draw, DoseLabel, .value) %>% 
    spread(DoseLabel, .value) %>% 
    mutate(CurveHeight = Top - Bottom),
  
  obj_resp_curve_height_summaries = obj_resp_curve_heights %>% 
    group_by(AnalysisSeriesId, Model) %>% 
    summarise(
      mu = mean(CurveHeight),
      sigma = sd(CurveHeight),
      n = n(),
      prob_pos = mean(CurveHeight > 0)
    ),
  
)}


# Work with plan ----
# drake_cache(".drake")$unlock()
outdated(plan)
# make(plan) # Fails for Stan models
make(plan, lock_envir = FALSE)
# plan
# drake_plan_source(plan)
# plot(plan)
which_clean()
# clean()
# clean(destroy = TRUE)



# Plots ----

# Params ----
readd(dlt_model_params) %>% 
  filter(Variable == 'ED50' | Variable == 'N') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(-5, 10)
# Generally ED50 and N increase when they can.
readd(dlt_model_params) %>% 
  filter(Variable == 'E0' | Variable == 'EMax') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(0, 1)

readd(obj_resp_model_params) %>% 
  filter(Variable == 'ED50' | Variable == 'N') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(-5, 10)
readd(obj_resp_model_params) %>% 
  filter(Variable == 'E0' | Variable == 'EMax') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(0, 1)

# Fitted series ----
# One series with SE bars
readd(dlt_model_fitted) %>% 
  filter(Model == 'Bayes1P1' & AnalysisSeriesId == 21) %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_line(col = 'blue') + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT')
# Many series
readd(dlt_model_fitted) %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_line(aes(group = Model, col = Model)) + 
  facet_grid(AnalysisSeriesId ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT')
readd(obj_resp_model_fitted) %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_line(aes(group = Model, col = Model)) + 
  facet_grid(AnalysisSeriesId ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp')

# One model type
readd(dlt_model_fitted) %>% 
  left_join(dlt, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  filter(Model == 'Bayes1P1') %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  theme(legend.position = 'bottom')
readd(obj_resp_model_fitted) %>% 
  left_join(obj_resp, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  filter(Model == 'Bayes1P1') %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  theme(legend.position = 'bottom')
# All model types
readd(dlt_model_fitted) %>% 
  left_join(dlt, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_grid(DoseVaryingTreatmentType ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  theme(legend.position = 'bottom')
readd(obj_resp_model_fitted) %>% 
  left_join(obj_resp, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_grid(DoseVaryingTreatmentType ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  theme(legend.position = 'bottom')


# Samples ----
readd(dlt_model_samples) %>% 
  filter(.draw <= 1000) %>%
  ggplot(aes(x = DoseLevelN, y = .value)) + 
  geom_line(aes(group = .draw), alpha = 0.01) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  facet_grid(AnalysisSeriesId ~ Model)
readd(obj_resp_model_samples) %>% 
  filter(.draw <= 1000) %>%
  ggplot(aes(x = DoseLevelN, y = .value)) + 
  geom_line(aes(group = .draw), alpha = 0.01) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  facet_grid(AnalysisSeriesId ~ Model)

# Curve heights ----
# Heights of dose-event curves
readd(dlt_curve_height_summaries) %>% 
  ggplot(aes(x = Model, y = mu)) +
  geom_pointrange(aes(ymin = mu - 1.96 * sigma, ymax = mu + 1.96 * sigma), 
                  size = 0.2) +
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  labs(y = 'Height of dose-DLT curve') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
readd(obj_resp_curve_height_summaries) %>% 
  ggplot(aes(x = Model, y = mu)) +
  geom_pointrange(aes(ymin = mu - 1.96 * sigma, ymax = mu + 1.96 * sigma), 
                  size = 0.2) +
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  labs(y = 'Height of dose-ObjResp curve') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# All model and treatment types
readd(dlt_curve_height_summaries) %>% 
  left_join(dlt %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = mu, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  labs(x = 'Dose', y = 'Gradient') + 
  theme(legend.position = 'bottom')
readd(obj_resp_curve_height_summaries) %>% 
  left_join(obj_resp %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = mu, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  labs(x = 'Dose', y = 'Gradient') + 
  theme(legend.position = 'bottom')
# Chances of positive curve height
readd(dlt_curve_height_summaries) %>% 
  ggplot(aes(x = Model, y = prob_pos)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  ylim(0, 1) +
  labs(y = 'Certainty that dose-DLT curve has positive gradient') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
readd(obj_resp_curve_height_summaries) %>% 
  ggplot(aes(x = Model, y = prob_pos)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  ylim(0, 1) +
  labs(y = 'Certainty that dose-ObjResp curve has positive gradient') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# All model and treatment types
readd(dlt_curve_height_summaries) %>% 
  left_join(dlt %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = prob_pos, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbPosGrad') + 
  theme(legend.position = 'bottom')
readd(obj_resp_curve_height_summaries) %>% 
  left_join(obj_resp %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = prob_pos, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbPosGrad') + 
  theme(legend.position = 'bottom')

# Distribution of curve heights
library(ggridges)
readd(dlt_curve_heights) %>% 
  ggplot(aes(x = CurveHeight)) + 
  geom_density() + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_grid(AnalysisSeriesId ~ Model)
# Or
readd(dlt_curve_heights) %>% 
  ggplot(aes(x = CurveHeight, y = Model)) + 
  geom_density_ridges() +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_wrap( ~ AnalysisSeriesId, ncol = 3)

readd(obj_resp_curve_heights) %>% 
  ggplot(aes(x = CurveHeight)) + 
  geom_density() + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of ObjResp curve') + 
  facet_grid(AnalysisSeriesId ~ Model)
# Or
readd(obj_resp_curve_heights) %>% 
  ggplot(aes(x = CurveHeight, y = Model)) + 
  geom_density_ridges() +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of ObjResp curve') + 
  facet_wrap( ~ AnalysisSeriesId, ncol = 3)


