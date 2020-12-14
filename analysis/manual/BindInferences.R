
# Load ----
library(here)
source(here('Load.R'))
source(here('RunTests.R'))

library(purrr)
library(tidyr)
library(DoseFinding)
library(brms)

source(here('analysis', 'manual', 'Objects.R'))

# Params ----
dlt_sig_df_params <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_sig_df_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p1_params <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p1_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p2_params <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p2_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p1_params <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p1_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p2_params <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p2_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_model_params = bind_rows(
  dlt_sig_df_params, 
  dlt_type1_brms_p1_params,
  dlt_type1_brms_p2_params,
  dlt_type2_brms_p1_params,
  dlt_type2_brms_p2_params,
) %>% left_join(variable_lookup, by = 'term') 

obj_resp_sig_df_params <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_sig_df_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p1_params <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p1_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p2_params <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p2_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p1_params <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p1_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p2_params <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p2_params_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_model_params = bind_rows(
  obj_resp_sig_df_params, 
  obj_resp_type1_brms_p1_params,
  obj_resp_type1_brms_p2_params,
  obj_resp_type2_brms_p1_params,
  obj_resp_type2_brms_p2_params,
) %>% left_join(variable_lookup, by = 'term')


# Fitted values ----
dlt_sig_df_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_sig_df_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p1_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p1_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p2_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p2_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p1_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p1_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p2_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p2_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_model_fitted = bind_rows(
  dlt_sig_df_fitted, 
  dlt_type1_brms_p1_fitted,
  dlt_type1_brms_p2_fitted,
  dlt_type2_brms_p1_fitted,
  dlt_type2_brms_p2_fitted,
) %>% 
  group_by(AnalysisSeriesId, Model, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  ungroup()

# dlt_model_fitted %>% count(AnalysisSeriesId)
# dlt_model_fitted %>% filter(AnalysisSeriesId == 3)
# dlt_model_fitted %>% filter(AnalysisSeriesId == 3) %>% tail

obj_resp_sig_df_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_sig_df_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p1_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p1_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p2_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p2_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p1_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p1_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p2_fitted <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p2_fitted_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_model_fitted = bind_rows(
  obj_resp_sig_df_fitted, 
  obj_resp_type1_brms_p1_fitted,
  obj_resp_type1_brms_p2_fitted,
  obj_resp_type2_brms_p1_fitted,
  obj_resp_type2_brms_p2_fitted,
) %>% 
  group_by(AnalysisSeriesId, Model, DoseLevelN) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(conf.low = pmax(0, .fitted - 1.96 * .se.fit),
         conf.high = pmin(1, .fitted + 1.96 * .se.fit)) %>% 
  ungroup()


# Samples ----
dlt_type1_brms_p1_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p1_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p2_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p2_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p1_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p1_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p2_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p2_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_model_samples = bind_rows(
  dlt_type1_brms_p1_samples,
  dlt_type1_brms_p2_samples,
  dlt_type2_brms_p1_samples,
  dlt_type2_brms_p2_samples,
)

obj_resp_type1_brms_p1_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p1_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p2_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p2_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p1_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p1_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p2_samples <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p2_samples_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_model_samples = bind_rows(
  obj_resp_type1_brms_p1_samples,
  obj_resp_type1_brms_p2_samples,
  obj_resp_type2_brms_p1_samples,
  obj_resp_type2_brms_p2_samples,
)


# Model diagnostics ----
dlt_type1_brms_p1_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p1_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type1_brms_p2_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type1_brms_p2_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p1_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p1_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_type2_brms_p2_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "dlt_type2_brms_p2_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
dlt_model_diagnostics = bind_rows(
  dlt_type1_brms_p1_diagnostics,
  dlt_type1_brms_p2_diagnostics,
  dlt_type2_brms_p1_diagnostics,
  dlt_type2_brms_p2_diagnostics,
)

obj_resp_type1_brms_p1_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p1_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type1_brms_p2_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type1_brms_p2_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p1_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p1_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_type2_brms_p2_diagnostics <- dir(
  path = 'analysis/manual/.cache', pattern = "obj_resp_type2_brms_p2_diagnostics_", 
  full.names = TRUE) %>%
  map(readRDS) %>%
  reduce(bind_rows)
obj_resp_model_diagnostics = bind_rows(
  obj_resp_type1_brms_p1_diagnostics,
  obj_resp_type1_brms_p2_diagnostics,
  obj_resp_type2_brms_p1_diagnostics,
  obj_resp_type2_brms_p2_diagnostics,
)

# Curve heights ----
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
  ungroup() %>%
  select(Model, AnalysisSeriesId, .draw, DoseLabel, .value) %>% 
  spread(DoseLabel, .value) %>% 
  mutate(CurveHeight = Top - Bottom)

obj_resp_curve_heights <- bind_rows(
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
  mutate(CurveHeight = Top - Bottom)

# Curve height summaries ----
dlt_sif_df_curve_heights <- bind_rows(
  # Estimates for bottom dose for each study:
  dlt_sig_df_fitted %>% 
    select(AnalysisSeriesId, Model, DoseLevelN, .fitted) %>% 
    unique() %>% 
    semi_join(
      dlt_sig_df_fitted %>% 
        group_by(AnalysisSeriesId, Model) %>% 
        summarise(Dose1 = min(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'Dose1')) %>% 
    mutate(DoseLabel = 'Bottom'),
  # Estimates for top dose for each study:
  dlt_sig_df_fitted %>% 
    select(AnalysisSeriesId, Model, DoseLevelN, .fitted) %>% 
    unique() %>% 
    semi_join(
      dlt_sig_df_fitted %>% 
        group_by(AnalysisSeriesId, Model) %>% 
        summarise(DoseN = max(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'DoseN')) %>% 
    mutate(DoseLabel = 'Top')
) %>% 
  ungroup() %>%
  select(AnalysisSeriesId, Model, DoseLabel, .fitted) %>% 
  spread(DoseLabel, .fitted) %>% 
  mutate(CurveHeight = Top - Bottom)

dlt_curve_height_summaries <- bind_rows(
  dlt_sif_df_curve_heights %>% 
    select(AnalysisSeriesId, Model, mu = CurveHeight) %>% 
    mutate(sigma = NA, n = NA, prob_pos = NA),
  dlt_curve_heights %>% 
    group_by(AnalysisSeriesId, Model) %>% 
    summarise(
      mu = mean(CurveHeight),
      sigma = sd(CurveHeight),
      n = n(),
      prob_pos = mean(CurveHeight > 0)
    )
)

obj_resp_sif_df_curve_heights <- bind_rows(
  # Estimates for bottom dose for each study:
  obj_resp_sig_df_fitted %>% 
    select(AnalysisSeriesId, Model, DoseLevelN, .fitted) %>% 
    unique() %>% 
    semi_join(
      obj_resp_sig_df_fitted %>% 
        group_by(AnalysisSeriesId, Model) %>% 
        summarise(Dose1 = min(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'Dose1')) %>% 
    mutate(DoseLabel = 'Bottom'),
  # Estimates for top dose for each study:
  obj_resp_sig_df_fitted %>% 
    select(AnalysisSeriesId, Model, DoseLevelN, .fitted) %>% 
    unique() %>% 
    semi_join(
      obj_resp_sig_df_fitted %>% 
        group_by(AnalysisSeriesId, Model) %>% 
        summarise(DoseN = max(DoseLevelN)),
      by = c('Model' = 'Model', 'AnalysisSeriesId' = 'AnalysisSeriesId',
             'DoseLevelN' = 'DoseN')) %>% 
    mutate(DoseLabel = 'Top')
) %>% 
  ungroup() %>%
  select(AnalysisSeriesId, Model, DoseLabel, .fitted) %>% 
  spread(DoseLabel, .fitted) %>% 
  mutate(CurveHeight = Top - Bottom)

obj_resp_curve_height_summaries <- bind_rows(
  obj_resp_sif_df_curve_heights %>% 
    select(AnalysisSeriesId, Model, mu = CurveHeight) %>% 
    mutate(sigma = NA, n = NA, prob_pos = NA),
  obj_resp_curve_heights %>% 
    group_by(AnalysisSeriesId, Model) %>% 
    summarise(
      mu = mean(CurveHeight),
      sigma = sd(CurveHeight),
      n = n(),
      prob_pos = mean(CurveHeight > 0)
    )
)

