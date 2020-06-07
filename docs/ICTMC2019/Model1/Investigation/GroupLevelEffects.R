
summary(dlt_mod)

fixef(dlt_mod)
inv_logit_scaled(fixef(dlt_mod)[1]) 
# 7% DLT risk, on average

ranef(dlt_mod)
# For AnalysisSeriesId == 1 (the first series), the AnalysisSeriesId-level 
# Intercept is -0.94 and DoseLevel coefficient is -0.31

dlt %>% filter(AnalysisSeriesId == 1)
# This series is Sharma2014_1
studies %>% filter(Manuscript == 'Sharma2014')
# The study examines the monoclonal antibody LBY135 in advanced solid tumours.

get_variables(dlt_mod)
as.data.frame(dlt_mod) %>% colnames()



# spread_draws just extracts parameter samples:

# These match:
as.data.frame(dlt_mod) %>% 
  select(`r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.`
  ) %>% head
dlt_mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Monoclonal.Antibody')) %>% 
  filter(term == 'Intercept') %>% head
# These match:
as.data.frame(dlt_mod) %>% 
  select(`r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.`
  ) %>% tail
dlt_mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Monoclonal.Antibody')) %>% 
  filter(term == 'Intercept') %>% tail

# These match:
as.data.frame(dlt_mod) %>% 
  select(`r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`
  ) %>% head
dlt_mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Monoclonal.Antibody')) %>% 
  filter(term == 'DoseLevel') %>% head
# These match:
as.data.frame(dlt_mod) %>% 
  select(`r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`
  ) %>% tail
dlt_mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Monoclonal.Antibody')) %>% 
  filter(term == 'DoseLevel') %>% tail



# add_fitted_draws
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  add_fitted_draws(dlt_mod) %>% 
  mutate(EstProbEvent = .value / n) -> dlt_fitted_draws1
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  add_fitted_draws(dlt_mod) %>% 
  mutate(EstProbEvent = .value / n) -> dlt_fitted_draws2

dlt_fitted_draws1 %>% head %>% ungroup %>% select(.value)
dlt_fitted_draws2 %>% head %>% ungroup %>% select(.value)
# These match, so add_fitted_draws is not a stochastic transform

# add_fitted_draws simply calculates the mean fitted value by adding the 
# various applicable sample, scaled by exposure. For binary outcomes (as here),
# that fitted value is scaled up by the prevailing n to give an expected number 
# of events rather than an expected prob. E.g. dividing by n simply takes you
# back to the probability.
# These match:
dlt_fitted_draws1 %>% 
  ungroup() %>% 
  filter(AnalysisSeriesId == 1) %>% 
  filter(DoseLevel == -3) %>% 
  select(AnalysisSeriesId, DoseLevel, n, .value, .draw) %>% 
  head
as.data.frame(dlt_mod) %>% 
  mutate(
    exp_events = (b_Intercept +
      `r_AnalysisSeriesId.1.Intercept.` +
      `r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.` + 
      -3*(`r_AnalysisSeriesId.1.DoseLevel.` + 
            `r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`)) %>% 
      inv_logit_scaled() * 4
  ) %>% 
  select(exp_events) %>% 
  head
# These match:
dlt_fitted_draws1 %>% 
  ungroup() %>% 
  filter(AnalysisSeriesId == 1) %>% 
  filter(DoseLevel == -3) %>% 
  select(AnalysisSeriesId, DoseLevel, n, .value, .draw) %>% 
  tail
as.data.frame(dlt_mod) %>% 
  mutate(
    exp_events = (b_Intercept +
                    `r_AnalysisSeriesId.1.Intercept.` +
                    `r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.` + 
                    -3*(`r_AnalysisSeriesId.1.DoseLevel.` + 
                          `r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`)) %>% 
      inv_logit_scaled() * 4
  ) %>% 
  select(exp_events) %>% 
  tail


# These match:
dlt_fitted_draws1 %>% 
  ungroup() %>% 
  filter(AnalysisSeriesId == 1) %>% 
  filter(DoseLevel == 2) %>% 
  select(AnalysisSeriesId, DoseLevel, n, .value, .draw) %>% 
  head
as.data.frame(dlt_mod) %>% 
  mutate(
    exp_events = (b_Intercept +
                    `r_AnalysisSeriesId.1.Intercept.` +
                    `r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.` + 
                    2*(`r_AnalysisSeriesId.1.DoseLevel.` + 
                          `r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`)) %>% 
      inv_logit_scaled() * 8
  ) %>% 
  select(exp_events) %>% 
  head
# These match:
dlt_fitted_draws1 %>% 
  ungroup() %>% 
  filter(AnalysisSeriesId == 1) %>% 
  filter(DoseLevel == 2) %>% 
  select(AnalysisSeriesId, DoseLevel, n, .value, .draw) %>% 
  tail
as.data.frame(dlt_mod) %>% 
  mutate(
    exp_events = (b_Intercept +
                    `r_AnalysisSeriesId.1.Intercept.` +
                    `r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.` + 
                    2*(`r_AnalysisSeriesId.1.DoseLevel.` + 
                          `r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`)) %>% 
      inv_logit_scaled() * 8
  ) %>% 
  select(exp_events) %>% 
  tail



# add_predicted_draws
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  add_predicted_draws(dlt_mod) -> dlt_predicted_draws1
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  add_predicted_draws(dlt_mod) -> dlt_predicted_draws2

dlt_predicted_draws1 %>% head %>% ungroup %>% select(.prediction)
dlt_predicted_draws2 %>% head %>% ungroup %>% select(.prediction)
# These differ because add_predicted_draws is stochastic. It samples outcomes 
# from the posterior predictive distribution.



# Scratch ----

# EXTRACT ALL
as.data.frame(dlt_mod) %>% 
  select(b_Intercept , 
         `r_AnalysisSeriesId.1.Intercept.`,
         `r_AnalysisSeriesId.1.DoseLevel.`,
         `r_DoseVaryingTreatmentType.Monoclonal.Antibody.Intercept.`,
         `r_DoseVaryingTreatmentType.Monoclonal.Antibody.DoseLevel.`
  ) %>% head
