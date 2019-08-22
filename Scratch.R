
library(glue)
library(ggplot2)
library(broom)
library(purrr)
library(broom)
library(tidyr)

library(brms)
library(tidybayes)
library(ggridges)

source('Load.R')

studies %>% head
outcomes %>% head
binary_events %>% head(20)
binary_series %>% head(20)

dlt %>% select(Study) %>% distinct() %>% nrow
obj_resp %>% select(Study) %>% distinct() %>% nrow

# Analyse all DLT and objective response series in separate GLMs ----
dlt %>% 
  group_by(Study, AnalysisSeriesId) %>% 
  nest() %>% 
  mutate(
    num_dose = map_int(data, nrow),
    num_pat = map_dbl(data, .f = function(df) sum(df$n)),
    num_event = map_dbl(data, .f = function(df) sum(df$Events)),
    model = map(.f = function(df) {
      glm(ProbEvent ~ 1 + DoseLevel, data = df, weights = n,
          family = binomial('logit')) }, 
      .x = data),
    tidy_model = map(model, tidy),
    fitted = map(model, augment, type.predict = 'response'),
    # Column 2, row 2 of tidy model is estimated slope parameter for dose-level
    dose_grad = map(tidy_model, 2) %>% map_dbl(2) 
  ) -> dlt_models
warnings()
# Several warnings on fitted probabilities numerically 0 or 1 occurred
dlt_models %>% head
dlt_models$num_dose %>% summary()

obj_resp %>% 
  group_by(Study, AnalysisSeriesId) %>% 
  nest() %>% 
  mutate(
    num_dose = map_int(data, nrow),
    num_pat = map_dbl(data, .f = function(df) sum(df$n)),
    num_event = map_dbl(data, .f = function(df) sum(df$Events)),
    model = map(.f = function(df) {
      glm(ProbEvent ~ 1 + DoseLevel, data = df, weights = n,
          family = binomial('logit')) }, 
      .x = data),
    tidy_model = map(model, tidy),
    fitted = map(model, augment, type.predict = 'response'),
    # Column 2, row 2 of tidy model is estimated slope parameter for dose-level
    dose_grad = map(tidy_model, 2) %>% map_dbl(2) 
  ) -> obj_resp_models
obj_resp_models %>% head

mean(dlt_models$dose_grad > 0)  # 78%
mean(obj_resp_models$dose_grad > 0)  # 51%

dlt_models %>% 
  # filter(num_event >= 2) %>% 
  ggplot(aes(x = dose_grad)) + 
  geom_histogram()
obj_resp_models %>% 
  ggplot(aes(x = dose_grad)) + 
  geom_histogram()


dlt_models %>% 
  group_by(Study) %>% 
  summarise(dlt_dose_grad = mean(dose_grad)) %>% 
  inner_join(
    obj_resp_models %>% 
      group_by(Study) %>% 
      summarise(obj_resp_dose_grad = mean(dose_grad)),
    by = 'Study'
  ) -> paired_dose_sensitivities

paired_dose_sensitivities %>% 
  summarise(mean(dlt_dose_grad > obj_resp_dose_grad)) # 72%

paired_dose_sensitivities %>% 
  ggplot(aes(x = dlt_dose_grad, y = obj_resp_dose_grad)) + 
  geom_point() + 
  geom_abline(col = 'red', linetype = 'dashed') + 
  labs(x = 'Sensitivity of DLT to dose',
       y = 'Sensitivity of objective response to dose')

paired_dose_sensitivities %>% 
  filter(abs(dlt_dose_grad) <= 2 & abs(obj_resp_dose_grad) <= 2) %>% 
  summarise(mean(dlt_dose_grad > obj_resp_dose_grad)) # 61%

paired_dose_sensitivities %>% 
  left_join(studies, by = 'Study') %>% 
  filter(abs(dlt_dose_grad) <= 2 & abs(obj_resp_dose_grad) <= 2) %>% 
  ggplot(aes(x = dlt_dose_grad, y = obj_resp_dose_grad)) + 
  geom_point(aes(col = DoseVaryingTreatmentType)) + 
  geom_abline(col = 'red', linetype = 'dashed') + 
  labs(x = 'Sensitivity of DLT to dose increases',
       y = 'Sensitivity of objective response to dose increases')

paired_dose_sensitivities %>% 
  filter(abs(dlt_dose_grad) <= 2 & abs(obj_resp_dose_grad) <= 2) %>% 
  left_join(studies, by = 'Study') %>% 
  group_by(DoseVaryingTreatmentType) %>% 
  summarise(n = n(), mean(dlt_dose_grad), mean(obj_resp_dose_grad))


# Fitted lines - DLT
dlt_models %>% 
  left_join(studies, by = 'Study') %>% 
  select(Study, AnalysisSeriesId, DoseVaryingTreatmentType, fitted) %>% 
  unnest %>% 
  filter(abs(DoseLevel) <= 5) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) +
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 4, nrow = 4) + 
  theme(legend.position = 'none')

# Just certain types
dlt_models %>% 
  left_join(studies, by = 'Study') %>% 
  select(Study, AnalysisSeriesId, DoseVaryingTreatmentType, fitted) %>% 
  unnest %>% 
  filter(abs(DoseLevel) <= 5) %>% 
  filter(DoseVaryingTreatmentType %in% c('Radiotherapy', 'Monoclonal Antibody', 
                                         'Chemotherapy', 'Inhibitor')) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) +
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2, nrow = 2) + 
  theme(legend.position = 'none')

# Fitted lines - ObjResp
obj_resp_models %>% 
  left_join(studies, by = 'Study') %>% 
  select(Study, AnalysisSeriesId, DoseVaryingTreatmentType, fitted) %>% 
  unnest %>% 
  filter(abs(DoseLevel) <= 5) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) +
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 4, nrow = 4) + 
  theme(legend.position = 'bottom')

# Generally, it appears that objective response has been less sensitive to dose
# increases than DLT. I.e. increases in dose have on average increased the 
# chances of DLT faster than the chances of response.


# Bayesian hierarchical analysis ----



## DLT ----
dlt_mod <- brm(Events | trials(n) ~ (1 + DoseLevel | AnalysisSeriesId) + 
                  (1 + DoseLevel | DoseVaryingTreatmentType), 
                data = dlt %>% left_join(studies, by = 'Study'), 
                family = binomial('logit'), 
                control = list(adapt_delta = 0.95), cores = 4)

prior_summary(dlt_mod)
summary(dlt_mod)
# No probs highlighted in fit

mod <- dlt_mod
pp_check(mod) + labs(title = 'Posterior predictive checks for DLT model')



# Ridges showing dose-sensitivity by tmt type
mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(term == 'DoseLevel') %>% 
  ggplot(aes(x = r_DoseVaryingTreatmentType, y = TmtType, 
             group = TmtType, fill = TmtType, )) + 
  geom_density_ridges() + 
  labs(x = 'Gradient with respect to dose', y = 'Treatment type',
       title = 'Posterior distribution of slope of dose-DLT curves') +
  theme(legend.position = 'none', text = element_text(size = 15)) + 
  xlim(-1.5, 2.5)

# Just certain tmt types
mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Radiotherapy', 'Monoclonal.Antibody', 'Chemotherapy', 
                        'Inhibitor', 'Immunomodulatory.drug')) %>% 
  mutate(TmtTypeO = factor(TmtType, levels = c('Monoclonal.Antibody',
                                               'Immunomodulatory.drug',
                                               'Radiotherapy',
                                               'Inhibitor',
                                               'Chemotherapy'), 
                           ordered = TRUE)) %>%
  filter(term == 'DoseLevel') %>% 
  ggplot(aes(y = TmtTypeO, group = TmtTypeO, fill = TmtTypeO, 
             x = r_DoseVaryingTreatmentType)) + 
  geom_density_ridges() + 
  labs(x = 'Gradient with respect to dose', y = 'Treatment type',
       title = 'Posterior distribution of sensitivity of DLT to dose increases') +
  theme(legend.position = 'none', text = element_text(size = 15)) + 
  xlim(-1.5, 2.5)

# Fitted series
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 'Radiotherapy', 
                                         'Monoclonal Antibody', 'Inhibitor')) %>% 
  add_fitted_draws(mod) %>%
  mutate(EstProbEvent = .value / n) %>% 
  summarise(EstProbEvent = mean(EstProbEvent)) %>% 
  ggplot(aes(x = DoseLevel, y = EstProbEvent, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  theme(text = element_text(size = 15)) +
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Drug type', y = 'Prob(DLT)')


## ObjResp ----
obj_resp_mod <- brm(Events | trials(n) ~ (1 + DoseLevel | AnalysisSeriesId) + 
                      (1 + DoseLevel | DoseVaryingTreatmentType), 
                    data = obj_resp %>% 
                      left_join(studies, by = 'Study') %>% 
                      filter(n > 0), 
                    family = binomial('logit'), 
                    control = list(adapt_delta = 0.99), cores = 4)

prior_summary(obj_resp_mod)
summary(obj_resp_mod)
# No probs highlighted in fit

mod <- obj_resp_mod
pp_check(mod) + labs(title = 'Posterior predictive checks for ObjResp model')

# Ridges showing dose-sensitivity by tmt type
mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(term == 'DoseLevel') %>% 
  ggplot(aes(x = r_DoseVaryingTreatmentType, y = TmtType, 
             group = TmtType, fill = TmtType, )) + 
  geom_density_ridges() + 
  labs(x = 'Gradient with respect to dose', y = 'Treatment type',
       title = 'Posterior distribution of slope of dose-response curves') +
  theme(legend.position = 'none', text = element_text(size = 15)) + 
  xlim(-1.5, 2.5)

# Just certain tmt types
mod %>% 
  spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
  filter(TmtType %in% c('Radiotherapy', 'Monoclonal.Antibody', 'Chemotherapy', 
                        'Inhibitor', 'Immunomodulatory.drug')) %>% 
  mutate(TmtTypeO = factor(TmtType, levels = c('Monoclonal.Antibody',
                                               'Immunomodulatory.drug',
                                               'Radiotherapy',
                                               'Inhibitor',
                                               'Chemotherapy'), 
                           ordered = TRUE)) %>%
  filter(term == 'DoseLevel') %>% 
  ggplot(aes(y = TmtTypeO, group = TmtTypeO, fill = TmtTypeO, 
             x = r_DoseVaryingTreatmentType)) + 
  geom_density_ridges() + 
  labs(x = 'Gradient with respect to dose', y = 'Treatment type',
       title = 'Posterior distribution of sensitivity of response to dose increases') +
  theme(legend.position = 'none', text = element_text(size = 15)) + 
  xlim(-1.5, 2.5)

# Fitted series
obj_resp %>% 
  filter(n > 0) %>% 
  left_join(studies, by = 'Study') %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 'Radiotherapy', 
                                         'Monoclonal Antibody', 'Inhibitor')) %>% 
  add_fitted_draws(mod) %>%
  mutate(EstProbEvent = .value / n) %>% 
  summarise(EstProbEvent = mean(EstProbEvent)) %>% 
  ggplot(aes(x = DoseLevel, y = EstProbEvent, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  theme(text = element_text(size = 15)) +
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Drug type', 
       y = 'Prob(ObjectiveResponse)')
