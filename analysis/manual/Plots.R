
library(ggplot2)

# Params ----
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

obj_resp_model_params %>% 
  filter(Variable == 'ED50' | Variable == 'N') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(-5, 10)
obj_resp_model_params %>% 
  filter(Variable == 'E0' | Variable == 'EMax') %>% 
  ggplot(aes(x = Model, y = estimate, col = Model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01) +
  facet_grid(AnalysisSeriesId ~ Variable) + 
  ylim(0, 1)

# Fitted series ----
# One series with SE bars
dlt_model_fitted %>% 
  filter(Model == 'Bayes1P1' & AnalysisSeriesId == 21) %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_line(col = 'blue') + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT')
# Many series
dlt_model_fitted %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_line(aes(group = Model, col = Model)) + 
  facet_grid(AnalysisSeriesId ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT')
obj_resp_model_fitted %>% 
  ggplot(aes(x = DoseLevelN, y = .fitted)) + 
  geom_line(aes(group = Model, col = Model)) + 
  facet_grid(AnalysisSeriesId ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp')

# One model type
dlt_model_fitted %>% 
  left_join(dlt, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  filter(Model == 'Bayes2P2') %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  theme(legend.position = 'bottom')
obj_resp_model_fitted %>% 
  left_join(obj_resp, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  filter(Model == 'Bayes2P2') %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  theme(legend.position = 'bottom')
# All model types
dlt_model_fitted %>% 
  left_join(dlt, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_grid(DoseVaryingTreatmentType ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  theme(legend.position = 'bottom')
obj_resp_model_fitted %>% 
  left_join(obj_resp, by = c('AnalysisSeriesId', 'DoseLevelN')) %>% 
  left_join(studies, by = 'Study') %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = DoseLevel, y = .fitted)) + 
  geom_line(aes(group = AnalysisSeriesId, col = DoseVaryingTreatmentType)) + 
  facet_grid(DoseVaryingTreatmentType ~ Model) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  theme(legend.position = 'bottom')


# Samples ----
dlt_model_samples %>% 
  filter(.draw <= 1000) %>%
  ggplot(aes(x = DoseLevelN, y = .value)) + 
  geom_line(aes(group = .draw), alpha = 0.01) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbDLT') + 
  facet_grid(AnalysisSeriesId ~ Model)
obj_resp_model_samples %>% 
  filter(.draw <= 1000) %>%
  ggplot(aes(x = DoseLevelN, y = .value)) + 
  geom_line(aes(group = .draw), alpha = 0.01) + 
  ylim(0, 1) + labs(x = 'Dose', y = 'ProbObjResp') + 
  facet_grid(AnalysisSeriesId ~ Model)

# Curve heights ----
# Heights of dose-event curves
dlt_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = mu)) +
  geom_pointrange(aes(ymin = mu - 1.96 * sigma, ymax = mu + 1.96 * sigma), 
                  size = 0.2) +
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  labs(y = 'Height of dose-DLT curve') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
obj_resp_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = mu)) +
  geom_pointrange(aes(ymin = mu - 1.96 * sigma, ymax = mu + 1.96 * sigma), 
                  size = 0.2) +
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  labs(y = 'Height of dose-ObjResp curve') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# All model and treatment types
dlt_curve_height_summaries %>% 
  left_join(dlt %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = mu, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 3) + 
  labs(x = 'Dose', y = 'Gradient') + 
  theme(legend.position = 'bottom')
obj_resp_curve_height_summaries %>% 
  left_join(obj_resp %>% select(Study, AnalysisSeriesId) %>% distinct(), 
            by = 'AnalysisSeriesId') %>% 
  left_join(studies, by = 'Study') %>% 
  ungroup() %>% 
  mutate(AnalysisSeriesId = factor(AnalysisSeriesId)) %>% 
  ggplot(aes(x = Model, y = mu, col = DoseVaryingTreatmentType)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 3) + 
  labs(x = 'Dose', y = 'Gradient') + 
  theme(legend.position = 'bottom')
# Chances of positive curve height
dlt_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = prob_pos)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  ylim(0, 1) +
  labs(y = 'Certainty that dose-DLT curve has positive gradient') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
obj_resp_curve_height_summaries %>% 
  ggplot(aes(x = Model, y = prob_pos)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5, col = 'red', linetype = 'dashed') + 
  facet_wrap(~ AnalysisSeriesId) + 
  ylim(0, 1) +
  labs(y = 'Certainty that dose-ObjResp curve has positive gradient') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# All model and treatment types
dlt_curve_height_summaries %>% 
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
obj_resp_curve_height_summaries %>% 
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
dlt_curve_heights %>% 
  ggplot(aes(x = CurveHeight)) + 
  geom_density() + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_grid(AnalysisSeriesId ~ Model)
# Or
dlt_curve_heights %>% 
  ggplot(aes(x = CurveHeight, y = Model)) + 
  geom_density_ridges() +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of DLT curve') + 
  facet_wrap( ~ AnalysisSeriesId, ncol = 3)

obj_resp_curve_heights %>% 
  ggplot(aes(x = CurveHeight)) + 
  geom_density() + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of ObjResp curve') + 
  facet_grid(AnalysisSeriesId ~ Model)
# Or
obj_resp_curve_heights %>% 
  ggplot(aes(x = CurveHeight, y = Model)) + 
  geom_density_ridges() +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') + 
  xlim(-0.05, 0.1) + 
  labs(x = 'Height of ObjResp curve') + 
  facet_wrap( ~ AnalysisSeriesId, ncol = 3)
