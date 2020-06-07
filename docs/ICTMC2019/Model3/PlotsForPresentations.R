

# This script in the root of the project loads the datasets
source('Load.R')

# Load objective response model and samples
dlt_mod <- readRDS('docs/ICTMC2019/Model3/dlt_mod.rds')
dlt_fitted_draws <- readRDS('docs/ICTMC2019/Model3/dlt_fitted_draws.rds')
obj_resp_mod <- readRDS('docs/ICTMC2019/Model3/obj_resp_mod.rds')
obj_resp_fitted_draws <- readRDS('docs/ICTMC2019/Model3/obj_resp_fitted_draws.rds')




# This script loads objects specific to this modelling task.
# It relies on dlt_fitted_draws being loaded.
source('docs/ICTMC2019/Model3/ModellingObjects.R')

library(patchwork)


# Example where assumptions are met ----
## Toxicity assumptions
dlt_analysis <- analyse_dlt(
  dlt %>% filter(Study == 'Saji2008_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = TRUE)
dlt_analysis$p1 + labs(subtitle = '')

## Efficacy assumptions
efficacy_analysis <- analyse_obj_resp(
  obj_resp %>% filter(Study == 'Saji2008_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = TRUE,
  line_col = 'darkgreen', ribbon_fill = 'chartreuse')
efficacy_analysis$p1 + labs(subtitle = '')
efficacy_analysis$p2

## Side by side
dlt_analysis <- analyse_dlt(
  dlt %>% filter(Study == 'Saji2008_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = FALSE)
efficacy_analysis <- analyse_obj_resp(
  obj_resp %>% filter(Study == 'Saji2008_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = FALSE, legend1 = TRUE, 
  line_col = 'darkgreen', ribbon_fill = 'chartreuse')

(dlt_analysis$p1 + labs(subtitle = 'Toxicity') + 
  theme(text = element_text(size=20))) + 
  (efficacy_analysis$p1 + labs(subtitle = 'Response') + 
  theme(text = element_text(size=20)))


# Example where asumptions appear to be violated ----

## Side by side
dlt_analysis <- analyse_dlt(
  dlt %>% filter(Study == 'Kim2009_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = FALSE)
efficacy_analysis <- analyse_obj_resp(
  obj_resp %>% filter(Study == 'Kim2009_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = FALSE, legend1 = TRUE, line_col = 'red', ribbon_fill = 'coral')

(dlt_analysis$p1 + labs(subtitle = 'Toxicity') + 
    theme(text = element_text(size=20))) + 
  (efficacy_analysis$p1 + labs(subtitle = 'Response') + 
     theme(text = element_text(size=20)))



# DLT modelled series ----
dlt %>% 
  left_join(studies, by = 'Study') %>% 
  left_join(manuscripts, by = 'Manuscript') %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
                                         'Radiotherapy', 
                                         'Monoclonal Antibody', 
                                         'Inhibitor')) %>% 
  add_fitted_draws(dlt_mod) %>%
  mutate(EstProbEvent = .value / n) %>% 
  summarise(EstProbEvent = mean(EstProbEvent)) %>% 
  ggplot(aes(x = DoseLevel, y = EstProbEvent, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Drug type', y = 'Prob(DLT)') + 
  theme(legend.position = 'none') + 
  theme(text = element_text(size=20))

# Obj-Resp modelled series ----
obj_resp %>% 
  filter(n > 0) %>% 
  left_join(studies, by = 'Study') %>% 
  left_join(manuscripts, by = 'Manuscript') %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
                                         'Radiotherapy', 
                                         'Monoclonal Antibody', 
                                         'Inhibitor')) %>% 
  add_fitted_draws(obj_resp_mod) %>%
  mutate(EstProbEvent = .value / n) %>% 
  summarise(EstProbEvent = mean(EstProbEvent)) %>% 
  ggplot(aes(x = DoseLevel, y = EstProbEvent, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Drug type', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'none') + 
  theme(text = element_text(size=20))

# Excluding the eventless series
obj_resp %>% 
  group_by(AnalysisSeriesId) %>% 
  filter(n > 0) %>% 
  summarise(TotalResp = sum(Events), TotalN = sum(n)) %>% 
  mutate(RespRate = TotalResp / TotalN) %>% 
  filter(TotalResp > 0) %>% 
  mutate(Class = 'Responses seen') -> efficacious_series
efficacious_series %>% nrow  # 61 series

obj_resp %>% 
  group_by(AnalysisSeriesId) %>% 
  filter(n > 0) %>% 
  summarise(TotalResp = sum(Events), TotalN = sum(n)) %>% 
  mutate(RespRate = TotalResp / TotalN) %>% 
  filter(TotalResp == 0) %>% 
  mutate(Class = 'No responses seen') -> non_efficacious_series
non_efficacious_series %>% nrow  # 32 series

obj_resp %>% 
  filter(n > 0) %>% 
  left_join(studies, by = 'Study') %>% 
  left_join(manuscripts, by = 'Manuscript') %>% 
  left_join(bind_rows(efficacious_series, non_efficacious_series), 
            by = 'AnalysisSeriesId') %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
                                         'Radiotherapy', 
                                         'Monoclonal Antibody', 
                                         'Inhibitor')) %>% 
  filter(Class == 'Responses seen') %>% 
  add_fitted_draws(obj_resp_mod) %>%
  mutate(EstProbEvent = .value / n) %>% 
  summarise(EstProbEvent = mean(EstProbEvent)) %>% 
  ggplot(aes(x = DoseLevel, y = EstProbEvent, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Drug type', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'none') + 
  theme(text = element_text(size=20))

# Highest-n example ----
dlt %>% 
  group_by(Study, AnalysisSeriesId) %>% 
  summarise(NumDoses = n(), NumPats = sum(n)) %>% 
  arrange(-NumPats)
# Sessa2013_1

dlt_analysis <- analyse_dlt(
  dlt %>% filter(Study == 'Sessa2013_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = FALSE, legend2 = FALSE)
efficacy_analysis <- analyse_obj_resp(
  obj_resp %>% filter(Study == 'Sessa2013_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = FALSE, legend1 = FALSE, legend2 = TRUE,
  line_col = 'darkgreen', ribbon_fill = 'chartreuse')

dlt_analysis$p2 + labs(title = dlt_analysis$label, subtitle = 'Toxicity') + 
  efficacy_analysis$p2 + labs(title = '', subtitle = 'Response')




# High-n examples that saw responses and uses more than two doses ----
obj_resp %>% 
  group_by(Study, AnalysisSeriesId) %>% 
  summarise(NumDoses = n(), NumPats = sum(n), NumResp = sum(Events)) %>% 
  filter(NumResp > 0 & NumDoses > 2) %>% 
  arrange(-NumPats)

# Larocca2013_1 
dlt_analysis <- analyse_dlt(
  dlt %>% filter(Study == 'Larocca2013_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = TRUE, legend1 = FALSE, legend2 = FALSE)
efficacy_analysis <- analyse_obj_resp(
  obj_resp %>% filter(Study == 'Larocca2013_1') %>% distinct(AnalysisSeriesId) %>% .[[1]], 
  alpha = 0.05, add_title = FALSE, legend1 = FALSE, legend2 = TRUE,
  line_col = 'darkgreen', ribbon_fill = 'chartreuse')

dlt_analysis$p2 + labs(title = dlt_analysis$label, subtitle = 'Toxicity') + 
  efficacy_analysis$p2 + labs(title = '', subtitle = 'Response')


