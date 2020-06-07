

# DLT modelled series ----
# By DoseVaryingTreatmentType
fitted_emax_dlt_series %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
                                         'Radiotherapy', 
                                         'Monoclonal Antibody', 
                                         'Inhibitor')) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Drug type', y = 'Prob(DLT)') + 
  theme(legend.position = 'none') +
  theme(text = element_text(size = 20))

# By FixedDoseChemo
fitted_emax_dlt_series %>% 
  filter(FixedDoseChemo %in% 0:1) %>% 
  mutate(FixedDoseChemo = as.factor(FixedDoseChemo)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = FixedDoseChemo)) + 
  geom_line(size = 1) + 
  facet_wrap(~ FixedDoseChemo, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Chemotherapy backbone', 
       y = 'Prob(DLT)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))

# By HaemNonhaem
fitted_emax_dlt_series %>% 
  filter(HaemNonhaem %in% c('Haematological', 'NonHaematological')) %>% 
  mutate(HaemNonhaem = as.factor(HaemNonhaem)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = HaemNonhaem)) + 
  geom_line(size = 1) + 
  facet_wrap(~ HaemNonhaem, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Disease type', 
       y = 'Prob(DLT)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))

# By Source
fitted_emax_dlt_series %>% 
  mutate(Source = as.factor(Source)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = Source)) + 
  geom_line(size = 1) + 
  facet_wrap(~ Source, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-DLT curves', col = 'Manuscript type', 
       y = 'Prob(DLT)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))


# Obj-Resp modelled series ----
# By DoseVaryingTreatmentType
fitted_emax_obj_resp_series %>% 
  filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
                                         'Radiotherapy', 
                                         'Monoclonal Antibody', 
                                         'Inhibitor')) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = DoseVaryingTreatmentType)) + 
  geom_line(size = 1) + 
  facet_wrap(~ DoseVaryingTreatmentType, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Drug type', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'none') +
  theme(text = element_text(size = 20))

# By FixedDoseChemo
fitted_emax_obj_resp_series %>% 
  filter(FixedDoseChemo %in% 0:1) %>% 
  mutate(FixedDoseChemo = as.factor(FixedDoseChemo)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = FixedDoseChemo)) + 
  geom_line(size = 1) + 
  facet_wrap(~ FixedDoseChemo, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Chemotherapy backbone', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))

# By HaemNonhaem
fitted_emax_obj_resp_series %>% 
  filter(HaemNonhaem %in% c('Haematological', 'NonHaematological')) %>% 
  mutate(HaemNonhaem = as.factor(HaemNonhaem)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = HaemNonhaem)) + 
  geom_line(size = 1) + 
  facet_wrap(~ HaemNonhaem, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Disease type', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))

# By Source
fitted_emax_obj_resp_series %>% 
  mutate(Source = as.factor(Source)) %>% 
  ggplot(aes(x = DoseLevel, y = prob_event, group = AnalysisSeriesId, 
             col = Source)) + 
  geom_line(size = 1) + 
  facet_wrap(~ Source, ncol = 2) + 
  xlim(-5, 5) +
  labs(title = 'Fitted dose-response curves', col = 'Manuscript type', 
       y = 'Prob(ObjectiveResponse)') + 
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 20))
