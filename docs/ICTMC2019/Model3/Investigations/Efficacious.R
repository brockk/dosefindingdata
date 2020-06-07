

obj_resp %>% count(AnalysisSeriesId) %>% nrow  # 93 series

# Tox in efficacious vs non-efficacious series
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

bind_rows(efficacious_series, non_efficacious_series) 





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
  theme(legend.position = 'none')

