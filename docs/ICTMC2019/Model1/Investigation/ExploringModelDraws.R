dlt %>% 
  left_join(studies, by = 'Study') %>% 
  add_fitted_draws(dlt_mod) %>% 
  ungroup -> ABC 

ABC %>%  
  mutate(EstProbEvent = .value / n) %>% 
  summarise(max(EstProbEvent))

ABC %>% 
  group_by(AnalysisSeriesId, DoseLevel, .value, n) %>% 
  summarise(N = n()) %>% head

dlt_mod

ABC %>% head

ABC %>% 
  filter(AnalysisSeriesId == 1 & DoseLevel == -3) %>% 
  select(Study, .draw, Dose, n, .value) %>% tail

ABC %>% 
  group_by(AnalysisSeriesId, DoseLevel) %>% 
  summarise(N = n(), 
            v_l = min(.value), v_u = max(.value), 
            n_l = min(n), n_u = max(n),
            prob = mean(.value / n)
            ) -> ABC_expanded

ABC_expanded %>% filter(n_l != n_u)  # Empty because n_l = n_u everywhere.

ABC %>% count(AnalysisSeriesId)
ABC_expanded %>% 
  filter(AnalysisSeriesId == 433)

ABC_expanded %>% arrange(-v_u)
ABC_expanded %>% 
  ggplot(aes(x = v_u, y = n_u)) + 
  geom_point() + geom_abline()
# Never does v_u > n