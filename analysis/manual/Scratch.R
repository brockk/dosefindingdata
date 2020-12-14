
# Is ED50 ever greater than EMax?


dlt_model_params %>% dim
dlt_model_params %>% head

dlt_model_params %>% 
  filter(Model == 'DF-Sig') %>% 
  filter(term == 'ed50') %>% 
  select(AnalysisSeriesId, estimate) %>% 
  left_join(
    binary_series %>% 
      group_by(AnalysisSeriesId) %>% 
      summarise(NumDose = n()),
    by = 'AnalysisSeriesId'
  ) %>% mutate(ABC = estimate > NumDose) %>% 
  count(ABC)
  
dlt_model_params %>% 
  filter(Model == 'Bayes2P2') %>% 
  filter(term == 'b_ED50_Intercept') %>% 
  select(AnalysisSeriesId, estimate) %>% 
  left_join(
    binary_series %>% 
      group_by(AnalysisSeriesId) %>% 
      summarise(NumDose = n()),
    by = 'AnalysisSeriesId'
  ) %>% 
  summarise(
    n = n(),
    X1 = sum(estimate > NumDose),
    X2 = sum(estimate < 1)
  )

