
dlt_model_diagnostics %>% 
  filter(!ModelIsNull) %>% 
  group_by(Model) %>% 
  summarise(
    n = n(),
    n_with_div = sum(num_divergent > 0),
    n_exceeding_tree = sum(num_max_treedepth > 0),
    n_with_low_bfmi = sum(num_low_bfmi_chains > 0)
  )

dlt_model_diagnostics %>% 
  filter(Model == 'Bayes2P2') %>% 
  filter(num_divergent > 0) %>% 
  arrange(AnalysisSeriesId, Model)

# 70, 106, 175, 197, 229, 245, 433
sought_object_name = paste0('dlt_model_type2_brms_p2_', 175)
x <- .cache_retrieve(object_name = sought_object_name)
model <- x$object

sought_object_name = paste0('dlt_type2_brms_p2_fitted_', 175)
x <- .cache_retrieve(object_name = sought_object_name)
fitted <- x$object

sought_object_name = paste0('dlt_type2_brms_p2_samples_', 175)
x <- .cache_retrieve(object_name = sought_object_name)
samples <- x$object
samples %>% 
  


obj_resp_model_diagnostics %>% 
  filter(!ModelIsNull) %>% 
  group_by(Model) %>% 
  summarise(
    n = n(),
    n_with_div = sum(num_divergent > 0),
    n_exceeding_tree = sum(num_max_treedepth > 0),
    n_with_low_bfmi = sum(num_low_bfmi_chains > 0)
  )

obj_resp_model_diagnostics %>% 
  filter(Model == 'Bayes2P2') %>% 
  filter(num_divergent > 0) %>% 
  arrange(AnalysisSeriesId, Model)
# 66, 75, 108, 175, 197, 229, 245, 433
