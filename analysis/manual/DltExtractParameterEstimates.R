
# Load ----
library(here)
source(here('Load.R'))
source(here('RunTests.R'))

library(purrr)
library(tidyr)
library(DoseFinding)
library(brms)

source(here('analysis', 'manual', 'Objects.R'))

for(series in dlt_series) {
  
  sought_object_name = paste0('dlt_model_sig_df_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    params <- broom::tidy(model) %>% 
      mutate(AnalysisSeriesId = series, Model = 'DF-Sig')
    object_name <- paste0('dlt_sig_df_params_', series)
    .cache_save(params, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type1_brms_p1_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    params <- broom::tidy(model) %>% 
      mutate(AnalysisSeriesId = series, Model = 'Bayes1P1')
    object_name <- paste0('dlt_type1_brms_p1_params_', series)
    .cache_save(params, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type1_brms_p2_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    params <- broom::tidy(model) %>% 
      mutate(AnalysisSeriesId = series, Model = 'Bayes1P2')
    object_name <- paste0('dlt_type1_brms_p2_params_', series)
    .cache_save(params, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type2_brms_p1_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    params <- broom::tidy(model) %>% 
      mutate(AnalysisSeriesId = series, Model = 'Bayes2P1')
    object_name <- paste0('dlt_type2_brms_p1_params_', series)
    .cache_save(params, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type2_brms_p2_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    params <- broom::tidy(model) %>% 
      mutate(AnalysisSeriesId = series, Model = 'Bayes2P2')
    object_name <- paste0('dlt_type2_brms_p2_params_', series)
    .cache_save(params, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
}

