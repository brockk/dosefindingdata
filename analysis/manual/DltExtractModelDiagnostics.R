

# Load ----
library(here)
source(here('Load.R'))
source(here('RunTests.R'))

library(purrr)
library(tidyr)
library(DoseFinding)
library(brms)

source(here('analysis', 'manual', 'Objects.R'))

rstan_diagnostics_to_tibble <- function(stanfit) {
  tibble(
    'num_divergent' = rstan::get_num_divergent(stanfit),
    'num_max_treedepth' = rstan::get_num_max_treedepth(stanfit),
    'num_low_bfmi_chains' = length(rstan::get_low_bfmi_chains(stanfit))
  )
}

for(series in dlt_series) {
  
  sought_object_name = paste0('dlt_model_type1_brms_p1_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    if(!is.null(model)) {
      object <- rstan_diagnostics_to_tibble(model$fit) %>% 
        mutate(AnalysisSeriesId = series, Model = 'Bayes1P1',
               ModelIsNull = FALSE)
    } else {
      object <- tibble(AnalysisSeriesId = series, Model = 'Bayes1P1', 
                       ModelIsNull = TRUE)
    }
    object_name <- paste0('dlt_type1_brms_p1_diagnostics_', series)
    .cache_save(object, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type1_brms_p2_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    if(!is.null(model)) {
      object <- rstan_diagnostics_to_tibble(model$fit) %>% 
        mutate(AnalysisSeriesId = series, Model = 'Bayes1P2',
               ModelIsNull = FALSE)
    } else {
      object <- tibble(AnalysisSeriesId = series, Model = 'Bayes1P2', 
                       ModelIsNull = TRUE)
    }
    object_name <- paste0('dlt_type1_brms_p2_diagnostics_', series)
    .cache_save(object, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type2_brms_p1_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    if(!is.null(model)) {
      object <- rstan_diagnostics_to_tibble(model$fit) %>% 
        mutate(AnalysisSeriesId = series, Model = 'Bayes2P1',
               ModelIsNull = FALSE)
    } else {
      object <- tibble(AnalysisSeriesId = series, Model = 'Bayes2P1', 
                       ModelIsNull = TRUE)
    }
    object_name <- paste0('dlt_type2_brms_p1_diagnostics_', series)
    .cache_save(object, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
  sought_object_name = paste0('dlt_model_type2_brms_p2_', series)
  x <- .cache_retrieve(object_name = sought_object_name)
  if(x$present) {
    model <- x$object
    if(!is.null(model)) {
      object <- rstan_diagnostics_to_tibble(model$fit) %>% 
        mutate(AnalysisSeriesId = series, Model = 'Bayes2P2',
               ModelIsNull = FALSE)
    } else {
      object <- tibble(AnalysisSeriesId = series, Model = 'Bayes2P2', 
                       ModelIsNull = TRUE)
    }
    object_name <- paste0('dlt_type2_brms_p2_diagnostics_', series)
    .cache_save(object, object_name)
  } else {
    warning(paste0('Object ', sought_object_name, ' not present.'))
  }
  
}
