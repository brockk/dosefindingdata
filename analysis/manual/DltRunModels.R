
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
  
  df <- get_expanded_dlt_data(series)
  
  model <- fit_sig_df_model(
    df, object_name = paste0('dlt_model_sig_df_', series)
  )
  
  model <- fit_type1_brms_model_p1(
    df, seed, object_name = paste0('dlt_model_type1_brms_p1_', series)
  )
  
  model <- fit_type1_brms_model_p2(
    df, seed, object_name = paste0('dlt_model_type1_brms_p2_', series)
  )
  
  model <- fit_type2_brms_model_p1(
    df, seed, object_name = paste0('dlt_model_type2_brms_p1_', series)
  )
  
  model <- fit_type2_brms_model_p2(
    df, seed, object_name = paste0('dlt_model_type2_brms_p2_', series)
  )
}
