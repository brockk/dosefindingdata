

# This script in the root of the project loads the datasets
source('Load.R')

dlt_fitted_draws <- readRDS('dlt_fitted_draws.rds')
dlt_fitted_draws%>% dim

# This script loads objects specific to this modelling task.
# It relies on dlt_fitted_draws being loaded.
source('ModellingObjects.R')




library(glue)
out_path <- 'modelling-2019-08-22/Output'
series_ids <- c(428, 108, 240, 317, 125, 192, 201)
for(series_id in series_ids) {
  print(Sys.time())
  dlt_analysis <- analyse_dlt(series_id, alpha = alpha)
  saveRDS(dlt_analysis, glue('{out_path}/DLT-{series_id}.rds'))
}

