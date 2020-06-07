
getwd()
source('Load.R')

library(dplyr)

dlt_expected_rates_1 <- readRDS(
  'docs/ICTMC2019/Model1/dlt_expected_rates.rds')
obj_resp_expected_rates_1 <- readRDS(
  'docs/ICTMC2019/Model1/obj_resp_expected_rates.rds')

dlt_expected_rates_3 <- readRDS(
  'docs/ICTMC2019/Model3/dlt_expected_rates.rds')
obj_resp_expected_rates_N <- readRDS(
  'docs/ICTMC2019/Model3/obj_resp_expected_rates.rds')

# Compare
dlt_expected_rates_1 %>% 
  left_join(dlt_expected_rates_3, 
            by = c('AnalysisSeriesId' = 'AnalysisSeriesId', 
                   'DoseLevel' = 'DoseLevel')) %>% 
  left_join(binary_series %>% distinct(AnalysisSeriesId, Study), 
            by = 'AnalysisSeriesId') %>% 
  mutate(Diff = EstProbEvent.x - EstProbEvent.y,
         AbsDiff = abs(Diff)) %>% 
  arrange(-AbsDiff)
# Having reconciled these with the charts, they are all such modest changes
# that they are barely preceptible. The Jakub series is more sensible under 
# model N.
