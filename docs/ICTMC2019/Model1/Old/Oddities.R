

# Oddity 1 ----
series_id <- 192
# 'Jones2012_1'

# Raw data
dlt %>% filter(AnalysisSeriesId == series_id)

# GLM model
dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit'))
# Model fails!

dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  filter(DoseLevel >= 0) %>% 
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit'))
# Model succeeds

# *Removing* data makes the model fit.