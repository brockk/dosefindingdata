
alpha <- 0.05
z = qnorm(1 - alpha / 2)

library(broom)

dlt %>% distinct(AnalysisSeriesId) %>% .[[1]] %>% head(11) %>% tail(1)

# On 2019-09-10:
series_id <- 70  # Doi2014_1

dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  select(Study, ProbEvent, DoseLevel, n)

dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  select(ProbEvent, DoseLevel, n) %>%
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit')) -> glm_mod1a

# This model fits OK
glm_mod1a

