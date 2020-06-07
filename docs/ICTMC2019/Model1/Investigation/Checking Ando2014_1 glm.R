
alpha <- 0.05
z = qnorm(1 - alpha / 2)

library(broom)

dlt %>% distinct(AnalysisSeriesId) %>% .[[1]]

# On 2019-09-10:
series_id <- 21  # Ando2014_1

dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  select(ProbEvent, DoseLevel, n)

dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  select(ProbEvent, DoseLevel, n) %>%
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit')) -> glm_mod1a

# This model warns because it numerically fits probabilities of 0 or 1
glm_mod1a


# Remove the zero-event dose-levels at the start:
dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  filter(Events > 0) %>% 
  select(ProbEvent, DoseLevel, n) %>%
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit')) -> glm_mod1b

glm_mod1b
# This is what Stata reports

