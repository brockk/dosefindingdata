# Plot of Prob(DLT) from non-pooled model
dlt %>% filter(AnalysisSeriesId == series_id) %>% 
  glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
      family = binomial('logit')) %>% 
  augment(type.predict = "link") %>% 
  rename(.fitted.l = .fitted, n = X.weights.) %>% 
  mutate(
    .lower.l = .fitted.l - z * .se.fit, 
    .upper.l = .fitted.l + z * .se.fit,
    .ci.width.l = .upper.l - .lower.l,
    .fitted = inv_logit_scaled(.fitted.l),
    .lower = inv_logit_scaled(.lower.l),
    .upper = inv_logit_scaled(.upper.l),
    .ci_width = .upper - .lower
  ) %>% 
  ggplot(aes(x = DoseLevel)) +
  geom_point(aes(y = ProbEvent, size = n)) + 
  geom_line(aes(y = .fitted), col = 'darkorange', size = 1.3) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2, fill = 'orange') +
  theme(legend.position = 'none') +
  labs(title = 'Non-pooled maximum likelihood model', y = 'Prob(DLT)') + 
  ylim(0, 1) -> p1

# CI widths
bind_rows(
  # CI widths from frequentist, non-pooled model
  dlt %>% filter(AnalysisSeriesId == series_id) %>% 
    glm(ProbEvent ~ DoseLevel, data = ., weights = n, 
        family = binomial('logit')) %>% 
    augment(type.predict = "link") %>% 
    rename(.fitted.l = .fitted) %>% 
    mutate(
      .lower.l = .fitted.l - z * .se.fit, 
      .upper.l = .fitted.l + z * .se.fit,
      .ci.width.l = .upper.l - .lower.l,
      .fitted = inv_logit_scaled(.fitted.l),
      .lower = inv_logit_scaled(.lower.l),
      .upper = inv_logit_scaled(.upper.l),
      .ci.width = .upper - .lower
    ) %>% 
    select(DoseLevel, .lower.l, .upper.l, .ci.width.l, .lower, .upper, 
           .ci.width) %>% 
    mutate(Method = 'NonPooled'),
  # CI widths from Bayesian, partially-pooled model
  dlt_fitted_draws %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    ungroup() %>% 
    group_by(DoseLevel) %>% 
    summarise(
      .lower.l = quantile(logit_scaled(EstProbEvent), probs = alpha / 2),
      .upper.l = quantile(logit_scaled(EstProbEvent), probs = 1 - alpha / 2),
      .ci.width.l = .upper.l - .lower.l,
      .lower = quantile(EstProbEvent, probs = alpha / 2),
      .upper = quantile(EstProbEvent, probs = 1 - alpha / 2),
      .ci.width = .upper - .lower
    ) %>% 
    mutate(Method = 'PartiallyPooled')
) -> ci_inferences

# Plot of Prob(DLT) from partially-pooled model
dlt_fitted_draws %>% 
  filter(AnalysisSeriesId == series_id) %>% 
  ungroup() %>% 
  ggplot(aes(x = DoseLevel)) + 
  geom_line(aes(y = EstProbEvent, group = .draw), alpha = 0.01, 
            col = 'darkorange') +
  geom_point(aes(y = ProbEvent, size = n), 
             data = dlt %>% filter(AnalysisSeriesId == series_id)) +
  # geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.1, fill = 'orange',
  #             data = ci_inferences %>% filter(Method == 'PartiallyPooled')) +
  labs(title = 'Partially-pooled Bayesian model', y = '') +
  ylim(0, 1) -> p2

p1 + p2



# Removed
# filter(DoseVaryingTreatmentType %in% c('Chemotherapy', 
#                                        'Radiotherapy', 
#                                        'Monoclonal Antibody', 
#                                        'Inhibitor')) %>% 