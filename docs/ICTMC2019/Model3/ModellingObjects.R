

alpha <- 0.05

library(broom)
library(brms)
library(ggplot2)
library(dplyr)

get_dlt_series_label <- function(series_id) {
  dlt %>% 
    left_join(studies, by = 'Study') %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    select(Study, DoseVaryingTreatment, DoseVaryingTreatmentType, 
           PatientGroup) %>% 
    mutate(Label = paste0(DoseVaryingTreatment, ' (', 
                          DoseVaryingTreatmentType, ') in ', 
                          stringr::str_to_title(PatientGroup), ' [',
                          Study, ']'
    )) %>% 
    select(Label) %>% distinct() %>% .[[1]] -> label
  
  label
}

get_obj_resp_series_label <- function(series_id) {
  obj_resp %>% 
    left_join(studies, by = 'Study') %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    select(Study, DoseVaryingTreatment, DoseVaryingTreatmentType, 
           PatientGroup) %>% 
    mutate(Label = paste0(DoseVaryingTreatment, ' (', 
                          DoseVaryingTreatmentType, ') in ', 
                          stringr::str_to_title(PatientGroup), ' [',
                          Study, ']'
    )) %>% 
    select(Label) %>% distinct() %>% .[[1]] -> label
  
  label
}

analyse_dlt <- function(series_id, alpha = 0.05, add_title = TRUE,
                        legend1 = FALSE, legend2 = TRUE,
                        line_col = 'darkorange', ribbon_fill = 'orange') {
  
  z = qnorm(1 - alpha / 2)
  
  label <- get_dlt_series_label(series_id)
  
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
    geom_line(aes(y = .fitted), col = line_col, size = 1.3) + 
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2, 
                fill = ribbon_fill) +
    ylim(0, 1) -> p1
  if(!legend1) p1 <- p1 + theme(legend.position = 'none')
  if(add_title) {
    p1 <- p1 + labs(title = label, 
                    subtitle = 'Non-pooled maximum likelihood model', 
                    y = 'Prob(DLT)')
  } else {
    p1 <- p1 + labs(subtitle = 'Non-pooled maximum likelihood model', 
                    y = 'Prob(DLT)')
  }
  
  # Plot of Prob(DLT) from partially-pooled model
  dlt_fitted_draws %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    ungroup() %>% 
    ggplot(aes(x = DoseLevel)) + 
    geom_line(aes(y = EstProbEvent, group = .draw), alpha = 0.01, 
              col = line_col) +
    geom_point(aes(y = ProbEvent, size = n), 
               data = dlt %>% filter(AnalysisSeriesId == series_id)) +
    labs(title = '', subtitle = 'Partially-pooled Bayesian model', y = '') +
    ylim(0, 1) -> p2
  if(!legend2) p2 <- p2 + theme(legend.position = 'none')
  
  list(ci_inferences = ci_inferences, p1 = p1, p2 = p2, label = label)
}


analyse_obj_resp <- function(series_id, alpha = 0.05, add_title = TRUE,
                             legend1 = FALSE, legend2 = TRUE,
                             line_col = 'darkorange', ribbon_fill = 'orange') {
  
  z = qnorm(1 - alpha / 2)
  
  label <- get_obj_resp_series_label(series_id)
  
  # CI widths
  bind_rows(
    # CI widths from frequentist, non-pooled model
    obj_resp %>% filter(AnalysisSeriesId == series_id) %>% 
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
    obj_resp_fitted_draws %>% 
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
  
  # Plot of Prob(ObjResp) from non-pooled model
  obj_resp %>% 
    filter(AnalysisSeriesId == series_id) %>% 
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
    geom_line(aes(y = .fitted), col = line_col, size = 1.3) + 
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2, 
                fill = ribbon_fill) +
    ylim(0, 1) -> p1
  if(!legend1) p1 <- p1 + theme(legend.position = 'none')
  if(add_title) {
    p1 <- p1 + labs(title = label, 
                    subtitle = 'Non-pooled maximum likelihood model', 
                    y = 'Prob(ObjResp)')
  } else {
    p1 <- p1 + labs(subtitle = 'Non-pooled maximum likelihood model', 
                    y = 'Prob(ObjResp)')
  }
  
  # Plot of Prob(DLT) from partially-pooled model
  obj_resp_fitted_draws %>% 
    filter(AnalysisSeriesId == series_id) %>% 
    ungroup() %>% 
    ggplot(aes(x = DoseLevel)) + 
    geom_line(aes(y = EstProbEvent, group = .draw), alpha = 0.01, 
              col = line_col) +
    geom_point(aes(y = ProbEvent, size = n), 
               data = obj_resp %>% filter(AnalysisSeriesId == series_id)) +
    labs(title = '', subtitle = 'Partially-pooled Bayesian model', y = '') +
    ylim(0, 1) -> p2
  if(!legend2) p2 <- p2 + theme(legend.position = 'none')
  
  list(ci_inferences = ci_inferences, p1 = p1, p2 = p2, label = label)
}

