bind_rows(
  dlt_mod %>% 
    spread_draws(r_DoseVaryingTreatmentType[TmtType,term], b_ContainsChemoTRUE) %>%
    filter(TmtType == 'Chemotherapy') %>% 
    filter(term == 'Intercept') %>% 
    mutate(ParamValue = r_DoseVaryingTreatmentType + b_ContainsChemoTRUE) %>% 
    select(-r_DoseVaryingTreatmentType, -b_ContainsChemoTRUE),
  dlt_mod %>% 
    spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
    filter(TmtType %in% c('Radiotherapy', 'Monoclonal.Antibody', 'Inhibitor')) %>% 
    filter(term == 'Intercept') %>% 
    rename(ParamValue = r_DoseVaryingTreatmentType)
) %>% mutate(TmtTypeO = factor(TmtType, levels = c('Monoclonal.Antibody',
                                                   'Radiotherapy',
                                                   'Inhibitor',
                                                   'Chemotherapy'), 
                               ordered = TRUE)) %>%
  filter(term == 'Intercept') %>% 
  ggplot(aes(y = TmtTypeO, group = TmtTypeO, fill = TmtTypeO, 
             x = ParamValue)) + 
  geom_density_ridges() + 
  labs(x = 'Adjustment to log(odds) of DLT compared to entire dataset', 
       y = 'Treatment type',
       title = 'Distributions of baseline rates of DLT') +
  theme(legend.position = 'none') + 
  xlim(-1.75, 2.5)


bind_rows(
  obj_resp_mod %>% 
    spread_draws(r_DoseVaryingTreatmentType[TmtType,term], b_ContainsChemoTRUE) %>%
    filter(TmtType == 'Chemotherapy') %>% 
    filter(term == 'Intercept') %>% 
    mutate(ParamValue = r_DoseVaryingTreatmentType + b_ContainsChemoTRUE) %>% 
    select(-r_DoseVaryingTreatmentType, -b_ContainsChemoTRUE),
  obj_resp_mod %>% 
    spread_draws(r_DoseVaryingTreatmentType[TmtType,term]) %>%
    filter(TmtType %in% c('Radiotherapy', 'Monoclonal.Antibody', 'Inhibitor')) %>% 
    filter(term == 'Intercept') %>% 
    rename(ParamValue = r_DoseVaryingTreatmentType)
) %>% mutate(TmtTypeO = factor(TmtType, levels = c('Monoclonal.Antibody',
                                                   'Radiotherapy',
                                                   'Inhibitor',
                                                   'Chemotherapy'), 
                               ordered = TRUE)) %>%
  filter(term == 'Intercept') %>% 
  ggplot(aes(y = TmtTypeO, group = TmtTypeO, fill = TmtTypeO, 
             x = ParamValue)) + 
  geom_density_ridges() + 
  labs(x = 'Adjustment to log(odds) of Objective Response compared to entire dataset', 
       y = 'Treatment type',
       title = 'Distributions of baseline rates of Objective Response') +
  theme(legend.position = 'none') + 
  xlim(-2.5, 6)