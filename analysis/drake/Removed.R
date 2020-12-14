# To load objects

# file_loc = file_in('Database.xlsx'),
# 
# manuscripts = read_excel(file_loc, sheet = 'Manuscripts'),
# studies = read_excel(file_loc, sheet = 'Studies'),
# outcomes = read_excel(file_loc, sheet = 'Outcomes'),
# binary_events = read_excel(file_loc, sheet = 'BinaryOutcomeEvents'),
# binary_series = read_excel(file_loc, sheet = 'BinaryOutcomeAnalysisSeries'),
# 
# dlt_series = dlt %>% pull(AnalysisSeriesId) %>% unique() %>% head(2),
# 
# dlt_outcome_id = outcomes %>% 
#   filter(OutcomeText == 'Patients with DLT') %>% 
#   select(OutcomeId) %>% .[[1]],
# 
# # Objective response by RECIST:
# recist_or_outcome_id = outcomes %>% 
#   filter(OutcomeText == 'Patients with Objective Response by RECIST') %>% 
#   select(OutcomeId) %>% .[[1]],
# 
# # All objective response variants:
# obj_response_outcome_ids = outcomes %>% 
#   filter(str_match(OutcomeText, '[Oo]bjective [Rr]espo') %>% nchar > 0) %>% 
#   select(OutcomeId) %>% .[[1]],
# 
# # DLT outomes by series
# dlt = binary_series %>% 
#   left_join(binary_events, by = c('Study' = 'Study',
#                                   'Dose' = 'Dose',
#                                   'OutcomeId' = 'OutcomeId')) %>% 
#   left_join(outcomes, by = 'OutcomeId') %>% 
#   left_join(binary_series %>% 
#               group_by(AnalysisSeriesId) %>% 
#               summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
#   mutate(DoseLevel = Order - MidDose) %>% 
#   filter(OutcomeId == dlt_outcome_id) %>% 
#   arrange(AnalysisSeriesId, Order) %>% 
#   rename(DoseLevelN = Order) %>% 
#   select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
#   mutate(ProbEvent = Events / n),
# 
# # RECIST response outcomes by series
# recist_obj_resp = binary_series %>% 
#   left_join(binary_events, by = c('Study' = 'Study',
#                                   'Dose' = 'Dose',
#                                   'OutcomeId' = 'OutcomeId')) %>% 
#   left_join(outcomes, by = 'OutcomeId') %>% 
#   left_join(binary_series %>% 
#               group_by(AnalysisSeriesId) %>% 
#               summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
#   mutate(DoseLevel = Order - MidDose) %>% 
#   filter(OutcomeId == recist_or_outcome_id) %>% 
#   arrange(AnalysisSeriesId, Order) %>% 
#   rename(DoseLevelN = Order) %>% 
#   select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
#   mutate(ProbEvent = Events / n),
# 
# # Objective response outcomes by series
# obj_resp = binary_series %>% 
#   left_join(binary_events, by = c('Study' = 'Study',
#                                   'Dose' = 'Dose',
#                                   'OutcomeId' = 'OutcomeId')) %>% 
#   left_join(outcomes, by = 'OutcomeId') %>% 
#   left_join(binary_series %>% 
#               group_by(AnalysisSeriesId) %>% 
#               summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
#   mutate(DoseLevel = Order - MidDose) %>% 
#   filter(OutcomeId %in% obj_response_outcome_ids) %>% 
#   arrange(AnalysisSeriesId, Order) %>% 
#   rename(DoseLevelN = Order) %>% 
#   select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
#   mutate(ProbEvent = Events / n),