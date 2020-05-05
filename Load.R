
library(readxl)
library(httr)
library(dplyr)
library(stringr)

# The file is available to download  from:
# http://edata.bham.ac.uk/337/

# Download from URL:
data_url <- 'http://edata.bham.ac.uk/337/2/Database_v1.1.xlsx'
GET(data_url, write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_xlsx(tf)

# And load datasets:
manuscripts <- read_excel(tf, sheet = 'Manuscripts')
studies <- read_excel(tf, sheet = 'Studies')
outcomes <- read_excel(tf, sheet = 'Outcomes')
binary_events <- read_excel(tf, sheet = 'BinaryOutcomeEvents')
binary_series <- read_excel(tf, sheet = 'BinaryOutcomeAnalysisSeries')


# OutcomeId of useful outcomes ----
# DLT:
outcomes %>% 
  filter(OutcomeText == 'Patients with DLT') %>% 
  select(OutcomeId) %>% .[[1]] -> dlt_outcome_id

# Objective response by RECIST:
outcomes %>% 
  filter(OutcomeText == 'Patients with Objective Response by RECIST') %>% 
  select(OutcomeId) %>% .[[1]] -> recist_or_outcome_id

# All objective response variants:
outcomes %>% 
  filter(str_match(OutcomeText, '[Oo]bjective [Rr]espo') %>% nchar > 0) %>% 
  select(OutcomeId) %>% .[[1]] -> obj_response_outcome_ids



# Create useful subsets of the data ----

# DLT is the most frequently reported outcome
binary_series %>% 
  left_join(binary_events, by = c('Study' = 'Study',
                                  'Dose' = 'Dose',
                                  'OutcomeId' = 'OutcomeId')) %>% 
  left_join(outcomes, by = 'OutcomeId') %>% 
  left_join(binary_series %>% 
              group_by(AnalysisSeriesId) %>% 
              summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
  mutate(DoseLevel = Order - MidDose) %>% 
  filter(OutcomeId == dlt_outcome_id) %>% 
  arrange(AnalysisSeriesId, Order) %>% 
  rename(DoseLevelN = Order) %>% 
  select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
  mutate(ProbEvent = Events / n) -> dlt

# RECIST response is the most common efficacy outcome
binary_series %>% 
  left_join(binary_events, by = c('Study' = 'Study',
                                  'Dose' = 'Dose',
                                  'OutcomeId' = 'OutcomeId')) %>% 
  left_join(outcomes, by = 'OutcomeId') %>% 
  left_join(binary_series %>% 
              group_by(AnalysisSeriesId) %>% 
              summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
  mutate(DoseLevel = Order - MidDose) %>% 
  filter(OutcomeId == recist_or_outcome_id) %>% 
  arrange(AnalysisSeriesId, Order) %>% 
  rename(DoseLevelN = Order) %>% 
  select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
  mutate(ProbEvent = Events / n) -> recist_obj_resp

# Objective response
binary_series %>% 
  left_join(binary_events, by = c('Study' = 'Study',
                                  'Dose' = 'Dose',
                                  'OutcomeId' = 'OutcomeId')) %>% 
  left_join(outcomes, by = 'OutcomeId') %>% 
  left_join(binary_series %>% 
              group_by(AnalysisSeriesId) %>% 
              summarise(MidDose = median(Order)), by = 'AnalysisSeriesId') %>% 
  mutate(DoseLevel = Order - MidDose) %>% 
  filter(OutcomeId %in% obj_response_outcome_ids) %>% 
  arrange(AnalysisSeriesId, Order) %>% 
  rename(DoseLevelN = Order) %>% 
  select(Study, AnalysisSeriesId, Dose, DoseLevelN, DoseLevel, n, Events) %>% 
  mutate(ProbEvent = Events / n) -> obj_resp
