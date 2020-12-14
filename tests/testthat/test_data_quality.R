
library(testthat)

# source('Load.R')

# manuscripts %>% head
# studies %>% head
# outcomes %>% head
# binary_events %>% head
# binary_series %>% head

# testthat::source_test_setup()

test_that('Primary key fields contain no missing data', {

  expect_equal(
    manuscripts %>%
      filter(Manuscript %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    studies %>%
      filter(Study %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    outcomes %>%
      filter(OutcomeId %>% is.na) %>%
      nrow,
    0)
})

test_that('Foreign key fields contain no missing data', {

  expect_equal(
    studies %>%
      filter(Manuscript %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_events %>%
      filter(Study %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_events %>%
      filter(OutcomeId %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_series %>%
      filter(Study %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_series %>%
      filter(Dose %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_series %>%
      filter(OutcomeId %>% is.na) %>%
      nrow,
    0)
})

test_that('Key data fields contain no missing data', {

  expect_equal(
    manuscripts %>%
      filter(Source %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    studies %>%
      filter(PatientGroup %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    studies %>%
      filter(DoseVaryingTreatmentType %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    outcomes %>%
      filter(OutcomeText %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_events %>%
      filter(Dose %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_events %>%
      filter(n %>% is.na) %>%
      nrow,
    0)

  expect_equal(
    binary_events %>%
      filter(Events %>% is.na) %>%
      nrow,
    0)
})

test_that('Manuscripts joins Studies correctly', {
  expect_equal(
    studies %>%
      anti_join(manuscripts, by = 'Manuscript') %>%
      nrow,
    0)
})

test_that('BinaryOutcomeEvents joins Studies correctly', {
  expect_equal(
    binary_events %>%
      anti_join(studies, by = 'Study') %>%
      nrow,
    0)
})

test_that('BinaryOutcomeEvents joins Outcomes correctly', {
  expect_equal(
    binary_events %>%
      anti_join(outcomes, by = 'OutcomeId') %>%
      nrow,
    0)
})

test_that('BinaryOutcomeAnalysisSeries joins Studies correctly', {
  expect_equal(
    binary_series %>%
      anti_join(studies, by = 'Study') %>%
      nrow,
    0)
})

test_that('BinaryOutcomeAnalysisSeries joins BinaryOutcomeEvents correctly', {
  expect_equal(
    binary_series %>%
      anti_join(binary_events, by = 'Dose') %>%
      nrow,
    0)
})

test_that('BinaryOutcomeAnalysisSeries joins Outcomes correctly', {
  expect_equal(
    binary_series %>%
      anti_join(outcomes, by = 'OutcomeId') %>%
      nrow,
    0)
})

test_that('No DOI is used twice', {
  expect_equal(
    manuscripts %>%
      count(DOI) %>%
      filter(n > 1) %>%
      nrow,
    0)
})

test_that('There are no orphaned doses in the analysis series', {
  expect_equal(
    binary_series %>%
      left_join(binary_events, by = c('Study' = 'Study',
                                      'Dose' = 'Dose',
                                      'OutcomeId' = 'OutcomeId')) %>%
      left_join(outcomes, by = 'OutcomeId') %>%
      filter(OutcomeText == 'Patients with DLT') %>%
      filter(Orphaned == TRUE) %>%
      nrow,
    0)
})

test_that('There are no per-patient binary outcomes where events exceeds n', {
  expect_equal(
    binary_events %>%
      left_join(outcomes, by = 'OutcomeId') %>%
      filter(PerPatientOutcome == 1) %>%
      filter(Events > n) %>%
      nrow,
    0)
})

test_that('Doses and dose-levels largely pair up within study', {
  # This rule is infrequently legitimately violated.
  # Legitimate violations arise when:
  # dlt and objective response are presented in non-congruent ways;
  # a dose appears in many series to avoid orphaned data.
  expect_lt(
    dlt %>%
      inner_join(obj_resp, by = c('Study' = 'Study',
                                  'Dose' = 'Dose'),
                 suffix = c('.dlt', '.obj_resp')) %>%
      filter(DoseLevel.dlt != DoseLevel.obj_resp) %>%
      nrow,
    6  # Currently, there are 5 legitimate violations
  )
})

test_that('The AnalysisSeriesIds for a study never span more than 30', {
  # This rule could be legitimtely broken. But violations of it tend to
  # indicate data-entry errors.
  expect_equal(
    binary_series %>%
      group_by(Study) %>%
      summarise(
        min_series_id = min(AnalysisSeriesId),
        max_series_id = max(AnalysisSeriesId),
      ) %>%
      filter(max_series_id - min_series_id > 30) %>%
      nrow,
    0
  )
})


test_that('No AnalysisSeriesId is mapped to more than one Study.', {
  
  expect_equal(
    dlt %>% 
      group_by(AnalysisSeriesId, Study) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      group_by(AnalysisSeriesId) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      filter(n > 1) %>% nrow,  
    0
  )
  
  expect_equal(
    obj_resp %>% 
      group_by(AnalysisSeriesId, Study) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      group_by(AnalysisSeriesId) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      filter(n > 1) %>% nrow,  
    0
  )
  
})
