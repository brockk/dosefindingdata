

library(here)

# These scripts write onjects to the disl. Run them in parallel.
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'DltRunModels.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'ObjRespRunModels.R')
)

rstudioapi::jobRunScript(
  here('analysis', 'manual', 'DltExtractParameterEstimates.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'DltExtractFittedValues.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'DltExtractSampledSeries.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'DltExtractModelDiagnostics.R')
)

rstudioapi::jobRunScript(
  here('analysis', 'manual', 'ObjRespExtractParameterEstimates.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'ObjRespExtractFittedValues.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'ObjRespExtractSampledSeries.R')
)
rstudioapi::jobRunScript(
  here('analysis', 'manual', 'ObjRespExtractModelDiagnostics.R')
)

# There scripts load objects from disk to collate inferences.
source(here('analysis', 'manual', 'BindInferences.R'))
