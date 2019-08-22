Outcomes in Dose-Finding Trials
================
Kristian Brock
14 May, 2019

  - [Introduction](#introduction)
  - [Data](#data)
      - [Availability](#availability)
      - [Citing the work](#citing-the-work)
      - [File format](#file-format)
  - [References](#references)

# Introduction

Dose-finding clinical trials vary the dose of an experimental treatment
in search of a dose that is sufficiently tolerable and active. The
overwhelming majority of these trials adopt an experimental design that
assumes the probabilities of efficacy and toxicity increase
monotonically in dose (Rogatko et al. 2007; Chiuzan et al. 2017). To
test the empirical appropriateness of this assumption, we collected
efficacy and toxicity outcomes by doses from a large number of published
dose-finding clinical trials.

# Data

The outcomes are collected in the file `Database.xlsx`. We describe
briefly here the volume of data contained.

``` r
library(readxl)
library(dplyr)

# The file will eventually move to:
# http://edata.bham.ac.uk/337/1/Database.xlsx
file_path = '../Database.xlsx'

manuscripts <- read_excel(file_path, sheet = 'Manuscripts')
studies <- read_excel(file_path, sheet = 'Studies')
outcomes <- read_excel(file_path, sheet = 'Outcomes')
binary_events <- read_excel(file_path, sheet = 'BinaryOutcomeEvents')
# binary_series <- read_excel(file_path, sheet = 'BinaryOutcomeAnalysisSeries')
```

``` r
manuscripts %>% nrow
```

    ## [1] 122

``` r
studies %>% nrow
```

    ## [1] 139

Data have been extracted from 139 studies in 122 manuscripts.

``` r
outcomes %>% nrow
```

    ## [1] 96

The database contains data on 96 different outcome measures. However,
some of these are very sparsely reported.

``` r
binary_events %>% 
  left_join(studies, by = 'Study') %>% 
  left_join(outcomes, by = 'OutcomeId') %>% 
  group_by(Outcome = OutcomeText) %>% 
  summarise(
    NumObs = n(), 
    NumStudies = length(unique(Study))
    ) %>% 
  mutate(ObsPerStudy = NumObs / NumStudies) %>% 
  arrange(-NumObs) %>% 
  head %>% 
  knitr::kable(digits = 1)
```

| Outcome                                    | NumObs | NumStudies | ObsPerStudy |
| :----------------------------------------- | -----: | ---------: | ----------: |
| Patients with DLT                          |    648 |        131 |         4.9 |
| Patients with Objective Response by RECIST |    273 |         54 |         5.1 |
| Patients with Disease Control by RECIST    |    223 |         44 |         5.1 |
| Patients with Best response SD by RECIST   |    222 |         44 |         5.0 |
| Patients with Best response PD by RECIST   |    170 |         36 |         4.7 |
| Patients with Best response PR by RECIST   |    170 |         34 |         5.0 |

Overwhelmingly the most commonly reported outcome measure by dose is
incidence of DLT. 131 studies report this outcome, with an average of
4.9 datapoints per series. Efficacy outcomes are reported far less
frequently but objective response as measured by RECIST (Eisenhauer et
al. 2009) is reported in over 50 studies.

## Availability

The dataset is available at:

<http://edata.bham.ac.uk/337/1/Database.xlsx>

## Citing the work

To cite the dataset, please use:

> Kristian Brock, Victoria Homer, Gurjinder Soul, Claire Potter, Cody
> Chiuzan and Shing Lee “Dose-level toxicity and efficacy outcomes from
> dose-finding clinical trials in oncology”. 2019. doi:
> 10.25500/edata.bham.00000337

or use the BibTex entry

    @misc{BrockDoseFindingData,
        author = {Kristian Brock and Victoria Homer and Gurjinder Soul and Claire Potter and Cody Chiuzan and Shing Lee},
        year = {2019},
        title = {Dose-level toxicity and efficacy outcomes from dose-finding clinical trials in oncology},
        doi = {10.25500/edata.bham.00000337},
        howpublished= {\url{https://doi.org/10.25500/edata.bham.00000337}},
        timestamp = {2019.05.05}
    }

## File format

The `database.xlsx` file contains many tabs. The following sections
describe in depth the format of the file.

### Manuscripts

This tab details the manuscripts studied in this research.

Columns:

  - `Manuscript`, `string`: Primary key for the manuscript in this
    project.
  - `Year`, `int`: Year manuscript was published.
  - `DOI`, `string`: Manuscript DOI.
  - `Source`, `string`: How the manuscript came to be in the database.
    Options are:
      - `ChiuzanModelBased`, listed in Chiuzan et al. (2017) as a trial
        using a model-based dose-finding design.
      - `ChiuzanRuleBased1`, randomly selected from the unpublished list
        of trials using rule-based dose-finding designs assembled by
        Chiuzan et al. (2017) during their review.
  - `SupplementAppendix`, `bool`: `TRUE` if manuscript has a supplement
    or appendix.
  - `DataExtraction1`, `string`: Person who extracted the data.
  - `DataExtraction2`, `string`: Person who extracted the data a second
    time or checked the first extraction.
  - `AddToDB`, `bool`: TRUE if data has been added to the database.
  - `Note`, `string`: Items noted during data extraction.

### Studies

In this database, a `Study` is an abstract concept encapsulating:

  - a set of doses of some treatment or combination of treatments;
  - given to patients;
  - yielding outcome data.

In a simple scenario, one manuscript would contain one Study. However,
there can be multiple Studies in a manuscript. For example, if more than
treatment or treatment combination is the subject of dose investigation
in a single manuscript, they are seperate studies.

Columns:

  - `Manuscript`, `string`: Foreign key to Manuscripts, reflecting the
    manuscript that reported the data.
  - `Study`, `string`: Primary key for the study in this project.
  - `PatientGroup`, `string`: brief description of the patient group.
  - `PatientGroupDetailed`, `string`: more detailed description of the
    patient group.
  - `HaemNonhaem`, `string`: Options are:
      - `Haematological`, if the disease under study was haematological,
        like leukaemia or lymphoma.
      - `NonHaematological`, if the disease under study was solid tumour
        and therefore non-haematological, like lung cancer.
      - `Mixed`, if both disease types were studied.
      - `Unknown`, where not specified.
  - `Treatment`, `string`: brief description of all treatments given,
    whether dose-varying or not.
  - `ContainsChemo`, `bool`: `TRUE` if the treatment contains a
    chemotherapy element. This is provided to address the reasonable
    expectation that the presence of chemotherapy increases the
    expectation of toxicity. A full decomposition of treatment types is
    not provided here (e.g. there is no ContainsInhibitor field) but
    standardised types of the dose-varying treatment(s) are given in the
    column DoseVaryingTreatmentType. A special case is made here for
    chemo to allow an analysis to reflect the reasonably-expected
    population-level effect that chemotherapy is associated with
    toxicity.
  - `DoseVaryingTreatment`, `string`: the treatment(s) that have their
    dose varied. In the case of many treatments, items are separated by
    the + symbol. Any treatment identified in Treatment but not in
    DoseVaryingTreatment can be assumed to be constant across the doses
    under investigation.
  - `DoseVaryingTreatmentType`, `string`: type of the treatment(s)
    undergoing dose variation. Options are:
      - `Cell therapy`
      - `Chemoprevention`
      - `Chemotherapy`
      - `Cytokine`
      - `GeneTherapy`
      - `Immunomodulatory drug`
      - `Inhibitor`
      - `Monoclonal Antibody`
      - `Not disclosed`
      - `Oncolytic virus`
      - `Radiopharmaceutical`
      - `Radiotherapy` or combinations thereof.
  - `DoseVaryingTreatmentTypeDetail`, `string`: more detailed and
    precise description of the type of dose-varying treatment, provided
    where available.
  - `MultiVarying`, `bool`: `TRUE` if the dose of several treatments was
    varied.
  - `MonotonicDoses`, `bool`: `TRUE` if the doses investigated are
    monotonically increasing.
  - `DoseUnits`, `string`: the units of the doses.
  - `MTDorRP2D`, `string`: the dose recommended as the MTD or RP2D.
  - `ToxByDose`, `bool`: `TRUE` if toxicity outcomes were reported by
    dose.
  - `EffByDose`, `bool`: `TRUE` if efficacy outcomes were reported by
    dose.
  - `AdverseEventLevelCounts`, `bool`: `TRUE` if adverse events were
    tabulated by dose for specific events (e.g. Anaemia) in contrast to
    the broad level (e.g. Grade 3/4 AE).

### Outcomes

This tab details the outcome measures collected from the manuscripts.

Columns:

  - `OutcomeId`, `int`: Primary key for the outcome measure in this
    project.
  - `OutcomeText`, `string`: Description of the outcome measure.
  - `OutcomeClass`, `string`: Class of the outcome measure. Options are:
      - `Safety`
      - `Efficacy`
  - `Include`, `bool`: This may be deprecated.
  - `HighIsGood`, `bool`: `TRUE` if a high value is a good thing for
    patients. FALSE otherwise. E.g. high response rate is good and low
    toxicity rate is good.
  - `PerPatientOutcome`, `bool`: `TRUE` if the outcome measure is binary
    at the patient level, e.g. “Any AE” is binary at the patient-level
    because a patient either experiences any AE (in which case it is
    `TRUE`) or they do not (in which case it is `FALSE`). In contrast,
    “Total AEs” is not binary because a patient may experience many
    AEs.
  - `Note`, `string`: Items noted during data extraction.

### BinaryOutcomeEvents

This tab contains the data extracted from manuscripts on binary outcome
measures.

Columns:

  - `Study`, `string`: Foreign key to Studies tab.
  - `Dose`, `string`: Description of dose as reported. In the main, this
    is just a number, and simple to interpret. In more complicated
    scenarios, it could contain information reflecting: the frequency
    that treatments were given; several doses reported together like
    “10mg - 25mg” (an irritating practice - please do not do this); or
    doses for several treatments. The bewildering variety in this field
    is what encouraged us to think about *dose-levels* (rather than
    actual doses) in monontonically increasing series.
  - `OutcomeId`, `int`: Foreign key to Outcomes tab.
  - `n`, `int`: Number of patients. The denominator in a binary event
    rate.
  - `Events`, `int`: number of events.
  - `Orphaned`, `bool`: `TRUE` if the dose-level is orphaned and
    therefore has no unambiguous comparator.

### BinaryOutcomeAnalyisSeries

Doses investigated and reported in clinial trials are not always
monotonically increasing, despite the fact that they should be under the
most commonly-used experimental designs like 3+3 and CRM. Blithely
analysing all of the doses as they are reported in publications would
sometimes create scenarios where it is impossible to definitively say
whether a dose is greater or less than some other (e.g. “5mg per day” vs
“10mg every second day”; or “10mg of A + 5mg of B” vs “5mg of A + 10mg
of B”).

An analysis-series is a set of doses from a particular study that are
strictly monotonically increasing, evaluated with respect to a
particular outcome measure. There are many ways to create such subsets.
The analysis series presented here are merely those preferred by the
author. They favour series with as many doses as possible (whilst still
retaining unambiguous monotonic order) and as little repetition as
possible. A small amount of repetition has been tolerated where
necessary to avoid an “orphaned” dose-level, i.e. a dose-level with no
comparator.

You are free to create our own analysis-series if you prefer.

Columns:

  - `NewSeries`, `bit`: This field exists to automate the generation of
    `AnalysisSeriesId` and `Order`.
  - `AnalysisSeriesId`, `int`: Primary key for the analysis series.
    Automatically generated by simple logic in Excel.
  - `Order`, `int`: The order of the dose in this analysis-series.
    Automatically generated by simple logic in Excel.
  - `Study`, `string`: Foreign key to Studies tab.
  - `Dose`, `string`: Description of dose as reported. In the main, this
    is just a number, and simple to interpret. In more complicated
    scenarios, it could contain information reflecting: the frequency
    that treatments were given; several doses reported together like
    “10mg - 25mg” (an irritating practice - please do not do this); or
    doses for several treatments. The bewildering variety in this field
    is what encouraged us to think about *dose-levels* (rather than
    actual doses) in monontonically increasing series.
  - `OutcomeId`, `int`: Foreign key to Outcomes tab.

# References

<div id="refs" class="references">

<div id="ref-Chiuzan2017">

Chiuzan, Cody, Jonathan Shtaynberger, Gulam A. Manji, Jimmy K. Duong,
Gary K. Schwartz, Anastasia Ivanova, and Shing M. Lee. 2017.
“Dose-Finding Designs for Trials of Molecularly Targeted Agents and
Immunotherapies.” *Journal of Biopharmaceutical Statistics* 27 (3):
477–94. <https://doi.org/10.1080/10543406.2017.1289952>.

</div>

<div id="ref-Eisenhauer2009">

Eisenhauer, E. a., P. Therasse, J. Bogaerts, L. H. Schwartz, D. Sargent,
R. Ford, J. Dancey, et al. 2009. “New Response Evaluation Criteria in
Solid Tumours: Revised RECIST Guideline (Version 1.1).” *European
Journal of Cancer* 45 (2): 228–47.
<https://doi.org/10.1016/j.ejca.2008.10.026>.

</div>

<div id="ref-Rogatko2007">

Rogatko, André, David Schoeneck, William Jonas, Mourad Tighiouart, Fadlo
R. Khuri, and Alan Porter. 2007. “Translation of Innovative Designs into
Phase I Trials.” *Journal of Clinical Oncology* 25 (31): 4982–6.
<https://doi.org/10.1200/JCO.2007.12.1012>.

</div>

</div>
