clear

// Input observed event counts
set obs 1
generate DoseLevel = -1 in 1
generate n = 3 in 1
generate DLT = 0 in 1

set obs 2
replace DoseLevel = 0 in 2
replace n = 3 in 2
replace DLT = 0 in 2

set obs 3
replace DoseLevel = 1 in 3
replace n = 8 in 3
replace DLT = 0 in 3

set obs 4
replace DoseLevel = 1 in 4
replace n = 1 in 4
replace DLT = 1 in 4

recast float DoseLevel
recast int n
recast int DLT

// Expand from aggregated data to individual data:
expand n

// Fit logit model
logit DLT DoseLevel

// The Stata routine reports:

// note: DoseLevel != 1 predicts failure perfectly
//      DoseLevel dropped and 6 obs not used
