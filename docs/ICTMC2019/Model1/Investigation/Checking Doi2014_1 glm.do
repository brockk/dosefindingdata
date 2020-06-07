clear

// Input observed event counts
set obs 1
generate DoseLevel = -3 in 1
generate n = 3 in 1
generate DLT = 0 in 1

set obs 2
replace DoseLevel = -2 in 2
replace n = 3 in 2
replace DLT = 0 in 2

set obs 3
replace DoseLevel = -1 in 3
replace n = 3 in 3
replace DLT = 0 in 3

set obs 4
replace DoseLevel = 0 in 4
replace n = 5 in 4
replace DLT = 0 in 4

set obs 5
replace DoseLevel = 1 in 5
replace n = 3 in 5
replace DLT = 0 in 5

set obs 6
replace DoseLevel = 2 in 6
replace n = 1 in 6
replace DLT = 1 in 6

set obs 7
replace DoseLevel = 2 in 7
replace n = 5 in 7
replace DLT = 0 in 7

set obs 8
replace DoseLevel = 3 in 8
replace n = 8 in 8
replace DLT = 0 in 8

recast float DoseLevel
recast int n
recast int DLT

// Expand from aggregated data to individual data:
expand n

// Fit logit model
logit DLT DoseLevel

// The Stata routine fits OK.
