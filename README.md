# Sleep Justice Study: Analysis Code

This repository contains the analysis code for:

**Dreaming of JUSTICE: Sleep Patterns of Formerly Incarcerated
Individuals with Cardiovascular Risk: Insights from the Sleep Justice
Study**

Elumn J, Cohen I, Aminawung JA, Puglisi LB, Horton N, Lin H, Yaggi HK,
Wang EA.

## Overview

The Sleep Justice Study examined multidimensional sleep health among
adults recently released from incarceration with cardiovascular risk
factors (N=266), enrolled through the JUSTICE cohort study (R01
HL137696) between 2020–2025.

## Files

-   `01_cleaning_sleep_surveys.R` — Cleans and scores raw Qualtrics
    survey data (PSQI, STOP-Bang, BRISC)
-   `02_latent_class_analysis.Rmd` — Identifies sleep phenotypes via LCA
    (poLCA); produces tables and figures
-   `03_multinom_regression.Rmd` — Multinomial logistic regression of
    phenotype predictors; produces forest plots and regression tables

## Data Availability

Raw data are not included in this repository and are not publicly
available due to IRB restrictions and participant confidentiality. 
