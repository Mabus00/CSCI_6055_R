# CSCI 6055 Reproducibility Project  
**Reproduction and Statistical Reanalysis of**  
*Everyday Discrimination and Satisfaction with Nature Experiences*  
(Frontiers in Epidemiology, 2024)  

## Source and Attribution
This repository contains code used to reproduce and extend the analyses presented in:  

Schinasi LH, Lawrence JA. *Everyday Discrimination and Satisfaction with Nature Experiences.* Front Epidemiol. 2024;4:1212114.  
DOI: [10.3389/fepid.2024.1212114](https://doi.org/10.3389/fepid.2024.1212114)  

All analytical materials originate from the author’s official GitHub repository, which was shared directly by Dr. Leah H. Schinasi.  
The reproduction presented here preserves the structure and computational logic of the original code, with only minor technical modifications required for local execution and verification.

## Environment and Execution
The reproduction was conducted in **R (v4.5.2)** on a **Windows 10** system using **Visual Studio Code** as the IDE.  
All analyses were executed within the same software environment and package ecosystem specified by the original author.

Installed libraries include:  
`survey`, `gtsummary`, `haven`, `tidyverse`, `plyr`, `magrittr`, `gtExtras`, and `berryFunctions`.

The analytical workflow was executed sequentially as follows:

1. **R1_gss_discrimination_dataprep.R**  
   Prepares the analytic dataset from the 2018 GSS Stata file and defines survey design variables.  
2. **v4_R1_gss_discrimination_descriptive_analysis.R**  
   Generates weighted descriptive statistics and exports `tableone_descriptive_updated02082024.csv`.  
3. **Embedded model-fitting code**  
   Produces `model_results_fulladjustment_updated_20240208.csv` and `more_model_results_fulladjustment_updated20240208.csv`.

## Modifications to Original Code
The following technical adjustments were made to enable local execution while preserving analytical integrity:

- **Path corrections:**  
  Original scripts contained hard-coded OneDrive paths referencing Drexel University directories.  
  These were modified to absolute local paths under:  
  `C:/Users/wayne/Dal/CSCI_6055_R/gss_green_eds/`.

- **Runtime diagnostics:**  
  Incremental `cat()` statements were inserted throughout the scripts to verify execution order and diagnose early runtime errors in the `svyby()` summary functions.

- **Dependency handling:**  
  Several missing R packages were installed iteratively via CRAN.  
  Because R installs packages globally rather than in isolated environments, path management was verified manually to ensure consistent reproducibility.

No analytical, statistical, or data-transformation logic was altered.  
All parameter definitions, model specifications, and weighting procedures remain identical to the original author’s implementation.

## Results and Verification
Execution of the modified scripts reproduced all published outputs exactly:

- **`tableone_descriptive_updated02082024.csv`** replicated *Table 1* (demographic distributions and median discrimination scores).  
- **`model_results_fulladjustment_updated_20240208.csv`** replicated *Table 3* (fully adjusted Poisson regression models).  
- **`more_model_results_fulladjustment_updated20240208.csv`** matched supplementary robustness analyses.  

Reproduced prevalence ratios and 95% confidence intervals:  
- PR = 1.07 (95% CI: 1.02–1.11) — dissatisfaction with nature experiences  
- PR = 1.02 (95% CI: 1.00–1.05) — insufficient time in nature  
- PR = 1.00 (95% CI: 0.97–1.04) — weekly nature exposure  

All estimates matched to two decimal places, confirming complete computational equivalence with the published findings.

## Repository Structure

```txt
CSCI_6055_R/
│
├── data/                 # GSS data (Stata .dta files, cited from GSS Data Explorer)
├── code/                 # Original and modified R scripts
├── results/              # Reproduced CSV outputs
├── figures/              # PNG exports of reproduced tables
└── README.md             # Documentation (this file)



## Citation and Acknowledgment
Original code and analytic framework by Dr. Leah H. Schinasi.  
Modifications, verification, and documentation by Wayne Renaud (Dalhousie University, CSCI 6055).  

This repository fulfills the reproducibility requirements of the CSCI 6055 project:  
- **f.** Record all code  
- **g.** Record all results  
- **h.** Record explanations of what was done and why  

