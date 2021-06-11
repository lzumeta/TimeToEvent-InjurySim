# Overview

This is the accompanying code repository for the paper Zumeta-Olaskoaga et al. (2020) - ''Prediction of sports injuries in football: a recurrent time-to-event approach using regularized Cox models''.

It contains the R code for all relevant analyses in the paper, including the supplementary material. Real data and settings from the first scenario simulation, which are based on real data, are excluded, due to the data are not allowed to be freely available.

It is noted that the code have dependencies. It open sources two **R** packages such as: `sim.survdata.R` function in `coxed` package ([https://cran.r-project.org/web/packages/coxed/index.html](https://cran.r-project.org/web/packages/coxed/index.html)) and `pec` function in `pec` package ([https://cran.r-project.org/web/packages/pec/index.html](https://cran.r-project.org/web/packages/pec/index.html)), both licensed under the GPL-2 and GPL-3 licenses, respectively.

# File structure

- Code

- Results
  * Scenario2
  * Scenario3

 - Figures
   * Scenario2
   * Scenario3


## Code

```
├── Code                                  # R code folder
│   |
│   ├── SimulationSettings.R              # contains the defined settings and launches the simulations
│   ├── Main.R                            # script that performs all the analyses and calls to other (sub)scripts
│   |
│   ├── design_matrix.R                   # data generation scripts
│   ├── mysim_survdata.R
│   │   ├── sim.survdata.R  
│   │   ├── generate.lm.R
│   │   ├── baseline.build.R
│   │   ├── user.baseline.R
│   │   ├── make.margeffect.R
│   │   └── censor.x.R
│   |
│   ├── Main_variable_selection.R         # script that computes the proposed variables selection methods
│   │   ├── doBess.R                      # BeSS method
│   │   ├── doRegularization.R            # Lasso, Elastic Net (alpha = 0.5 or cv-alpha) and Ridge regression methods 
│   │   ├── doGroupLasso.R                # GroupLasso method
│   │   ├── doCoxBoost.R                  # Cox Boost
│   │   └── myfunction.R
│   |
│   ├── Main_predictive_performance.R     # script that calculates prediction error curves
│   │   ├── predictSurvProb.R  
│   │   └── BootstrapCrossValidation.R 
│   |
│   ├── plot_selectedVars.R               # plotting scripts
│   ├── plot_Brier_Curves.R
│   ├── plot_IBS_boxplots.R
│   ├── plot_all_SelectedVars.R
│   ├── plot_all_meanBS.R
│   ├── plot_all_IBS_boxplots.R
│   |
│   ├── Measuers_evaluate_performance.R  # script that calculates performance measures to evaluate simulation results
│   └── Error_decrease.R
└── 
```
