# Overview

This is the accompanying code repository for the paper: "Zumeta-Olaskoaga, L., Weigert, M., Larruskain, J., Bikandi, E., Setuain, I., Lekue, J., … Lee, D.-J. (2021). [Prediction of sports injuries in football: a recurrent time-to-event approach using regularized Cox models.](https://doi.org/10.1007/s10182-021-00428-2) *AStA Advances in Statistical Analysis, 1–26*. doi: 10.1007/s10182-021-00428-2" 

It contains the R code for all relevant analyses in the paper, as well as the [supplementary material](https://github.com/lzumeta/TimeToEvent-InjurySim/blob/main/SupplementaryMaterial/Zumeta-Olaskoaga2021-SupplementaryMaterial.pdf). Real data and settings from the first scenario simulation (based on real data), are excluded because the data cannot be made publicly available.

The code have some dependencies. It open sources two **R** packages: `sim.survdata.R` function in `coxed` package ([https://cran.r-project.org/web/packages/coxed/index.html](https://cran.r-project.org/web/packages/coxed/index.html)) and `pec` function in `pec` package ([https://cran.r-project.org/web/packages/pec/index.html](https://cran.r-project.org/web/packages/pec/index.html)), both licensed under the GPL-2 and GPL-3 licenses, respectively.


# File structure

- Code

- Supplementary Material

- Results
  * Scenario2
  * Scenario3

 - Figures
   * Scenario2
   * Scenario3


# Code

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

<br/><br/>

------------------


## Prediction of sports injuries in football: a recurrent time-to-event approach using regularized Cox models

**Citation** <br/>
Zumeta-Olaskoaga, L., Weigert, M., Larruskain, J., Bikandi, E., Setuain, I., Lekue, J., … Lee, D.-J. (2021). Prediction of sports injuries in football: a recurrent time-to-event approach using regularized Cox models. *AStA Advances in Statistical Analysis*, 1–26. doi: 10.1007/s10182-021-00428-2

**DOI** <br/>
https://doi.org/10.1007/s10182-021-00428-2

**Keywords** <br/>
Shared frailty models · Regularized Cox methods · Sports injury prevention · Survival analysis

<br/>

**Illustration of lower-limb injury data**: <br/><br/>

![imga](https://user-images.githubusercontent.com/29726315/142850933-83bcbbbc-8b49-45f0-8290-7b703c5a5354.png)


From: [https://link.springer.com/article/10.1007/s10182-021-00428-2/figures/1](https://link.springer.com/article/10.1007/s10182-021-00428-2/figures/1) <br/><br/>

**Supplementary Material**<br/>
[https://github.com/lzumeta/TimeToEvent-InjurySim/SupplementaryMaterial/Zumeta-Olaskoaga2021-SupplementaryMaterial.pdf](https://github.com/lzumeta/TimeToEvent-InjurySim/blob/main/SupplementaryMaterial/Zumeta-Olaskoaga2021-SupplementaryMaterial.pdf)

