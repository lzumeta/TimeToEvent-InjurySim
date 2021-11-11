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