######################## SimulationSettings.R ###################################
## This file launches all the simulation scenarios considered. 
## Above of each scenario a seed is set to reproduce the same analyses we did. 
################################################################################


## Packages ---------------------------------------------------------------
library(extraDistr) ## rtpois()

# Scenario 2 --------------------------------------------------------------
## Fixed parameters
Nsim <- 100
Tmax <- 4000
knots <- 500
xvars <- 50
scenario <- "Scenario 2"

frailty <- "normal"

ci <- TRUE
CaseStudy <- FALSE

censorship <- 0.75


## Scenario 2  setting 1 (VERY SMALL SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22
set.seed(10)
cluster_sizes <- rtpois(22, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 60

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"   
dir <- paste0("Results/Scenario2/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)
set.seed(222) 
source("Code/Main.R")


## Scenario 2  setting 2 (SMALL SAMPLE SIZE) ------------
## Parameters:
cluster_num <- 22*3
set.seed(10) 
cluster_sizes <- rtpois(22 * 3, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 191

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"   
dir <- paste0("Results/Scenario2/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)

set.seed(221)
source("Code/Main.R")


## Scenario 2  setting 3 (BIG SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22*6
set.seed(10) 
cluster_sizes <- rtpois(22 * 6, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 391

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"   
dir <- paste0("Results/Scenario2/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)
set.seed(222)
source("Code/Main.R")


## Scenario 2  setting 4 (VERY LARGE SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22*10
set.seed(10)
cluster_sizes <- rtpois(22*10, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 670

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"   
dir <- paste0("Results/Scenario2/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)
set.seed(224)  
source("Code/Main.R")


# Scenario 3 --------------------------------------------------------------
## Fixed parameters --------------------
Nsim <- 100
Tmax <- 4000
knots <- 500
xvars <- 50
scenario <- "Scenario 3"

frailty <- "normal"

ci <- TRUE
CaseStudy <- FALSE

censorship <- 0.75


## Scenario 3  setting 1 (VERY SMALL SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22
set.seed(10) 
cluster_sizes <- rtpois(22, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 60

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_60_cens_0.75_xvars_50_verysmallsize"   
dir <- paste0("Results/Scenario3/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)
set.seed(333)  
source("Code/Main.R")


## Scenario 3 setting 2 (SMALL SAMPLE SIZE) ------------
## Parameters: 
cluster_num <- 22*3
set.seed(10)
cluster_sizes <- rtpois(22 * 3, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 191

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_191_cens_0.75_xvars_50_smallsize"   
dir <- paste0("Results/Scenario3/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)

set.seed(331)
source("Code/Main.R")


## Scenario 3 setting 3 (BIG SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22*6
set.seed(10)
cluster_sizes <- rtpois(22 * 6, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 391

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_391_cens_0.75_xvars_50_largesize"   
dir <- paste0("Results/Scenario3/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)

set.seed(332)
source("Code/Main.R")


## Scenario 3  setting 4 (VERY LARGE SAMPLE SIZE) -----------
## Parameters:
cluster_num <- 22*10
set.seed(10) 
cluster_sizes <- rtpois(22*10, lambda = 3, a = 0, b = Inf) ## truncated poisson 
Nobs <- sum(cluster_sizes) ## 670

set.seed(22) 
w <- exp(rnorm(cluster_num, sd = 0.3))

noise <- 0.9 # percentage of noise variables <- NK
true_beta <- c(0.4, 0.2, 0.2, 0.2, 0.1)  ## 5 non zero, 45 zeros
true_beta <- c(true_beta, rep(0, xvars*noise))
group <- rep(1:10, each=5)

name <- "Nsim_100_Nobs_670_cens_0.75_xvars_50_verylargesize"   
dir <- paste0("Results/Scenario3/", name, "/")
## create dir
if (!dir.exists(dir)) dir.create(path = dir)
set.seed(334)  
source("Code/Main.R")





