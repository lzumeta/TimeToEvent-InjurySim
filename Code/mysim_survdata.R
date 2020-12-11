## Function to generate data ---------------------------------

## Sourcing of needed functions:
source("Code/sim.survdata.R")
source("Code/generate.lm.R")
source("Code/baseline.build.R")
source("Code/user.baseline.R")
source("Code/make.margeffect.R")
source("Code/censor.x.R")

## Main function: modified version of sim.survdata (coxced package)
# sim.survadata() function, modified from the original sim.survadata (coxed packaged) function
mysim_survdata <- function(Nobs, Tmax, Nsim, xvars, true_beta, censorship = 0.05, knots = 10, X = NULL, frailty=NULL, cluster.num=NULL, cluster.sizes=NULL, w=NULL) { # xvars = length(true_beta)
  
  
  simdata <- sim.survdata(N=Nobs, T=Tmax, num.data.frames = Nsim, xvars = xvars, beta = true_beta, censor = censorship, k =knots, X = X,
                          frailty=frailty, cluster.num=cluster.num, cluster.sizes=cluster.sizes, w=w)
  
  dfs <- map(simdata, 'data')  # dfs is a list which elemet of it is a list itself: we take the "data" component from each element of dfs
  # we change the col names: (y, failed) to (time, status)  (necessary for bess)
  m <- ncol(dfs[[1]])
  dfs <- future_map(dfs, function(df) {
    df <- df[,c(1:(m-4), (m-2), (m-1), (m-3), m)] # reorder columns, such that the four last columns are: family, rep, y, failed
    names(df)[(m-3):m] <- c("id", "rep", "time", "status")  # rename columns
    return(df)}, .options = future_options(seed = 123456L)) 
  
  return(dfs)
} 
##
