######################## design_matrix.R ############################################################
## Computation of the design X matrix
## 
## INPUT:
## scenario: name of the senario                              character "Scenario 1" or "Scenario 2")
## xvar: the number of covariates to generate                 integer
## noise: percentage for noise variables, i.e. variables that have
## no effect on the outcome, are not relevant                 numeric 0-100
## Nobs: number of observations, i.e. sum of cluster_sizes    integer sum of cluster_sizes
## cluster_sizes: frequencies of observations per cluster 
## (number of tests done per player)                          vector of integers >=1
## augment_rows: sequence of numbers to replicated rows       vector of integers >=1
## data: original data set                                    data.frame

## OUTPUT: X design matrix of dim Nobs x xvars

## Function structure: two if blocks. According to Scenario2, Scenario 3 and Scenario 1. 
## Scenario 2 creates first a matrix of Nobs x inf_vars, each column following either a 
## normal, uniform, gamma, poisson or binomial distribution. Then the rest of noise vars
## are drawn from a normal distribution and these two matrices are put together. Scenario 3
## we create a pre-specified covariance structure, see covM, and using this matrix as the 
## Sigma (cov-var matrix) of a multivariate normal distribution we draw a small design_matrix
## X^* of cluster x xvars. Then we create duplicates of rows according to the number of 
## observations per each player. This way we mimic the real data set as close as possible. 
## We add some random noise to these vectors. Finally, there is a last block where we categorize
## every 4th column according to its median. Scenario 1, needs two additional arguments: data 
## and augmented_rows. Original data is replicated according to the vector augmented_rows and 
## then a noise is added to each row taking  into account whether the column is continuous or 
## categorical variable.
######################################################################################################


design_matrix <- function(scenario, xvars, noise, Nobs, cluster_sizes,
                          augment_rows = NULL, data = NULL, w = NULL) {
  
  set.seed(22)
  cluster    <- length(cluster_sizes)                ## number of players
  noise_vars <- trunc(x = xvars * noise, digits = 0) ## noise variables
  inf_vars   <- xvars - noise_vars                   ## influential variables
  if (scenario != "Scenario 1" ) {
    if (scenario == "Scenario 2") {
  
      set.seed(20)
      covM <- diag(1, nrow = xvars)
      X <- mvrnorm(n = cluster, mu = rep(0, xvars), Sigma = covM, empirical = F) %>%   ## If xvars > cluster, this don't work. mvrnrorm can't compute a matrix product
        as.data.frame
      colnames(X) <- paste0('X', 1:xvars)
      
      # Creation of duplicates for each player according to the number of
      # observations per player:
      X <- X[rep(x = 1:nrow(X), times = cluster_sizes), ]
      row.names(X) <- 1:nrow(X)
      
      # Adding random noise to each random vector:
      X <- as.data.frame(apply(X = X, MARGIN = 2, FUN = function(x) {
        #x <- x + rnorm(n = length(x), mean = 0, sd = sd(x)/10)
        x <- x + rnorm(n = length(x), mean = 0, sd = sd(x)/100)
        return(x)
      }))
      
      X <- scale(X)
      X <- as.data.frame(X)
      X$w <- rep(x = w, times = cluster_sizes)
    }
    
    if (scenario == "Scenario 3") {

      covM <- diag(1, nrow = xvars) ## variance covariance matrix
      covM <- outer(1:nrow(covM), 1:ncol(covM) , FUN=function(r,c) 0.65^(abs(r-c)))
      ## to ensure that is positive definite
      #covM <- covM%*%t(covM)
      #image(1:xvars, 1:xvars, covM[,xvars:1])
      
      X <- mvrnorm(n = cluster, mu = rep(0, xvars), Sigma = covM, empirical = F) %>%   ## If xvars > cluster, this don't work. mvrnrorm can't compute a matrix product
        as.data.frame
      colnames(X) <- paste0('X', 1:xvars)
      
      # Creation of duplicates for each player according to the number of
      # observations per player:
      X <- X[rep(x = 1:nrow(X), times = cluster_sizes), ]
      row.names(X) <- 1:nrow(X)
      
      # Adding random noise to each random vector:
      X <- as.data.frame(apply(X = X, MARGIN = 2, FUN = function(x) {
        #x <- x + rnorm(n = length(x), mean = 0, sd = sd(x)/10)
        x <- x + rnorm(n = length(x), mean = 0, sd = sd(x)/100)
        return(x)
      }))
      
      X <- scale(X)
      X <- as.data.frame(X)
      X$w <- rep(x = w, times = cluster_sizes)
    }
    
    
    
    
  } else { ## if (scenario == "Scenario 1") 
    x <- sample(1:22, size = cluster_num, replace = T)
    
    dd <- NULL
    for (i in seq_along(x)) { 
      dd$id[i] <- paste0("Id", i)
      jugorig <- paste0("Id", x[i])
      dd$origid[i] <- jugorig
      dd$w[i] <- w[x[i]]
      rowid <- which(data$id == jugorig)
      
      ind <-  which(which(x == x[i]) == i) %% length(rowid)  ## index of the row to select
      ind <- ifelse(ind == 0, length(rowid), ind)  ## if modulus zero then, the maximum number of obs
      
      dd$rowid[i] <- rowid[ind]
    }
    dd <- data.frame(dd)
    
    
    ## Augmented dataset of  covariates
    X <- data[,9:ncol(data)]
    X <- data.frame(X)
    
    ## We use the auxiliar dataframe dd
    ## Creation of duplicates for each player according to the number of
    ## observations per player:
    # X <- X[rep(x = 1:nrow(X), times = augment_rows), ] ## BEFORE
    X <- X[rep(x = dd$rowid, each = cluster_sizes[[1]]), ]
    row.names(X) <- 1:nrow(X)
    
    ## Adding random noise to each random vector: (lapply is by column)
    X <- as.data.frame(lapply(X = X, FUN = function(x) {
      if (is.numeric(x)) {
        x <- x + rnorm(n = length(x), mean = 0, sd = sd(x)/10)
        x <- scale(x)  ## if not survival times 108000, 10800, 10800.
      } 
      if (is.factor(x)) {
        x <- (as.numeric(x)-1) + rbinom(length(x), size = 1, prob = 0.3)
        x <- ifelse(x==2, 1, x)
        #x <- as.factor(x); levels(x) <- c("desequilibrio", "equilibrio")
      }
      return(x)
    }))
    
    X <- as.data.frame(X)
    colnames(X) <- paste0("X", 1:ncol(X))
    X$w <- rep(x = dd$w, each = cluster_sizes[[1]])
  }
  
  ## Return of the resulting data.frame:
  return(X) 
}




