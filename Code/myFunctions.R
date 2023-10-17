
myMeans <- function(coefs) {
  return(colMeans(coefs))
}

myBias <- function(coefs, true_beta) {
  coefs.bias <- apply(coefs, 1, function(vec) (vec - true_beta)) %>% 
    reduce(rbind) %>% 
    matrix(ncol = length(true_beta), byrow = T)
  return(coefs.bias)
}

myCoverage <- function(low, up, true_beta) {
  lowlimit <- apply(low, 1, function(vec) (vec <= true_beta))
  uplimit <- apply(up, 1, function(vec) (vec >= true_beta))
  coefs.coverage <- t(lowlimit == uplimit)
  return(coefs.coverage)
}
