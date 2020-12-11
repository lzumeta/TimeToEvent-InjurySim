################ predictSurvProb.R ################################################ 
## predictSurvProb.coxph.penal.R a function that extracts predicted survival 
## probabilities from a coxph.penal object
## Code source: https://rdrr.io/cran/pec/src/R/predictSurvProb.R
## (  predictSurvProb.coxph.penal: no visible global function definition for
## ‘predict’)
#' 
#' The function predictSurvProb is a generic function that means it invokes
#' specifically designed functions depending on the 'class' of the first
#' argument.
#' 
#' The function \code{pec} requires survival probabilities for each row in
#' newdata at requested times. These probabilities are extracted from a fitted
#' model of class \code{CLASS} with the function \code{predictSurvProb.CLASS}.
#####################################################################################


predictSurvProb.coxph.penal <- function(object,newdata,times,...){
  require(survival)
  frailhistory <- object[["history"]][[1]][["history"]]
  frailVar <- frailhistory[nrow(frailhistory),1]
  #survfit.object <- survfit(object,newdata=newdata,se.fit=FALSE,conf.int=FALSE)
  linearPred <- predict(object,newdata=newdata,se.fit=FALSE,conf.int=FALSE)
  basehaz <- basehaz(object)
  bhTimes <- basehaz[,2]
  bhValues <- basehaz[,1]
  survPred <- do.call("rbind",lapply(1:nrow(newdata),function(i){
    (1+frailVar*bhValues*exp(linearPred[i]))^{-1/frailVar}
  }))
  where <- prodlim::sindex(jump.times=bhTimes,eval.times=times)
  p <- cbind(1,survPred)[,where+1]
  if ((miss.time <- (length(times) - ncol(p)))>0)
    p <- cbind(p,matrix(rep(NA,miss.time*nrow(p)),nrow=nrow(p)))
  if (nrow(p) != nrow(newdata) || ncol(p) != length(times))
    stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",nrow(newdata)," x ",length(times),"\nProvided prediction matrix: ",nrow(p)," x ",ncol(p),"\n\n",sep=""))
  p
}
