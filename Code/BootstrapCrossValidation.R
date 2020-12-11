#### This code rewrites the function "bootrstrapCrossValidation" from
#### package "pec".

my_bootstrapCrossValidation <- function(object,
                                        data,
                                        Y,
                                        status,
                                        event,
                                        times,
                                        cause,
                                        ipcw,
                                        ipcw.refit=FALSE,
                                        ipcw.call,
                                        splitMethod,
                                        multiSplitTest,
                                        keepResiduals,
                                        testIBS,
                                        testTimes,
                                        newdata,
                                        confInt,
                                        confLevel,
                                        getFromModel,
                                        giveToModel,
                                        predictHandlerFun,
                                        keepMatrix,
                                        verbose,
                                        savePath,slaveseed){
  # {{{ initializing
  B <- splitMethod$B
  N <- splitMethod$N
  M <- splitMethod$M
  NT <- length(times)
  NF <- length(object) 
  ResampleIndex <- splitMethod$index
  # }}}
  step <- function(b,seed){
    if (verbose==TRUE) internalTalk(b,B)
    # {{{ training and validation data
    vindex.b <- match(1:N,unique(ResampleIndex[,b]),nomatch=0)==0
    val.b <- data[vindex.b,,drop=FALSE]
    train.b <- data[ResampleIndex[,b],,drop=FALSE]
    ## print(c(NROW(train.b), NROW(val.b)))
    NV=sum(vindex.b)                    # NROW(val.b)
    # }}}
    # {{{ IPCW
    if (ipcw.refit==TRUE){
      ipcw.call.b <- ipcw.call
      ipcw.call.b$data <- val.b
      ipcw.call.b$subjectTimes <- Y[vindex.b]
      ipcw.b <- do.call("ipcw",ipcw.call.b)
      ipcwTimes.b <- ipcw.b$IPCW.times
      IPCW.subjectTimes.b <- ipcw.b$IPCW.subjectTimes
    }
    else{
      IPCW.subjectTimes.b <- ipcw$IPCW.subjectTimes[vindex.b]
      if (ipcw$dim==1)
        ipcwTimes.b <- ipcw$IPCW.times[vindex.b,]
      else
        ipcwTimes.b <- ipcw$IPCW.times
    }
    
    # }}}
    # {{{ Building the models in training data
    if (!is.null(seed)) {
      set.seed(seed)
      ## if (verbose) message("seed:",seed)
    }
    trainModels <- lapply(1:NF,function(f){
      fit.b <- internalReevalFit(object=object[[f]],data=train.b,step=b,silent=F,verbose=verbose)
      ## this was a good idea to reduce the memory usage:
      ## fit.b$call <- object[[f]]$call
      ## fit.b$call <- NULL
      ## however, it does not work with the new version of the survival package
      ## in which the survfit.coxph function checks the response 'y'
      ## next try
      ## print("before")
      ## print(object.size(fit.b))
      ## print("after")
      ## browser()
      ## fit.b$call$data <- substitute(train.b)
      ## print(object.size(fit.b))
      fit.b
    })
    # }}}
    # {{{ Saving the models?
    if (!is.null(savePath)){
      nix <- lapply(1:NF,function(f){
        fit.b <- trainModels[[f]]
        ## print(object.size(fit.b))
        fit.b$formula <- NULL
        ## print(environment(fit.b$formula))
        save(fit.b,file=paste(paste(savePath,"/",names(object)[f],"-bootstrap-",b,sep=""),".rda",sep=""))
      })
    }
    # }}}
    # {{{ Extracting parameters?
    if (!is.null(getFromModel)){
      ModelParameters <- lapply(1:NF,function(f){
        getParms <- getFromModel[[f]]
        print(trainModels[[f]][getParms])
        if (is.null(getParms)) trainModels[[f]][getParms] else NULL
      })
    }
    # }}}
    # {{{ Check fits
    fitFailed <- lapply(trainModels,function(fit.b) (is.null(fit.b)))
    # }}}
    # {{{ Predicting the validation data
    predVal <- lapply(1:NF,function(f){
      fit.b <- trainModels[[f]]
      extraArgs <- giveToModel[[f]]
      if (predictHandlerFun == "predictEventProb"){
        try2predict <- try(pred.b <- do.call(predictHandlerFun,
                                             c(list(object=fit.b,newdata=val.b,times=times,cause=cause),extraArgs)))
      }
      else{
        try2predict <- try(pred.b <- do.call(predictHandlerFun,
                                             c(list(object=fit.b,newdata=val.b,times=times),extraArgs)))
      }
      if (inherits(try2predict,"try-error")==TRUE){
        if (verbose==TRUE) warning(paste("During bootstrapping: prediction for model ",class(fit.b)," failed in step ",b),immediate.=TRUE)
        NULL
      }
      else{
        pred.b
      }
    })
    # }}}
    # {{{ Compute prediction error curves for step b
    if (multiSplitTest==TRUE){
      Residuals <- lapply(predVal,function(pred.b){
        if (is.null(pred.b))
          NA
        else{
          if (predictHandlerFun == "predictEventProb"){
            matrix(.C("pecResidualsCR",
                      pec=double(NT),
                      resid=double(NT*NV),
                      as.double(Y[vindex.b]),
                      as.double(status[vindex.b]),
                      as.double(event[vindex.b]),
                      as.double(times),
                      as.double(pred.b),
                      as.double(ipcwTimes.b),
                      as.double(IPCW.subjectTimes.b),
                      as.integer(NV),
                      as.integer(NT),
                      as.integer(ipcw$dim),
                      as.integer(is.null(dim(pred.b))),
                      NAOK=TRUE,
                      PACKAGE="pec")$resid,ncol=NT,byrow=FALSE)
          }
          else{
            matrix(.C("pecResiduals",
                      pec=double(NT),
                      resid=double(NT*NV),
                      as.double(Y[vindex.b]),
                      as.double(status[vindex.b]),
                      as.double(times),
                      as.double(pred.b),
                      as.double(ipcwTimes.b),
                      as.double(IPCW.subjectTimes.b),
                      as.integer(NV),
                      as.integer(NT),
                      as.integer(ipcw$dim),
                      as.integer(is.null(dim(pred.b))),
                      NAOK=TRUE,
                      PACKAGE="pec")$resid,ncol=NT,byrow=FALSE)
          }
        }
      })
      names(Residuals) <- names(object)
      PredErrStepB=lapply(Residuals,function(x){colMeans(x)})
    }
    else{
      PredErrStepB <- lapply(predVal,function(pred.b){
        if (is.null(pred.b))
          #NA
          pecOut <- rep(0.25, NT) ## NT = length(times)
        else{
          if (predictHandlerFun=="predictEventProb")
            pecOut <- .C("pecCR",
                         pec=double(NT),
                         as.double(Y[vindex.b]),
                         as.double(status[vindex.b]),
                         as.double(event[vindex.b]),
                         as.double(times),
                         as.double(pred.b),
                         as.double(ipcwTimes.b),
                         as.double(IPCW.subjectTimes.b),
                         as.integer(NV),
                         as.integer(NT),
                         as.integer(ipcw$dim),
                         as.integer(is.null(dim(pred.b))),
                         NAOK=TRUE,
                         PACKAGE="pec")$pec
          else
            pecOut <- .C("pecSRC",pec=double(NT),as.double(Y[vindex.b]),as.double(status[vindex.b]),as.double(times),as.double(pred.b),as.double(ipcwTimes.b),as.double(IPCW.subjectTimes.b),as.integer(NV),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred.b))),NAOK=TRUE,PACKAGE="pec")$pec
        }
      })
    }
    # }}}
    # {{{ van de Wiel's test
    if (multiSplitTest==TRUE){
      testedResid <- testResiduals(Residuals,times=times,testTimes=testTimes,rangeInt=testIBS,confInt=confInt,confLevel=confLevel)
    }
    # }}}
    # {{{ looping output 
    if (multiSplitTest==TRUE)
      loopOut=list(PredErrStepB=PredErrStepB,testedResid=testedResid)
    else
      loopOut=list(PredErrStepB=PredErrStepB)
    if (keepResiduals==TRUE)  
      loopOut=c(loopOut,list(Residuals=lapply(Residuals,function(R){
        R[,prodlim::sindex(eval.times=testTimes,jump.times=times)]
      })))
    if (!is.null(getFromModel)){
      loopOut=c(loopOut,list(ModelParameters=ModelParameters))
    }
    loopOut
  }
  b <- 1
  ## if (require(foreach)){
  if (missing(slaveseed)||is.null(slaveseed))
    set.seed(22)
    slaveseed <- sample(1:1000000,size=B,replace=FALSE)
  
  Looping <- foreach::foreach (b= 1:B)  %dopar% step(b,slaveseed[[b]])
  ## }
  ## else{
  ## Looping <- lapply(1:B,function(b){step(b,seed=NULL)})
  ## }
  # }}}
  # {{{ output
  ## 
  ## 
  ##    1. a list of NF matrices each with B (rows) and NT columns
  ##       the prediction error curves
  ## 
  if (verbose==TRUE && B>1) cat("\n")
  BootstrapCrossValErrMat <- lapply(1:NF,function(f){
    ## matrix with NT columns and b rows
    do.call("rbind",lapply(Looping,function(b){
      b$PredErrStepB[[f]]
    }))
  })
  ## 
  ##    2. a list of NF average out-of-bag prediction error curves
  ##       with length NT
  ## 
  BootstrapCrossValErr <- lapply(BootstrapCrossValErrMat,colMeans) ## , na.rm = TRUE
  ##   function(x){
  ##     if (na.accept>0) colMeans(x,na.rm=sum(is.na(b))<na.accept)
  ##     else
  ##     colMeans(x)
  ##   })
  out <- list(BootstrapCrossValErr=BootstrapCrossValErr)
  ## 
  ##   3. the results of B residual tests 
  ##   
  if (multiSplitTest==TRUE){
    out$testedResid <- lapply(Looping,function(x)x$testedResid)
  }
  ## 
  ##   4. model parameters
  ##
  if (!is.null(getFromModel)){
    out$ModelParameters <- lapply(1:NF,function(f){
      lapply(Looping,function(x)x$ModelParameters[[f]])
    })
  }
  ## 
  ##   5. bootstrap crossvalidation results
  ##
  if (keepMatrix==TRUE)
    out$BootstrapCrossValErrMat <- BootstrapCrossValErrMat
  ## 
  ##   6. residuals
  ##
  if (keepResiduals==TRUE){
    out$Residuals <- lapply(1:NF,function(f){
      bootResiduals <- lapply(Looping,function(b){
        b$Residuals[[f]]
      })
      names(bootResiduals) <- paste("testSample",1:B,sep=".")
      bootResiduals
    })
    names(out$Residuals) <- names(object)
  }
  out
  # }}}
}




library(foreach)
my_pec <- function(object,
                   formula,
                   data,
                   traindata,
                   times,
                   cause,
                   ## time points
                   start,
                   maxtime,
                   exact=TRUE,
                   exactness=100,
                   fillChar=NA,
                   ## censoring weighting 
                   cens.model="cox",
                   ipcw.refit=FALSE,
                   ipcw.args=NULL,
                   ## data splitting
                   splitMethod="none",
                   B,
                   M,
                   ## misc parameters
                   reference=TRUE,
                   model.args=NULL,
                   model.parms=NULL,
                   keep.index=FALSE,
                   keep.matrix=FALSE,
                   keep.models=FALSE,
                   keep.residuals=FALSE,
                   keep.pvalues=FALSE,
                   noinf.permute=FALSE,
                   multiSplitTest=FALSE,
                   testIBS,
                   testTimes,
                   confInt=FALSE,
                   confLevel=0.95,
                   verbose=TRUE,
                   savePath=NULL,
                   slaveseed=NULL,
                   na.action=na.fail,
                   ...)
{
  # }}}
  # {{{ checking integrity some arguments
  
  theCall=match.call()
  if (match("replan",names(theCall),nomatch=FALSE))
    stop("The argument name 'replan' has been replaced by 'splitMethod'.")
  if (!missing(testIBS) && (!(is.logical(testIBS) || (length(testIBS)==2 && is.numeric(testIBS)))))
    stop("Argument testIBS can be TRUE/FALSE or a vector of two numeric values.")
  if (missing(testIBS)) testIBS <- FALSE
  if (keep.residuals && missing(testTimes))
    stop("To keep.residuals please specify testTimes.")
  if (missing(splitMethod) && multiSplitTest==TRUE){
    stop("Need data splitting to compute van de Wiel's test")
  }
  if (missing(M) && multiSplitTest) M <- NA
  
  # }}}
  # {{{ check and convert object
  if (class(object)[1]!="list") {
    object <- list(object)
  }
  # }}}
  # {{{ formula
  if (missing(formula)){
    if (length(grep("~",as.character(object[[1]]$call$formula)))==0){
      stop(paste("Argument formula is missing and first model has no usable formula:",as.character(object[[1]]$call$formula)))
    } else{
      ftry <- try(formula <- eval(object[[1]]$call$formula),silent=TRUE)
      if ((class(ftry)[1]=="try-error") || match("formula",class(formula),nomatch=0)==0)
        stop("Argument formula is missing and first model has no usable formula.")
      else if (verbose)
        warning("Formula missing. Using formula from first model")
    }
  }
  formula.names <- try(all.names(formula),silent=TRUE)
  if (!(formula.names[1]=="~")
      ||
      (match("$",formula.names,nomatch=0)+match("[",formula.names,nomatch=0)>0)){
    stop("Invalid specification of formula.\n Could be that you forgot the right hand side:\n ~covariate1 + covariate2 + ...?\nNote that any subsetting, ie data$var or data[,\"var\"], is not supported by this function.")
  }
  else{
    if (!(formula.names[2] %in% c("Surv","Hist")))
      survp <- FALSE
    else
      survp <- TRUE
  }
  # }}}
  # {{{ data
  if (missing(data)){
    data <- eval(object[[1]]$call$data)
    if (match("data.frame",class(data),nomatch=0)==0)
      stop("Argument data is missing.")
    else
      if (verbose)
        warning("Argument data is missing. I use the data from the call to the first model instead.")
  }
  # }}}
  # {{{ censoring model
  
  cens.model <- match.arg(cens.model,c("cox","marginal","nonpar","aalen","none","rfsrc"))
  
  # }}}
  # {{{ response
  
  histformula <- formula
  if (histformula[[2]][[1]]==as.name("Surv")){
    histformula[[2]][[1]] <- as.name("Hist")
  }
  ## m <- model.frame(histformula,data,na.action=na.fail)
  m <- model.frame(histformula,data,na.action=na.action)
  response <- model.response(m)
  if (match("Surv",class(response),nomatch=0)!=0){
    attr(response,"model") <- "survival"
    attr(response,"cens.type") <- "rightCensored"
    model.type <- "survival"
  }
  model.type <- attr(response,"model")
  if (model.type=="competing.risks"){
    predictHandlerFun <- "predictEventProb"
    if (missing(cause))
      cause <- attr(response,"state")[1]
  }
  else{
    if (survp==FALSE && NCOL(response)!=1) stop("Response must be one-dimensional.")
    if (survp==TRUE && NCOL(response)!=2) stop("Survival response must have two columns: time and status.")
    predictHandlerFun <- "predictSurvProb"
  }
  
  # }}}
  # {{{ prediction models
  if (reference==TRUE) {
    ProdLimform <- as.formula(update(formula,".~NULL"))
    ## ProdLimfit <- do.call(prodlim::prodlim,list(formula=ProdLimform,data=data))
    ProdLimfit <- prodlim::prodlim(formula=ProdLimform,data=data)
    ProdLimfit$call$data <- as.character(substitute(data))
    ProdLimfit$call$formula=ProdLimform
    ProdLimfit$formula <- as.formula(ProdLimfit$formula)
    ## print(environment(ProdLimfit$formula))
    ## if (model.type=="competing.risks")
    ## object <- c(list(Reference=ProdLimfit),object)
    ## else
    ## browser()
    object <- c(list("Reference"=ProdLimfit),object)
  }
  if (is.null(names(object))){
    names(object) <- sapply(object,function(o)class(o)[1])
    names(object) <- make.names(names(object),unique=TRUE)
  }
  else{ # fix missing names
    if (any(names(object)=="")){
      names(object)[(names(object)=="")] <- sapply(object[(names(object)=="")],function(o)class(o)[1])
      names(object) <- make.names(names(object),unique=TRUE)
    }else{
      # leave names as they were given
    }
  }
  ## names(object) <- make.names(names(object),unique=TRUE)
  NF <- length(object)
  # }}}  
  # {{{ sort the data 
  
  if (survp){
    neworder <- order(response[,"time"],-response[,"status"])
    if (predictHandlerFun=="predictEventProb"){
      event <- prodlim::getEvent(response,mode="character")
      event <- event[neworder]
    }
    response <- response[neworder,,drop=FALSE]
    Y <- response[,"time"]
    status <- response[,"status"]
  }
  else{
    cens.model <- "none"
    neworder <- order(response)
    Y <- response[neworder]
    status <- rep(1,length(Y))
  }
  ## for competing risks find the cause of interest.
  if (predictHandlerFun=="predictEventProb"){
    availableCauses <- unique(event)
    if (!match(cause, availableCauses,nomatch=FALSE))
      stop("Cause ",cause," is not among the available causes: ",paste(availableCauses,collapse=", "))
    event <- event==cause
  }
  ##   else{
  ##     event <- NULL
  ##   }
  data <- data[neworder,]
  unique.Y <- unique(Y)
  N <- length(Y)
  NU <- length(unique.Y)
  # }}}
  # {{{ splitMethod
  
  splitMethod <- resolvesplitMethod(splitMethod=splitMethod,B=B,N=N,M=M)
  B <- splitMethod$B
  ResampleIndex <- splitMethod$index
  k <- splitMethod$k
  do.resample <- !(is.null(ResampleIndex))
  if (keep.matrix==TRUE & !do.resample){
    warning("Argument keep.matrix set to FALSE, since no resampling/crossvalidation is requested.")
    keep.matrix <- FALSE
  }
  
  # }}}      
  # {{{ find maxtime, start, and jumptimes in the range of the response 
  if (missing(maxtime) || is.null(maxtime))
    maxtime <- unique.Y[NU]
  if (missing(start))
    if (survp==TRUE)
      start <- 0  ## survival times are positive
  else
    start <- min(unique.Y) 
  if (missing(times)){
    if (exact==TRUE)
      times <- unique(c(start,unique.Y))
    else
      times <- seq(start,maxtime,(maxtime - start)/exactness)
  }
  else{
    if (exact==TRUE) 
      times <- sort(c(start,unique(times),unique.Y))
    else
      times <- sort(unique(c(start,times)))
  }
  times <- times[times<=maxtime]
  NT <-  length(times)
  
  # }}}
  # {{{ IPCW (all equal to 1 without censoring) 
  
  if((cens.model %in% c("aalen","cox","nonpar"))){
    if (all(as.numeric(status)==1) || sum(status)==N){
      if (verbose)
        message("No censored observations: cens.model coerced to \"none\".")
      cens.model <- "none"
    }
    if ((cens.model!="nonpar") && length(attr(terms(formula),"factors"))==0){
      if (verbose==TRUE)
        message("No covariates  specified: Kaplan-Meier for censoring times used for weighting.")
      cens.model <- "marginal"}
  }
  if (predictHandlerFun=="predictEventProb"){
    iFormula <- as.formula(paste("Surv(itime,istatus)","~",as.character(formula)[[3]]))
    iData <- data
    iData$itime <- response[,"time"]
    iData$istatus <- response[,"status"]
    if (ipcw.refit==TRUE)
      stop("pec: internal refitting of censoring distribution not (not yet) supported for competing risks")
    ipcw.call <- NULL
    ipcw <- ipcw(formula=iFormula,
                 data=iData,
                 method=cens.model,
                 args=ipcw.args,
                 times=times,
                 subjectTimes=Y,
                 subjectTimesLag=1)
    ipcw$dim <- if (cens.model %in% c("marginal","none")) 0 else 1
  }
  else{
    if (ipcw.refit==TRUE && splitMethod$internal.name %in% c("Boot632plus","BootCv","Boot632"))
      ipcw.call <- list(formula=formula,data=NULL,method=cens.model,times=times,subjectTimes=NULL,subjectTimesLag=1)
    else
      ipcw.call <- NULL
    ipcw <- ipcw(formula=formula,
                 data=data,
                 method=cens.model,
                 args=ipcw.args,
                 times=times,
                 subjectTimes=Y,
                 subjectTimesLag=1)
    ipcw$dim <- if (cens.model %in% c("marginal","none")) 0 else 1
  }
  ## force ipc weights not to exaggerate
  ## weights should not be greater than 1/(sample size)
  ## if (ipcw$dim==1){
  ## ipcw$IPCW.times <- apply(ipcw$IPCW.times,1,function(x)pmax(x,1/N))
  ## }
  ## else{
  ## ipcw$IPCW.times <- pmax(ipcw$IPCW.times,1/N)
  ## }
  ## ipcw$IPCW.subjectTimes <- pmax(ipcw$IPCW.subjectTimes,1/N)
  ## browser()
  
  #  wt <- ipcw$IPCW.times
  #  wt.obs <- ipcw$IPCW.subjectTimes
  #  if (NCOL(wt)>1) {stopifnot(length(wt)==(N*NT))}  else{stopifnot(length(wt)==NT)}
  
  # }}}
  # {{{ checking the models for compatibility with resampling
  if (do.resample){
    cm <- checkModels(object=object,model.args=model.args,model.parms=model.parms,splitMethod=splitMethod$internal.name)
    model.args <- cm$model.args
    model.parms <- cm$model.parms
  }
  # }}}
  # {{{ ---------------------------Apparent error---------------------------
  
  AppErr <- lapply(1:NF,function(f){
    ## message(f)
    fit <- object[[f]]
    extraArgs <- model.args[[f]]
    if (predictHandlerFun=="predictEventProb"){ # competing risks
      pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times,cause=cause),extraArgs))
      if (class(fit)[[1]]%in% c("matrix","numeric")) pred <- pred[neworder,,drop=FALSE]
      .C("pecCR",pec=double(NT),as.double(Y),as.double(status),as.double(event),as.double(times),as.double(pred),as.double(ipcw$IPCW.times),as.double(ipcw$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred))),NAOK=TRUE,PACKAGE="pec")$pec
    }
    else{  # survival
      pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times),extraArgs))
      if (class(fit)[[1]]%in% c("matrix","numeric")) pred <- pred[neworder,,drop=FALSE]
      ## u <- list(as.double(Y),as.double(status),as.double(times),as.double(pred),as.double(ipcw$IPCW.times),as.double(ipcw$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred))))
      ## if (f==2) browser(skipCalls=1)
      .C("pecSRC",pec=double(NT),as.double(Y),as.double(status),as.double(times),as.double(pred),as.double(ipcw$IPCW.times),as.double(ipcw$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred))),NAOK=TRUE,PACKAGE="pec")$pec
    }
  })
  names(AppErr) <- names(object)
  
  ## se.Apperr <- lapply(1:NF,function(f){
  ## ## message(f)
  ## fit <- object[[f]]
  ## extraArgs <- model.args[[f]]
  ## if (predictHandlerFun=="predictEventProb"){ # competing risks
  ## pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times,cause=cause),extraArgs))
  ## if (class(object[[f]])[[1]]%in% c("matrix","numeric")) pred <- pred[neworder,,drop=FALSE]
  ## Paulo(as.double(Y),
  ## as.double(status),
  ## as.double(event),
  ## as.double(times),
  ## as.double(pred),
  ## as.double(ipcw$IPCW.times),
  ## as.double(ipcw$IPCW.subjectTimes))
  ## }
  ## else{  # survival
  ## pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times),extraArgs))
  ## if (class(object[[f]])[[1]]%in% c("matrix","numeric")) pred <- pred[neworder,,drop=FALSE]
  ## Paulo(as.double(Y),
  ## as.double(status),
  ## as.double(times),
  ## as.double(pred),
  ## as.double(ipcw$IPCW.times),
  ## as.double(ipcw$IPCW.subjectTimes))
  ## }})
  
  
  # }}}
  # {{{------------------------No information error------------------------
  
  if (splitMethod$internal.name %in% c("Boot632plus")){
    if (verbose==TRUE){
      message("Computing noinformation error using all permutations")
    }
    if (noinf.permute==FALSE){
      NoInfErr <- lapply(1:NF,function(f){
        fit <- object[[f]]
        extraArgs <- model.args[[f]]
        if (predictHandlerFun=="predictEventProb"){ # competing risks
          pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times,cause=cause),extraArgs))
        }
        else{  # survival
          pred <- do.call(predictHandlerFun,c(list(object=fit,newdata=data,times=times),extraArgs))
        }
        if (predictHandlerFun=="predictEventProb")
          .C("pec_noinfCR",pec=double(NT),as.double(Y),as.double(status),as.double(event),as.double(times),as.double(pred),as.double(ipcw$IPCW.times),as.double(ipcw$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred))),NAOK=TRUE,PACKAGE="pec")$pec
        else
          .C("pec_noinf",pec=double(NT),as.double(Y),as.double(status),as.double(times),as.double(pred),as.double(ipcw$IPCW.times),as.double(ipcw$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred))),NAOK=TRUE,PACKAGE="pec")$pec
      })
      names(NoInfErr) <- names(object)
    }else{
      if (verbose==TRUE){
        message("Noinformation error simulation loop (B=",B,")")
      }
      ## FIXME: need to parallelize noinf
      NoInfErrList <- lapply(1:B,function(b){
        if (verbose==TRUE){
          internalTalk(b,B,sign=".")
        }
        responseNames <- colnames(response)
        noinf.b <- data[sample(1:NROW(data),replace=FALSE),-match(responseNames,names(data))]
        noinf.b[,responseNames] <- response
        ipcw.b <- ipcw(formula=formula,data=noinf.b,method=cens.model,args=ipcw.args,times=times,subjectTimes=Y,subjectTimesLag=1)
        noinfPredErr <- lapply(1:NF,function(f){
          fit.b <- internalReevalFit(object=object[[f]],data=noinf.b,step=b,silent=FALSE,verbose=verbose)
          ## fit.b$call <- object[[f]]$call
          extraArgs <- model.args[[f]]
          
          pred.b <- do.call(predictHandlerFun,c(list(object=fit.b,newdata=noinf.b,times=times),extraArgs))
          if (predictHandlerFun=="predictEventProb"){
            pred.b <- do.call(predictHandlerFun,c(list(object=fit.b,newdata=noinf.b,times=times,cause=cause),extraArgs))
            .C("pecCR",pec=double(NT),as.double(Y),as.double(status),as.double(event),as.double(times),as.double(pred.b),as.double(ipcw.b$IPCW.times),as.double(ipcw.b$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred.b))),NAOK=TRUE,PACKAGE="pec")$pec
          }
          else{
            pred.b <- do.call(predictHandlerFun,c(list(object=fit.b,newdata=noinf.b,times=times),extraArgs))
            .C("pecSRC",pec=double(NT),as.double(Y),as.double(status),as.double(times),as.double(pred.b),as.double(ipcw.b$IPCW.times),as.double(ipcw.b$IPCW.subjectTimes),as.integer(N),as.integer(NT),as.integer(ipcw$dim),as.integer(is.null(dim(pred.b))),NAOK=TRUE,PACKAGE="pec")$pec
          }
        })
        noinfPredErr
      })
      NoInfErrMat <- lapply(1:NF,function(f){
        do.call("rbind",lapply(NoInfErrList,function(x){
          x[[f]]
        }))})
      NoInfErr <- lapply(NoInfErrMat,colMeans)
      names(NoInfErr) <- names(object)
    }
  }
  
  # }}}
  # {{{--------------k-fold and leave-one-out CrossValidation-----------------------
  
  if (splitMethod$internal.name %in% c("crossval","loocv")){
    kCV <- kFoldCrossValidation(object=object,data=data,Y=Y,status=status,event=event,times=times,cause=cause,ipcw=ipcw,splitMethod=splitMethod,giveToModel=model.args,predictHandlerFun=predictHandlerFun,keep=keep.matrix,verbose=verbose)
    CrossValErr <- kCV$CrossValErr
    if (keep.matrix && B>1)
      CrossValErrMat <- kCV$CrossValErrMat
  }
  
  # }}}
  # {{{ ----------------------BootstrapCrossValidation----------------------
  
  if (splitMethod$internal.name %in% c("Boot632plus","BootCv","Boot632")){
    if (verbose==TRUE){
      message("Split sample loop (B=",B,")")
    }
    if (missing(testTimes)){
      testTimes <- NULL
    }
    BootCv <- my_bootstrapCrossValidation(object=object,
                                          data=data,
                                          Y=Y,
                                          status=status,
                                          event=event,
                                          times=times,
                                          cause=cause,
                                          ipcw=ipcw,
                                          ipcw.refit=ipcw.refit,
                                          ipcw.call=ipcw.call,
                                          splitMethod=splitMethod,
                                          multiSplitTest=multiSplitTest,
                                          testIBS=testIBS,
                                          testTimes=testTimes,
                                          confInt=confInt,
                                          confLevel=confLevel,
                                          getFromModel=model.parms,
                                          giveToModel=model.args,
                                          predictHandlerFun=predictHandlerFun,
                                          keepMatrix=keep.matrix,
                                          keepResiduals=keep.residuals,
                                          verbose=verbose,
                                          savePath=savePath,
                                          slaveseed=slaveseed)
    BootstrapCrossValErr <- BootCv$BootstrapCrossValErr
    Residuals <- BootCv$Residuals
    names(BootstrapCrossValErr) <- names(object)
    if (multiSplitTest==TRUE){
      comparisons <- allComparisons(names(object))
      multiSplitTestResults <- list(testIBS=testIBS,B=B,M=M,N=N,testTimes=testTimes)
      multiSplitTestResults$Comparisons <- lapply(1:length(comparisons),function(cc){
        if (length(testTimes)>0){
          allPairwisePvaluesTimes <- do.call("rbind",lapply(BootCv$testedResid,function(b){
            b$pValue[[cc]]}))
          out <- list(pValueTimes=apply(allPairwisePvaluesTimes,2,median))
          if (keep.pvalues==TRUE){
            out$allPairwisePvaluesTimes <- allPairwisePvaluesTimes
          }
        }
        else out <- NULL
        if(length(testIBS)>0){
          allPairwisePvaluesIBS <- sapply(BootCv$testedResid,function(b){
            b$IBSpValue[[cc]]
          })
          out$pValueIBS <- median(allPairwisePvaluesIBS)
        }
        if (keep.pvalues==TRUE){
          out$allPairwisePvaluesIBS <- allPairwisePvaluesIBS}
        out
      })
      names(multiSplitTestResults$Comparisons) <- names(comparisons)
      ## multiSplitTest$splitMethod <- splitMethod
      class(multiSplitTestResults) <- "multiSplitTest"
    }
    ## upperLimits <- lapply(BootCv$testedResid,function(x){x[,1:length(testTimes)]})
    ##     if (testIBS==TRUE){
    ##       wtestIBSpValues <- do.call("cbind",apply(BootCv$testedResid,function(x){x[,length(testTimes)+1]}))
    ##     }
    ## wtestIBSupper <- BootCv$testedResid$wtestIBSupper
    ##   }
    if (keep.matrix==TRUE){
      BootstrapCrossValErrMat <- BootCv$BootstrapCrossValErrMat
      names(BootstrapCrossValErr) <- names(object)
    }
  }
  
  # }}}
  # {{{ Bootstrap .632
  if (splitMethod$internal.name=="Boot632"){
    B632Err <- lapply(1:NF,function(f){
      .368 * AppErr[[f]] + .632 * BootstrapCrossValErr[[f]]
    })
    names(B632Err) <- names(object)
  }
  # }}}    
  # {{{ Bootstrap .632+
  
  if (splitMethod$internal.name=="Boot632plus"){
    B632plusErr <- lapply(1:NF,function(f){
      Err1 <- pmin(BootstrapCrossValErr[[f]],NoInfErr[[f]])
      overfit <- (Err1 - AppErr[[f]]) / (NoInfErr[[f]] - AppErr[[f]])
      overfit[!(Err1>AppErr[[f]])] <- 0
      w <- .632 / (1 - .368 * overfit)
      B632plusErr <- (1-w) * AppErr[[f]]  + w * Err1
      B632plusErr
      ## w[NoInfErr<=BootstrapCrossValErr] <- 1
      ## B632plus.error <- (1-w) * AppErr  + w * BootstrapCrossValErr
    })
    names(B632plusErr) <- names(object)
  }
  
  # }}}
  # {{{ prepare output
  
  out <- switch(splitMethod$internal.name,
                "noPlan"=list("AppErr"=AppErr),
                "Boot632plus"=list("AppErr"=AppErr,"BootCvErr"=BootstrapCrossValErr,"NoInfErr"=NoInfErr,"Boot632plusErr"=B632plusErr),
                "Boot632"=list("AppErr"=AppErr,"BootCvErr"=BootstrapCrossValErr,"Boot632Err"=B632Err),
                "BootCv"=list("AppErr"=AppErr,"BootCvErr"=BootstrapCrossValErr),
                "loocv"=list("AppErr"=AppErr,"loocvErr"=CrossValErr),
                "crossval"=list("AppErr"=AppErr,"crossvalErr"=CrossValErr),
                "noinf"=list("AppErr"=AppErr,"NoInfErr"=NoInfErr))
  observed.maxtime <- sapply(out,function(x){
    ## lapply(x,function(y){times[length(y)-sum(is.na(y))-1]})
    lapply(x,function(y){times[length(y)-sum(is.na(y))]})
  })
  minmaxtime <- min(unlist(observed.maxtime))
  if (multiSplitTest==TRUE){
    out <- c(out,list(multiSplitTest=multiSplitTestResults))
  }
  if (keep.residuals==TRUE){
    out <- c(out,list(Residuals=Residuals))
  }
  if (keep.matrix==TRUE && splitMethod$internal.name!="noPlan"){
    if (splitMethod$internal.name %in% c("crossval","loocv")){
      if (B>1)
        out <- c(out,list("CrossValErrMat"=CrossValErrMat))
    }
    else{
      if (splitMethod$internal.name!="noinf")
        out <- c(out,list("BootstrapCrossValErrMat"=BootstrapCrossValErrMat))
    }
  }
  if (!is.na(fillChar))
    out <- lapply(out,function(o){
      o[is.na(o)] <- fillChar
      o
    })
  if (!is.null(model.parms))
    out <- c(out,list("ModelParameters"=BootCv$ModelParameters))
  
  
  if (!keep.index) splitMethod$index <- NULL
  n.risk <- N - prodlim::sindex(Y,times)
  # }}}
  # {{{ put out
  if(keep.models==TRUE){
    outmodels <- object
  } else{
    outmodels <- names(object)
    names(outmodels) <- names(object)
  }
  out <- c(out,
           list(call=theCall,
                response=model.response(m),
                time=times,
                ## ipcw.fit=as.character(ipcw$fit$call),
                n.risk=n.risk,
                models=outmodels,
                maxtime=maxtime,
                observed.maxtime=observed.maxtime,
                minmaxtime=minmaxtime,
                reference=reference,
                start=min(times),
                cens.model=cens.model,
                exact=exact,
                splitMethod=splitMethod))
  ##   if (verbose==TRUE && splitMethod$internal.name %in% c("BootCv","Boot632","Boot632plus","crossval","loocv")) cat("\n")
  class(out) <- "pec"
  out
  
  # }}}
}

checkModels <- function(object,model.args,model.parms,splitMethod,verbose=TRUE){
  checkF <- lapply(1:length(object),function(f){
    fit <- object[[f]]
    if(splitMethod != "noinf" && (match("call",names(fit),nomatch=0)==0))
      stop(paste("pec:::checkModels -> Model",names(object)[f],"does not have a call argument."),call.=FALSE)
    else fit$call$data <- NULL
  })
  
  # check model.args
  # --------------------------------------------------------------------
  if (!is.null(model.args)){
    if (!(is.list(model.args))){
      warning(paste("Argument model.args is not a list and therefore ignored." ))
      model.args <- NULL
    }
    else{
      if (!(all(match(make.names(names(model.args),unique=TRUE),names(object),nomatch=FALSE)))){
        if (verbose==TRUE)
          warning(paste("model.args should be a named list matching the entries of the object. Assume now that they are given in the correct order" ))
      }
      else{
        model.args <- model.args[names(object)]
      }
    }
  }
  
  # check model.parms
  # --------------------------------------------------------------------
  if (!is.null(model.parms)){
    if (!(is.list(model.parms))){
      warning(paste("Argument model.parms is not a list and therefore ignored." ))
      model.args <- NULL
    }
    else{
      if (!(all(match(make.names(names(model.parms),unique=TRUE),names(object),nomatch=FALSE)))){
        if (verbose==TRUE)
          warning(paste("model.parms should be a named list matching the list of model.\nIt is assumed that they are given in the correct order" ))
      }
      else{
        model.parms <- model.parms[names(object)]
      }
    }
  }
  list(model.args=model.args,model.parms=model.parms)
}

internalTalk <- function(x,y,sign="'"){
  if (y>100){
    if (y<500){
      if (x %in% seq(0,y,10)) message(x)
    }
    else{
      if (x %in% seq(0,y,100)) message(x)
    }
  }
  else{
    if (y>10){
      if (x %in% seq(0,y,10)) message(x)
    }
    else
      if (y>1)
        message(x)
  }
}

internalReevalFit <- function(object,data,step,silent=FALSE,verbose=FALSE){
  object$call$data <- data
  try2fit <- try(refit <- eval(object$call),silent=silent)
  if (inherits(try2fit,"try-error")==TRUE){
    if (verbose==TRUE)
      warning(paste("During bootstrapping: model ",class(object)," failed in step ",step),immediate.=TRUE)
    NULL
  }
  else
    refit
}
