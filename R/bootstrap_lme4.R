#' Bootstrapping Linear Mixed Effects Models
#'
#' \tabular{ll}{
#' Package: \tab lmeresampler\cr
#' Type: \tab Package\cr
#' Version: \tab 0.0.0\cr
#' Date: \tab 7/5/2014\cr
#' License: \tab GPLv3\cr
#' }
#'
#' This is a package to help with bootstrapping Linear Mixed Effects Models.
#'
#' @name lmeresampler
#' @docType package
#' @author Adam Loy \email{loya@lawrence.edu}
#' @author Spenser Steele \email{steeles@lawrence.edu}

library(lme4)
library(HLMdiag)
#library(roxygen)

#' @title Bootstrap for LMEs
#'
#' @description
#' \code{bootstrap} helps streamline the bootstrap process for the parametric,
#' residual, cases, CGR, and REB bootstraps.
#'
#' @details To choose a bootstrap use the \code{type} parameter.
#' For parametric use \code{"par"}, residual use \code{"res"}, cases use \code{"case"},
#' CGR use \code{"cgr"}, and REB use \code{"reb"}. The REB bootstrap has two types
#' which defaults to 1 but can be chosen using \code{"reb", "reb1", "reb2"}.
#'
#' @export
#' @param model The original model to use
#' @param fn The function the user is interested in testing
#' @param type The \code{type} of bootstrap requested, see details for types
#' @param B The number of bootstrap simulations
bootstrap <- function (model, fn, type, B){
  switch(type,
         par = parametric.lmerMod(model, fn, B),
         res = residual.lmerMod(model, fn, B),
         case = case.lmerMod(model, fn, B, extra_step = FALSE),
         cgr = cgr.lmerMod(model, fn, B),
         reb = reb.lmerMod(model, fn, B, reb_type = 0))
  # TODO: need to be able to save results
}


#' @title Parametric Bootstrap
#'
#' @description
#' The Parametric Bootstrap is uses the parametrically estimated
#' distribution function of the data to generate bootstrap samples.
#'
#' @details
#' This function extracts the fixed effects, simulates from the model, refits the model
#' and then returns the results in a list.
#'
#' @inheritParams model
#' @inheritParams fn
#' @inheritParams B
#'
#' @return list
#'
#' @references
#'   @cite Chambers:2013ba
#'   @cite vanderLeeden:208kv
parametric.lmerMod <- function(model, fn, B){
  fn <- match.fun(fn)

  model.fixef <- fixef(model) # Extract fixed effects
  ystar <- simulate(model, nsim = B, na.action = na.exclude)

  return(.bootstrap.completion(model, ystar, B, fn))

  # TODO: once we have things working, think about parallelization.
  #       using an llply statement would make this easy with the .parallel
  #       parameter, but it might be slower than using mclapply, which is
  #       found in the parallel package.
}


#' @title Residual Bootstrap
#'
#' @description
#' The Residual Bootstrap uses residuals to generate bootstrap samples.
#'
#' @details
#' This function extracts the Xbetas, random effects, residuals, and Z
#' design matrix in order to resample the residuals and complete the
#' bootstrap process.
#'
#' @inheritParams model
#' @inheritParams fn
#' @inheritParams B
#'
#' @return list
#'
#' @references
#'   @cite vanderLeeden:208kv
residual.lmerMod <- function (model, fn, B){
  fn <- match.fun(fn)
  
  ystar <- as.data.frame( replicate(n = B, .resample.resids(model = model)) )

  return(.bootstrap.completion(model, ystar, B, fn))
}

#' @title Cases Bootstrap
#'
#' @description
#' The Cases Bootstrap samples entire cases to generate the bootstrap.
#'
#' @details
#'
#' @param extra_step add the extra step
#' @inheritParams model
#' @inheritParams fn
#' @inheritParams B
#'
#' @return list
#'
#' @references
#'   @cite vanderLeeden:208kv
case.lmerMod <- function (model, fn, B, extra_step = FALSE){
  # TODO: put everything below into lapply to replicate
  .cases.resamp <- function (model, extra_step){
  # Draw sample of size J from level-2 units
  model.split <- split(x=model@frame, f=model@flist)
  model.split.samp <- sample(x=model.split, size = length(model.split), replace = TRUE)
  # For each sample, draw a sample of the cases from the level-2 unit
  
  if(extra_step == TRUE){
    model.resamp <- lapply(model.split.samp,
                           FUN = function(x) {
                             J <- nrow(x)
                             
                             # Sample of level-2 rows
                             model.sub.index <- sample(x = seq_len(J), size = J, replace = TRUE)
                             resampled <- x[model.sub.index,]
                             return(resampled)
                           })
    model.comb <- do.call('rbind', model.resamp)
  } else{ # else statement needs to be located here
    model.comb <- do.call('rbind', model.split.samp)
  }
  }
  
  # DEPRECATED rep.data <- as.data.frame( replicate(n = B, .cases.resamp(model = model, extra_step = extra_step)) )
  rep.data <- lapply(integer(B), eval.parent(substitute(function(...) .cases.resamp(model = model, extra_step = extra_step))))
  
  
  .cases.completion <- function(model, data, B, fn){
    t0 <- fn(model)
    
    # Refit the model and apply 'fn' to it using lapply
    form <- model@call$formula
    reml <- isREML(model)
    tstar <- lapply(data, function(x) {
      fn(lmer(formula = form, data = x, REML = reml)) 
    })
    
    tstar <- do.call("cbind", tstar) # Can these be nested?
    rownames(tstar) <- names(t0)
    
    RES <- structure(list(t0 = t0, t = t(tstar), R = B, data = model@frame,
                          seed = .Random.seed, statistic = fn,
                          sim = "parametric", call = match.call()),
                     class = "boot")
    
    return(RES)
  }
  
  # Plugin to .cases.completion due to small changes
  return(.cases.completion(model, rep.data, B, fn))
}

#' @title CGR Bootstrap
#'
#' @description
#' 
#'
#' @details
#'
#' @inheritParams model
#' @inheritParams fn
#' @inheritParams B
#'
#' @return list
#'
#' @references
#'   @cite Chambers:2013ba
cgr.lmerMod <- function (model, fn, B){
  fn <- match.fun(fn)
  # Extract random effects
  
  ystar <- as.data.frame( replicate(n = B, .resample.cgr(model = model)) )
  
  return(.bootstrap.completion(model, ystar, B, fn))
  
}


#' @title REB Bootstrap
#'
#' @description
#' 
#'
#' @details
#'
#' @inheritParams model
#' @inheritParams fn
#' @inheritParams B
#' @param reb_type Chooses the type of REB bootstrap
#'
#' @return list
#'
#' @references
#'   @cite 
reb.lmerMod <- function (model, fn, B, reb_type = 0){
  
  fn <- match.fun(fn)
  
  ystar <- as.data.frame( replicate(n = B, .resample.reb(model = model, reb_type = reb_type)) )
  
  t0 <- fn(model)
  # Refit the model and apply 'fn' to it using lapply
  tstar <- lapply(ystar[1,], function(x) {
    fn(refit(object = model, newresp = x))
  })
  
  tstar <- do.call("cbind", tstar) # Can these be nested?
  rownames(tstar) <- names(fn(model))
  u.vec <- as.numeric(t(ystar[2,]))
  e.vec <- as.numeric(t(ystar[3,]))
  ue.mat <- matrix(c(u.vec, e.vec), ncol = 2)
  #POST
  
  # Used JCGS code because it works
  Sb <- as.matrix(ue.mat)
  Mb <- apply(Sb,2,mean)
  CovSb <- cov(Sb)
  SdSb <- sqrt(diag(CovSb))
  EW <- eigen(solve(CovSb),symmetric=T)
  Whalf <- EW$vectors%*%diag(sqrt(EW$values))
  Sm <- cbind(rep(Mb[1],B),rep(Mb[2],B))
  Sbmod <- (Sb-Sm)%*%Whalf
  Sbmod[,1] <- Sbmod[,1]*SdSb[1]
  Sbmod[,2] <- Sbmod[,2]*SdSb[2]
  Lb <- exp(Sm+Sbmod)
  
  
  RES <- structure(list(t0 = t0, t = t(tstar), R = B, data = model@frame,
                        seed = .Random.seed, statistic = fn,
                        sim = "parametric", call = match.call(), reb2 = Lb),
                   class = "boot")
  
  return(RES)
}


#####################
# Utility Functions #
#####################

#' @title Zbstar combine
#'
#' @description
#' Combine \code{bstar} and \code{zstar} to create {Zbstar}.
#'
#' @details
#' This function combines \code{bstar} and \code{zstar} to create {Zbstar} using an lapply statement.
#'
#' @param bstar A list of matrices bstar
#' @param zstar A list of matrices zstar
#'
#' @return matrix
.Zbstar.combine <- function(bstar, zstar){
  lapply(1:length(bstar), function(i){
    t(zstar[[i]]) %*% bstar[[i]]
  })
}


#' @title Bootstrap Completion
#'
#' @description
#' Finishes the bootstrap process and makes the output readable.
#'
#' @details
#' This function is given \code{model, ystar, B, fn} and uses them to complete
#' the bootstrap process. They are then structured into a list for output and returned.
#'
#' @param ystar The ystar being passed in
#' @inheritParams model
#' @inheritParams B
#' @inheritParams fn
#'
#' @return list
.bootstrap.completion <- function(model, ystar, B, fn){
  t0 <- fn(model)

  # Refit the model and apply 'fn' to it using lapply
  tstar <- lapply(ystar, function(x) {
    fn(refit(object = model, newresp = x))
  })

  tstar <- do.call("cbind", tstar) # Can these be nested?
  rownames(tstar) <- names(t0)
  
  if ((nfail <- sum(apply(is.na(tstar), 2, all))) > 0) {
    warning("some bootstrap runs failed (", numFail, "/", nsim, ")")
  }

  RES <- structure(list(t0 = t0, t = t(tstar), R = B, data = model@frame,
                        seed = .Random.seed, statistic = fn,
                        sim = "parametric", call = match.call()),
                   class = "boot")

  return(RES)
}

#' CGR resampling procedures
#' 
#'
#' @inheritParams model
.resample.cgr <- function(model){
  model.ranef <- ranef(model)
  
  # Extract residuals
  model.resid <- resid(model)
  
  
  # Higher levels
  Uhat.list <- lapply(seq_along(model.ranef),
                      FUN = function(i) {
                        u <- scale(model.ranef[[i]], scale = FALSE)
                        S <- (t(u) %*% u) / length(u)
                        
                        re.name <- names(model.ranef)[i]
                        R <- bdiag(VarCorr(model)[[names(model.ranef)[i]]])
                        
                        Ls <- chol(S, pivot = TRUE)
                        Lr <- chol(R, pivot = TRUE)
                        A <- t(Lr %*% solve(Ls))
                        
                        Uhat <- as.matrix(u %*% A)
                        Uhat <- as.data.frame(Uhat)
                        
                        return(Uhat)
                      })  
  names(Uhat.list) <- names(model.ranef)
  
  # Level 1
  e <- as.numeric(scale(model.resid, scale = FALSE))
  sigma <- sigma(model)
  ehat <- sigma*e*((t(e)%*%e)/length(e))^(-1/2)
  
  # Extract Z design matrix
  Z <- getME(object = model, name = "Ztlist")
  
  
  Xbeta <- predict(model, re.form = NA)
  
  level.num <- getME(object = model, name = "n_rfacs")
  
  # Resample Uhat
  ustar <- lapply(Uhat.list,
                  FUN = function(df) {
                    index <- sample(x = seq_len(nrow(df)), size = nrow(df), replace = TRUE)
                    return(df[index,])
                  })
  
  # Structure u*
  if(level.num == 1){
    if(is.data.frame(ustar[[1]])){
      ustar <- lapply(ustar, FUN = function(x) as.list(x))[[1]] 
    }
    names(ustar) <- names(Z)
  } else {
    ustar <- lapply(ustar, FUN = function(x) as.data.frame(x))
    ustar <- do.call(c, ustar)
    names(ustar) <- names(Z)
  }
  
  # Get Zb*
  Zbstar <- .Zbstar.combine(bstar = ustar, zstar = Z)
  Zbstar.sum <- Reduce("+", Zbstar)
  
  # Get e*
  estar <- sample(x = ehat, size = length(ehat), replace = TRUE)
  
  # Combine
  y.star <- as.numeric(Xbeta + Zbstar.sum + estar)
  
  return(y.star)
}

#' Resampling residuals from mixed models
#'
#' @inheritParams model
#' 
.resample.resids <- function(model){
  
  # Extract fixed part of the model
  Xbeta <- predict(model, re.form = NA) # This is X %*% fixef(model)
  
  # Extract random effects
  model.ranef <- ranef(model)
  
  # Extract residuals
  model.resid <- resid(model)
  
  # Extract Z design matrix
  Z <- getME(object = model, name = "Ztlist")
  
  bstar <- lapply(model.ranef,
                  FUN = function(x) {
                    J <- nrow(x)
                    
                    # Sample of b*
                    bstar.index <- sample(x = seq_len(J), size = J, replace = TRUE)
                    bstar <- x[bstar.index,]
                    return(bstar)
                  })
  
  level.num <- getME(object = model, name = "n_rfacs")
  
  if(level.num == 1){
    bstar <- lapply(bstar, FUN = function(x) as.list(x))[[1]]
    names(bstar) <- names(Z)
  } else {
    bstar <- lapply(bstar, FUN = function(x) as.data.frame(x))
    bstar <- do.call(c, bstar)
    names(bstar) <- names(Z)
  }
  
  # Get Zb*
  Zbstar <- .Zbstar.combine(bstar = bstar, zstar = Z)
  Zbstar.sum <- Reduce("+", Zbstar)
  
  
  # Resample residuals
  estar <- sample(x = model.resid, size = length(model.resid), replace = TRUE)
  
  # Combine function
  y.star <- as.numeric(Xbeta + Zbstar.sum + estar)
  
  return(y.star)
  
}

#' REB resampling procedures
#' #'
#' @param reb_type Specifies the inclusion of REB/1
#' @inheritParams model
.resample.reb <- function(model, reb_type){
  # use HLMresid to extract marginal residuals
  model.mresid <- HLMresid(object = model, type = "EB", level = "marginal")
  
  # Extract Z design matrix
  Z <- getME(object = model, name = "Z")
  
  # level 2 resid
  u <- solve(t(Z) %*% Z) %*% t(Z) %*% model.mresid
  # level 1 resid
  e <- model.mresid - Z %*% u
  if(reb_type == 1){
    #PRE
    
    
    # Calculations
    
    S <- (t(u)%*%u)/length(u)
    R <- bdiag(VarCorr(model))
    Ls <- chol(S, pivot = TRUE)
    Lr <- chol(R, pivot = TRUE)
    A <- t(Lr%*%solve(Ls))
    
    Uhat <- u%*%A
    
    sigma <- sigma(model)
    estar <- sigma * e %*% ((t(e) %*% e) / length(e))^(-1/2)
    
    # center
    estar <- scale(estar, scale = FALSE) # faster than the for loop
    Uhat <- scale(Uhat, scale = FALSE) 
    
  } else{
    Uhat <- u
    estar <- e
  }
  
  Xbeta <- predict(model, re.form = NA)
  
  # resample uhats
  
  Uhat <- as.data.frame(as.matrix(Uhat))
  Uhat.list <- list(Uhat)
  
  level.num <- getME(object = model, name = "n_rfacs")
  
  # Extract Z design matrix separated by variance
  Ztlist <- getME(object = model, name = "Ztlist")
  
  if(level.num == 1){
    Uhat.list <- lapply(Uhat.list, FUN = function(x) as.list(x))[[1]]
    names(Uhat.list) <- names(Ztlist)
  } else {
    Uhat.list <- sapply(Uhat.list, FUN = function(x) as.list(x))
  }
  
  # Resample Uhat
  ustar <- sample(x = Uhat.list[[1]], size = length(Uhat.list[[1]]), replace = TRUE)
  
  # Get Zb*
  Zbstar <- .Zbstar.combine(bstar = as.data.frame(ustar), zstar = Ztlist)
  Zbstar.sum <- Reduce("+", Zbstar)
  
  # Resample residuals
  estar <- sample(x = model.mresid, size = length(model.mresid), replace = TRUE)
  
  # Combine function
  y.star <- as.numeric(Xbeta + Zbstar.sum + estar)
  
  # this is going to be a crude workaround
  u.lvar <- log(var(ustar))
  e.lvar <- log(var(estar))
  
  test.return <- structure(list(ystar = y.star, u = u.lvar, e = e.lvar))
  
  return(test.return)
}