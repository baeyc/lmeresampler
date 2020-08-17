library(lme4)
library(dplyr)
deer <- read.csv("http://math.carleton.edu/Chihara/Stat330/DeerEcervi.csv")
deer <- deer %>%
  mutate(Id = row_number()) 
deer$Id <- as.numeric(deer$Id)
deer.glmer3 <- glmer(formula = Ecervi ~ Sex*Length2 + (1|Farm), 
                     family = binomial, data = deer)

#' @rdname case_bootstrap
#' @export
case_bootstrap.glmerMod <- function(model, .f, B, resample, type){
  
  data <- model@frame
  # data$.id <- seq_len(nrow(data))
  clusters <- c(rev(names(lme4::getME(model, "flist"))), ".id")
  
  if(length(clusters) != length(resample))
    stop("'resample' is not the same length as the number of grouping variables. Please specify whether to resample the data at each level of grouping.")
  
  # rep.data <- purrr::map(integer(B), function(x) .cases.resamp(model = model, dat = data, cluster = clusters, resample = resample))
  tstar <- purrr::map(integer(B), function(x) .cases.resamp(model = model, .f = .f, dat = data, cluster = clusters, resample = resample))
  
  RES <- .bootstrap.completion(model, tstar, B, .f, type)
  return(RES)
}
