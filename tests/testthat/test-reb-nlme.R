library(nlme, quietly = TRUE)
library(RLRsim)

data(Socatt, package = "mlmRev")

Socatt$religion <- relevel(Socatt$religion, ref = "none")
Socatt$rv <- as.numeric(as.character(Socatt$numpos))
Socatt$rv <- scale(Socatt$rv) # a plot shows this is clearly non-normal

# ==============================================================================
context("REB bootstrap type = 0 (lme)")
# ==============================================================================

mySumm <- function(.) { 
  c(beta = fixef(.), sigma = as.numeric(.$sigma), sig01 = as.numeric(VarCorr(.)[1,2]))
}

nsim <- 10

test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lme(mathAge11 ~ mathAge8 + gender + class,
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodA)
  
  set.seed(7142015)
  boo <- reb_bootstrap(model = vcmodA, .f = mySumm, B = nsim, reb_type = 0)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb0")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------
test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lme(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class, 
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- reb_bootstrap(model = vcmodC, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb0")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------
test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lme(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class,
               random = ~ mathAge8c | school, data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- reb_bootstrap(model = rcmod, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb0")
  expect_equal(boo$.f, mySumm)
})


# ==============================================================================
context("REB bootstrap type = 1 (lme)")
# ==============================================================================

mySumm <- function(.) { 
  c(beta = fixef(.), sigma = as.numeric(.$sigma), sig01 = as.numeric(VarCorr(.)[1,2]))
}

nsim <- 10


test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lme(mathAge11 ~ mathAge8 + gender + class,
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodA)
  
  set.seed(7142015)
  boo <- reb_bootstrap(model = vcmodA, .f = mySumm, B = nsim, reb_type = 1)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb1")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lme(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class, 
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- reb_bootstrap(model = vcmodC, .f = mySumm, B = nsim, reb_type = 1)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb1")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lme(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class,
               random = ~ mathAge8c | school, data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- reb_bootstrap(model = rcmod, .f = mySumm, B = nsim, reb_type = 1)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb1")
  expect_equal(boo$.f, mySumm)
})


# ==============================================================================
context("REB bootstrap type = 2 (lme)")
# ==============================================================================

mySumm <- function(.) {
  c(beta = nlme::fixef(.), sigma = c(diag(nlme::getVarCov(.)), .$sigma^2))
}


test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lme(mathAge11 ~ mathAge8 + gender + class,
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodA)
  
  nsim <- 10
  
  set.seed(7142015)
  boo <- reb_bootstrap(model = vcmodA, .f = mySumm, B = nsim, reb_type = 2)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb2")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lme(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class, 
                random = ~ 1 | school, data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- reb_bootstrap(model = vcmodC, .f = mySumm, B = nsim, reb_type = 2)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb2")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lme(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class,
               random = ~ mathAge8c | school, data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- reb_bootstrap(model = rcmod, B = nsim, reb_type = 2)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "reb2")
  expect_equal(boo$.f, mySumm)
})
