library(nlme)
library(boot)
library(RLRsim)

data(Socatt, package = "mlmRev")

Socatt$religion <- relevel(Socatt$religion, ref = "none")
Socatt$rv <- as.numeric(as.character(Socatt$numpos))
Socatt$rv <- scale(Socatt$rv) # a plot shows this is clearly non-normal

# ==============================================================================
context("CGR bootstrap (lme)")
# ==============================================================================

## See p. 31 of Goldstein's book
vcmodA <- lme(mathAge11 ~ mathAge8 + gender + class,
              random = ~ 1 | school, data = jsp728)


mySumm <- function(.) { 
  c(beta = fixef(.), sigma = as.numeric(.$sigma), sig01 = as.numeric(VarCorr(.)[1,2]))
}

orig.stats <- mySumm(vcmodA)

nsim <- 10

boo <- cgr_bootstrap.lme(model = vcmodA, fn = mySumm, B = nsim)

test_that("two-level additive random intercept model",{
  expect_equal(class(boo), "boot")
  expect_equal(boo$t0, orig.stats)
  expect_equal(nrow(boo$t), nsim)
  expect_equal(ncol(boo$t), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$sim, "CGR")
  expect_equal(boo$statistic, mySumm)
})

# ------------------------------------------------------------------------------
## See p. 97 of Goldstein's book
rimod <- lme(mathAge11 ~ mathAge8c + gender + class,
             random = ~ 1 | school, data = jsp728)
  
orig.stats <- mySumm(rimod)
boo <- cgr_bootstrap.lme(model = rimod, fn = mySumm, B = nsim)


test_that("two-level random intercept model without interaction",{
  expect_equal(class(boo), "boot")
  expect_equal(boo$t0, orig.stats)
  expect_equal(nrow(boo$t), nsim)
  expect_equal(ncol(boo$t), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$sim, "CGR")
  expect_equal(boo$statistic, mySumm)
})


## See p. 34 of Goldstein's book
vcmodC <- lme(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class, 
              random = ~ 1 | school, data = jsp728)

orig.stats <- mySumm(vcmodC)
boo <- cgr_bootstrap.lme(model = vcmodC, fn = mySumm, B = nsim)

test_that("two-level random intercept model with interaction",{
  expect_equal(class(boo), "boot")
  expect_equal(boo$t0, orig.stats)
  expect_equal(nrow(boo$t), nsim)
  expect_equal(ncol(boo$t), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$sim, "CGR")
  expect_equal(boo$statistic, mySumm)
})

# ------------------------------------------------------------------------------

## See p. 35 of Goldstein's book
rcmod <- lme(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class,
             random = ~ mathAge8c | school, data = jsp728)

orig.stats <- mySumm(rcmod)
boo <- cgr_bootstrap.lme(model = rcmod, fn = mySumm, B = nsim)


test_that("two-level random coefficient model with interaction",{
  expect_equal(class(boo), "boot")
  expect_equal(boo$t0, orig.stats)
  expect_equal(nrow(boo$t), nsim)
  expect_equal(ncol(boo$t), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$sim, "CGR")
  expect_equal(boo$statistic, mySumm)
})

# ------------------------------------------------------------------------------

rmA <- lme(rv ~ religion + year, random = ~ 1 | district/respond, data = Socatt)


mySumm <- function(.) { 
  c(beta = fixef(.), sigma = as.numeric(.$sigma), sig01 = as.numeric(VarCorr(.)[1,2]))
}

orig.stats <- mySumm(rmA)


boo <- cgr_bootstrap.lme(model = rmA, fn = mySumm, B = nsim)


test_that("three-level random intercept model",{
  expect_equal(class(boo), "boot")
  expect_equal(boo$t0, orig.stats)
  expect_equal(nrow(boo$t), nsim)
  expect_equal(ncol(boo$t), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$sim, "CGR")
  expect_equal(boo$statistic, mySumm)
})