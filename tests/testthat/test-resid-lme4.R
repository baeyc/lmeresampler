library(lme4)
library(mlmRev)

Socatt$religion <- relevel(Socatt$religion, ref = "none")
Socatt$rv <- as.numeric(as.character(Socatt$numpos))
Socatt$rv <- scale(Socatt$rv) # a plot shows this is clearly non-normal

# ==============================================================================
context("residual bootstrap (lmerMod) linked = FALSE")
# ==============================================================================

mySumm <- function(.) { 
  s <- lme4::getME(., "sigma")
  c(beta = lme4::getME(., "beta"), sigma = s, sig01 = unname(s * lme4::getME(., "theta"))) 
}

nsim <- 10

test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lme4::lmer(mathAge11 ~ mathAge8 + gender + class + 
                         (1 | school), data = jsp728)
  
  
  orig.stats <- mySumm(vcmodA)
  
  set.seed(7142015)
  boo <- resid_bootstrap(model = vcmodA, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random intercept model without interaction",{
  skip_on_cran()
  ## See p. 97 of Goldstein's book
  rimod <- lmer(normAge11 ~ mathAge8c + gender + class + 
                  (1 | school), data = jsp728)
  
  orig.stats <- mySumm(rimod)
  boo <- resid_bootstrap(model = rimod, .f = mySumm, B = nsim)
  
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})


test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lmer(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class + 
                   (1 | school), data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- resid_bootstrap(model = vcmodC, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lme4::lmer(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class + 
                        (mathAge8c | school), data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- resid_bootstrap(model = rcmod, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("three-level random intercept model",{
  skip_on_cran()
  rmA <- lmer(rv ~ religion + year  + (1 | respond) + (1 | district), data = Socatt)
  
  orig.stats <- mySumm(rmA)
  boo <- resid_bootstrap(model = rmA, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ==============================================================================
context("residual bootstrap (lmerMod) linked = TRUE")
# ==============================================================================

mySumm <- function(.) { 
  s <- lme4::getME(., "sigma")
  c(beta = lme4::getME(., "beta"), sigma = s, sig01 = unname(s * lme4::getME(., "theta"))) 
}

nsim <- 10

test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lme4::lmer(mathAge11 ~ mathAge8 + gender + class + 
                         (1 | school), data = jsp728)
  
  
  orig.stats <- mySumm(vcmodA)
  
  set.seed(7142015)
  boo <- resid_bootstrap(model = vcmodA, .f = mySumm, B = nsim, linked = TRUE)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random intercept model without interaction",{
  skip_on_cran()
  ## See p. 97 of Goldstein's book
  rimod <- lmer(normAge11 ~ mathAge8c + gender + class + 
                  (1 | school), data = jsp728)
  
  orig.stats <- mySumm(rimod)
  boo <- resid_bootstrap(model = rimod, .f = mySumm, B = nsim, linked = TRUE)
  
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})


test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lmer(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class + 
                   (1 | school), data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- resid_bootstrap(model = vcmodC, .f = mySumm, B = nsim, linked = TRUE)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lme4::lmer(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class + 
                        (mathAge8c | school), data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- resid_bootstrap(model = rcmod, .f = mySumm, B = nsim, linked = TRUE)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("three-level random intercept model",{
  skip_on_cran()
  rmA <- lmer(rv ~ religion + year  + (1 | respond) + (1 | district), data = Socatt)
  
  orig.stats <- mySumm(rmA)
  boo <- resid_bootstrap(model = rmA, .f = mySumm, B = nsim, linked = TRUE)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "residual")
  expect_equal(boo$.f, mySumm)
})

