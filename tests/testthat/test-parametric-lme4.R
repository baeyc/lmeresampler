library(lme4)
library(mlmRev)

Socatt$religion <- relevel(Socatt$religion, ref = "none")
Socatt$rv <- as.numeric(as.character(Socatt$numpos))
Socatt$rv <- scale(Socatt$rv) # a plot shows this is clearly non-normal

# ==============================================================================
context("parametric bootstrap (lmerMod)")
# ==============================================================================

mySumm <- function(.) { 
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) 
}

nsim <- 10

jsp728$class <- relevel(jsp728$class, ref = "manual")


test_that("two-level additive random intercept model",{
  skip_on_cran()
  ## See p. 31 of Goldstein's book
  vcmodA <- lmer(mathAge11 ~ mathAge8 + gender + class + 
                   (1 | school), data = jsp728)
  
  orig.stats <- mySumm(vcmodA)
  
  boo <- parametric_bootstrap.lmerMod(model = vcmodA, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "parametric")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------
test_that("two-level random intercept model without interaction",{
  skip_on_cran()
  ## See p. 97 of Goldstein's book
  rimod <- lmer(normAge11 ~ mathAge8c + gender + class + 
                  (1 | school), data = jsp728)
  
  orig.stats <- mySumm(rimod)
  boo <- parametric_bootstrap.lmerMod(model = rimod, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "parametric")
  expect_equal(boo$.f, mySumm)
})

test_that("two-level random intercept model with interaction",{
  skip_on_cran()
  ## See p. 34 of Goldstein's book
  vcmodC <- lmer(mathAge11 ~ mathAge8 * schoolMathAge8 + gender + class + 
                   (1 | school), data = jsp728)
  
  orig.stats <- mySumm(vcmodC)
  boo <- parametric_bootstrap.lmerMod(model = vcmodC, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "parametric")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("two-level random coefficient model with interaction",{
  skip_on_cran()
  ## See p. 35 of Goldstein's book
  rcmod <- lmer(mathAge11 ~ mathAge8c * schoolMathAge8 + gender + class + 
                  (mathAge8c | school), data = jsp728)
  
  orig.stats <- mySumm(rcmod)
  boo <- parametric_bootstrap.lmerMod(model = rcmod, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "parametric")
  expect_equal(boo$.f, mySumm)
})

# ------------------------------------------------------------------------------

test_that("three-level random intercept model",{
  skip_on_cran()
  rmA <- lmer(rv ~ religion + year  + (1 | respond) + (1 | district), data = Socatt)
  
  orig.stats <- mySumm(rmA)
  boo <- parametric_bootstrap.lmerMod(model = rmA, .f = mySumm, B = nsim)
  
  expect_equal(class(boo), "lmeresamp")
  expect_equal(boo$stats$observed, orig.stats)
  expect_equal(nrow(boo$replicates), nsim)
  expect_equal(ncol(boo$replicates), length(orig.stats))
  expect_equal(boo$R, nsim)
  expect_equal(boo$type, "parametric")
  expect_equal(boo$.f, mySumm)
})