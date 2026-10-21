# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

test_that("Test the Sigma Matrix Computation for MAMSMEP(Parametric) designs", {
  # Test Case:1 Two-Stage Multi-Arm with one Endpoint
  # Arms: 3, Eps: 1, Hypothesis: 2
  CommonStdDev <- FALSE
  EpType <- list("EP1" = "Continuous")
  sigma <- list("EP1" = c(1, 1, 1))
  prop.ctr <- NA
  allocRatio <- c(1, 1, 1)
  maxLambda <- max((sigma$EP1[1]^2 + sigma$EP1[-1]^2 / allocRatio[-1])^-1)
  SS_Cum <- matrix(c(
    50, 50, 50,
    100, 100, 100
  ), nrow = 2, byrow = T)
  info_frac <- c(0.5, 1)

  out <- getSigma(EpType = EpType,
                  SS_Cum = SS_Cum,
                  prop.ctr =  prop.ctr,
                  sigma = sigma,
                  allocRatio = allocRatio,
                  CommonStdDev = CommonStdDev,
                  info_frac = info_frac)
  varZ <- diag(out$SigmaZ$EP1)
  names(varZ) <- NULL
  # Test Varience of Z stat is 1
  expect_equal(object = varZ, expected = rep(1, 4))

  informations <- out$InfoMatrix
  maxInfo <- max(unlist(informations))
  # Test Max Info
  expect_equal(object = maxInfo, expected = maxLambda * 100)

  # Score Scale Sigma Benchmark
  benchmarkSigmaS <- matrix(c(
    25.0, 12.5, 25.0, 12.5,
    12.5, 25.0, 12.5, 25.0,
    25.0, 12.5, 50.0, 25.0,
    12.5, 25.0, 25.0, 50.0
  ), nrow = 4, byrow = T)
  compSigmaS <- matrix(out$SigmaS$EP1, nrow = 4)
  expect_equal(object = compSigmaS, expected = benchmarkSigmaS)
})

test_that("getSigma uses treatment allocation ratios for unequal allocations", {
  vAllocRatio <- c(
    Control = 1,
    Treatment1 = 2,
    Treatment2 = 3,
    Treatment3 = 4
  )
  mCumulativeSampleSize <- matrix(
    c(
      100, 200, 300, 400,
      200, 400, 600, 800
    ),
    nrow = 2,
    byrow = TRUE
  )
  lSigma <- getSigma(
    EpType = list("EP1" = "Continuous"),
    SS_Cum = mCumulativeSampleSize,
    sigma = list("EP1" = rep(1, 4)),
    prop.ctr = NA,
    allocRatio = vAllocRatio,
    CommonStdDev = FALSE,
    info_frac = c(0.5, 1)
  )

  dExpectedCovariance <- sqrt(
    (vAllocRatio["Treatment2"] / (1 + vAllocRatio["Treatment2"])) *
      (vAllocRatio["Treatment3"] / (1 + vAllocRatio["Treatment3"]))
  )

  # Treatment indices 2 and 3 must use Treatment2 and Treatment3, not prior arms.
  expect_equal(
    object = lSigma$SigmaZ$EP1["Z21", "Z31"],
    expected = unname(dExpectedCovariance)
  )
})

# Test provided by Benjamin Fallmann<benjamin.fallmann@meduniwien.ac.at> while debugging issue #19
test_that("Test the Sigma Matrix Computation for MAMSMEP(Parametric) designs, with additional arms", {
  # Test Case:1 Two-Stage Multi-Arm with one Endpoint
  # Arms: 5, Eps: 1, Hypothesis: 4
  CommonStdDev <- FALSE
  EpType <- list("EP1" = "Continuous")
  sigma <- list("EP1" = c(1, 1, 1, 1, 1))
  prop.ctr <- NA
  allocRatio <- c(1, 1, 1, 1, 1)
  maxLambda <- max((sigma$EP1[1]^2 + sigma$EP1[-1]^2 / allocRatio[-1])^-1)
  SS_Cum <- matrix(
    c(50, 50, 50, 50, 50, 100, 100, 100, 100, 100),
    nrow = 2,
    byrow = T
  )
  info_frac <- c(0.5, 1)

  out <- getSigma(
    EpType = EpType,
    SS_Cum = SS_Cum,
    prop.ctr = prop.ctr,
    sigma = sigma,
    allocRatio = allocRatio,
    CommonStdDev = CommonStdDev,
    info_frac = info_frac
  )
  varZ <- diag(out$SigmaZ$EP1)
  names(varZ) <- NULL
  # Test Varience of Z stat is 1
  expect_equal(object = varZ, expected = rep(1, 8))

  informations <- out$InfoMatrix
  maxInfo <- max(unlist(informations))
  # Test Max Info
  expect_equal(object = maxInfo, expected = maxLambda * 100)

  # Score Scale Sigma Benchmark
  #fmt: skip
  benchmarkSigmaS <- matrix(c(
    25.0, 12.5, 12.5, 12.5, 25.0, 12.5, 12.5, 12.5,
    12.5, 25.0, 12.5, 12.5, 12.5, 25.0, 12.5, 12.5,
    12.5, 12.5, 25.0, 12.5, 12.5, 12.5, 25.0, 12.5,
    12.5, 12.5, 12.5, 25.0, 12.5, 12.5, 12.5, 25.0,
    25.0, 12.5, 12.5, 12.5, 50.0, 25.0, 25.0, 25.0,
    12.5, 25.0, 12.5, 12.5, 25.0, 50.0, 25.0, 25.0,
    12.5, 12.5, 25.0, 12.5, 25.0, 25.0, 50.0, 25.0,
    12.5, 12.5, 12.5, 25.0, 25.0, 25.0, 25.0, 50.0
  ), nrow = 8, byrow = T)
  compSigmaS <- matrix(out$SigmaS$EP1, nrow = 8)
  expect_equal(object = compSigmaS, expected = benchmarkSigmaS)
})
