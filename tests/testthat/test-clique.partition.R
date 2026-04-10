# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

test_that("clique.partition: single node returns list(1)", {
  m <- matrix(1, nrow = 1, ncol = 1)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(result, list(1))
})

test_that("clique.partition: two nodes with known correlation form one clique", {
  m <- matrix(c(1, 0.7,
                0.7, 1), nrow = 2, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(1L, 2L))
})

test_that("clique.partition: two nodes with unknown correlation form two singletons", {
  m <- matrix(c(1, NA,
                NA, 1), nrow = 2, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(length(result), 2)
  expect_equal(sort(unlist(result)), c(1L, 2L))
  expect_true(all(sapply(result, length) == 1))
})

test_that("clique.partition: fully known 3x3 forms one clique", {
  m <- matrix(c(1,   0.5, 0.3,
                0.5, 1,   0.4,
                0.3, 0.4, 1), nrow = 3, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(1L, 2L, 3L))
})

test_that("clique.partition: all-unknown 3x3 forms three singletons", {
  m <- matrix(c(1,  NA, NA,
                NA, 1,  NA,
                NA, NA, 1), nrow = 3, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(length(result), 3)
  expect_equal(sort(unlist(result)), c(1L, 2L, 3L))
  expect_true(all(sapply(result, length) == 1))
})

test_that("clique.partition: block-diagonal 4x4 forms two disjoint cliques", {
  # H1-H2 know each other; H3-H4 know each other; cross-block correlations unknown
  m <- matrix(c(1,   0.5, NA,  NA,
                0.5, 1,   NA,  NA,
                NA,  NA,  1,   0.3,
                NA,  NA,  0.3, 1), nrow = 4, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  expect_equal(length(result), 2)
  # All indices covered exactly once
  expect_equal(sort(unlist(result)), c(1L, 2L, 3L, 4L))
  # Each clique is a pair
  expect_true(all(sapply(result, length) == 2))
  # The two cliques are {1,2} and {3,4}
  clique.sets <- lapply(result, sort)
  expect_true(list(c(1L, 2L)) %in% clique.sets)
  expect_true(list(c(3L, 4L)) %in% clique.sets)
})

test_that("clique.partition: motivating population-enrichment 4x4 example", {
  # (1,4) and (2,3) correlations are unknown — conn.comp would merge all 4 into one group,
  # but clique.partition must not, since pmvnorm cannot handle NA entries.
  m <- matrix(c(1,   0.5, 0.5, NA,
                0.5, 1,   NA,  0.5,
                0.5, NA,  1,   0.5,
                NA,  0.5, 0.5, 1), nrow = 4, byrow = TRUE)
  result <- AdaptGMCP:::clique.partition(m)
  # Must NOT produce a single group of all 4 (that would fail pmvnorm)
  expect_false(length(result) == 1)
  # All indices covered exactly once
  expect_equal(sort(unlist(result)), c(1L, 2L, 3L, 4L))
  # Every pair within each clique must have a known (non-NA) correlation
  for (clique in result) {
    if (length(clique) > 1) {
      pairs <- combn(clique, 2)
      for (col.idx in seq_len(ncol(pairs))) {
        i <- pairs[1, col.idx]
        j <- pairs[2, col.idx]
        expect_false(is.na(m[i, j]),
          info = paste("NA correlation found within clique for pair", i, "-", j))
      }
    }
  }
})

test_that("clique.partition: partition covers all hypotheses with no duplicates", {
  # Property-based check across a few different matrix sizes
  check.coverage <- function(m) {
    result <- AdaptGMCP:::clique.partition(m)
    all.indices <- sort(unlist(result))
    n <- ncol(m)
    # All indices 1:n present
    expect_equal(all.indices, seq_len(n))
    # No duplicates
    expect_equal(length(all.indices), n)
  }

  # 5 nodes, arbitrary NAs
  m5 <- matrix(c(1,   0.4, NA,  0.3, NA,
                 0.4, 1,   0.5, NA,  0.2,
                 NA,  0.5, 1,   0.6, NA,
                 0.3, NA,  0.6, 1,   0.4,
                 NA,  0.2, NA,  0.4, 1), nrow = 5, byrow = TRUE)
  check.coverage(m5)

  # 2 nodes, all known
  check.coverage(matrix(c(1, 0.9, 0.9, 1), nrow = 2))

  # 3 nodes, all unknown
  check.coverage(matrix(c(1, NA, NA, NA, 1, NA, NA, NA, 1), nrow = 3))
})

# --------------------------------------------------------------------------------------------------
# Comparison: conn.comp() vs clique.partition()
#
# conn.comp() groups by connected components — a node is reachable if ANY known correlation
# path connects it. This can produce groups containing NA pairs, which would break pmvnorm.
# clique.partition() guarantees every pair within a group has a known (non-NA) correlation.
# --------------------------------------------------------------------------------------------------

test_that("conn.comp vs clique.partition: agree on fully-known correlation matrix", {
  # When no NAs are present both functions should cover the same set of indices.
  # conn.comp returns one connected component; clique.partition returns one clique.
  m <- matrix(c(1,   0.5, 0.3,
                0.5, 1,   0.4,
                0.3, 0.4, 1), nrow = 3, byrow = TRUE)

  cc <- AdaptGMCP:::conn.comp(m)
  cp <- AdaptGMCP:::clique.partition(m)

  expect_equal(length(cc), 1)
  expect_equal(length(cp), 1)
  expect_equal(sort(as.integer(cc[[1]])), sort(as.integer(cp[[1]])))
})

test_that("conn.comp vs clique.partition: agree on fully-zero (block-independent) matrix", {
  # Hypotheses with zero known correlations: each is its own group in both functions.
  m <- matrix(c(1,  NA, NA,
                NA, 1,  NA,
                NA, NA, 1), nrow = 3, byrow = TRUE)

  cc <- AdaptGMCP:::conn.comp(m)
  cp <- AdaptGMCP:::clique.partition(m)

  expect_equal(length(cc), 3)
  expect_equal(length(cp), 3)
  expect_equal(sort(unlist(cc)), sort(unlist(cp)))
})

test_that("conn.comp vs clique.partition: agree on clean block-diagonal matrix", {
  # Two independent blocks with no cross-block correlations —
  # both algorithms should produce the same two groups.
  m <- matrix(c(1,   0.6, NA,  NA,
                0.6, 1,   NA,  NA,
                NA,  NA,  1,   0.4,
                NA,  NA,  0.4, 1), nrow = 4, byrow = TRUE)

  cc <- AdaptGMCP:::conn.comp(m)
  cp <- AdaptGMCP:::clique.partition(m)

  expect_equal(length(cc), 2)
  expect_equal(length(cp), 2)
  # Same index sets (order may differ)
  # Convert to character keys for type-agnostic set comparison
  # (conn.comp accumulates into numeric(0), clique.partition produces integer vectors)
  sets.to.keys <- function(partition) sort(sapply(partition, function(g) paste(sort(g), collapse = "-")))
  expect_equal(sets.to.keys(cc), sets.to.keys(cp))
})

test_that("conn.comp vs clique.partition: DIVERGE on population-enrichment matrix", {
  # This is the motivating example from the code comments.
  # Correlations between H1-H4 and H2-H3 are unknown (NA).
  # conn.comp sees every node reachable via some known-correlation path and merges all 4.
  # clique.partition refuses to put pairs with unknown correlations in the same group.
  m <- matrix(c(1,   0.5, 0.5, NA,
                0.5, 1,   NA,  0.5,
                0.5, NA,  1,   0.5,
                NA,  0.5, 0.5, 1), nrow = 4, byrow = TRUE)

  cc <- AdaptGMCP:::conn.comp(m)
  cp <- AdaptGMCP:::clique.partition(m)

  # conn.comp merges all 4 into one connected component
  expect_equal(length(cc), 1)
  expect_equal(sort(cc[[1]]), c(1L, 2L, 3L, 4L))

  # clique.partition splits them into smaller safe groups
  expect_true(length(cp) > 1)

  # clique.partition result is safe: no NA pair within any clique
  for (clique in cp) {
    if (length(clique) > 1) {
      pairs <- combn(clique, 2)
      for (col.idx in seq_len(ncol(pairs))) {
        i <- pairs[1, col.idx]
        j <- pairs[2, col.idx]
        expect_false(is.na(m[i, j]),
          info = paste("NA correlation found in clique.partition group for pair", i, "-", j))
      }
    }
  }

  # conn.comp result is UNSAFE: the single group of 4 contains NA pairs
  unsafe <- FALSE
  if (length(cc[[1]]) > 1) {
    pairs <- combn(cc[[1]], 2)
    for (col.idx in seq_len(ncol(pairs))) {
      i <- pairs[1, col.idx]
      j <- pairs[2, col.idx]
      if (is.na(m[i, j])) unsafe <- TRUE
    }
  }
  expect_true(unsafe)
})

test_that("conn.comp vs clique.partition: DIVERGE on chain-connected matrix", {
  # H1-H2 and H2-H3 have known correlations, but H1-H3 is unknown.
  # conn.comp sees H1, H2, H3 as one connected component via H2.
  # clique.partition will not put H1 and H3 in the same clique since their
  # correlation is unknown.
  m <- matrix(c(1,   0.6, NA,
                0.6, 1,   0.5,
                NA,  0.5, 1), nrow = 3, byrow = TRUE)

  cc <- conn.comp(m)
  cp <- clique.partition(m)

  # conn.comp: one component covering all 3
  expect_equal(length(cc), 1)
  expect_equal(sort(cc[[1]]), c(1L, 2L, 3L))

  # clique.partition: H1 and H3 cannot share a group; must produce >= 2 groups
  expect_true(length(cp) >= 2)

  # Verify safety of clique.partition output
  for (clique in cp) {
    if (length(clique) > 1) {
      pairs <- combn(clique, 2)
      for (col.idx in seq_len(ncol(pairs))) {
        i <- pairs[1, col.idx]
        j <- pairs[2, col.idx]
        expect_false(is.na(m[i, j]),
          info = paste("NA correlation found in clique.partition group for pair", i, "-", j))
      }
    }
  }
})
