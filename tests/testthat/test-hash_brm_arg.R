test_that("hash_brm_arg() returns a character string", {
  h <- hash_brm_arg(gaussian())
  expect_type(h, "character")
  expect_length(h, 1)
})

test_that("hash_brm_arg() returns consistent hash for identical input", {
  h1 <- hash_brm_arg(gaussian())
  h2 <- hash_brm_arg(gaussian())
  expect_identical(h1, h2)
})

test_that("hash_brm_arg() distinguishes between different families", {
  h_gauss <- hash_brm_arg(gaussian())
  h_poiss <- hash_brm_arg(poisson())
  expect_false(h_gauss == h_poiss)
})

test_that("hash_brm_arg() works with formulas", {
  f1 <- y ~ x + z
  f2 <- y ~ x + z
  h1 <- hash_brm_arg(f1)
  h2 <- hash_brm_arg(f2)
  expect_identical(h1, h2)
})

test_that("hash_brm_arg() is environment-stable for formulas", {
  f1 <- y ~ x
  f2 <- y ~ x
  environment(f1) <- new.env()
  environment(f2) <- globalenv()
  expect_identical(hash_brm_arg(f1), hash_brm_arg(f2))
})

test_that("hash_brm_arg() works for brmsformula objects", {
  bf1 <- bf(y ~ x)
  bf2 <- bf(y ~ x)
  h1 <- hash_brm_arg(bf1)
  h2 <- hash_brm_arg(bf2)
  expect_identical(h1, h2)
})
test_that("hash_brm_arg() handles parameter-specific formulas (pforms)", {
  bf1 <- bf(y ~ x, sigma ~ z)
  bf2 <- bf(y ~ x, sigma ~ z)
  expect_identical(hash_brm_arg(bf1), hash_brm_arg(bf2))

  # Changing pform changes hash
  bf3 <- bf(y ~ x, sigma ~ z + w)
  expect_false(hash_brm_arg(bf1) == hash_brm_arg(bf3))
})

test_that("hash_brm_arg() handles nonlinear formulas and nlpars", {
  bf1 <- bf(y ~ eta, eta ~ x, nl = TRUE)
  bf2 <- bf(y ~ eta, eta ~ x, nl = TRUE)
  expect_identical(hash_brm_arg(bf1), hash_brm_arg(bf2))

  # Different nonlinear param names should give different hashes
  bf3 <- bf(y ~ theta, theta ~ x, nl = TRUE)
  expect_false(hash_brm_arg(bf1) == hash_brm_arg(bf3))
})

test_that("hash_brm_arg() is stable under repeated calls", {
  bf <- bf(y ~ x + z, sigma ~ w)
  h1 <- hash_brm_arg(bf)
  h2 <- hash_brm_arg(bf)
  expect_identical(h1, h2)
})

