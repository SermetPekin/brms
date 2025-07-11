test_that("create_filename_auto works", {
  c1 <-   brm(count ~  zAge + zBase * Trt + (1|patient),
              data = epilepsy, family = poisson(), call_only = TRUE, file_auto = TRUE)
  formula1 <- count ~  zAge + zBase * Trt + (1|patient)
  formula2 <- count ~  zAge + zBase * Trt
  c2 <- c1
  c3 <- c1
  c2$formula  <- formula2
  a <- create_filename_auto(c1)
  b <- create_filename_auto(c2)
  c <- create_filename_auto(c3)
  expect_false({ a== b  })
  expect_true(a == c)
})