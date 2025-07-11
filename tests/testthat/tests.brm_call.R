test_that("create_brm_call and call_only works", {
  # `call_only = T` creates a brm_call
  c1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
                data = epilepsy, family = poisson(), call_only = TRUE, file_auto = TRUE)
  # alternatively create_brm_call creates a brm_call without redefining parameters
  c2  <- create_brm_call(count ~ zAge + zBase * Trt + (1|patient),
                             data = epilepsy, family = poisson(), file_auto = TRUE)


  expect_equal(all.equal(c1 , c2), TRUE)

})

test_that("brm_call objects are consistent", {
  call_1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
                data = epilepsy, family = poisson(), call_only = T)
  call_2  <- create_brm_call(count ~ zBase * Trt + (1|patient),
                             data = epilepsy, family = poisson())
  expect_no_error(capture.output(print(call_1)))
  expect_no_error(capture.output(print(call_2)))
  expect_true(as.character(call_1$formula)[[3]] != as.character(call_2$formula)[[3]])
})

test_that("brm_call objects are consistent", {
  call_1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
                data = epilepsy, family = poisson(), call_only = T)
  call_2 <- brm(count ~ zAge + zBase * Trt + (1|patient),
                data = epilepsy, family = poisson(), call_only = T)
  expect_no_error(capture.output(print(call_1)))
  expect_no_error(capture.output(print(call_2)))
  expect_true(as.character(call_1$formula)[[3]] == as.character(call_2$formula)[[3]])
})

