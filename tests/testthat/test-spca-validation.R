test_that("spca rejects invalid inputs", {
  m <- spca_test_matrices()

  expect_error(spca("not a matrix"), "M must be a matrix or data.frame")
  expect_error(spca(matrix(letters[1:4], 2, 2)), "M must be numeric")
  expect_error(spca(replace(m$X_tall, 1, NA_real_)), "M must not contain missing values")
  

  expect_error(spca(m$X_tall, alpha = 0), "alpha must be between 0 and 1")
  expect_error(spca(m$X_tall, alpha = 1.1), "alpha must be between 0 and 1")
  expect_error(spca(m$X_tall, ncomps = -1), "ncomps must be a nonnegative scalar")
  expect_error(spca(m$X_tall, ncomp_byvexp = 1.2),
               "ncomp_byvexp must be FALSE or a number in")

  expect_error(spca(m$X_tall, method = "bad"),
               "method must start with one of u, c, or p")
  expect_error(spca(m$X_tall, var_selection = "xxx"),
               "var_selection must start with one of f, b, or s")
  expect_error(spca(m$X_tall, stop_criterion = "bad"),
               "stop_criterion must start with one of r or c")

  expect_error(spca(m$X_tall, intensive = NA))
  
  expect_error(spca(m$X_tall, fat_matrix = NA))
  expect_error(spca(m$X_tall, singleprecision = NA))

  expect_error(spca(m$X_tall, fixedindex_list = 1),
               "fixedindex_list must be a list")
  expect_error(spca(m$X_tall, fixedindex_list = list(c(1, NA))),
               "fixedindex_list cannot contain missing indices")

  expect_error(spca(m$X_tall, epsPMC = 0), "epsPMC must be a positive scalar")
  expect_error(spca(m$X_tall, maxiterPMC = 0), "maxiterPMC must be a positive scalar")
  expect_error(spca(m$X_tall, epsPMVS = 0), "epsPMVS must be a positive scalar")
  expect_error(spca(m$X_tall, maxiterPMVS = 0), "maxiterPMVS must be a positive scalar")
})
