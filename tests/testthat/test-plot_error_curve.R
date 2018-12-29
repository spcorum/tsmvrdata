context("test-plot_error_curve")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


steps <- 100
Hat.list <- rep(list(NULL), steps)
Star <- matrix(rep(0, 9), 3, 3)
for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
}
