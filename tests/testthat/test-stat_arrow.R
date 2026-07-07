test_that("overlapping arrows are stacked by conflict level", {
  source(file.path("..", "..", "R", "features.R"), chdir = TRUE)
  source(file.path("..", "..", "R", "arrow.R"), chdir = TRUE)
  source(file.path("..", "..", "R", "stat_arrow.R"), chdir = TRUE)

  data <- data.frame(
    start = c(100, 110, 140),
    end = c(200, 130, 160),
    direction = 1,
    label = c("long", "short", "shorter")
  )

  processed <- StatArrow$setup_data(data, params = list())
  longest <- which.max(processed$length)

  expect_equal(processed$middle[longest], 4)
  expect_true(all(processed$middle[-longest] > 4))
  expect_equal(processed$middle[2], processed$middle[3])
})
