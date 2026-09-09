test_that("Plot a data.frame object", {
  expect_equal({
    fl <- system.file('extdata', 'petm20.gb', package = "plasmapR")

    fl |>
      read_gb() |>
      as.data.frame() |>
      plot_plasmid(name = "pETM-20")

    TRUE
  }, TRUE)
  expect_snapshot({
    fl <- system.file('extdata', 'petm20.gb', package = "plasmapR")

    plasmid <- fl |>
      read_gb()

    dat <- plasmid |>
      as.data.frame()

    dat[dat$type == "CDS", ]
  })
})

test_that("show_label controls feature labels without hiding arrows", {
  dat <- data.frame(
    index = 1:2,
    name = c("shown", "hidden"),
    type = c("misc_feature", "misc_feature"),
    start = c(10, 30),
    end = c(20, 40),
    direction = 1,
    show_label = c(TRUE, FALSE)
  )

  plot <- plot_plasmid(dat, name = "", seq_length = 50)
  label_layers <- plot$layers[c("geom_label_repel", "geom_fit_text")]

  expect_true(all(vapply(label_layers, function(layer) {
    identical(layer$data$name, "shown")
  }, logical(1))))
  expect_equal(nrow(plot$data), 2)
})
