fixture_dir <- system.file("extdata", package = "plasmapR")
fixture_files <- list.files(
  fixture_dir,
  pattern = "\\.(gb|gbk|txt)$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in fixture_files) {
  local({
    fixture <- file

    test_that(paste(basename(fixture), "can be parsed and plotted"), {
      plasmid <- read_gb(fixture)
      expect_s3_class(plasmid, "plasmid")

      plot <- NULL
      built <- NULL
      invisible(capture.output({
        plot <- plot_plasmid(
          plasmid,
          name = tools::file_path_sans_ext(basename(fixture))
        )
        built <- ggplot2::ggplot_build(plot)
      }))

      expect_s3_class(plot, "ggplot")
      expect_s3_class(built, "ggplot_built")
    })
  })
}
