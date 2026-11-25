args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Usage: Rscript tools/render_extdata_gallery.R [output-directory]")
}
if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the plasmapR package root.")
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install the devtools package to render the local package.")
}

devtools::load_all(".", quiet = TRUE)

fixture_files <- list.files(
  "inst/extdata",
  pattern = "\\.(gb|gbk|txt)$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(fixture_files) == 0L) {
  stop("No GenBank fixtures found in inst/extdata.")
}

output_dir <- if (length(args) == 1L) args[[1L]] else "extdata-plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!dir.exists(output_dir)) {
  stop("Could not create output directory: ", output_dir)
}

for (file in fixture_files) {
  name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_dir, paste0(name, ".png"))

  invisible(capture.output({
    plasmid <- read_gb(file)
    plot <- plot_plasmid(plasmid, name = name) + ggplot2::coord_cartesian()
    ggplot2::ggsave(
      filename = output_file,
      plot = plot,
      width = 12,
      height = 4,
      dpi = 150
    )
  }))

  if (!file.exists(output_file) || file.info(output_file)$size == 0) {
    stop("Plot was not rendered successfully: ", output_file)
  }
  message("Saved ", output_file)
}
