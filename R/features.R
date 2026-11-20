

.feature_get_dim <- function(start,
                             end,
                             direction = 1,
                             phlange_angle = 4,
                             bp = 6000) {
  df <- lapply(seq_along(start), function(i) {
    if(is.na(bp) || is.null(bp)) {
      bp <- 6000
    }
    start <- start[i]
    end <- end[i]
    direction <- direction[i]

    if (start > end) {
      end_temp <- end
      end <- start
      start <- end_temp
      direction <- direction * -1
    }

    length <- end - start

    arrow_width <- bp / 360 * phlange_angle

    if (direction == -1) {
      end_temp <- end
      end <- start
      start <- end_temp
    }
    
    # Arrow phlange size is calculated as 0.5% of the plasmid length
    phlange <- end - direction * bp * 0.005

    if (abs(length) < arrow_width) {
      phlange <- start
    }

    data.frame(
      start = start,
      end = end,
      phlange = phlange,
      direction = direction
    )
  })

  do.call(rbind, df)
}
