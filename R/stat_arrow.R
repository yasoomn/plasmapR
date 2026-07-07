
#' @noRd
.create_arrow <- function(start,
                          end,
                          phlange,
                          middle,
                          width,
                          direction = 1,
                          plasmid_length,
                          arrowhead_width = 5,
                          arrowhead_size = 8) {


  if (direction == -1) {
    end_temp <- end
    end <- start
    start <- end_temp
    direction <- direction * -1
  }

  # convert to 'arrow' nomenclature to make it more clear what is going on
  base <- start
  tip <- end
  midpoint <- middle
  .arrow_points(base, tip, midpoint, phlange, arrowhead_width, width)
}

#' Compute positions for an Arrow
#' @export

StatArrow <- ggplot2::ggproto('StatArrow', ggplot2::Stat,
  setup_data = function(data, params) {
    wrong_orientation <- data$end < data$start
    end_temp <- data$end
    data$end[wrong_orientation] <- data$start[wrong_orientation]
    data$start[wrong_orientation] <- end_temp[wrong_orientation]
    data$middle <- 4

    vec_start <- data$start
    vec_end   <- data$end
    wrong_way <- vec_start > vec_end
    data$start[wrong_way] <- data$end[wrong_way]
    data$end[wrong_way] <- data$start[wrong_way]
    data$length <- data$end - data$start

    between <- function(x, y, z) {
      x > y & x < z
    }

    .overlaps <- function(x_idx, y_idx) {
      x_start <- data[x_idx, "start"]
      x_end   <- data[x_idx, "end"]
      y_start <- data[y_idx, "start"]
      y_end   <- data[y_idx, "end"]

      (
        between(x_end, y_start, y_end) |
        between(x_start, y_start, y_end) |
        between(y_end, x_start, x_end) |
        between(y_start, x_start, x_end)
      )
    }

    data$middle <- 4
    data$arrowhead_width <- 0.5

    ordered_idx <- order(-data$length)

    for (pos in seq_along(ordered_idx)) {
      current_idx <- ordered_idx[pos]
      current_level <- 0

      while (any(vapply(
        ordered_idx[seq_len(pos - 1)],
        function(previous_idx) {
          .overlaps(current_idx, previous_idx) && data$middle[previous_idx] == 4 + 0.6 * current_level
        },
        logical(1)
      ))) {
        current_level <- current_level + 1
      }

      data$middle[current_idx] <- 4 + 0.6 * current_level
    }

    points <- .feature_get_dim(
      start = data$start,
      end = data$end,
      direction = data$direction,
      phlange_angle = 8,
      bp = 6000
    )

    # data$start   <- points$start
    # data$end     <- points$end
    data$direction <- points$direction
    data$phlange <- points$phlange

    data
  },
  compute_group = function(data,
                           scales,
                           width = 0.15,
                           bp = 6000,
                           arrowhead_size = 8){


    arrows <-
      .create_arrow(
        start = data$start,
        end = data$end,
        middle = data$middle,
        phlange = data$phlange,
        direction = data$direction,
        arrowhead_width = 0,
        width = width,
        plasmid_length = bp,
        arrowhead_size = arrowhead_size
      )

    arrows
  },
  required_aes = c('start', 'end'),
  default_aes = ggplot2::aes(
    direction = 1
  )
)

#' Compute required values for labels
#' @export
StatArrowLabel <- ggplot2::ggproto('StatArrowLabel', StatArrow,
  compute_group = function(data,
                           scales,
                           bp = 6000,
                           invert = TRUE) {
    # data$length <- data$end - data$start

    # Calculate label height range based on arrow width
    label_width <- 0.4  # width of the label area
    
    df <- data.frame(
      x = mean(c(data$start, data$end)),
      y = data$middle,
      ymin = data$middle - label_width,
      ymax = data$middle + label_width,
      xmin = data$start,
      xmax = data$end
    )

    fil <- data$length / bp > nchar(data$label) / bp * 60

    if (invert) fil <- !fil
    # if (! invert) df <- df[, c("y", "xmin", "xmax")]
    df[fil]
  }#,
  # required_aes = c('start', 'end')
)

#' Custom Stat: Arrow
#'
#' This stat creates arrow shapes in a ggplot.
#'
#' @param mapping The aesthetic mapping, usually constructed with aes().
#' @param data The dataset to be used in the layer.
#' @param geom The geometric object to use for the layer, default is "polygon".
#' @param position Position adjustment, default is "identity".
#' @param na.rm Should missing values be removed?
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Should inherit aesthetics from the parent plot?
#' @param ... Other arguments passed to layer().
#' @param bp The base parameter for the arrow shape.
#' @param arrowhead_size The size of the arrowhead.
#' @return A ggplot layer with arrow shapes.
#' @export
#'
#' @seealso \code{\link[ggplot2]{geom_segment}}
#'
#' @details
#' Used for drawing features for a plasmid that work in both cartesian and
#' polad coordinate systems.
#'
#' @note
#' The `bp` parameter determines the base parameter of the arrow shape.
#' The `arrowhead_size` parameter controls the size of the arrowhead.
#'
#' @keywords plotting
stat_arrow <- function(mapping = NULL,
                       data = NULL,
                       geom = "polygon",
                       position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...,
                       bp = 6000,
                       arrowhead_size = 8
) {
  ggplot2::layer(
    stat = "arrow",
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      bp = bp,
      arrowhead_size = arrowhead_size,
      ...
    )
  )
}
