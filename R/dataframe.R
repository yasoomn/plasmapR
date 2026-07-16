.feature_list_to_df <- function(x, bp = NULL, ...) {

  feats <- lapply(seq(length(x)), \(i) {
    feat <- x[[i]]
    if (is.null(feat$note)) {
      feat$note <- ""
    }
    data.frame(
      index = i,
      name = feat$name,
      type = feat$type,
      note = feat$note,
      start = feat$start_end[1],
      end = feat$start_end[2],
      direction = as.numeric(feat$direction)
    )
  })
  dat <- do.call(rbind, feats)

  # turn certain features in to numeric columns
  dat$start <- as.numeric(dat$start)
  dat$end <- as.numeric(dat$end)
  dat$direction <- as.numeric(dat$direction)

  valid_coords <- !is.na(dat$start) & !is.na(dat$end) & is.finite(dat$start) & is.finite(dat$end)
  over_origin <- valid_coords & dat$start > dat$end & dat$direction == 1

  if (any(over_origin)) {
    if (is.null(bp)) {
      bp <- max(c(dat$start[valid_coords], dat$end[valid_coords]), na.rm = TRUE)
    }
    
    # For origin-spanning features like join(4891..5096,1..751):
    # - We need to offset the coordinate system so this becomes continuous
    # - The offset should be chosen so that the feature becomes [new_start..new_end]
    # - All other features get shifted by the same offset
    
    # Find the origin-spanning feature with the smallest end coordinate
    # This determines our offset. Only consider features with valid coordinates.
    min_end <- min(dat$end[over_origin])
    offset <- min_end
    
    # Apply the same offset to valid coordinates so the feature becomes continuous
    # in a single coordinate system without changing rows with missing values.
    original_start <- dat$start
    original_end <- dat$end

    dat$start[valid_coords] <- original_start[valid_coords] - offset
    dat$end[valid_coords] <- original_end[valid_coords] - offset
    
    # For origin-spanning features, calculate the correct end position from the
    # original coordinates so each feature is reconstructed independently.
    # Original: join(4891..5096, 1..751) with bp=5096
    # After offset by 751: start=4140, end=0
    # Correct end should be: start + ((5096-4891+1) + 751 - 1) = 4140 + 956 = 5096
   
    for (i in which(over_origin)) {
      feature_start <- original_start[i]
      feature_end <- original_end[i]
      
      # Calculate total feature length: (bp - start + 1) + end
      part1_length <- bp - feature_start + 1  # From start to end of plasmid
      part2_length <- feature_end             # From beginning to end position
      total_length <- part1_length + part2_length
      
      # Set the new end position while keeping the shifted start coordinate.
      dat$end[i] <- dat$start[i] + total_length - 1
    }
    
    # Handle any negative coordinates by wrapping them around
    negative_coords <- valid_coords & (dat$start < 0 | dat$end < 0)
    dat$start[negative_coords & dat$start < 0] <- 
      dat$start[negative_coords & dat$start < 0] + bp
    dat$end[negative_coords & dat$end < 0] <- 
      dat$end[negative_coords & dat$end < 0] + bp
  }

# only return features where a start was successfully parsed
  # dat[!is.na(dat$start), ]
  dat
}

#' Extract Features of a Plasmid as a DataFrame
#'
#' @param x A list of class 'plasmid' from `read_gb()`
#' @param row.names Ignored.
#' @param optional Ignored.
#' @param ... Ignored.
#'
#' @return a DataFrame
#' @rdname as.data.frame.plasmid
#' @export
as.data.frame.plasmid <- function(x, row.names, optional, ...) {
  .feature_list_to_df(x$features, bp = x$length)
}
