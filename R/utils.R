#' Escape special XML characters
#'
#' @param x A character vector to escape.
#' @return A character vector with XML entities escaped.
#' @keywords internal
escape_xml <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  # Basic XML entities
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Validate and parse the input data frame for KronaR
#'
#' @param df A data frame.
#' @param count_col Name or index of the column containing counts.
#' @param fill_col Name or index of the column containing fill values/colors.
#' @param hier_cols Optional character vector of column names or numeric vector of column indices representing the hierarchical structure. If NULL, all columns except count_col and fill_col are used.
#' @return A list containing `hier` (a data frame of character columns) and `counts` (a numeric vector).
#' @keywords internal
validate_and_parse_df <- function(df, count_col = NULL, fill_col = NULL, hier_cols = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  if (nrow(df) == 0) {
    stop("Input data frame is empty.")
  }

  # Identify count column
  if (is.null(count_col)) {
    # Find the first numeric column
    num_cols <- which(sapply(df, is.numeric))
    if (length(num_cols) == 0) {
      stop("No numeric column found in the data frame to use as count/magnitude.")
    }
    count_index <- num_cols[1]
  } else {
    if (is.character(count_col)) {
      if (!count_col %in% colnames(df)) {
        stop(sprintf("Count column '%s' not found in data frame.", count_col))
      }
      count_index <- which(colnames(df) == count_col)
    } else if (is.numeric(count_col)) {
      count_col <- as.integer(count_col)
      if (count_col < 1 || count_col > ncol(df)) {
        stop(sprintf("Count column index %d is out of bounds.", count_col))
      }
      count_index <- count_col
    } else {
      stop("count_col must be a character string, numeric index, or NULL.")
    }
  }

  if (!is.numeric(df[[count_index]])) {
    stop("Selected count column is not numeric.")
  }

  # Identify fill column
  fill_index <- NULL
  if (!is.null(fill_col)) {
    if (is.character(fill_col)) {
      if (!fill_col %in% colnames(df)) {
        stop(sprintf("Fill column '%s' not found in data frame.", fill_col))
      }
      fill_index <- which(colnames(df) == fill_col)
    } else if (is.numeric(fill_col)) {
      fill_col <- as.integer(fill_col)
      if (fill_col < 1 || fill_col > ncol(df)) {
        stop(sprintf("Fill column index %d is out of bounds.", fill_col))
      }
      fill_index <- fill_col
    } else {
      stop("fill_col must be a character string, numeric index, or NULL.")
    }
  }

  # Identify hierarchical columns
  if (is.null(hier_cols)) {
    # Default behavior: all columns except count and fill
    exclude_indices <- count_index
    if (!is.null(fill_index)) {
      exclude_indices <- unique(c(exclude_indices, fill_index))
    }
    hier_indices <- setdiff(seq_len(ncol(df)), exclude_indices)
  } else {
    # Explicitly select hierarchical columns
    if (is.character(hier_cols)) {
      missing_cols <- setdiff(hier_cols, colnames(df))
      if (length(missing_cols) > 0) {
        stop(sprintf("Hierarchical columns not found in data frame: %s", paste(missing_cols, collapse = ", ")))
      }
      hier_indices <- match(hier_cols, colnames(df))
    } else if (is.numeric(hier_cols)) {
      hier_cols <- as.integer(hier_cols)
      if (any(hier_cols < 1 | hier_cols > ncol(df))) {
        stop("Some hierarchical column indices are out of bounds.")
      }
      hier_indices <- hier_cols
    } else {
      stop("hier_cols must be a character vector of column names, a numeric vector of indices, or NULL.")
    }

    # Ensure hierarchical columns do not overlap with count or fill columns
    if (count_index %in% hier_indices) {
      stop("Hierarchical columns cannot include the count column.")
    }
    if (!is.null(fill_index) && fill_index %in% hier_indices) {
      stop("Hierarchical columns cannot include the fill column.")
    }
  }

  if (length(hier_indices) == 0) {
    stop("Data frame must have at least one hierarchical column.")
  }

  # Clean the hierarchical columns: convert to character, fill NAs/Nulls with empty string
  hier_df <- as.data.frame(lapply(df[hier_indices], function(col) {
    col_char <- as.character(col)
    col_char[is.na(col_char)] <- ""
    col_char
  }), stringsAsFactors = FALSE)

  counts_clean <- df[[count_index]]
  counts_clean[is.na(counts_clean)] <- 0

  list(
    hier = hier_df,
    counts = counts_clean
  )
}

#' Resolve fill colors from a column
#'
#' @param df Data frame.
#' @param fill_col Name or index of the column containing fill values/colors.
#' @param fill_palette Optional custom palette. For numeric columns, a vector of colors defining a gradient. For discrete columns, a vector of colors or name of a palette.
#' @return A character vector of hexadecimal colors of the same length as rows in `df`.
#' @keywords internal
resolve_fill_colors <- function(df, fill_col, fill_palette = NULL) {
  if (is.numeric(fill_col)) {
    fill_col <- colnames(df)[as.integer(fill_col)]
  }

  vals <- df[[fill_col]]

  # Handle all NAs
  if (all(is.na(vals))) {
    return(rep("", nrow(df)))
  }

  # Check if numeric (continuous scale)
  if (is.numeric(vals)) {
    vals_clean <- vals[!is.na(vals)]
    min_val <- min(vals_clean)
    max_val <- max(vals_clean)

    if (max_val == min_val) {
      normalized <- rep(0.5, length(vals))
    } else {
      normalized <- (vals - min_val) / (max_val - min_val)
    }
    normalized[is.na(normalized)] <- 0.5  # default for NAs

    # Define gradient
    if (is.null(fill_palette)) {
      # Professional blue-to-red divergent palette
      fill_palette <- c("#313695", "#74add1", "#fee090", "#fdae61", "#d73027")
    }

    ramp <- grDevices::colorRamp(fill_palette)
    rgb_matrix <- ramp(normalized)

    # Convert to hex
    hex_cols <- grDevices::rgb(rgb_matrix[, 1], rgb_matrix[, 2], rgb_matrix[, 3], maxColorValue = 255)
    return(hex_cols)
  }

  # Convert to character if it's factor
  vals_char <- as.character(vals)
  vals_char[is.na(vals_char)] <- ""

  # Check if values are already valid colors (literal case)
  unique_non_empty <- unique(vals_char[vals_char != ""])

  is_literal_color <- FALSE
  if (length(unique_non_empty) > 0) {
    is_literal_color <- tryCatch({
      grDevices::col2rgb(unique_non_empty)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  if (is_literal_color) {
    # Convert all valid colors to hex, leave empty strings empty
    hex_cols <- rep("", length(vals_char))
    valid_indices <- which(vals_char != "")
    if (length(valid_indices) > 0) {
      rgb_matrix <- grDevices::col2rgb(vals_char[valid_indices])
      hex_cols[valid_indices] <- grDevices::rgb(
        rgb_matrix[1, ], rgb_matrix[2, ], rgb_matrix[3, ],
        maxColorValue = 255
      )
    }
    return(hex_cols)
  }

  # Discrete case (categories)
  unique_vals <- unique(vals_char)
  n_unique <- length(unique_vals)

  if (is.null(fill_palette)) {
    # Generate Set 2 colors from hcl.colors
    palette_cols <- grDevices::hcl.colors(n_unique, palette = "Set 2")
    color_map <- setNames(palette_cols, unique_vals)
  } else if (!is.null(names(fill_palette))) {
    # Named vector: explicit custom mapping (manual scale)
    color_map <- fill_palette[unique_vals]
    names(color_map) <- unique_vals

    # Check if there are any unmapped categories (NA values in color_map)
    missing_idx <- which(is.na(color_map))
    if (length(missing_idx) > 0) {
      # Fallback for missing/unmapped values using Set 2 colors
      fallback_cols <- grDevices::hcl.colors(length(missing_idx), palette = "Set 2")
      color_map[missing_idx] <- fallback_cols
    }
  } else {
    # Unnamed vector: sequential mapping
    if (length(fill_palette) >= n_unique) {
      palette_cols <- fill_palette[1:n_unique]
    } else {
      # Recycled palette colors
      palette_cols <- rep_len(fill_palette, n_unique)
    }
    color_map <- setNames(palette_cols, unique_vals)
  }

  if ("" %in% names(color_map)) {
    color_map[""] <- ""
  }

  hex_cols <- color_map[vals_char]
  as.character(hex_cols)
}
