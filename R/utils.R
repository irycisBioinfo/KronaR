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
#' @param color_col Name or index of the column containing colors.
#' @return A list containing `hier` (a data frame of character columns) and `counts` (a numeric vector).
#' @keywords internal
validate_and_parse_df <- function(df, count_col = NULL, color_col = NULL) {
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

  # Identify color column
  color_index <- NULL
  if (!is.null(color_col)) {
    if (is.character(color_col)) {
      if (!color_col %in% colnames(df)) {
        stop(sprintf("Color column '%s' not found in data frame.", color_col))
      }
      color_index <- which(colnames(df) == color_col)
    } else if (is.numeric(color_col)) {
      color_col <- as.integer(color_col)
      if (color_col < 1 || color_col > ncol(df)) {
        stop(sprintf("Color column index %d is out of bounds.", color_col))
      }
      color_index <- color_col
    } else {
      stop("color_col must be a character string, numeric index, or NULL.")
    }
  }

  # Hierarchical columns are all columns except the count column and color column
  exclude_indices <- count_index
  if (!is.null(color_index)) {
    exclude_indices <- unique(c(exclude_indices, color_index))
  }
  hier_indices <- setdiff(seq_len(ncol(df)), exclude_indices)
  if (length(hier_indices) == 0) {
    stop("Data frame must have at least one hierarchical (non-numeric) column.")
  }

  # Clean the hierarchical columns: convert to character, fill NAs/Nulls with empty string
  hier_df <- as.data.frame(lapply(df[hier_indices], function(col) {
    col_char <- as.character(col)
    col_char[is.na(col_char)] <- ""
    col_char
  }), stringsAsFactors = FALSE)

  list(
    hier = hier_df,
    counts = df[[count_index]]
  )
}
