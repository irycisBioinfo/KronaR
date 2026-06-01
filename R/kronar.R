#' Generate Krona XML from a Data Frame
#'
#' Converts a hierarchical data frame into Krona-compatible XML.
#'
#' @param df A data frame with hierarchical columns (character or factor) and a numeric count column.
#' @param count_col Name or index of the column containing counts. If NULL, the first numeric column is used.
#' @param root_name Name of the root node. Default is "Root".
#' @param dataset_name Name of the dataset. Default is "Dataset".
#' @param collapse Logical. If TRUE (default), initial rendering collapses the hierarchy.
#' @return A character string containing the Krona XML.
#' @export
kronar_xml <- function(df, count_col = NULL, root_name = "Root", dataset_name = "Dataset", collapse = TRUE) {
  # Validate and parse data frame
  parsed <- validate_and_parse_df(df, count_col)
  hier <- parsed$hier
  counts <- parsed$counts

  # Node environment constructor helper
  NodeEnv <- function(name = "Root", value = 0) {
    env <- new.env(parent = emptyenv())
    env$name <- name
    env$value <- value
    env$children <- list()
    env
  }

  # Helper to recursively add path
  add_path <- function(root_env, path, count) {
    curr <- root_env
    curr$value <- curr$value + count

    for (step in path) {
      if (!step %in% names(curr$children)) {
        new_node <- NodeEnv(name = step, value = 0)
        curr$children[[step]] <- new_node
      }
      curr <- curr$children[[step]]
      curr$value <- curr$value + count
    }
  }

  # Extract path by truncating at the first empty level
  extract_path <- function(row_vec) {
    empty_idx <- which(row_vec == "" | is.na(row_vec))
    if (length(empty_idx) > 0) {
      first_empty <- min(empty_idx)
      if (first_empty == 1) {
        return(character(0))
      }
      return(row_vec[1:(first_empty - 1)])
    }
    return(row_vec)
  }

  # Build tree using environment nodes
  root_node <- NodeEnv(name = root_name, value = 0)

  for (i in seq_len(nrow(hier))) {
    row_vec <- as.character(hier[i, ])
    path <- extract_path(row_vec)
    if (length(path) == 0) {
      root_node$value <- root_node$value + counts[i]
      next
    }
    add_path(root_node, path, counts[i])
  }

  # If root value is still 0 (e.g. all counts were 0 or negative), check counts
  if (root_node$value <= 0) {
    sum_counts <- sum(counts)
    if (sum_counts > 0) {
      root_node$value <- sum_counts
    }
  }

  # Convert tree to XML
  node_to_xml <- function(node, indent_level = 0) {
    indent_str <- paste0(rep("  ", indent_level), collapse = "")
    escaped_name <- escape_xml(node$name)
    node_open <- sprintf('%s<node name="%s">', indent_str, escaped_name)
    node_val <- sprintf('%s  <magnitude><val>%s</val></magnitude>', indent_str, format(node$value, scientific = FALSE, justify = "none", trim = TRUE))

    if (length(node$children) == 0) {
      return(paste(node_open, node_val, sprintf('%s</node>', indent_str), sep = "\n"))
    }

    # Sort children by descending value (largest wedges first)
    child_names <- names(node$children)
    child_values <- sapply(child_names, function(n) node$children[[n]]$value)
    sorted_names <- child_names[order(-child_values, child_names)]

    children_xml <- sapply(sorted_names, function(child_name) {
      node_to_xml(node$children[[child_name]], indent_level + 1)
    })

    children_xml_str <- paste(children_xml, collapse = "\n")
    paste(node_open, node_val, children_xml_str, sprintf('%s</node>', indent_str), sep = "\n")
  }

  # Build the outer Krona wrapper
  collapse_str <- if (collapse) "true" else "false"

  xml_header <- paste(
    sprintf('<krona collapse="%s" key="true">', collapse_str),
    '  <attributes magnitude="magnitude">',
    '    <attribute display="Magnitude">magnitude</attribute>',
    '  </attributes>',
    '  <datasets>',
    sprintf('    <dataset>%s</dataset>', escape_xml(dataset_name)),
    '  </datasets>',
    sep = "\n"
  )

  root_xml <- node_to_xml(root_node, 1)
  xml_footer <- '</krona>'

  paste(xml_header, root_xml, xml_footer, sep = "\n")
}

#' Helper to resolve and read packaged assets
#'
#' @param filename File name of the asset (e.g. "krona-2.0.js").
#' @return File content as a single string.
#' @keywords internal
get_asset_path <- function(filename) {
  path <- system.file("assets", filename, package = "KronaR")
  if (path == "") {
    # Fallback for development/testing if package is not fully installed but loaded via devtools
    path <- file.path(system.file(package = "KronaR"), "inst", "assets", filename)
    if (!file.exists(path)) {
      # Fallback for raw path in case system.file returns empty under some test environments
      path <- file.path("/storage/bioinfo/KronaR/inst/assets", filename)
    }
  }
  if (!file.exists(path)) {
    stop(sprintf("Asset file '%s' not found.", filename))
  }
  path
}

read_asset <- function(filename) {
  path <- get_asset_path(filename)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}

#' Build Self-Contained Krona HTML
#'
#' Combines the Krona JavaScript engine, Base64 image assets, and XML data
#' into a single self-contained HTML string.
#'
#' @param xml_data A character string containing the Krona XML (e.g., from [kronar_xml()]).
#' @return A character string containing the self-contained HTML.
#' @export
kronar_html <- function(xml_data) {
  js_content <- read_asset("krona-2.0.js")
  hidden_uri <- read_asset("hidden.uri")
  loading_uri <- read_asset("loading.uri")
  favicon_uri <- read_asset("favicon.uri")
  logo_uri <- read_asset("logo-med.uri")

  # Construct HTML string
  html_str <- paste(
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">',
    '  <head>',
    '    <meta charset="utf-8"/>',
    sprintf('    <link rel="shortcut icon" href="%s"/>', favicon_uri),
    '    <script id="notfound" type="text/javascript">window.onload=function(){document.body.innerHTML="Could not get resources."}</script>',
    '    <script language="javascript" type="text/javascript">',
    js_content,
    '    </script>',
    '  </head>',
    '  <body>',
    sprintf('    <img id="hiddenImage" src="%s" style="display:none" alt="Hidden Image"/>', hidden_uri),
    sprintf('    <img id="loadingImage" src="%s" style="display:none" alt="Loading Indicator"/>', loading_uri),
    sprintf('    <img id="logo" src="%s" style="display:none" alt="Logo of Krona"/>', logo_uri),
    '    <noscript>Javascript must be enabled to view this page.</noscript>',
    '    <div style="display:none">',
    xml_data,
    '    </div>',
    '  </body>',
    '</html>',
    sep = "\n"
  )

  html_str
}

#' Write Krona Chart to HTML File
#'
#' Generates the self-contained HTML chart and writes it to a file.
#'
#' @param df A data frame.
#' @param file Path to the output HTML file.
#' @param count_col Name or index of the count column.
#' @param root_name Name of the root node.
#' @param dataset_name Name of the dataset.
#' @param collapse Logical. If TRUE, initial rendering collapses the hierarchy.
#' @return The file path to the written HTML file, invisibly.
#' @export
kronar_write <- function(df, file, count_col = NULL, root_name = "Root", dataset_name = "Dataset", collapse = TRUE) {
  xml_data <- kronar_xml(df, count_col = count_col, root_name = root_name, dataset_name = dataset_name, collapse = collapse)
  html_data <- kronar_html(xml_data)

  writeLines(html_data, file, useBytes = TRUE)
  invisible(file)
}

#' Plot Krona Chart in R Viewer / Notebooks
#'
#' Returns an HTML iframe tag object enclosing the Krona chart using htmltools.
#' This allows the chart to render inside RStudio Viewer, Shiny apps, and Quarto/RMarkdown notebooks.
#'
#' @param df A data frame.
#' @param count_col Name or index of the count column.
#' @param root_name Name of the root node.
#' @param dataset_name Name of the dataset.
#' @param collapse Logical. If TRUE, initial rendering collapses the hierarchy.
#' @param width Width of the iframe (CSS layout, e.g. "100%").
#' @param height Height of the iframe (CSS layout, e.g. "600px").
#' @return An `htmltools::tag` object representing the iframe.
#' @importFrom htmltools tags HTML
#' @export
kronar_plot <- function(df, count_col = NULL, root_name = "Root", dataset_name = "Dataset", collapse = TRUE, width = "100%", height = "600px") {
  xml_data <- kronar_xml(df, count_col = count_col, root_name = root_name, dataset_name = dataset_name, collapse = collapse)
  html_data <- kronar_html(xml_data)

  # Return an iframe containing the html code in srcdoc
  htmltools::tags$iframe(
    srcdoc = html_data,
    width = width,
    height = height,
    style = "border: none; margin: 0; padding: 0; overflow: hidden;",
    class = "kronar-chart"
  )
}

#' Take a Static PNG Snapshot of a Krona Chart
#'
#' Generates a Krona chart, renders it in a headless browser via `webshot2`,
#' and saves a static PNG snapshot.
#'
#' @param df A data frame.
#' @param file Path to save the PNG file. If NULL, a temporary file path is generated.
#' @param count_col Name or index of the count column.
#' @param root_name Name of the root node.
#' @param dataset_name Name of the dataset.
#' @param collapse Logical. If TRUE, initial rendering collapses the hierarchy.
#' @param delay Number of seconds to wait for the chart to render before taking the screenshot. Default is 1.0.
#' @param ... Additional arguments passed to `webshot2::webshot()`.
#' @return Path to the generated PNG file.
#' @export
kronar_snapshot <- function(df, file = NULL, count_col = NULL, root_name = "Root", dataset_name = "Dataset", collapse = TRUE, delay = 1.0, ...) {
  if (is.null(file)) {
    file <- tempfile(fileext = ".png")
  }

  # Create a temporary HTML file for the chart
  temp_html <- tempfile(fileext = ".html")
  on.exit(unlink(temp_html), add = TRUE)

  kronar_write(
    df = df,
    file = temp_html,
    count_col = count_col,
    root_name = root_name,
    dataset_name = dataset_name,
    collapse = collapse
  )

  # Take the screenshot using webshot2
  webshot2::webshot(
    url = temp_html,
    file = file,
    delay = delay,
    ...
  )

  file
}
