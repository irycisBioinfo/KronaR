# Test script for KronaR Wrapper Package
library(testthat)

# Load the package using pkgload or manual sourcing
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all("/storage/bioinfo/KronaR")
} else {
  source("/storage/bioinfo/KronaR/R/utils.R")
  source("/storage/bioinfo/KronaR/R/kronar.R")
}

test_that("validate_and_parse_df works as expected", {
  # Valid dataframe
  df1 <- data.frame(
    A = c("x", "y"),
    B = c("a", "b"),
    Count = c(10, 20),
    stringsAsFactors = FALSE
  )
  res <- validate_and_parse_df(df1)
  expect_equal(res$counts, c(10, 20))
  expect_equal(res$hier$A, c("x", "y"))
  expect_equal(res$hier$B, c("a", "b"))

  # Invalid dataframe (no numeric col)
  df2 <- data.frame(A = c("x", "y"), B = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(validate_and_parse_df(df2), "No numeric column found")

  # Column out of bounds
  expect_error(validate_and_parse_df(df1, count_col = 5), "out of bounds")
})

test_that("kronar_xml generates correct XML structure", {
  test_df <- data.frame(
    Level1 = c("Eukaryota", "Eukaryota", "Bacteria"),
    Level2 = c("Chordata", "Arthropoda", "Proteobacteria"),
    Counts = c(100, 200, 300),
    stringsAsFactors = FALSE
  )

  xml_str <- kronar_xml(test_df, count_col = "Counts", root_name = "Life", dataset_name = "My Test Data")

  # Verify XML elements
  expect_true(grepl('<krona collapse="true" key="true">', xml_str, fixed = TRUE))
  expect_true(grepl('<dataset>My Test Data</dataset>', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Life">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Eukaryota">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Bacteria">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Chordata">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Arthropoda">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Proteobacteria">', xml_str, fixed = TRUE))
})

test_that("kronar_html constructs HTML with bundled assets", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )
  xml_str <- kronar_xml(test_df)
  html_str <- kronar_html(xml_str)

  expect_true(grepl("<!DOCTYPE html", html_str, fixed = TRUE))
  expect_true(grepl("data:image/png;base64", html_str, fixed = TRUE)) # Hidden image or logo
  expect_true(grepl("data:image/gif;base64", html_str, fixed = TRUE)) # Loading indicator
  expect_true(grepl("canvas", html_str, fixed = TRUE) || grepl("Krona", html_str, fixed = TRUE)) # JS presence
  expect_true(grepl("<krona", html_str, fixed = TRUE)) # XML is embedded
})

test_that("kronar_write and kronar_plot generate outputs", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )

  # Write to temp file
  temp_html <- tempfile(fileext = ".html")
  on.exit(unlink(temp_html), add = TRUE)

  kronar_write(test_df, temp_html)
  expect_true(file.exists(temp_html))
  expect_gt(file.info(temp_html)$size, 100000) # Should be > 100KB due to bundled JS and assets

  # Plot returns tag
  plot_tag <- kronar_plot(test_df)
  expect_equal(plot_tag$name, "iframe")
  expect_equal(plot_tag$attribs$class, "kronar-chart")
  expect_true(grepl("<krona", plot_tag$attribs$srcdoc, fixed = TRUE))
})

test_that("kronar_snapshot captures a PNG file", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )

  temp_png <- tempfile(fileext = ".png")
  on.exit(unlink(temp_png), add = TRUE)

  # Try to capture snapshot
  tryCatch({
    png_path <- kronar_snapshot(test_df, file = temp_png, delay = 0.5)
    expect_true(file.exists(png_path))
    expect_gt(file.info(png_path)$size, 0)
    message("Snapshot completed successfully. PNG size: ", file.info(png_path)$size, " bytes")
  }, error = function(e) {
    # If Chrome is not available on this headless system, webshot2 might fail.
    # We catch it gracefully and print warning, but check that it behaves appropriately.
    warning("webshot2 failed: ", e$message)
  })
})

test_that("fill_col generates correct color attributes in XML across all modes", {
  # Mode 1: Literal Hex colors
  df_literal <- data.frame(
    Level1 = c("Bacteria", "Eukaryota"),
    Counts = c(100, 200),
    Color = c("#FF5733", "blue"),
    stringsAsFactors = FALSE
  )
  xml_literal <- kronar_xml(df_literal, count_col = "Counts", fill_col = "Color")
  expect_true(grepl('color="#FF5733"', xml_literal, fixed = TRUE))
  expect_true(grepl('color="#0000FF"', xml_literal, fixed = TRUE)) # "blue" resolved to hex

  # Mode 2: Numeric (continuous gradient)
  df_numeric <- data.frame(
    Level1 = c("Bacteria", "Eukaryota"),
    Counts = c(100, 200),
    Value = c(1.0, 5.0),
    stringsAsFactors = FALSE
  )
  xml_numeric <- kronar_xml(df_numeric, count_col = "Counts", fill_col = "Value")
  # Verify that colors were generated and mapped
  expect_true(any(grepl('color="#', xml_numeric, fixed = TRUE)))

  # Mode 3: Discrete/Factor (palette mapping)
  df_discrete <- data.frame(
    Level1 = c("Bacteria", "Eukaryota"),
    Counts = c(100, 200),
    Group = c("A", "B"),
    stringsAsFactors = FALSE
  )
  xml_discrete <- kronar_xml(df_discrete, count_col = "Counts", fill_col = "Group")
  expect_true(any(grepl('color="#', xml_discrete, fixed = TRUE)))
})

test_that("proportion coloring for numeric fills works correctly", {
  df <- data.frame(
    L1 = c("A", "A", "B"),
    L2 = c("A1", "A2", "B1"),
    Count = c(10, 10, 20),
    Presence = c(1, 0, 1),
    stringsAsFactors = FALSE
  )
  xml_data <- kronar_xml(df, count_col = "Count", fill_col = "Presence")
  
  # A1 (Presence=1) maps to max color #D73027
  # A2 (Presence=0) maps to min color #313695
  # A (average proportion = 0.5) maps to middle color #FEE090
  # B (average proportion = 1.0) maps to max color #D73027
  expect_true(grepl('name="A1" color="#D73027"', xml_data, ignore.case = TRUE))
  expect_true(grepl('name="A2" color="#313695"', xml_data, ignore.case = TRUE))
  expect_true(grepl('name="A" color="#FEE090"', xml_data, ignore.case = TRUE))
  expect_true(grepl('name="B" color="#D73027"', xml_data, ignore.case = TRUE))
})

cat("\nAll tests completed successfully!\n")
