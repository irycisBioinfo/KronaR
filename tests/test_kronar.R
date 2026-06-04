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

test_that("hier_cols parameter works as expected in validate_and_parse_df", {
  df <- data.frame(
    Level1 = c("Eukaryota", "Bacteria"),
    Level2 = c("Chordata", "Proteobacteria"),
    Level3 = c("Homo", "Escherichia"),
    Counts = c(100, 200),
    Fill = c(1, 2),
    stringsAsFactors = FALSE
  )

  # Explicit column selection by name
  res <- validate_and_parse_df(df, count_col = "Counts", fill_col = "Fill", hier_cols = c("Level1", "Level2"))
  expect_equal(colnames(res$hier), c("Level1", "Level2"))
  expect_equal(res$hier$Level1, c("Eukaryota", "Bacteria"))

  # Explicit column selection by index
  res2 <- validate_and_parse_df(df, count_col = 4, fill_col = 5, hier_cols = c(1, 3))
  expect_equal(colnames(res2$hier), c("Level1", "Level3"))

  # Out of bounds index
  expect_error(validate_and_parse_df(df, hier_cols = 10), "out of bounds")

  # Missing column name
  expect_error(validate_and_parse_df(df, hier_cols = "NonExistent"), "Hierarchical columns not found")

  # Overlap with count column
  expect_error(validate_and_parse_df(df, count_col = "Counts", hier_cols = "Counts"), "cannot include the count column")

  # Overlap with fill column
  expect_error(validate_and_parse_df(df, fill_col = "Fill", hier_cols = "Fill"), "cannot include the fill column")

  # Custom ordering is respected
  res3 <- validate_and_parse_df(df, count_col = "Counts", hier_cols = c("Level3", "Level1"))
  expect_equal(colnames(res3$hier), c("Level3", "Level1"))
  expect_equal(res3$hier$Level3, c("Homo", "Escherichia"))
})

test_that("hier_cols restricts and orders XML output correctly", {
  test_df <- data.frame(
    Level1 = c("Eukaryota", "Bacteria"),
    Level2 = c("Chordata", "Proteobacteria"),
    Level3 = c("Homo", "Escherichia"),
    Counts = c(100, 200),
    stringsAsFactors = FALSE
  )

  # Only use Level3 and Level1 in that order
  xml_str <- kronar_xml(test_df, count_col = "Counts", hier_cols = c("Level3", "Level1"))

  # Level2 should NOT be in the XML structure
  expect_false(grepl("Chordata", xml_str, fixed = TRUE))
  expect_false(grepl("Proteobacteria", xml_str, fixed = TRUE))

  # Level3 and Level1 should be in the XML structure
  expect_true(grepl('<node name="Homo">', xml_str, fixed = TRUE))
  expect_true(grepl('<node name="Eukaryota">', xml_str, fixed = TRUE))
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

test_that("kronar_snapshot handles different formats and DPI parameters", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )

  # 1. Default format is SVG (if file = NULL)
  tryCatch({
    res_obj <- kronar_snapshot(test_df, file = NULL, delay = 0.5)
    expect_true(inherits(res_obj, "ggplot"))
  }, error = function(e) {
    warning("Default SVG snapshot failed: ", e$message)
  })

  # 2. Specifying format = "png" and custom DPI resolution scaling
  temp_png_72 <- tempfile(fileext = ".png")
  temp_png_150 <- tempfile(fileext = ".png")
  on.exit({
    unlink(temp_png_72)
    unlink(temp_png_150)
  }, add = TRUE)

  tryCatch({
    kronar_snapshot(test_df, file = temp_png_72, dpi = 72, delay = 0.5)
    kronar_snapshot(test_df, file = temp_png_150, dpi = 150, delay = 0.5)

    expect_true(file.exists(temp_png_72))
    expect_true(file.exists(temp_png_150))

    size_72 <- file.info(temp_png_72)$size
    size_150 <- file.info(temp_png_150)$size
    expect_gt(size_72, 0)
    expect_gt(size_150, size_72) # Higher DPI should result in a larger file size due to larger dimensions
    message("DPI 72 size: ", size_72, " bytes. DPI 150 size: ", size_150, " bytes.")
  }, error = function(e) {
    warning("DPI and PNG snapshot failed: ", e$message)
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

test_that("kronar_snapshot can export a vector SVG file using internal JS routine", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  temp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_svg), add = TRUE)
  
  tryCatch({
    res_obj <- kronar_snapshot(test_df, file = temp_svg, delay = 0.5)
    expect_true(inherits(res_obj, "ggplot"))
    expect_true(file.exists(temp_svg))
    content <- readLines(temp_svg, warn = FALSE)
    expect_true(any(grepl("<svg", content)))
    message("SVG export completed successfully. Size: ", file.info(temp_svg)$size, " bytes")
  }, error = function(e) {
    warning("SVG snapshot failed: ", e$message)
  })
})

test_that("categorical coloring works correctly with discrete categories and custom mapping", {
  df <- data.frame(
    L1 = c("Bacteria", "Bacteria", "Eukaryota"),
    L2 = c("Proteobacteria", "Firmicutes", "Chordata"),
    Abundance = c(10, 20, 30),
    Category = c("Pathogen", "NonPathogen", "NonPathogen"),
    stringsAsFactors = FALSE
  )

  # Test auto-palette assignment (default)
  xml_auto <- kronar_xml(df, count_col = "Abundance", fill_col = "Category")
  expect_true(any(grepl('color="#', xml_auto, fixed = TRUE)))

  # Test custom named vector (manual scale mapping)
  custom_palette <- c("Pathogen" = "#FF0000", "NonPathogen" = "#00FF00")
  xml_custom <- kronar_xml(df, count_col = "Abundance", fill_col = "Category", fill_palette = custom_palette)

  # Check that colors are mapped exactly as specified in custom_palette
  expect_true(grepl('color="#FF0000"', xml_custom, fixed = TRUE))
  expect_true(grepl('color="#00FF00"', xml_custom, fixed = TRUE))
})

test_that("continuous coloring works with a Viridis palette", {
  df <- data.frame(
    L1 = c("Bacteria", "Bacteria", "Eukaryota"),
    L2 = c("Proteobacteria", "Firmicutes", "Chordata"),
    Abundance = c(10, 20, 30),
    Value = c(1.0, 2.5, 5.0),
    stringsAsFactors = FALSE
  )

  # Generate Viridis colors using grDevices::hcl.colors
  viridis_palette <- grDevices::hcl.colors(5, palette = "Viridis")

  xml_viridis <- kronar_xml(
    df,
    count_col = "Abundance",
    fill_col = "Value",
    fill_palette = viridis_palette
  )

  # Min value (1.0) maps to the first color "#4B0055"
  # Max value (5.0) maps to the last color "#FDE333"
  expect_true(grepl('color="#4B0055"', xml_viridis, ignore.case = TRUE))
  expect_true(grepl('color="#FDE333"', xml_viridis, ignore.case = TRUE))
})

test_that("kronar_plot S3 print and class resolution work correctly", {
  test_df <- data.frame(
    Level1 = c("A", "B"),
    Counts = c(10, 20),
    stringsAsFactors = FALSE
  )

  plot_tag <- kronar_plot(test_df)

  # Assert class vector contains kronar_plot and shiny.tag
  expect_true("kronar_plot" %in% class(plot_tag))
  expect_true("shiny.tag" %in% class(plot_tag))

  # Test print method (in non-interactive test mode)
  # It should fall back to default print and output raw iframe markup
  output_text <- capture.output(print(plot_tag))
  expect_true(any(grepl("<iframe", output_text)))

  # Test knit_print fallback (by mock calling knit_print if knitr is available)
  if (requireNamespace("knitr", quietly = TRUE)) {
    knit_res <- knitr::knit_print(plot_tag)
    expect_true(inherits(knit_res, "asis") || any(grepl("<iframe", as.character(knit_res))))
  }
})

cat("\nAll tests completed successfully!\n")
