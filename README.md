# KronaR: R Wrapper for Krona Interactive Charts

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`KronaR` is an R package that wraps the [Krona](https://github.com/marbl/Krona) visualization engine. It allows users to generate interactive, zoomable, hierarchical pie charts directly from R data frames, and view or import them dynamically (via HTML/iframes) or statically (via PNG snapshots).

## Key Features

* **Zero System Dependencies:** All necessary Krona JavaScript engines and UI assets are bundled directly inside the package. You do not need to install Perl or command-line KronaTools on your system.
* **Arbitrary Hierarchical Data:** Visualizes any data frame where character/factor columns represent the hierarchy from left to right, and a numeric column represents magnitudes/counts.
* **Dynamic Integration:** Embeds HTML pages into R sessions using sandboxed `iframe` tags via the `srcdoc` attribute. Works seamlessly with RStudio Viewer, Shiny applications, and Quarto/RMarkdown notebooks.
* **Static Snapshotting:** Captures high-quality, static PNG images of the rendered charts using `webshot2` for inclusion in papers or standard R plots.

---

## Installation

You can install the development version of `KronaR` from GitHub (once uploaded) using `devtools`:

```r
# If you don't have devtools installed:
# install.packages("devtools")

devtools::install_github("irycisBioinfo/KronaR")
```

---

## Quick Start

### 1. Prepare your Hierarchical Data
Create a data frame where taxonomic or hierarchical levels go from left to right, followed by a numeric column:

```r
library(KronaR)

data <- data.frame(
  Kingdom = c("Bacteria", "Bacteria", "Eukaryota", "Eukaryota"),
  Phylum  = c("Proteobacteria", "Firmicutes", "Chordata", "Arthropoda"),
  Class   = c("Gammaproteobacteria", "Bacilli", "Mammalia", "Insecta"),
  Count   = c(300, 150, 100, 250),
  stringsAsFactors = FALSE
)
```

### 2. Plot Dynamically in R / RStudio Viewer
Render the chart in RStudio Viewer, Shiny, or a notebook using an interactive iframe:

```r
# Returns an htmltools tag ready to be viewed
kronar_plot(data, count_col = "Count", root_name = "Life Tree")
```

### 3. Save as HTML File
Write the standalone HTML chart to disk:

```r
kronar_write(data, file = "my_chart.html", count_col = "Count", root_name = "Life Tree")
```

### 5. Take a Static PNG Snapshot
Generate a static PNG screenshot using a headless browser:

```r
kronar_snapshot(data, file = "my_snapshot.png", count_col = "Count", root_name = "Life Tree")
```

### 6. Custom Node Coloring
You can specify custom hexadecimal colors (e.g. `#FF5733`) for individual nodes by including a color column and passing it to the functions:

```r
colored_data <- data.frame(
  Kingdom = c("Bacteria", "Bacteria", "Eukaryota"),
  Phylum  = c("Proteobacteria", "Firmicutes", "Chordata"),
  Count   = c(300, 150, 100),
  Color   = c("#FF5733", "#33FF57", "#3357FF"), # Custom node colors
  stringsAsFactors = FALSE
)

# Render with custom colors (which also propagate to children automatically)
kronar_plot(colored_data, count_col = "Count", color_col = "Color")
```

---

## Functions Reference

* `kronar_xml(df, count_col, color_col, root_name, dataset_name, collapse)`: Converts a hierarchical data frame into Krona-compliant XML.
* `kronar_html(xml_data)`: Builds a self-contained HTML page by inlining JS and base64 encoded images.
* `kronar_write(df, file, count_col, color_col, ...)`: Writes the self-contained HTML chart to a file.
* `kronar_plot(df, count_col, color_col, ...)`: Returns an `htmltools` tag object (iframe) to render in RStudio Viewer or notebooks.
* `kronar_snapshot(df, file, count_col, color_col, ...)`: Captures a static PNG snapshot of the chart using `webshot2`.

## License

This package is licensed under the **MIT License**. Krona assets included are subject to their own original licenses. See [LICENSE](LICENSE) for details.
