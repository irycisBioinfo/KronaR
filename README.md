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

### 6. Custom Node Coloring (`fill_col`)
The `fill_col` parameter allows you to color the chart wedges in three different modes:

#### Mode 1: Literal Colors (`scale_fill_manual`/`scale_fill_identity` style)
Provide hex strings or R color names directly in the column:

```r
data_literal <- data.frame(
  Kingdom = c("Bacteria", "Bacteria", "Eukaryota"),
  Phylum  = c("Proteobacteria", "Firmicutes", "Chordata"),
  Count   = c(300, 150, 100),
  Color   = c("#FF5733", "blue", "green"), # Hex or standard names
  stringsAsFactors = FALSE
)

# Render with literal colors (colors also propagate to children automatically)
kronar_plot(data_literal, count_col = "Count", fill_col = "Color")
```

#### Mode 2: Numeric Variables (`scale_fill_continuous` style)
Provide a numeric column. The package will map values to a continuous color gradient (blue to red by default):

```r
data_numeric <- data.frame(
  Kingdom = c("Bacteria", "Bacteria", "Eukaryota"),
  Phylum  = c("Proteobacteria", "Firmicutes", "Chordata"),
  Count   = c(300, 150, 100),
  Value   = c(1.2, 5.8, 9.4), # Continuous scores
  stringsAsFactors = FALSE
)

# Render with numeric gradient
kronar_plot(data_numeric, count_col = "Count", fill_col = "Value")
```

#### Mode 3: Discrete/Factor Variables (`scale_fill_discrete` style)
Provide a discrete categories column. The package will automatically map unique categories to a professional discrete color palette (`Set 2` by default):

```r
data_discrete <- data.frame(
  Kingdom = c("Bacteria", "Bacteria", "Eukaryota"),
  Phylum  = c("Proteobacteria", "Firmicutes", "Chordata"),
  Count   = c(300, 150, 100),
  Group   = c("Group A", "Group B", "Group A"), # Categories
  stringsAsFactors = FALSE
)

# Render with discrete palette colors
kronar_plot(data_discrete, count_col = "Count", fill_col = "Group")
```

---

## Functions Reference

* `kronar_xml(df, count_col, fill_col, fill_palette, root_name, dataset_name, collapse)`: Converts a hierarchical data frame into Krona-compliant XML.
* `kronar_html(xml_data)`: Builds a self-contained HTML page by inlining JS and base64 encoded images.
* `kronar_write(df, file, count_col, fill_col, fill_palette, ...)`: Writes the self-contained HTML chart to a file.
* `kronar_plot(df, count_col, fill_col, fill_palette, ...)`: Returns an `htmltools` tag object (iframe) to render in RStudio Viewer or notebooks.
* `kronar_snapshot(df, file, count_col, fill_col, fill_palette, ...)`: Captures a static PNG snapshot of the chart using `webshot2`.

## License

This package is licensed under the **MIT License**. Krona assets included are subject to their own original licenses. See [LICENSE](LICENSE) for details.
