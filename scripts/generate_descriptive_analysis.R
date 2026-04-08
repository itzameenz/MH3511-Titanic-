data_path <- file.path("data", "processed", "billionaires_analysis_ready.csv")
out_dir <- file.path("output", "descriptive")
hist_dir <- file.path(out_dir, "histograms")
box_dir <- file.path(out_dir, "boxplots")
bar_dir <- file.path(out_dir, "barplots")

manifest_path <- file.path(out_dir, "column_manifest.csv")
numeric_summary_path <- file.path(out_dir, "numeric_summary.csv")
categorical_counts_path <- file.path(out_dir, "categorical_level_counts.csv")
date_summary_path <- file.path(out_dir, "date_summary.csv")
run_summary_path <- file.path(out_dir, "descriptive_run_summary.txt")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(hist_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(box_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bar_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop("Missing processed dataset at: ", data_path, call. = FALSE)
}

coerce_numeric_like <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  y <- gsub("[$, ]", "", x)
  y[y == ""] <- NA
  suppressWarnings(as.numeric(y))
}

parse_date_like <- function(x) {
  if (inherits(x, "Date")) return(as.POSIXct(x))
  formats <- c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%m/%d/%Y")
  for (fmt in formats) {
    parsed <- as.POSIXct(x, format = fmt, tz = "UTC")
    if (sum(!is.na(parsed)) > 0) {
      return(parsed)
    }
  }
  rep(as.POSIXct(NA), length(x))
}

skewness_value <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  sigma <- stats::sd(x)
  if (!is.finite(sigma) || sigma == 0) return(NA_real_)
  mu <- mean(x)
  mean(((x - mu) / sigma)^3)
}

safe_name <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", x)
}

non_missing_mask <- function(x) {
  if (is.character(x)) {
    !is.na(x) & x != ""
  } else {
    !is.na(x)
  }
}

numeric_summary_row <- function(column, x) {
  ok <- is.finite(x)
  x <- x[ok]
  if (!length(x)) {
    return(
      data.frame(
        column = column,
        n = 0L,
        missing = NA_integer_,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        q1 = NA_real_,
        q3 = NA_real_,
        max = NA_real_,
        iqr = NA_real_,
        skewness = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  data.frame(
    column = column,
    n = length(x),
    missing = NA_integer_,
    mean = mean(x),
    median = qs[2],
    sd = stats::sd(x),
    min = min(x),
    q1 = qs[1],
    q3 = qs[3],
    max = max(x),
    iqr = stats::IQR(x),
    skewness = skewness_value(x),
    stringsAsFactors = FALSE
  )
}

save_histogram <- function(x, column_name) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_character_)
  out_path <- file.path(hist_dir, paste0(safe_name(column_name), "_hist.png"))
  grDevices::png(out_path, width = 1100, height = 700, res = 130)
  graphics::hist(
    x,
    breaks = "FD",
    col = "#6BAED6",
    border = "white",
    main = paste("Histogram of", column_name),
    xlab = column_name
  )
  graphics::grid(nx = NA, ny = NULL, col = "#E0E0E0", lty = "dotted")
  grDevices::dev.off()
  out_path
}

save_boxplot <- function(x, column_name) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  out_path <- file.path(box_dir, paste0(safe_name(column_name), "_boxplot.png"))
  grDevices::png(out_path, width = 900, height = 700, res = 130)
  graphics::boxplot(
    x,
    horizontal = TRUE,
    col = "#9ECAE1",
    border = "#2171B5",
    main = paste("Boxplot of", column_name),
    xlab = column_name
  )
  graphics::grid(nx = NA, ny = NULL, col = "#E0E0E0", lty = "dotted")
  grDevices::dev.off()
  out_path
}

save_barplot <- function(counts, column_name, suffix) {
  if (!length(counts)) return(NA_character_)
  counts <- sort(counts, decreasing = TRUE)
  out_path <- file.path(bar_dir, paste0(safe_name(column_name), "_", suffix, ".png"))
  height_px <- max(900, 120 + 40 * length(counts))
  grDevices::png(out_path, width = 1200, height = height_px, res = 130)
  graphics::par(mar = c(5, max(10, 0.18 * max(nchar(names(counts)))), 4, 2) + 0.1)
  graphics::barplot(
    rev(as.numeric(counts)),
    names.arg = rev(names(counts)),
    horiz = TRUE,
    las = 1,
    col = "#74C476",
    border = "#238B45",
    main = paste("Bar Chart of", column_name, "(", suffix, ")", sep = ""),
    xlab = "Count",
    cex.names = 0.8
  )
  graphics::grid(nx = NULL, ny = NA, col = "#E0E0E0", lty = "dotted")
  grDevices::dev.off()
  out_path
}

save_date_plot <- function(x, column_name) {
  parsed <- parse_date_like(x)
  ok <- !is.na(parsed)
  parsed <- parsed[ok]
  if (!length(parsed)) return(NA_character_)

  years <- format(parsed, "%Y")
  counts <- sort(table(years), decreasing = TRUE)

  if (length(counts) <= 20L) {
    return(save_barplot(counts, column_name, "date_counts"))
  }

  out_path <- file.path(bar_dir, paste0(safe_name(column_name), "_year_hist.png"))
  year_num <- as.numeric(years)
  grDevices::png(out_path, width = 1100, height = 700, res = 130)
  graphics::hist(
    year_num,
    breaks = "FD",
    col = "#FDBE85",
    border = "white",
    main = paste("Year Distribution of", column_name),
    xlab = "Year"
  )
  graphics::grid(nx = NA, ny = NULL, col = "#E0E0E0", lty = "dotted")
  grDevices::dev.off()
  out_path
}

detect_family <- function(column_name, x) {
  if (is.logical(x)) return("categorical")
  if (is.numeric(x) || is.integer(x)) return("numeric")

  mask <- non_missing_mask(x)
  non_missing <- x[mask]
  if (!length(non_missing)) return("empty")

  parsed_dates <- parse_date_like(non_missing)
  date_ratio <- mean(!is.na(parsed_dates))
  if (date_ratio >= 0.8) return("date")

  numeric_like <- coerce_numeric_like(non_missing)
  numeric_ratio <- mean(!is.na(numeric_like))
  if (numeric_ratio >= 0.8) return("numeric_like_text")

  unique_n <- length(unique(non_missing))
  if (unique_n <= 20) return("categorical")
  "high_cardinality_categorical"
}

data <- utils::read.csv(data_path, stringsAsFactors = FALSE)

manifest <- data.frame(
  column = character(),
  original_class = character(),
  analysis_family = character(),
  non_missing = integer(),
  missing = integer(),
  unique_values = integer(),
  chart_1_type = character(),
  chart_1_file = character(),
  chart_2_type = character(),
  chart_2_file = character(),
  stringsAsFactors = FALSE
)

numeric_summary <- data.frame(
  column = character(),
  n = integer(),
  missing = integer(),
  mean = numeric(),
  median = numeric(),
  sd = numeric(),
  min = numeric(),
  q1 = numeric(),
  q3 = numeric(),
  max = numeric(),
  iqr = numeric(),
  skewness = numeric(),
  stringsAsFactors = FALSE
)

categorical_counts <- data.frame(
  column = character(),
  level = character(),
  count = integer(),
  percent = numeric(),
  included_in_chart = logical(),
  stringsAsFactors = FALSE
)

date_summary <- data.frame(
  column = character(),
  n = integer(),
  missing = integer(),
  earliest = character(),
  latest = character(),
  unique_timestamps = integer(),
  stringsAsFactors = FALSE
)

for (column_name in names(data)) {
  x <- data[[column_name]]
  mask <- non_missing_mask(x)
  non_missing_n <- sum(mask)
  missing_n <- length(x) - non_missing_n
  unique_n <- length(unique(x[mask]))
  family <- detect_family(column_name, x)

  chart_1_type <- ""
  chart_1_file <- ""
  chart_2_type <- ""
  chart_2_file <- ""

  if (family %in% c("numeric", "numeric_like_text")) {
    values <- coerce_numeric_like(x)
    summary_row <- numeric_summary_row(column_name, values)
    summary_row$missing <- missing_n
    numeric_summary <- rbind(numeric_summary, summary_row)
    chart_1_type <- "histogram"
    chart_1_file <- save_histogram(values, column_name)
    chart_2_type <- "boxplot"
    chart_2_file <- save_boxplot(values, column_name)
  } else if (family == "categorical") {
    values <- as.character(x[mask])
    counts <- sort(table(values), decreasing = TRUE)
    categorical_counts <- rbind(
      categorical_counts,
      data.frame(
        column = column_name,
        level = names(counts),
        count = as.integer(counts),
        percent = 100 * as.numeric(counts) / sum(counts),
        included_in_chart = TRUE,
        stringsAsFactors = FALSE
      )
    )
    chart_1_type <- "barplot_all_levels"
    chart_1_file <- save_barplot(counts, column_name, "all_levels")
  } else if (family == "high_cardinality_categorical") {
    values <- as.character(x[mask])
    counts <- sort(table(values), decreasing = TRUE)
    top_counts <- head(counts, 10)
    categorical_counts <- rbind(
      categorical_counts,
      data.frame(
        column = column_name,
        level = names(top_counts),
        count = as.integer(top_counts),
        percent = 100 * as.numeric(top_counts) / sum(counts),
        included_in_chart = TRUE,
        stringsAsFactors = FALSE
      )
    )
    chart_1_type <- "barplot_top10_levels"
    chart_1_file <- save_barplot(top_counts, column_name, "top10_levels")
  } else if (family == "date") {
    parsed <- parse_date_like(x)
    parsed_ok <- parsed[!is.na(parsed)]
    date_summary <- rbind(
      date_summary,
      data.frame(
        column = column_name,
        n = length(parsed_ok),
        missing = missing_n,
        earliest = if (length(parsed_ok)) format(min(parsed_ok), "%Y-%m-%d %H:%M:%S") else "",
        latest = if (length(parsed_ok)) format(max(parsed_ok), "%Y-%m-%d %H:%M:%S") else "",
        unique_timestamps = length(unique(parsed_ok)),
        stringsAsFactors = FALSE
      )
    )
    chart_1_type <- "date_distribution"
    chart_1_file <- save_date_plot(x, column_name)
  }

  manifest <- rbind(
    manifest,
    data.frame(
      column = column_name,
      original_class = class(x)[1],
      analysis_family = family,
      non_missing = non_missing_n,
      missing = missing_n,
      unique_values = unique_n,
      chart_1_type = chart_1_type,
      chart_1_file = chart_1_file,
      chart_2_type = chart_2_type,
      chart_2_file = chart_2_file,
      stringsAsFactors = FALSE
    )
  )
}

utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
utils::write.csv(numeric_summary, numeric_summary_path, row.names = FALSE, na = "")
utils::write.csv(categorical_counts, categorical_counts_path, row.names = FALSE, na = "")
utils::write.csv(date_summary, date_summary_path, row.names = FALSE, na = "")

run_summary <- c(
  paste("Rows:", nrow(data)),
  paste("Columns:", ncol(data)),
  paste("Numeric-like columns summarized:", sum(manifest$analysis_family %in% c("numeric", "numeric_like_text"))),
  paste("Categorical columns summarized:", sum(manifest$analysis_family == "categorical")),
  paste("High-cardinality categorical columns summarized:", sum(manifest$analysis_family == "high_cardinality_categorical")),
  paste("Date-like columns summarized:", sum(manifest$analysis_family == "date")),
  paste("Manifest:", manifest_path),
  paste("Numeric summary:", numeric_summary_path),
  paste("Categorical counts:", categorical_counts_path),
  paste("Date summary:", date_summary_path)
)
writeLines(run_summary, run_summary_path)

cat("Descriptive analysis written to:", out_dir, "\n")
cat("Columns processed:", ncol(data), "\n")
