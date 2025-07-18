#' Combine and Normalize Spectral Data from Multiple Files
#'
#' Reads spectral data from multiple files in a folder, merges them by a common column (e.g., wavelength),
#' optionally filters by a range, applies normalization and returns the data in either row-wise or column-wise format.
#'
#' @importFrom stats na.omit
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 expansion
#' @importFrom dplyr %>%
#' @importFrom data.table :=
#'
#' @param folder Character. Path to the folder containing spectral files. Default is working directory (`"."`).
#' @param file_type Character. File extension (without dot) to search for. Default is `"csv"`.
#' @param sep Character. Delimiter for file columns. Use `","` for comma-separated (default) or `"\\t"` for tab-delimited files.
#' @param header Logical. Whether the files contain a header row. Default is `TRUE`.
#' @param common_col_pos Integer. Column position for the common variable (e.g., wavelength). Defaults to `1`.
#' @param data_col_pos Integer. Column position for the spectral intensity values. Defaults to `2`.
#' @param range Numeric vector of length 2. Optional range filter for the common column (e.g., wavelength limits). Defaults to `NULL` (no filtering).
#' @param normalization Character. Normalization method to apply to spectra. One of `"none"`, `"simple"` (divide by max), `"min-max"`, or `"z-score"`. Default is `"none"`.
#' @param orientation Character. Output orientation. Use `"columns"` (default) to keep each spectrum as a column, or `"rows"` to transpose so each spectrum is a row.
#'
#' @return A `tibble` that can be exported as, for example, a CSV file. Each spectrum is either a column (default) or row, depending on `orientation`. The common column (e.g., wavelength) is retained.
#'
#' @examples
#' # Create a temporary directory for mock CSV files
#' tmp_dir <- tempdir()
#'
#' # Define file paths
#' tmp1 <- file.path(tmp_dir, "file1.csv")
#' tmp2 <- file.path(tmp_dir, "file2.csv")
#'
#' # Write two mock CSV files in the temporary folder
#' write.csv(data.frame(ID = c("A", "B", "C"), val = c(1, 2, 3)), tmp1, row.names = FALSE)
#' write.csv(data.frame(ID = c("A", "B", "C"), val = c(4, 5, 6)), tmp2, row.names = FALSE)
#'
#' # Merge the CSV files in the temporary folder, normalize with z-score, and return transposed
#' result <- combineSpectra(
#'   folder = tmp_dir,
#'   file_type = "csv",
#'   sep = ",",
#'   common_col_pos = 1,
#'   data_col_pos = 2,
#'   normalization = "z-score",
#'   orientation = "rows"
#' )
#'
#' @importFrom readr read_delim
#' @importFrom purrr map
#' @importFrom dplyr filter mutate across all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom rlang sym
#' @export
combineSpectra <- function(
    folder = ".",
    file_type = "csv",
    sep = ",", # Use "\t" if it is tab
    header = TRUE,
    common_col_pos = 1, # Position of the common column in each file (default = 1)
    data_col_pos = 2, # Position of the data column to merge (default = 2)
    range = NULL, # Numeric vector of length 2. Range for filtering the common column (default = NULL)
    normalization = c("none", "simple", "min-max", "z-score"),
    orientation = c("columns", "rows") # Whether each spectrum is a column or a row
) {
  normalization <- match.arg(normalization)
  orientation <- match.arg(orientation)

  files <- list.files(folder, pattern = paste0("\\.", file_type, "$"), full.names = TRUE)
  if (length(files) == 0) stop("No files found with the given extension in the folder.")

  # Read common column from first file
  first_file <- read_delim(files[1], delim = sep, col_names = header, show_col_types = FALSE)
  if (common_col_pos > ncol(first_file)) stop("common_col_pos exceeds number of columns in the files.")
  common_col_name <- colnames(first_file)[common_col_pos]
  common_col_data <- first_file[[common_col_pos]]

  # Read data columns from all files, force numeric, warn on coercion
  data_list <- map(files, function(f) {
    dat <- read_delim(f, delim = sep, col_names = header, show_col_types = FALSE)
    if (data_col_pos > ncol(dat)) stop(paste0("data_col_pos exceeds columns in file: ", f))
    col_data <- dat[[data_col_pos]]
    # Coerce to numeric if needed
    if (!is.numeric(col_data)) {
      suppressWarnings(numeric_data <- as.numeric(col_data))
      if (any(is.na(numeric_data) & !is.na(col_data))) {
        warning(paste("NAs introduced by coercion to numeric in file:", f))
      }
      col_data <- numeric_data
    }
    return(col_data)
  })

  combined_spectra <- tibble(!!common_col_name := common_col_data)
  data_names <- tools::file_path_sans_ext(basename(files))

  for (i in seq_along(data_list)) {
    combined_spectra[[data_names[i]]] <- data_list[[i]]
  }

  if (!is.null(range)) {
    combined_spectra <- combined_spectra %>% filter(!!sym(common_col_name) >= range[1], !!sym(common_col_name) <= range[2])
  }

  data_cols <- setdiff(colnames(combined_spectra), common_col_name)

  combined_spectra <- combined_spectra %>%
    mutate(across(all_of(data_cols), ~ switch(normalization,
                                              none = .x,
                                              simple = .x / max(.x, na.rm = TRUE),
                                              "min-max" = (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
                                              "z-score" = as.numeric(scale(.x))
    )))

  if (orientation == "rows") {
    # Transpose combined_spectra (excluding the common column)
    transposed <- t(combined_spectra[, -1])
    # Set column names to original common column values
    colnames(transposed) <- combined_spectra[[common_col_name]]

    # Convert back to tibble/data.frame, add a Spectrum column for row names (original file names)
    transposed_spectra <- as_tibble(transposed, rownames = "Spectrum")

    return(transposed_spectra)
  } else {
    return(combined_spectra)
  }
}
