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
#' @param folder Character. Path to the folder containing spectra files. Default is `"."` (working directory).
#' @param file_type Character. File extension (without dot) to search for. Default is `"csv"`.
#' @param sep Character. Delimiter for file columns. Use `","` for comma-separated (default) or `"\\t"` for tab-delimited files.
#' @param header Logical. Whether the files contain a header row. Default is `TRUE`.
#' @param common_col_pos Integer. Column position for the common variable (e.g., wavelength). Default is `1`.
#' @param data_col_pos Integer. Column position for the spectral intensity values. Default is `2`.
#' @param range Numeric vector of length 2. Range filter for the common column (e.g., wavelength limits). Default is `NULL` (no filtering).
#' @param normalization Character. Normalization method to apply to intensity values. One of:
#'   \describe{
#'     \item{`"none"`}{No normalization is applied (default).}
#'     \item{`"simple"`}{Divide by the maximum intensity.}
#'     \item{`"min-max"`}{Scale intensities to the [0,1] range.}
#'     \item{`"z-score"`}{Subtract the mean and divide by the standard deviation of intensities.}
#'     \item{`"area"`}{Divide by the total sum of intensities so the spectrum area = 1.}
#'     \item{`"vector"`}{Normalize the spectrum as a unit vector by dividing by the square root of the sum of squared intensities; also known as L2 normalization.}
#'   }
#' @param norm_scope Character. Determines whether normalization is applied to the full spectrum or only to a specified range. One of: `"global"` (full spectrum), or `"range"` (specified range). Default is `"global"`.
#' @param orientation Character. Output organization. One of: `"columns"`, to keep each spectrum as a column, or `"rows"`, to transpose so each spectrum is a row. Default is `"columns"`.
#'
#' @return A `tibble` that can be exported as, for example, a CSV file. Each spectrum is either a column (default) or row, depending on `orientation`; a sum or mean spectrum can optionally be computed using `rowSums()` or `colSums()`, or `rowMeans()` or `colMeans()`, respectively. The common column (e.g., wavelength) is retained.
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
                sep = ",",
                header = TRUE,
                common_col_pos = 1,
                data_col_pos = 2,
                range = NULL,
                normalization = c("none", "simple", "min-max", "z-score", "area", "vector"),
                norm_scope = c("global", "range"),
                orientation = c("columns", "rows")
) {

        normalization <- match.arg(normalization)
        orientation <- match.arg(orientation)
        norm_scope <- match.arg(norm_scope)

        files <- list.files(folder, pattern = paste0("\\.", file_type, "$"), full.names = TRUE)
        if (length(files) == 0) stop("No files found with the given extension in the folder.")

        # Read common column from first file
        first_file <- readr::read_delim(files[1], delim = sep, col_names = header, show_col_types = FALSE)

        if (max(common_col_pos) > ncol(first_file)) stop("common_col_pos exceeds number of columns.")
        if (max(data_col_pos) > ncol(first_file)) stop("data_col_pos exceeds number of columns.")

        common_col_names <- colnames(first_file)[common_col_pos]
        common_cols <- first_file[, common_col_pos, drop = FALSE]

        # Read data columns from all files, force numeric, warn on coercion
        data_list <- purrr::map(files, function(f) {
                dat <- readr::read_delim(f, delim = sep, col_names = header, show_col_types = FALSE)

                if (max(data_col_pos) > ncol(dat)) stop(paste0("data_col_pos exceeds columns in file: ", f))
                if (!identical(
                        dat[, common_col_pos, drop = FALSE],
                        common_cols
                )) {
                        stop(paste0("Common columns do not match in file: ", f))
                }

                dplyr::mutate(
                        dat[, data_col_pos, drop = FALSE],
                        dplyr::across(
                                dplyr::everything(),
                                ~ {
                                        if (is.numeric(.x)) {
                                                .x
                                        } else {
                                                suppressWarnings(nx <- as.numeric(.x))
                                                if (any(is.na(nx) & !is.na(.x))) {
                                                        warning(paste("NAs introduced by coercion in file:", f))
                                                }
                                                nx
                                        }
                                }
                        )
                )
        })

        combined_spectra <- tibble::as_tibble(common_cols)

        for (i in seq_along(data_list)) {
                df <- data_list[[i]]
                colnames(df) <- paste0(tools::file_path_sans_ext(basename(files[i])), "_", colnames(df))
                combined_spectra <- dplyr::bind_cols(combined_spectra, df)
        }

        data_cols <- setdiff(colnames(combined_spectra), common_col_names)

        normalize_fn <- function(df) {
                df |>
                        dplyr::mutate(
                                dplyr::across(
                                        dplyr::all_of(data_cols),
                                        ~ switch(
                                                normalization,
                                                none      = .x,
                                                simple    = .x / max(.x, na.rm = TRUE),
                                                "min-max" = (.x - min(.x, na.rm = TRUE)) /
                                                        (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
                                                "z-score" = as.numeric(scale(.x)),
                                                area      = .x / sum(.x, na.rm = TRUE),
                                                vector    = .x / sqrt(sum(.x^2, na.rm = TRUE))
                                        )
                                )
                        )
        }

        filter_fn <- function(df) {
                if (is.null(range)) return(df)
                filter_col <- common_col_names[1]
                df |>
                        dplyr::filter(
                                !!rlang::sym(filter_col) >= range[1],
                                !!rlang::sym(filter_col) <= range[2]
                        )
        }

        combined_spectra <-
                if (norm_scope == "global") {
                        combined_spectra |> normalize_fn() |> filter_fn()
                } else {
                        combined_spectra |> filter_fn() |> normalize_fn()
                }

        if (orientation == "rows") {
                spectral_data <- combined_spectra[, !(colnames(combined_spectra) %in% common_col_names)]
                transposed <- t(spectral_data)

                colnames(transposed) <- apply(
                        combined_spectra[, common_col_names, drop = FALSE],
                        1, paste, collapse = "_"
                )

                tibble::as_tibble(transposed, rownames = "Spectrum")
        } else {
                combined_spectra
        }
}
