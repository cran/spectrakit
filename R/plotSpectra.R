#' Plot Spectral Data from Multiple Files
#'
#' Reads, normalizes and plots spectral data from files in a folder. Supports multiple plot modes, color palettes, axis customization, annotations and automatic saving of plots to files.
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
#' @param normalization Character. Normalization method to apply to y-axis data. One of `"none"`, `"simple"` (divide by max), `"min-max"`, or `"z-score"`. Default is `"none"`.
#' @param x_config Numeric vector of length 3. Specifies x-axis range and breaks: `c(min, max, step)`.
#' @param x_reverse Logical. If `TRUE`, reverses the x-axis. Default is `FALSE`.
#' @param y_trans Character. Transformation for the y-axis. One of `"linear"`, `"log10"`, or `"sqrt"`. Default is `"linear"`.
#' @param x_label Character or expression. Label for the x-axis. Supports mathematical notation via `expression()`.
#' @param y_label Character or expression. Label for the y-axis.
#' @param line_size Numeric. Width of the spectral lines. Default is `0.5`.
#' @param palette Character or vector. Color setting: a single color (e.g., `"black"`), a ColorBrewer palette name (e.g., `"Dark2"`), or a custom color vector.
#' @param plot_mode Character. Plotting style. One of `"individual"` (one plot per spectrum), `"overlapped"` (all in one), or `"stacked"` (faceted). Default is `"individual"`.
#' @param display_names Logical. If `TRUE`, adds file names as titles to individual spectra or a legend to combined spectra. Default is `FALSE`.
#' @param vertical_lines Numeric vector. Adds vertical dashed lines at given x positions.
#' @param shaded_ROIs List of numeric vectors. Each vector must have two elements (`xmin`, `xmax`) to define shaded x regions.
#' @param annotations Data frame with columns `file` (file name without extension), `x`, `y`, and `label`. Adds annotation labels to specific points in spectra.
#' @param output_format Character. File format for saving plots. Examples: `"tiff"`, `"png"`, `"pdf"`. Default is `"tiff"`.
#' @param output_folder Character. Path to folder where plots are saved. If NULL (default), plots are not saved; if `"."`, plots are saved in the working directory.
#'
#' @details
#' Color settings can support color-blind-friendly palettes from `RColorBrewer`. Use `display.brewer.all(colorblindFriendly = TRUE)` to preview.
#'
#' @return Saves plots to a specified output folder. Returns `NULL` (used for side-effects).
#'
#' @examples
#' # Create a temporary directory and write mock spectra files
#' tmp_dir <- tempdir()
#' write.csv(data.frame(Energy = 0:30, Counts = rpois(31, lambda = 100)),
#'           file.path(tmp_dir, "spec1.csv"), row.names = FALSE)
#' write.csv(data.frame(Energy = 0:30, Counts = rpois(31, lambda = 120)),
#'           file.path(tmp_dir, "spec2.csv"), row.names = FALSE)
#'
#' # Plot the mock spectra using various configuration options
#' plotSpectra(
#'   folder = tmp_dir,
#'   file_type = "csv",
#'   sep = ",",
#'   normalization = "min-max",
#'   x_config = c(0, 30, 5),
#'   x_reverse = FALSE,
#'   y_trans = "linear",
#'   x_label = expression(Energy~(keV)),
#'   y_label = expression(Counts/1000~s),
#'   line_size = 0.7,
#'   palette = c("black","red"),
#'   plot_mode = "overlapped",
#'   display_names = TRUE,
#'   vertical_lines = c(10, 20),
#'   shaded_ROIs = list(c(12, 14), c(18, 22)),
#'   output_format = "png",
#'   output_folder = tmp_dir
#' )
#'
#' @importFrom readr read_delim
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom ggplot2 ggplot aes geom_line labs scale_x_reverse scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_color_manual scale_color_brewer
#' @importFrom ggplot2 geom_vline annotate geom_text facet_wrap theme_bw theme element_blank
#' @importFrom ggplot2 element_line element_text element_rect margin ggsave
#' @importFrom tibble as_tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom utils head
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang sym
#' @export
plotSpectra <- function(
    folder = ".",
    file_type = "csv",
    sep = ",",  # Use "\t" if it is tab
    header = TRUE,
    normalization = c("none", "simple", "min-max", "z-score"),
    x_config = NULL, # Numeric vector of length 3 specifying axis limits and break positions, e.g. c(min, max, step)
    x_reverse = FALSE,
    y_trans = c("linear", "log10", "sqrt"), # Choose y-axis transformation: linear (default), log10, or sqrt
    x_label = "Energy (keV)", # For complex formatting, use expression(), e.g. expression(Wavenumber~(cm^{-1}))
    y_label = "Counts/1000 s", # For complex formatting, use expression(), e.g. expression(Delta*E["00"]^{"*"}~(a.u.))
    line_size = 0.5,
    palette = "black", # A single colour, or the ColorBrewer palette name "Dark2", or a custom vector
    plot_mode = c("individual", "overlapped", "stacked"),  # Default is "individual"
    display_names = FALSE, # If TRUE, displays the title for individual spectra or the legend for combined spectra
    vertical_lines = NULL,  # A numeric vector of x positions where vertical dashed lines will be drawn
    shaded_ROIs = NULL, # A list of numeric vectors, each with two elements c(xmin, xmax), defining shaded rectangular regions along x
    annotations = NULL, # A data frame with columns 'file', 'x', 'y', 'label'; adds text annotations at specified points in each spectrum
    output_format = "tiff", # Choose output file format ("tiff", "png", "pdf", etc.)
    output_folder = NULL
    ) {
  normalization <- match.arg(normalization)
  plot_mode <- match.arg(plot_mode)
  y_trans <- match.arg(y_trans)

  # Define the custom theme with dynamic legend control
  plot_theme <- theme_bw(base_family = "sans") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.x = element_text(color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 10),
      axis.title.x = element_text(color = "black", size = 12),
      axis.title.y = element_text(color = "black", size = 12),
      legend.position = if (display_names) "right" else "none",
      legend.title = element_blank(),
      plot.margin = margin(0.2,0.5,0.2,0.2, "cm")
    )

  # Read files
  files <- list.files(folder, pattern = paste0("\\.", file_type, "$"), full.names = TRUE)
  spectra_list <- lapply(files, function(file) {
    data <- read_delim(file, delim = sep, col_names = header, show_col_types = FALSE)

    # Rename first two columns to "x" and "y"
    if (ncol(data) >= 2) {
      colnames(data)[1:2] <- c("x", "y")
    } else {
      stop(paste("File", file, "must have at least two columns for x and y"))
    }

    # Ensure numeric
    data <- data %>%
      mutate(x = as.numeric(x),
             y = as.numeric(y))

    # Apply normalization
    data$y <- switch(normalization,
                     none = data$y,
                     simple = data$y / max(data$y, na.rm = TRUE),
                     "min-max" = (data$y - min(data$y, na.rm = TRUE)) / (max(data$y, na.rm = TRUE) - min(data$y, na.rm = TRUE)),
                     "z-score" = scale(data$y)[,1])
    data$file <- tools::file_path_sans_ext(basename(file))
    return(data)
  })

  spectra <- bind_rows(spectra_list)

  # Determine color scale
  n_files <- length(unique(spectra$file))
  color_scale <- if (length(palette) == 1 && palette != "Dark2") {
    scale_color_manual(values = rep(palette, n_files))
  } else if (identical(palette, "Dark2")) {
    scale_color_brewer(palette = "Dark2")
  } else if (length(palette) > 1) {
    scale_color_manual(values = rep(palette, length.out = n_files))
  } else {
    stop("Invalid `palette`. Use a single color, 'Dark2', or a custom vector.")
  }

  if (plot_mode == "individual") {
    for (file_name in unique(spectra$file)) {
      data_sub <- filter(spectra, file == !!file_name)
      p <- ggplot(data_sub, aes(x = x, y = y)) +
        geom_line(linewidth = line_size, color = if (length(palette) == 1 && palette != "Dark2") palette else "black") +
        labs(x = x_label, y = y_label, title = if (display_names) file_name else NULL) +
        plot_theme

      # x-axis scale
      if (!is.null(x_config)) {
              if (x_reverse) {
                      p <- p + scale_x_reverse(
                              limits = c(x_config[2], x_config[1]),
                              breaks = seq(x_config[2], x_config[1], -x_config[3]),
                              expand = expansion()
                      )
              } else {
                      p <- p + scale_x_continuous(
                              limits = x_config[1:2],
                              breaks = seq(x_config[1], x_config[2], x_config[3]),
                              expand = expansion()
                      )
              }
      }

      # y-axis scale
      if (y_trans != "linear") {
        p <- p + scale_y_continuous(trans = y_trans)
      }

      # Optional extras
      if (!is.null(vertical_lines)) {
        for (v in vertical_lines) p <- p + geom_vline(xintercept = v, linetype = "dashed", color = "grey30")
      }
      if (!is.null(shaded_ROIs)) {
        for (roi in shaded_ROIs) p <- p + annotate("rect", xmin = roi[1], xmax = roi[2], ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey55")
      }
      if (!is.null(annotations)) {
        ann_sub <- filter(annotations, file == !!file_name)
        if (nrow(ann_sub) > 0) {
          p <- p + geom_text(data = ann_sub, aes(x = x, y = y, label = label), inherit.aes = FALSE)
        }
      }

      if (!is.null(output_folder)) {
      ggsave(
        filename = paste0(tools::file_path_sans_ext(file_name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", output_format),
        plot = p,
        device = output_format,
        path = output_folder,
        scale = 1,
        width = 15,
        height = 9.3,
        units = "cm",
        dpi = 300,
        limitsize = TRUE,
        bg = "white"
      )
      }
      invisible(NULL)
    }
  } else {
    p <- ggplot(spectra, aes(x = x, y = y, color = file)) +
      geom_line(linewidth = line_size) +
      labs(x = x_label, y = y_label, color = "Spectrum") +
      plot_theme +
      color_scale

    # x-axis scale
    if (!is.null(x_config)) {
            if (x_reverse) {
                    p <- p + scale_x_reverse(
                            limits = c(x_config[2], x_config[1]),
                            breaks = seq(x_config[2], x_config[1], -x_config[3]),
                            expand = expansion()
                    )
            } else {
                    p <- p + scale_x_continuous(
                            limits = x_config[1:2],
                            breaks = seq(x_config[1], x_config[2], x_config[3]),
                            expand = expansion()
                    )
            }
    }

    # y-axis scale
    if (y_trans != "linear") {
      p <- p + scale_y_continuous(trans = y_trans)
    }

    if (plot_mode == "stacked") {
      p <- p +
        facet_wrap(~ file, ncol = 1, scales = "free_y") +
        theme(
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", linewidth = 0.25),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0, "mm"),            # Remove spacing between facets
          strip.background = element_blank(),       # Remove gray title bars
          strip.text = element_blank()              # Remove text in the facets
        )
    }
    if (!is.null(vertical_lines)) {
      for (v in vertical_lines) p <- p + geom_vline(xintercept = v, linetype = "dashed", color = "grey30")
    }
    if (!is.null(shaded_ROIs)) {
      for (roi in shaded_ROIs) p <- p + annotate("rect", xmin = roi[1], xmax = roi[2], ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey55")
    }
    if (!is.null(annotations)) {
      p <- p + geom_text(data = annotations, aes(x = x, y = y, label = label), inherit.aes = FALSE)
    }

    if (!is.null(output_folder)) {
    ggsave(
      filename = paste0("Combined_Spectra_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", output_format),
      plot = p,
      device = output_format,
      path = output_folder,
      scale = 1,
      width = 15,
      height = 9.3,
      units = "cm",
      dpi = 300,
      limitsize = TRUE,
      bg = "white"
    )
    }
    invisible(NULL)
  }
}
