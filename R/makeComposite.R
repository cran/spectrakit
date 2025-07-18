#' Create a labeled image grid
#'
#' Generates a composite image grid with customizable layout, labels and resizing options. Suitable for spectra and other image types.
#'
#' @importFrom stats na.omit
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 expansion
#' @importFrom dplyr %>%
#' @importFrom data.table :=
#' @importFrom magick image_read image_info image_resize image_annotate image_join image_append image_blank image_border image_write
#' @importFrom glue glue
#'
#' @param folder Character. Path to the folder containing images. Default is working directory (`"."`).
#' @param custom_order Character vector. Set of filenames (use NA for blank slots).
#' @param rows Integer. Number of rows in the grid.
#' @param cols Integer. Number of columns in the grid.
#' @param spacing Integer. Spacing (in pixels) between tiles. Default is `15`
#' @param resize_mode Character. One of "none", "fit", "fill", "width", "height", "both";
#'       - "none": keep each panel at original size
#'       - "fit": scale each panel to fit within the smallest image dimensions (preserving aspect ratio)
#'       - "fill": scale and crop each panel to exactly fill the smallest dimensions
#'       - "width": resize to minimum width, keep original height
#'       - "height": resize to minimum height, keep original width
#'       - "both": force exact width and height (may distort aspect ratio).
#' @param labels List of up to 4 character vectors. Each vector corresponds to one label layer and must be the same length as the number of non-NA images. Use empty strings "" or NULL entries to omit specific labels.
#' @param label_settings List of named lists. Each named list specifies settings for a label layer;
#'       - size: font size (e.g., 100)
#'       - color: font color
#'       - font: font family (e.g., "Arial")
#'       - boxcolor: background color behind text (or NA for none)
#'       - location: offset from gravity anchor (e.g., "+10+10")
#'       - gravity: placement anchor (e.g., "northwest")
#'       - weight: font weight (e.g., 400 = normal, 700 = bold).
#' @param background_color Character. Background color used for blank tiles and borders. Use `"none"` for transparency. Default is `"white"`.
#' @param desired_width Numeric. Desired width of final image (in cm or px). Default is `15`
#' @param width_unit Character. Either "cm" or "px". Default is `"cm"`
#' @param ppi Numeric. Resolution (pixels per inch) for output file. Default is `300`
#' @param output_format Character. File format for saving plots. Examples: `"tiff"`, `"png"`, `"pdf"`. Default is `"tiff"`.
#' @param output_folder Character. Path to folder where image is saved. If NULL (default), image is not saved; if `"."`, image is saved in the working directory.
#'
#' @return Saves image composite to a specified output folder. Returns `NULL` (used for side-effects).
#'
#' @examples
#' library(magick)
#'
#' tmp_dir <- file.path(tempdir(), "spectrakit_imgs")
#' dir.create(tmp_dir, showWarnings = FALSE)
#'
#' # Create and save img1
#' img1 <- image_blank(100, 100, "white")
#' img1 <- image_draw(img1)
#' symbols(50, 50, circles = 30, inches = FALSE, add = TRUE, bg = "red")
#' dev.off()
#' img1_path <- file.path(tmp_dir, "img1.png")
#' image_write(img1, img1_path)
#'
#' # Create and save img2
#' img2 <- image_blank(100, 100, "white")
#' img2 <- image_draw(img2)
#' rect(20, 20, 80, 80, col = "blue", border = NA)
#' dev.off()
#' img2_path <- file.path(tmp_dir, "img2.png")
#' image_write(img2, img2_path)
#'
#' # Create composite
#' makeComposite(
#'         folder = tmp_dir,
#'         custom_order = c("img1.png", "img2.png"),
#'         rows = 1,
#'         cols = 2,
#'         labels = list(c("Red Circle", "Blue Rectangle")),
#'         label_settings = list(
#'                 list(size = 5, font = "Arial", color = "black", boxcolor = "white",
#'                      gravity = "northwest", location = "+10+10", weight = 400)
#'         ),
#'         resize_mode = "none",
#'         desired_width = 10,
#'         width_unit = "cm",
#'         ppi = 300,
#'         output_format = "png",
#'         output_folder = tmp_dir
#' )
#'
#' @importFrom magick image_read image_info image_resize image_annotate image_join image_append image_blank image_border image_write
#' @importFrom glue glue
#' @export
makeComposite <- function(
                folder = ".",
                custom_order,
                rows,
                cols,
                spacing = 15,
                resize_mode = c("none", "fit", "fill", "width", "height", "both"),
                labels = list(),
                label_settings = list(),
                background_color = "white",
                desired_width = 15,
                width_unit = "cm",
                ppi = 300,
                output_format = "tiff",
                output_folder = NULL
) {

        `%||%` <- function(a, b) if (!is.null(a)) a else b

        resize_mode <- match.arg(resize_mode)
        expected_n <- rows * cols

        if (length(custom_order) != expected_n) {
                stop("Length of custom_order does not match grid size.")
        }

        valid_files <- na.omit(custom_order)
        image_paths <- file.path(folder, valid_files)

        missing <- image_paths[!file.exists(image_paths)]
        if (length(missing)) {
                stop("Missing files:\n", paste(missing, collapse = "\n"))
        }

        if (length(labels) > 0) {
                for (i in seq_along(labels)) {
                        if (length(labels[[i]]) != length(valid_files)) {
                                stop(glue("Label set {i} must have {length(valid_files)} elements."))
                        }
                }
        }

        imgs_info <- lapply(image_paths, function(p) image_info(image_read(p)))
        dims <- do.call(rbind, imgs_info)[, c("width", "height")]
        min_w <- min(dims[, "width"])
        min_h <- min(dims[, "height"])

        # Resize helper
        resize_image <- function(img) {
                switch(resize_mode,
                       none   = img,
                       fit    = image_resize(img, glue("{min_w}x{min_h}")),
                       fill   = image_resize(img, glue("{min_w}x{min_h}!")),
                       width  = image_resize(img, glue("{min_w}")),
                       height = image_resize(img, glue("x{min_h}")),
                       both   = image_resize(img, glue("{min_w}!x{min_h}!"))
                )
        }

        # Annotate helper
        annotate_image <- function(img, idx) {
                for (i in seq_along(labels)) {
                        if (length(labels[[i]]) >= idx && !is.null(labels[[i]][idx])) {
                                s <- label_settings[[i]]

                                args <- list(
                                        text     = labels[[i]][idx],
                                        size     = s$size     %||% 100,
                                        font     = s$font     %||% "Arial",
                                        color    = s$color    %||% "black",
                                        gravity  = s$gravity  %||% "northwest",
                                        location = s$location %||% "+10+10",
                                        weight   = s$weight   %||% 400
                                )

                                if (!is.null(s$boxcolor) && !is.na(s$boxcolor)) {
                                        args$boxcolor <- s$boxcolor
                                }

                                img <- do.call(image_annotate, c(list(img), args))
                        }
                }
                img
        }

        # Build tiles
        tiles <- vector("list", length(custom_order))
        real_idx <- 1
        for (i in seq_along(custom_order)) {
                if (is.na(custom_order[i])) {
                        tiles[[i]] <- image_blank(width = min_w, height = min_h, color = background_color)
                } else {
                        img <- image_read(file.path(folder, custom_order[i]))
                        img <- resize_image(img)
                        img <- annotate_image(img, real_idx)
                        tiles[[i]] <- image_border(img, color = background_color, geometry = paste0(spacing, "x", spacing))
                        real_idx <- real_idx + 1
                }
        }

        # Build grid
        rows_list <- split(tiles, ceiling(seq_along(tiles) / cols))
        grid_rows <- lapply(rows_list, function(row_imgs) image_append(image_join(row_imgs)))
        composite <- image_append(image_join(grid_rows), stack = TRUE)

        # Resize output
        desired_width_px <- if (width_unit == "cm") {
                round(desired_width * ppi / 2.54)
        } else {
                desired_width
        }

        composite_resized <- image_resize(composite, paste0(desired_width_px, "x"))

        if (!is.null(output_folder)) {
                output_file <- file.path(output_folder, paste0("Composite_Image_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", output_format))
                image_write(
                        composite_resized,
                        path = output_file,
                        format = output_format,
                        density = paste0(ppi, "x", ppi)
                )
                message("Composite saved to: ", output_file)
        }
        invisible(NULL)
}
