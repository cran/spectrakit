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
#' @param folder Character. Path to the folder containing images. Default is `"."` (working directory).
#' @param custom_order Character vector. Ordered set of filenames (use NA for blank slots). Argument is required.
#' @param rows Integer. Number of rows in the grid. Argument is required.
#' @param cols Integer. Number of columns in the grid. Argument is required.
#' @param spacing Integer. Spacing (in pixels) between tiles. Default is `15`
#' @param resize_mode Character. Method to resize panels in the composite. One of:
#'   \describe{
#'     \item{`"none"`}{Keeps each panel at its original size (default).}
#'     \item{`"fit"`}{Scales each panel to fit within the smallest width and height among all images, preserving aspect ratio; no cropping occurs, empty space may remain.}
#'     \item{`"fill"`}{Scales each panel to completely cover the smallest width and height among all images, preserving aspect ratio, then crops any excess.}
#'     \item{`"width"`}{Resizes each panel to match the minimum width among all images, preserving aspect ratio; height scales accordingly.}
#'     \item{`"height"`}{Resizes each panel to match the minimum height among all images, preserving aspect ratio; width scales accordingly.}
#'     \item{`"both"`}{Resizes each panel to exactly match the minimum width and height among all images, without preserving aspect ratio; may cause distortion.}
#'   }
#' @param labels List of up to 4 character vectors. Labels to apply to each panel. Each vector corresponds to one label layer and must be the same length as the number of non-NA images. Use empty strings "" or NULL entries to omit specific labels. Default is `list()` (no labels).
#' @param label_settings List of named lists. Each named list specifies styling options for a label layer. Options include:
#'   \describe{
#'     \item{`size`}{Font size (e.g., `100`).}
#'     \item{`color`}{Font color (e.g., `"black"`.}
#'     \item{`font`}{Font family (e.g., `"Arial"`).}
#'     \item{`boxcolor`}{Background color behind text (e.g., `"white"`), or `NA` for none.}
#'     \item{`location`}{Offset from the gravity anchor (e.g., `"+10+10"`).}
#'     \item{`gravity`}{Placement anchor for the label (e.g., `"northwest"`).}
#'     \item{`weight`}{Font weight (e.g., `400` = normal, `700` = bold).}
#'   }
#'   Default is `list()` (default styling is used).
#' @param background_color Character. Background color used for blank tiles and borders. Use `"none"` for transparency. Default is `"white"`.
#' @param desired_width Numeric. Desired width of final image (in centimeters, inches or pixels). Default is `15`
#' @param width_unit Character. One of: `"cm"`, `"in"`, or `"px"`. Default is `"cm"`
#' @param ppi Numeric. Resolution (pixels per inch) for output file. Default is `300`
#' @param output_format Character. File format for saving image. Examples: `"tiff"`, `"png"`, `"pdf"`. Default is `"tiff"`.
#' @param output_folder Character. Path to folder where the composite image is saved. If NULL (default), the image is not saved and a `magick image` object is returned; If specified, the image is saved automatically; if `"."`, the image is saved in the working directory.
#'
#' @return If `output_folder = NULL`, the function returns a `magick image` object. When `output_folder` is specified, the composite image is written directly to disk and returned invisibly.
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
	  } else if (width_unit == "in") {
    		round(desired_width * ppi)
	  } else {
    		desired_width
	  }

        composite_resized <- image_resize(composite, paste0(desired_width_px, "x"))

        if (!is.null(output_folder)) {
                output_file <- file.path(
                        output_folder,
                        paste0("Composite_Image_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", output_format)
                )

                image_write(
                        composite_resized,
                        path = output_file,
                        format = output_format,
                        density = paste0(ppi, "x", ppi)
                )

                message("Composite saved to: ", output_file)

                return(invisible(composite_resized))
        } else {
                return(composite_resized)
        }
}
