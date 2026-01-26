# Fix global variables first (no roxygen needed)
if (getRversion() >= "2.15.1") utils::globalVariables(c(".data", "PC1", "PC2", "varname"))

#' Perform PCA and Create a Plot of Scores, Loadings, or Biplot
#'
#' Computes principal component analysis (PCA) on numeric variables in a dataset
#' and generates a PC1 vs PC2 visualization (scores, loadings, or biplot).
#'
#' @param data A data frame containing numeric variables. Non-numeric columns are ignored.
#' @param color_var Optional character. Column name for coloring points by group. Converted to factor internally.
#' @param shape_var Optional character. Column name for shaping points by group. Converted to factor internally.
#' @param plot_type Character. Type of PCA plot to generate:
#'   \describe{
#'     \item{`"score"`}{Plot PCA scores (observations).}
#'     \item{`"loadings"`}{Plot PCA loadings (variables).}
#'     \item{`"biplot"`}{Combine scores and loadings in a biplot.}
#'   }
#' @param palette Character or vector. Color palette for groups:
#'   \describe{
#'     \item{`"Dark2"`}{Use Dark2 palette from RColorBrewer (requires the package).}
#'     \item{single color}{A single color repeated for all groups.}
#'     \item{vector of colors}{Custom vector of colors, recycled to match number of groups.}
#'   }
#' @param show_labels Logical. Display labels for points (scores) or variables (loadings). Default is TRUE.
#' @param ellipses Logical. Draw confidence ellipses around groups in score/biplot. Grouping logic for ellipses follows this priority:
#' \itemize{
#'   \item If \code{ellipse_var} is provided, ellipses are drawn by that variable.
#'   \item Else, if \code{color_var} is provided, ellipses are drawn by color groups.
#'   \item Else, if \code{shape_var} is provided, ellipses are drawn by shape groups.
#'   \item If none are provided, no ellipses are drawn.
#' }
#' Default is FALSE.
#' @param ellipse_var Optional character. Name of the variable used to group ellipses. Takes precedence over all other grouping variables. Converted to factor internally. Default is NULL.
#' @param display_names Logical. Show legend if TRUE. Default is TRUE.
#' @param legend_title Optional character. Legend title corresponding to `color_var` or `shape_var`. Default is NULL.
#' @param return_pca Logical. If TRUE, return a list with plot and PCA object. Default is FALSE.
#' @param output_format Character. File format for saving plots. Examples: `"tiff"`, `"png"`, `"pdf"`. Default is `"tiff"`.
#' @param output_folder Character. Path to folder where plots are saved. If NULL (default), returns a ggplot object (or list with plot and PCA if `return_pca = TRUE`). If specified, plot is saved automatically (function returns PCA object only if `return_pca = TRUE`); if `"."`, plot is saved in the working directory.
#'
#' @return A ggplot2 object representing the PCA plot, or a list with `plot` and `pca` if `return_pca = TRUE`.
#'
#' @examples
#' plotPCA(
#'   data = iris,
#'   color_var = "Species",
#'   shape_var = "Species",
#'   plot_type = "biplot",
#'   palette = "Dark2",
#'   show_labels = TRUE,
#'   ellipses = FALSE,
#'   display_names = TRUE,
#'   legend_title = "Iris Species"
#' )
#'
#' @importFrom stats complete.cases prcomp sd
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text geom_hline geom_vline labs theme_bw theme element_blank element_line element_text margin scale_fill_brewer scale_fill_manual scale_shape_manual scale_alpha_continuous stat_ellipse arrow unit
#' @importFrom ggrepel geom_text_repel
#' @export
plotPCA <- function(data,
                    color_var = NULL,
                    shape_var = NULL,
                    plot_type = c("score", "loadings", "biplot"),
                    palette = "Dark2",
                    show_labels = TRUE,
                    ellipses = FALSE,
                    ellipse_var = NULL,
                    display_names = TRUE,
                    legend_title = NULL,
                    return_pca = FALSE,
                    output_format = "tiff",
                    output_folder = NULL
) {

        # --- Required packages ---
        pkgs <- c("ggplot2", "ggrepel")
        sapply(pkgs, function(p) {
                if (!requireNamespace(p, quietly = TRUE))
                        stop(sprintf("Package '%s' is required.", p))
        })

        plot_type <- match.arg(plot_type)

        # --- Keep numeric columns and handle NAs ---
        numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
        if (ncol(numeric_data) == 0)
                stop("No numeric columns found. PCA requires numeric input.")

        complete_idx <- complete.cases(numeric_data)
        numeric_data <- numeric_data[complete_idx, , drop = FALSE]
        data_used    <- data[complete_idx, , drop = FALSE]

        # --- PCA ---
        pca <- prcomp(numeric_data, scale. = TRUE)
        eig_vals <- (pca$sdev^2) / sum(pca$sdev^2) * 100

        # --- Shape settings ---
        shape_values <- c(21, 24, 22, 23, 25, 8)
        point_alpha  <- 0.8
        point_stroke <- 0.8

        # --- Custom theme ---
        plot_theme <- ggplot2::theme_bw(base_family = "sans") +
                ggplot2::theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.ticks = element_line(color = "black"),
                        axis.text = element_text(color = "black", size = 10),
                        axis.title = element_text(color = "black", size = 12),
                        legend.position = if (display_names) "right" else "none",
                        plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
                )

        p <- NULL

        # ============================================================
        # Score plot or Biplot
        # ============================================================
        if (plot_type %in% c("score", "biplot")) {

                scores <- as.data.frame(pca$x[, 1:2])
                colnames(scores) <- c("PC1", "PC2")

                # --- Attach grouping variables ---
                vars_to_add <- unique(c(color_var, shape_var, ellipse_var))
                vars_to_add <- vars_to_add[!is.null(vars_to_add)]
                for (v in vars_to_add) {
                        if (!v %in% colnames(data_used))
                                stop(sprintf("`%s` not found in data.", v))
                        scores[[v]] <- factor(data_used[[v]])
                }

                # --- Aesthetic mapping ---
                aes_map <- aes(PC1, PC2)

                if (!is.null(color_var) && !is.null(shape_var)) {
                        aes_map <- aes(
                                PC1, PC2,
                                fill  = .data[[color_var]],
                                shape = .data[[shape_var]]
                        )
                } else if (!is.null(color_var)) {
                        aes_map <- aes(PC1, PC2, color = .data[[color_var]])
                } else if (!is.null(shape_var)) {
                        aes_map <- aes(PC1, PC2, shape = .data[[shape_var]])
                }

                p <- ggplot(scores, aes_map) +
                        geom_point(
                                size   = 3,
                                alpha  = point_alpha,
                                color  = "black",
                                stroke = point_stroke
                        ) +
                        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        labs(
                                x = paste0("PC1 (", round(eig_vals[1], 1), "%)"),
                                y = paste0("PC2 (", round(eig_vals[2], 1), "%)")
                        ) +
                        plot_theme

                # --- Legend titles ---
                lab_args <- list()
                if (!is.null(color_var)) lab_args$color <- legend_title
                if (!is.null(color_var) && !is.null(shape_var)) lab_args$fill <- legend_title
                if (!is.null(shape_var)) lab_args$shape <- legend_title
                if (length(lab_args)) p <- p + do.call(labs, lab_args)

                # --- Color palette ---
                if (!is.null(color_var)) {

                        n_groups <- length(unique(scores[[color_var]]))

                        if (!is.null(shape_var)) {
                                scale_fun  <- scale_fill_manual
                                brewer_fun <- scale_fill_brewer
                        } else {
                                scale_fun  <- scale_color_manual
                                brewer_fun <- scale_color_brewer
                        }

                        if (length(palette) == 1 && palette == "Dark2") {
                                if (!requireNamespace("RColorBrewer", quietly = TRUE))
                                        stop("Package 'RColorBrewer' is required for palette = 'Dark2'.")
                                p <- p + brewer_fun(palette = "Dark2")
                        } else if (length(palette) >= 1) {
                                p <- p + scale_fun(values = rep(palette, length.out = n_groups))
                        } else {
                                stop("Invalid `palette`.")
                        }
                }

                # --- Shape scale ---
                if (!is.null(shape_var)) {
                        n_shapes <- length(unique(scores[[shape_var]]))
                        if (n_shapes > length(shape_values))
                                stop("Too many levels in `shape_var` for available shapes.")
                        p <- p + scale_shape_manual(values = shape_values)
                }

                # --- Ellipses ---
                if (ellipses) {

                        grp <- if (!is.null(ellipse_var)) ellipse_var
                        else if (!is.null(color_var)) color_var
                        else if (!is.null(shape_var)) shape_var
                        else NULL

                        if (!is.null(grp)) {
                                ellipse_aes <- aes(group = .data[[grp]])
                                if (is.null(shape_var)) {
                                        ellipse_aes <- aes(group = .data[[grp]], color = .data[[grp]])
                                }
                                p <- p + stat_ellipse(ellipse_aes)
                        }
                }

                # --- Labels ---
                if (show_labels) {
                        p <- p + ggrepel::geom_text_repel(
                                aes(label = rownames(scores)),
                                color = "black",
                                size  = 3
                        )
                }

                # --- Biplot loadings ---
                if (plot_type == "biplot") {

                        loadings <- as.data.frame(pca$rotation[, 1:2])
                        loadings$varname <- rownames(loadings)

                        rx <- diff(range(loadings$PC1))
                        ry <- diff(range(loadings$PC2))
                        if (rx == 0 || ry == 0)
                                stop("Cannot scale biplot loadings: zero variance on an axis.")

                        scale_fac <- min(
                                diff(range(scores$PC1)) / rx,
                                diff(range(scores$PC2)) / ry
                        ) * 0.7

                        loadings[, 1:2] <- loadings[, 1:2] * scale_fac

                        p <- p +
                                geom_segment(
                                        data = loadings,
                                        aes(x = 0, y = 0, xend = PC1, yend = PC2),
                                        arrow = arrow(length = unit(0.2, "cm")),
                                        inherit.aes = FALSE,
                                        color = "black"
                                ) +
                                geom_text(
                                        data = loadings,
                                        aes(x = PC1, y = PC2, label = varname),
                                        inherit.aes = FALSE,
                                        fontface = "bold",
                                        vjust = -0.7
                                )
                }
        }

        # ============================================================
        # Loadings-only plot
        # ============================================================
        if (plot_type == "loadings") {

                loadings <- as.data.frame(pca$rotation[, 1:2])
                colnames(loadings) <- c("PC1", "PC2")
                loadings$varname <- rownames(loadings)

                contrib <- (loadings$PC1^2 + loadings$PC2^2) /
                        sum(loadings$PC1^2 + loadings$PC2^2)

                p <- ggplot(loadings, aes(PC1, PC2)) +
                        geom_segment(
                                aes(x = 0, y = 0, xend = PC1, yend = PC2, alpha = contrib),
                                arrow = arrow(length = unit(0.2, "cm")),
                                color = "black"
                        ) +
                        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
                        labs(
                                x = paste0("PC1 (", round(eig_vals[1], 1), "%)"),
                                y = paste0("PC2 (", round(eig_vals[2], 1), "%)")
                        ) +
                        plot_theme

                if (show_labels) {
                        p <- p + ggrepel::geom_text_repel(
                                aes(label = varname),
                                size = 3
                        )
                }
        }

        # ============================================================
        # Saving / return
        # ============================================================
        if (!is.null(output_folder)) {

                if (!dir.exists(output_folder))
                        dir.create(output_folder, recursive = TRUE)

                ggsave(
                        filename = paste0(
                                "PCA_", format(Sys.time(), "%Y%m%d_%H%M%S"),
                                ".", output_format
                        ),
                        plot   = p,
                        device = output_format,
                        path   = output_folder,
                        width  = 15,
                        height = 9.3,
                        units  = "cm",
                        dpi    = 300,
                        bg     = "white"
                )

                if (return_pca) return(list(pca = pca))
                invisible(NULL)

        } else {
                if (return_pca) return(list(plot = p, pca = pca))
                return(p)
        }
}
