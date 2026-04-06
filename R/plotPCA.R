# Fix global variables first (no roxygen needed)
if (getRversion() >= "2.15.1") utils::globalVariables(c(".data", "varname", "PC", "Cumulative", "ylim"))

#' Perform PCA and Create a Plot of Scores, Loadings, or Biplot for Selected Principal Components
#'
#' Computes principal component analysis (PCA) on numeric variables in a dataset
#' and generates a plot of two selected principal components (scores, loadings, or biplot).
#'
#' @param data Data frame containing numeric variables. Numeric variables are centered and scaled by default; non-numeric columns are ignored. Argument is required.
#' @param pcs Numeric vector of length 2. Indicates which principal components to plot. Default is c(1, 2).
#' @param color_var Character. Column name for coloring points by group; converted to factor internally. Default is `NULL` (all points same color).
#' @param shape_var Character. Column name for shaping points by group; converted to factor internally. Default is `NULL` (all points same shape).
#' @param plot_type Character. Type of PCA plot to generate. One of:
#'   \describe{
#'     \item{`"score"`}{Plot PCA scores, i.e., observations (default).}
#'     \item{`"loading"`}{Plot PCA loadings, i.e., variables.}
#'     \item{`"biplot"`}{Combine scores and loadings in a biplot.}
#'     \item{`"cumvar"`}{Plot cumulative variance explained across principal components.}
#'   }
#' @param palette Character or vector. Color setting for groups. One of:
#' \itemize{
#'         \item A single color repeated for all groups.
#'         \item A ColorBrewer palette name (default is `"Dark2"`; requires the package to be installed).
#'         \item A custom color vector, recycled to match the number of groups.
#' }
#' @param score_labels Logical or character. Controls labeling of points (scores). One of:
#' \describe{
#'         \item{TRUE}{Uses row names for labels (default).}
#'         \item{column name}{Uses a column in the data frame for labels.}
#'         \item{FALSE}{No labels are shown.}
#' }
#' @param loading_labels Logical. If `TRUE`, displays labels for variables (loadings). Default is TRUE.
#' @param ellipses Logical. If `TRUE`, draws confidence ellipses around groups in score/biplot. Grouping logic for ellipses follows this priority:
#' \describe{
#'   \item{ellipse_var}{
#'     If provided, ellipses are drawn by that variable.
#'   }
#'   \item{color_var}{
#'     If \code{ellipse_var} is not provided, ellipses are drawn by color groups.
#'   }
#'   \item{shape_var}{
#'     If neither \code{ellipse_var} nor \code{color_var} is provided, ellipses are drawn by shape groups.
#'   }
#'   \item{none}{
#'     If none are provided, no ellipses are drawn.
#'   }
#' }
#' Default is FALSE.
#' @param ellipse_var Character. Name of the variable used to group ellipses. Takes precedence over all other grouping variables; converted to factor internally. Default is NULL.
#' @param display_names Logical. Shows legend if TRUE. Default is FALSE.
#' @param legend_title Character. Legend title corresponding to `color_var` and/or `shape_var`. Default is NULL.
#' @param return_pca Logical. If TRUE, return a list with plot and PCA object. Default is FALSE.
#' @param output_format Character. File format for saving plots. Examples: `"tiff"`, `"png"`, `"pdf"`. Default is `"tiff"`.
#' @param output_folder Character. Path to folder where plots are saved. If NULL (default), plot is not saved and the ggplot object (or list with plot and PCA if `return_pca = TRUE`) is returned. If specified, plot is saved automatically (function returns PCA object only if `return_pca = TRUE`); if `"."`, plot is saved in the working directory.
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
                    pcs = c(1, 2),
                    color_var = NULL,
                    shape_var = NULL,
                    plot_type = c("score", "loading", "biplot", "cumvar"),
                    palette = "Dark2",
                    score_labels = TRUE,
                    loading_labels = TRUE,
                    ellipses = FALSE,
                    ellipse_var = NULL,
                    display_names = FALSE,
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
        cum_var <- cumsum(eig_vals)
        n_pc_95 <- which(cum_var >= 95)[1]
        n_pc_99 <- which(cum_var >= 99)[1]

        # --- Validate and clean pcs ---
        if (length(pcs) != 2)
                stop("`pcs` must be a numeric vector of length 2 (e.g., c(1,2)).")

        if (!is.numeric(pcs))
                stop("`pcs` must be numeric.")

        pcs <- unique(pcs)

        if (length(pcs) != 2)
                stop("`pcs` must contain two different PC indices.")

        if (any(pcs > ncol(pca$x)))
                stop("Selected PCs exceed available components.")

        if (any(pcs < 1))
                stop("`pcs` must be positive integers.")

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
        xpc <- paste0("PC", pcs[1])
        ypc <- paste0("PC", pcs[2])

        if (plot_type %in% c("score", "biplot")) {

                scores <- as.data.frame(pca$x[, pcs])
                colnames(scores) <- paste0("PC", pcs)

                # --- Attach grouping variables ---
                vars_to_add <- unique(c(color_var, shape_var, ellipse_var))
                for (v in vars_to_add) {
                        if (!v %in% colnames(data_used))
                                stop(sprintf("`%s` not found in data.", v))
                        scores[[v]] <- factor(data_used[[v]])
                }

                # --- Aesthetic mapping ---
                aes_map <- aes(.data[[xpc]], .data[[ypc]])

                if (!is.null(color_var) && !is.null(shape_var)) {
                        aes_map <- aes(
                                .data[[xpc]], .data[[ypc]],
                                fill  = .data[[color_var]],
                                shape = .data[[shape_var]]
                        )
                } else if (!is.null(color_var)) {
                        aes_map <- aes(.data[[xpc]], .data[[ypc]], color = .data[[color_var]])
                } else if (!is.null(shape_var)) {
                        aes_map <- aes(.data[[xpc]], .data[[ypc]], shape = .data[[shape_var]])
                }

                geom_args <- list(
                        size   = 3,
                        alpha  = point_alpha,
                        stroke = point_stroke
                )
                if (!is.null(shape_var)) geom_args$color <- "black"

                p <- ggplot(scores, aes_map) +
                        do.call(geom_point, geom_args) +
                        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        labs(
                                x = paste0("PC", pcs[1], " (", round(eig_vals[pcs[1]], 1), "%)"),
                                y = paste0("PC", pcs[2], " (", round(eig_vals[pcs[2]], 1), "%)")
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

                        if (length(palette) == 1 &&
                            requireNamespace("RColorBrewer", quietly = TRUE) &&
                            palette %in% rownames(RColorBrewer::brewer.pal.info)) {

                                cols <- scales::brewer_pal(palette = palette)(n_groups)
                                p <- p + scale_fun(values = cols)

                        } else if (length(palette) == 1) {

                                p <- p + scale_fun(values = rep(palette, n_groups))

                        } else if (length(palette) > 1) {

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
                if (!identical(score_labels, FALSE)) {

                        label_vec <-
                                if (isTRUE(score_labels)) {
                                        rownames(scores)
                                } else if (is.character(score_labels) && length(score_labels) == 1) {
                                        if (!score_labels %in% names(data_used))
                                                stop(sprintf("Column '%s' not found in data.", score_labels))
                                        data_used[[score_labels]]
                                } else {
                                        stop("`score_labels` must be TRUE, FALSE, or a column name.")
                                }

                        p <- p + ggrepel::geom_text_repel(
                                aes(label = label_vec),
                                color = "black",
                                size  = 3
                        )
                }

                # --- Biplot loadings ---
                if (plot_type == "biplot") {

                        loadings <- as.data.frame(pca$rotation[, pcs])
                        colnames(loadings) <- paste0("PC", pcs)
                        loadings$varname <- rownames(loadings)

                        rx <- diff(range(loadings[[xpc]]))
                        ry <- diff(range(loadings[[ypc]]))
                        if (rx == 0 || ry == 0)
                                stop("Cannot scale biplot loadings: zero variance on an axis.")

                        scale_fac <- min(
                                diff(range(scores[[xpc]])) / rx,
                                diff(range(scores[[ypc]])) / ry
                        ) * 0.7

                        loadings[, 1:2] <- loadings[, 1:2] * scale_fac

                        p <- p +
                                geom_segment(
                                        data = loadings,
                                        aes(x = 0, y = 0,
                                            xend = .data[[xpc]],
                                            yend = .data[[ypc]]),
                                        arrow = arrow(length = unit(0.2, "cm")),
                                        inherit.aes = FALSE,
                                        color = "black"
                                )
                        if (loading_labels) {
                                p <- p + geom_text(
                                        data = loadings,
                                        aes(x = .data[[xpc]],
                                            y = .data[[ypc]],
                                            label = varname),
                                        inherit.aes = FALSE,
                                        fontface = "bold",
                                        vjust = -0.7
                                )
                        }
                }
        }

        # ============================================================
        # Loadings-only plot
        # ============================================================
        if (plot_type == "loading") {

                loadings <- as.data.frame(pca$rotation[, pcs])
                colnames(loadings) <- paste0("PC", pcs)
                loadings$varname <- rownames(loadings)

                contrib <- (loadings[[xpc]]^2 + loadings[[ypc]]^2) /
                        sum(loadings[[xpc]]^2 + loadings[[ypc]]^2)

                p <- ggplot(loadings, aes(.data[[xpc]], .data[[ypc]])) +
                        geom_segment(
                                aes(x = 0, y = 0, xend = .data[[xpc]], yend = .data[[ypc]], alpha = contrib),
                                arrow = arrow(length = unit(0.2, "cm")),
                                color = "black"
                        ) +
                        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
                        scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
                        labs(
                                x = paste0("PC", pcs[1], " (", round(eig_vals[pcs[1]], 1), "%)"),
                                y = paste0("PC", pcs[2], " (", round(eig_vals[pcs[2]], 1), "%)")
                        ) +
                        plot_theme

                if (loading_labels) {
                        p <- p + ggrepel::geom_text_repel(
                                aes(label = varname),
                                size = 3
                        )
                }
        }

        # ============================================================
        # Cumulative variance plot
        # ============================================================
        if (plot_type == "cumvar") {

                df_var <- data.frame(
                        PC = seq_along(eig_vals),
                        Variance = eig_vals,
                        Cumulative = cum_var
                )

                p <- ggplot(df_var, aes(PC, Cumulative)) +
                        geom_line() +
                        geom_point(size = 2) +
                        geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
                        geom_hline(yintercept = 99, linetype = "dotted", color = "blue") +
                        annotate("text",
                                 x = n_pc_95, y = 95,
                                 label = paste0("PC", n_pc_95),
                                 vjust = -0.5) +
                        annotate("text",
                                 x = n_pc_99, y = 99,
                                 label = paste0("PC", n_pc_99),
                                 vjust = -0.5) +
                        labs(
                                x = "Principal component (PC)",
                                y = "Cumulative variance explained (%)"
                        ) +
                        ylim(0, 100) +
                        plot_theme
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
