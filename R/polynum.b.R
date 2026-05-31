polyNumClass <- R6::R6Class(
    "polyNumClass",
    inherit = polyNumBase,
    private = list(

        .run = function() {
            vars <- self$options$vars
            if (length(vars) == 0) return()

            data <- self$data[, vars, drop = FALSE]
            complete <- apply(data, 1, function(x) !all(is.na(x)))
            data <- data[complete, , drop = FALSE]
            if (nrow(data) == 0) return()

            image <- self$results$plot
            image$setSize(self$options$plotWidth, self$options$plotHeight)
            image$setState(list(data = data, vars = vars))
        },

        .plot = function(image, gg, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            data <- state$data
            vars <- state$vars
            opts <- self$options

            # Reshape to long (base R)
            long <- do.call(rbind, lapply(vars, function(v) {
                data.frame(variable = v, value = as.numeric(data[[v]]),
                           stringsAsFactors = FALSE)
            }))
            long$variable <- factor(long$variable, levels = vars)
            long <- long[!is.na(long$value), ]

            n_vars    <- length(vars)
            pal       <- private$.palette(opts$colorScheme, n_vars)
            alpha_val <- opts$plotAlpha / 100
            box_w     <- opts$boxWidth  / 100
            jit_w     <- opts$jitterWidth / 100 * 0.4
            is_horiz  <- opts$orientation == "horizontal"

            if (is_horiz) {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x = value, y = variable, fill = variable, colour = variable))
            } else {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x = variable, y = value, fill = variable, colour = variable))
            }

            # ── Violin ────────────────────────────────────────────────────
            if (opts$showViolin) {
                p <- p + ggplot2::geom_violin(
                    scale = opts$violinScale, trim = FALSE,
                    alpha = alpha_val, colour = NA, linewidth = 0)
            }

            # ── Box ───────────────────────────────────────────────────────
            if (opts$showBox) {
                if (opts$showViolin) {
                    p <- p + ggplot2::geom_boxplot(
                        width = box_w * 0.28, alpha = 0.88,
                        colour = "white", linewidth = 0.7,
                        outlier.shape = if (opts$showOutliers) 19 else NA,
                        outlier.alpha = 0.4, outlier.size = 1.2)
                } else {
                    p <- p + ggplot2::geom_boxplot(
                        width = box_w, alpha = alpha_val, linewidth = 0.5,
                        outlier.shape = if (opts$showOutliers) 19 else NA,
                        outlier.alpha = 0.45, outlier.size = 1.5)
                }
            }

            # ── Jitter ────────────────────────────────────────────────────
            if (opts$showJitter) {
                if (is_horiz) {
                    p <- p + ggplot2::geom_jitter(
                        height = jit_w, width = 0,
                        alpha = pmin(alpha_val * 0.6, 0.45),
                        size = 1.5, colour = "grey30")
                } else {
                    p <- p + ggplot2::geom_jitter(
                        width = jit_w, height = 0,
                        alpha = pmin(alpha_val * 0.6, 0.45),
                        size = 1.5, colour = "grey30")
                }
            }

            # ── Rug ───────────────────────────────────────────────────────
            if (opts$showRug) {
                p <- p + ggplot2::geom_rug(
                    sides = if (is_horiz) "b" else "l",
                    alpha = 0.25, linewidth = 0.3,
                    length = ggplot2::unit(0.025, "npc"))
            }

            # ── Mean diamond ──────────────────────────────────────────────
            if (opts$showMean) {
                p <- p + ggplot2::stat_summary(
                    fun = mean, geom = "point",
                    shape = 23, size = 3.5,
                    fill = "white", colour = "grey20", stroke = 0.8)
            }

            # ── Colours & theme ───────────────────────────────────────────
            p <- p +
                ggplot2::scale_fill_manual(values = pal) +
                ggplot2::scale_colour_manual(values = pal) +
                private$.theme(opts$themeChoice) +
                ggplot2::theme(
                    legend.position    = "none",
                    plot.title         = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
                    axis.text          = ggplot2::element_text(size = 10.5),
                    axis.title         = ggplot2::element_text(size = 11),
                    panel.grid.minor   = ggplot2::element_blank(),
                    plot.margin        = ggplot2::margin(10, 14, 8, 10))

            # ── Labels ───────────────────────────────────────────────────
            val_lbl <- "Value"
            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

            # N caption
            n_per <- tapply(long$value, long$variable, length)
            caption <- paste(paste0(names(n_per), " (n=", n_per, ")"), collapse = "   ")

            if (is_horiz) {
                p <- p + ggplot2::labs(title = title_text, x = val_lbl, y = NULL, caption = caption)
            } else {
                p <- p + ggplot2::labs(title = title_text, x = NULL, y = val_lbl, caption = caption)
            }

            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 7.5, colour = "grey55", hjust = 0))

            print(p)
            TRUE
        },

        .palette = function(scheme, n) {
            n <- max(n, 1L)
            hcl_map <- list(
                viridis = "viridis", plasma = "plasma", mako = "Mako",
                rocket  = "Rocket",  turbo  = "Turbo",
                tableau = "Tableau 10", dark = "Dark 2",
                pastel  = "Pastel 1",   warm  = "Warm",  cold = "Cold")
            nm <- hcl_map[[scheme]]
            if (is.null(nm)) nm <- "viridis"
            tryCatch(
                grDevices::hcl.colors(n, palette = nm),
                error = function(e) grDevices::hcl.colors(n, palette = "viridis"))
        },

        .theme = function(nm) {
            switch(nm,
                minimal = ggplot2::theme_minimal(base_size = 12),
                classic = ggplot2::theme_classic(base_size = 12),
                bw      = ggplot2::theme_bw(base_size = 12),
                light   = ggplot2::theme_light(base_size = 12),
                ggplot2::theme_minimal(base_size = 12))
        }
    )
)
