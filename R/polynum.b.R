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

            # ── Build primary plot ─────────────────────────────────────────
            p <- switch(opts$plotType,
                box       = private$.box_plot(long, vars, pal, alpha_val, box_w, jit_w, opts, is_horiz),
                violin    = private$.violin_plot(long, vars, pal, alpha_val, box_w, jit_w, opts, is_horiz),
                strip     = private$.strip_plot(long, vars, pal, alpha_val, jit_w, opts, is_horiz),
                histogram = private$.histogram_plot(long, vars, pal, alpha_val, opts),
                ridge     = private$.ridge_plot(long, vars, pal, alpha_val, opts),
                private$.box_plot(long, vars, pal, alpha_val, box_w, jit_w, opts, is_horiz))

            # ── Rug (not for histogram/ridge) ──────────────────────────────
            if (opts$showRug && !opts$plotType %in% c("histogram","ridge")) {
                p <- p + ggplot2::geom_rug(
                    sides = if (is_horiz) "b" else "l",
                    alpha = 0.25, linewidth = 0.3,
                    length = ggplot2::unit(0.025, "npc"))
            }

            # ── Mean + optional CI (not for histogram/ridge) ───────────────
            if (opts$showMean && !opts$plotType %in% c("histogram","ridge")) {
                p <- private$.add_mean(p, long, vars, opts, is_horiz)
            }

            # ── Shared colours & theme ─────────────────────────────────────
            if (!opts$plotType %in% c("histogram","ridge")) {
                p <- p +
                    ggplot2::scale_fill_manual(values = pal) +
                    ggplot2::scale_colour_manual(values = pal)
            }

            p <- p +
                private$.theme(opts$themeChoice) +
                ggplot2::theme(
                    legend.position  = "none",
                    plot.title       = ggplot2::element_text(size=13, face="bold", hjust=0.5),
                    axis.text        = ggplot2::element_text(size=10.5),
                    axis.title       = ggplot2::element_text(size=11),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.margin      = ggplot2::margin(10, 14, 8, 10))

            # ── Title + n caption ──────────────────────────────────────────
            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL
            n_per   <- tapply(long$value, long$variable, length)
            caption <- paste(paste0(names(n_per), " (n=", n_per, ")"), collapse = "   ")

            if (opts$plotType == "histogram") {
                p <- p + ggplot2::labs(title = title_text, caption = caption)
            } else if (opts$plotType == "ridge") {
                p <- p + ggplot2::labs(title = title_text, caption = caption)
            } else if (is_horiz) {
                p <- p + ggplot2::labs(title = title_text, x = "Value", y = NULL, caption = caption)
            } else {
                p <- p + ggplot2::labs(title = title_text, x = NULL, y = "Value", caption = caption)
            }

            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size=7.5, colour="grey55", hjust=0))

            print(p)
            TRUE
        },

        # ── Box plot ───────────────────────────────────────────────────────
        .box_plot = function(long, vars, pal, alpha_val, box_w, jit_w, opts, is_horiz) {
            if (is_horiz) {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=value, y=variable, fill=variable, colour=variable))
            } else {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=variable, y=value, fill=variable, colour=variable))
            }
            p <- p + ggplot2::geom_boxplot(
                width = box_w, alpha = alpha_val, linewidth = 0.5,
                outlier.shape = if (opts$showOutliers) 19 else NA,
                outlier.alpha = 0.45, outlier.size = 1.5)
            if (opts$showJitter) {
                if (is_horiz)
                    p <- p + ggplot2::geom_jitter(height=jit_w, width=0,
                        alpha=pmin(alpha_val*0.5,0.4), size=1.2, colour="grey30")
                else
                    p <- p + ggplot2::geom_jitter(width=jit_w, height=0,
                        alpha=pmin(alpha_val*0.5,0.4), size=1.2, colour="grey30")
            }
            p
        },

        # ── Violin plot ────────────────────────────────────────────────────
        .violin_plot = function(long, vars, pal, alpha_val, box_w, jit_w, opts, is_horiz) {
            if (is_horiz) {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=value, y=variable, fill=variable, colour=variable))
            } else {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=variable, y=value, fill=variable, colour=variable))
            }
            p <- p + ggplot2::geom_violin(
                scale=opts$violinScale, trim=FALSE,
                alpha=alpha_val, colour=NA, linewidth=0)
            p <- p + ggplot2::geom_boxplot(
                width=box_w*0.22, alpha=0.9,
                colour="white", linewidth=0.7,
                outlier.shape=NA)
            if (opts$showJitter) {
                if (is_horiz)
                    p <- p + ggplot2::geom_jitter(height=jit_w, width=0,
                        alpha=pmin(alpha_val*0.4,0.35), size=1.0, colour="grey25")
                else
                    p <- p + ggplot2::geom_jitter(width=jit_w, height=0,
                        alpha=pmin(alpha_val*0.4,0.35), size=1.0, colour="grey25")
            }
            p
        },

        # ── Strip plot ─────────────────────────────────────────────────────
        .strip_plot = function(long, vars, pal, alpha_val, jit_w, opts, is_horiz) {
            bw <- opts$boxWidth / 100
            if (is_horiz) {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=value, y=variable, fill=variable, colour=variable)) +
                    ggplot2::geom_jitter(height=bw*0.45, width=0,
                        alpha=alpha_val, size=1.6)
            } else {
                p <- ggplot2::ggplot(long, ggplot2::aes(
                    x=variable, y=value, fill=variable, colour=variable)) +
                    ggplot2::geom_jitter(width=bw*0.45, height=0,
                        alpha=alpha_val, size=1.6)
            }
            p
        },

        # ── Histogram ─────────────────────────────────────────────────────
        .histogram_plot = function(long, vars, pal, alpha_val, opts) {
            long$variable <- factor(long$variable, levels=vars)
            p <- ggplot2::ggplot(long, ggplot2::aes(x=value, fill=variable)) +
                ggplot2::geom_histogram(
                    bins=20, colour="white", linewidth=0.15,
                    alpha=alpha_val) +
                ggplot2::facet_wrap(
                    ~variable, ncol=1, scales="free_y",
                    strip.position="left") +
                ggplot2::scale_fill_manual(values=pal, name=NULL) +
                ggplot2::labs(x="Value", y=NULL) +
                ggplot2::theme(
                    strip.text.y.left = ggplot2::element_text(angle=0, hjust=1, size=10.5),
                    strip.background  = ggplot2::element_blank(),
                    panel.spacing     = ggplot2::unit(0.3,"lines"),
                    legend.position   = "none")
            p
        },

        # ── Ridge plot (no external packages) ─────────────────────────────
        .ridge_plot = function(long, vars, pal, alpha_val, opts) {
            long$variable <- factor(long$variable, levels=vars)
            dens_list <- lapply(seq_along(vars), function(i) {
                v    <- vars[i]
                vals <- long$value[long$variable == v]
                vals <- vals[!is.na(vals)]
                if (length(vals) < 2) return(NULL)
                d <- stats::density(vals)
                data.frame(variable=v, var_idx=i, x=d$x, dens=d$y,
                           stringsAsFactors=FALSE)
            })
            dens_df <- do.call(rbind, Filter(Negate(is.null), dens_list))
            if (is.null(dens_df) || nrow(dens_df) == 0) return(
                ggplot2::ggplot() + ggplot2::labs(title="Not enough data for ridge plot"))
            max_d <- max(dens_df$dens)
            dens_df$y_top  <- dens_df$var_idx + dens_df$dens / max_d * 0.9
            dens_df$y_base <- as.numeric(dens_df$var_idx)
            dens_df$variable <- factor(dens_df$variable, levels=vars)

            p <- ggplot2::ggplot(dens_df) +
                ggplot2::geom_ribbon(
                    ggplot2::aes(x=x, ymin=y_base, ymax=y_top, fill=variable),
                    alpha=alpha_val, colour="white", linewidth=0.3) +
                ggplot2::geom_line(
                    ggplot2::aes(x=x, y=y_top, colour=variable),
                    linewidth=0.5, alpha=0.8) +
                ggplot2::scale_y_continuous(
                    breaks=seq_along(vars), labels=vars,
                    expand=c(0.02,0)) +
                ggplot2::scale_fill_manual(values=pal, name=NULL) +
                ggplot2::scale_colour_manual(values=pal, name=NULL) +
                ggplot2::labs(x="Value", y=NULL) +
                ggplot2::theme(legend.position="none")
            p
        },

        # ── Mean + CI overlay ──────────────────────────────────────────────
        .add_mean = function(p, long, vars, opts, is_horiz) {
            lvls <- levels(long$variable)
            ci_prop <- opts$ciWidth / 100

            mean_df <- do.call(rbind, lapply(lvls, function(v) {
                x  <- long$value[long$variable == v & !is.na(long$value)]
                m  <- mean(x)
                n  <- length(x)
                se <- if (n > 1) stats::sd(x) / sqrt(n) else 0
                z  <- stats::qnorm(1 - (1 - ci_prop) / 2)
                data.frame(variable=v, m=m, lo=m-z*se, hi=m+z*se,
                           stringsAsFactors=FALSE)
            }))
            mean_df$variable <- factor(mean_df$variable, levels=lvls)

            if (is_horiz) {
                p <- p + ggplot2::geom_point(
                    data=mean_df, inherit.aes=FALSE,
                    ggplot2::aes(x=m, y=variable),
                    shape=23, size=3.5, fill="white", colour="grey15", stroke=0.8)
                if (opts$showMeanCI)
                    p <- p + ggplot2::geom_errorbarh(
                        data=mean_df, inherit.aes=FALSE,
                        ggplot2::aes(y=variable, xmin=lo, xmax=hi),
                        height=0.14, colour="grey15", linewidth=0.8)
            } else {
                p <- p + ggplot2::geom_point(
                    data=mean_df, inherit.aes=FALSE,
                    ggplot2::aes(x=variable, y=m),
                    shape=23, size=3.5, fill="white", colour="grey15", stroke=0.8)
                if (opts$showMeanCI)
                    p <- p + ggplot2::geom_errorbar(
                        data=mean_df, inherit.aes=FALSE,
                        ggplot2::aes(x=variable, ymin=lo, ymax=hi),
                        width=0.14, colour="grey15", linewidth=0.8)
            }
            p
        },

        .palette = function(scheme, n) {
            n <- max(n, 1L)
            hcl_map <- list(
                viridis="viridis", plasma="plasma", mako="Mako",
                rocket="Rocket",   turbo="Turbo",
                dark="Dark 2",     pastel="Pastel 1",
                warm="Warm",       cold="Cold")
            nm <- hcl_map[[scheme]]
            if (is.null(nm)) nm <- "viridis"
            tryCatch(
                grDevices::hcl.colors(n, palette=nm),
                error=function(e) grDevices::hcl.colors(n, palette="viridis"))
        },

        .theme = function(nm) {
            switch(nm,
                minimal = ggplot2::theme_minimal(base_size=12),
                classic = ggplot2::theme_classic(base_size=12),
                bw      = ggplot2::theme_bw(base_size=12),
                light   = ggplot2::theme_light(base_size=12),
                ggplot2::theme_minimal(base_size=12))
        }
    )
)
