PolyNumClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
  R6::R6Class(
    'PolyNumClass',
    inherit = PolyNumBase,
    private = list(

      .run = function() {
        vars <- self$options$vars
        if (length(vars) == 0) return()

        data <- self$data[, vars, drop = FALSE]

        # Drop all-NA rows
        complete <- apply(data, 1, function(x) !all(is.na(x)))
        data <- data[complete, , drop = FALSE]

        if (nrow(data) == 0) {
          jmvcore::reject(.('No complete cases found'), code = 'noData')
          return()
        }

        image <- self$results$plot
        image$setSize(self$options$plotWidth, self$options$plotHeight)
        image$setState(list(data = data, vars = vars))
      },

      .plot = function(image, gg, theme, ...) {
        state <- image$state
        if (is.null(state)) return(FALSE)

        data <- state$data
        vars  <- state$vars
        opts  <- self$options

        # ── Reshape to long ────────────────────────────────────────────────
        long <- do.call(rbind, lapply(vars, function(v) {
          vals <- data[[v]]
          data.frame(
            variable = v,
            value    = as.numeric(vals),
            stringsAsFactors = FALSE
          )
        }))
        long$variable <- factor(long$variable, levels = vars)
        long <- long[!is.na(long$value), ]

        n_vars    <- length(vars)
        pal       <- private$.palette(opts$colorScheme, n_vars)
        alpha_val <- opts$plotAlpha / 100
        box_w     <- opts$boxWidth  / 100
        jit_w     <- opts$jitterWidth / 100 * 0.4

        # ── Base aesthetics ────────────────────────────────────────────────
        is_horiz <- opts$orientation == 'horizontal'

        if (is_horiz) {
          p <- ggplot2::ggplot(long, ggplot2::aes(
            x = value, y = variable, fill = variable, colour = variable
          ))
        } else {
          p <- ggplot2::ggplot(long, ggplot2::aes(
            x = variable, y = value, fill = variable, colour = variable
          ))
        }

        # ── Violin ────────────────────────────────────────────────────────
        if (opts$showViolin) {
          p <- p + ggplot2::geom_violin(
            scale    = opts$violinScale,
            trim     = FALSE,
            alpha    = alpha_val,
            colour   = NA,
            linewidth = 0
          )
        }

        # ── Boxplot ───────────────────────────────────────────────────────
        if (opts$showBox) {
          if (opts$showViolin) {
            # Narrow white-outline box over violin
            p <- p + ggplot2::geom_boxplot(
              width           = box_w * 0.28,
              alpha           = 0.85,
              colour          = 'white',
              linewidth       = 0.7,
              outlier.shape   = if (opts$showOutliers) 19 else NA,
              outlier.alpha   = 0.35,
              outlier.size    = 1.2
            )
          } else {
            p <- p + ggplot2::geom_boxplot(
              width           = box_w,
              alpha           = alpha_val,
              linewidth       = 0.5,
              outlier.shape   = if (opts$showOutliers) 19 else NA,
              outlier.alpha   = 0.45,
              outlier.size    = 1.5
            )
          }
        }

        # ── Jitter ────────────────────────────────────────────────────────
        if (opts$showJitter) {
          jitter_colour <- if (n_vars == 1) pal[1] else 'grey35'
          if (is_horiz) {
            p <- p + ggplot2::geom_jitter(
              height  = jit_w,
              width   = 0,
              alpha   = pmin(alpha_val * 0.6, 0.45),
              size    = opts$pointSize,
              colour  = jitter_colour
            )
          } else {
            p <- p + ggplot2::geom_jitter(
              width   = jit_w,
              height  = 0,
              alpha   = pmin(alpha_val * 0.6, 0.45),
              size    = opts$pointSize,
              colour  = jitter_colour
            )
          }
        }

        # ── Rug ───────────────────────────────────────────────────────────
        if (opts$showRug) {
          rug_sides <- if (is_horiz) 'b' else 'l'
          p <- p + ggplot2::geom_rug(
            sides     = rug_sides,
            alpha     = 0.25,
            linewidth = 0.3,
            length    = ggplot2::unit(0.025, 'npc')
          )
        }

        # ── Mean diamond ──────────────────────────────────────────────────
        if (opts$showMean) {
          p <- p + ggplot2::stat_summary(
            fun      = mean,
            geom     = 'point',
            shape    = 23,
            size     = 3.5,
            fill     = 'white',
            colour   = 'grey20',
            stroke   = 0.8
          )
        }

        # ── Colours ───────────────────────────────────────────────────────
        p <- p +
          ggplot2::scale_fill_manual(values = pal) +
          ggplot2::scale_colour_manual(values = pal)

        # ── Theme ─────────────────────────────────────────────────────────
        base_theme <- private$.theme(opts$themeChoice)

        grid_x <- if (opts$showGridX) ggplot2::element_line(colour = 'grey88', linewidth = 0.35) else ggplot2::element_blank()
        grid_y <- if (opts$showGridY) ggplot2::element_line(colour = 'grey88', linewidth = 0.35) else ggplot2::element_blank()

        p <- p +
          base_theme +
          ggplot2::theme(
            legend.position    = 'none',
            plot.title         = ggplot2::element_text(size = 13, face = 'bold', hjust = 0.5, margin = ggplot2::margin(b = 8)),
            axis.text          = ggplot2::element_text(size = 10.5),
            axis.title         = ggplot2::element_text(size = 11),
            panel.grid.major.x = if (is_horiz) grid_x else grid_y,
            panel.grid.major.y = if (is_horiz) grid_y else grid_x,
            panel.grid.minor   = ggplot2::element_blank(),
            plot.margin        = ggplot2::margin(10, 14, 8, 10)
          )

        # ── Axis labels ───────────────────────────────────────────────────
        value_label <- if (nchar(trimws(opts$ylab)) > 0) opts$ylab else 'Value'
        group_label <- if (nchar(trimws(opts$xlab)) > 0) opts$xlab else NULL
        title_text  <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

        if (is_horiz) {
          p <- p + ggplot2::labs(title = title_text, x = value_label, y = group_label)
        } else {
          p <- p + ggplot2::labs(title = title_text, x = group_label, y = value_label)
        }

        # ── N per variable subtitle (small) ───────────────────────────────
        n_per_var <- tapply(long$value, long$variable, function(x) sum(!is.na(x)))
        subtitle  <- paste(names(n_per_var), paste0('n=', n_per_var), sep = ': ', collapse = '   ')
        p <- p + ggplot2::labs(caption = subtitle) +
          ggplot2::theme(
            plot.caption = ggplot2::element_text(size = 7.5, colour = 'grey55', hjust = 0)
          )

        print(p)
        TRUE
      },

      # ── Helpers ───────────────────────────────────────────────────────────

      .palette = function(scheme, n) {
        n <- max(n, 1L)
        pal_map <- list(
          viridis = 'viridis',
          plasma  = 'plasma',
          mako    = 'mako',
          rocket  = 'rocket',
          turbo   = 'turbo',
          tableau = 'Tableau 10',
          dark    = 'Dark 2',
          pastel  = 'Pastel 1',
          warm    = 'Warm',
          cold    = 'Cold'
        )
        hcl_name <- pal_map[[scheme]]
        if (is.null(hcl_name)) hcl_name <- 'viridis'
        cols <- tryCatch(
          grDevices::hcl.colors(n, palette = hcl_name),
          error = function(e) grDevices::hcl.colors(n, palette = 'viridis')
        )
        cols
      },

      .theme = function(name) {
        switch(name,
          minimal = ggplot2::theme_minimal(base_size = 12),
          classic = ggplot2::theme_classic(base_size = 12),
          bw      = ggplot2::theme_bw(base_size = 12),
          light   = ggplot2::theme_light(base_size = 12),
          ggplot2::theme_minimal(base_size = 12)
        )
      }
    )
  )
}
