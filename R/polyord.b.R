PolyOrdClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
  R6::R6Class(
    'PolyOrdClass',
    inherit = PolyOrdBase,
    private = list(

      .run = function() {
        vars <- self$options$vars
        if (length(vars) == 0) return()

        data <- self$data[, vars, drop = FALSE]

        # Ensure all are factors
        for (v in vars) {
          if (!is.factor(data[[v]])) {
            data[[v]] <- as.factor(data[[v]])
          }
        }

        # Drop rows where all selected vars are NA
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

        # ── Collect all levels across variables ───────────────────────────
        all_levels <- unique(unlist(lapply(vars, function(v) levels(data[[v]]))))

        # ── Build proportion table per variable ───────────────────────────
        prop_list <- lapply(vars, function(v) {
          x   <- data[[v]]
          x   <- x[!is.na(x)]
          n   <- length(x)
          tbl <- table(factor(x, levels = all_levels))
          pct <- as.numeric(tbl) / n * 100
          data.frame(
            variable = v,
            category = all_levels,
            pct      = pct,
            n_total  = n,
            stringsAsFactors = FALSE
          )
        })
        long <- do.call(rbind, prop_list)
        long$category <- factor(long$category, levels = all_levels)

        # ── Sorting ───────────────────────────────────────────────────────
        var_order <- switch(opts$sortVars,
          name       = sort(vars),
          freq_first = {
            first_cat <- all_levels[1]
            first_pct <- sapply(vars, function(v) {
              long$pct[long$variable == v & long$category == first_cat]
            })
            vars[order(first_pct)]
          },
          freq_last  = {
            last_cat <- all_levels[length(all_levels)]
            last_pct <- sapply(vars, function(v) {
              long$pct[long$variable == v & long$category == last_cat]
            })
            vars[order(-last_pct)]
          },
          vars
        )

        if (opts$reverseVars) var_order <- rev(var_order)
        long$variable <- factor(long$variable, levels = var_order)

        # ── Colour palette ────────────────────────────────────────────────
        n_cats <- length(all_levels)
        pal    <- private$.palette(opts$colorScheme, n_cats, opts$diverging)
        names(pal) <- all_levels

        # ── Diverging layout ──────────────────────────────────────────────
        if (opts$diverging && n_cats >= 2) {
          p <- private$.diverging_plot(long, var_order, all_levels, pal, opts)
        } else {
          p <- private$.stacked_plot(long, var_order, all_levels, pal, opts)
        }

        print(p)
        TRUE
      },

      # ── Straight 100% stacked bars ────────────────────────────────────────
      .stacked_plot = function(long, var_order, all_levels, pal, opts) {
        bar_h <- opts$barHeight / 100

        p <- ggplot2::ggplot(long, ggplot2::aes(
          x    = pct,
          y    = variable,
          fill = category
        )) +
          ggplot2::geom_col(
            position = ggplot2::position_stack(reverse = TRUE),
            width    = bar_h,
            colour   = NA
          )

        # Percentage labels
        if (opts$showPct) {
          label_data <- long[long$pct >= opts$minPctLabel, ]
          label_data$label <- paste0(round(label_data$pct, 1), '%')
          p <- p + ggplot2::geom_text(
            data     = label_data,
            ggplot2::aes(label = label),
            position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
            size     = 3.2,
            colour   = 'white',
            fontface = 'bold'
          )
        }

        p <- p +
          ggplot2::scale_x_continuous(
            limits = c(0, 100),
            breaks = seq(0, 100, 25),
            labels = scales::label_percent(scale = 1, suffix = '%'),
            expand = c(0, 0)
          ) +
          ggplot2::scale_fill_manual(
            values = pal,
            name   = if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL,
            guide  = ggplot2::guide_legend(reverse = FALSE, nrow = 1)
          )

        p <- private$.add_theme(p, opts)

        # N labels on right
        if (opts$showN) {
          n_data <- unique(long[, c('variable', 'n_total')])
          p <- p + ggplot2::geom_text(
            data   = n_data,
            ggplot2::aes(x = 103, y = variable, label = paste0('n=', n_total), fill = NULL),
            hjust  = 0,
            size   = 3,
            colour = 'grey40'
          ) +
            ggplot2::coord_cartesian(clip = 'off') +
            ggplot2::theme(plot.margin = ggplot2::margin(8, 50, 8, 10))
        }

        p
      },

      # ── Diverging (Likert-style) bars ─────────────────────────────────────
      .diverging_plot = function(long, var_order, all_levels, pal, opts) {
        n_cats  <- length(all_levels)
        bar_h   <- opts$barHeight / 100

        # Split levels: negative (left), neutral (centre, halved), positive (right)
        mid_idx     <- ceiling(n_cats / 2)
        neg_levels  <- if (mid_idx > 1) all_levels[1:(mid_idx - 1)] else character(0)
        neut_level  <- if (n_cats %% 2 == 1) all_levels[mid_idx] else character(0)
        pos_levels  <- all_levels[(mid_idx + (length(neut_level) > 0)):n_cats]

        div_list <- lapply(var_order, function(v) {
          sub <- long[long$variable == v, ]
          rows <- list()

          for (cat in rev(neg_levels)) {
            pct_val <- sub$pct[sub$category == cat]
            if (length(pct_val) == 0) pct_val <- 0
            rows[[length(rows) + 1]] <- data.frame(
              variable  = v,
              category  = cat,
              xmin      = 0,
              xmax      = -pct_val,
              stringsAsFactors = FALSE
            )
          }

          # Neutral: split half-left, half-right
          if (length(neut_level) > 0) {
            pct_n <- sub$pct[sub$category == neut_level]
            if (length(pct_n) == 0) pct_n <- 0
            rows[[length(rows) + 1]] <- data.frame(
              variable = v, category = neut_level,
              xmin = -pct_n / 2, xmax = pct_n / 2,
              stringsAsFactors = FALSE
            )
          }

          for (cat in pos_levels) {
            pct_val <- sub$pct[sub$category == cat]
            if (length(pct_val) == 0) pct_val <- 0
            rows[[length(rows) + 1]] <- data.frame(
              variable = v, category = cat,
              xmin = 0, xmax = pct_val,
              stringsAsFactors = FALSE
            )
          }

          # Accumulate positions
          neg_rows <- lapply(rev(neg_levels), function(cat) {
            sub[sub$category == cat, ]
          })
          # Re-build with stacking
          acc <- do.call(rbind, rows)
          acc
        })

        # Proper stacking using cumulative sums
        stack_side <- function(var_data, levels_ordered, sign) {
          cum_x <- 0
          out   <- list()
          for (cat in levels_ordered) {
            pct_val <- var_data$pct[var_data$category == cat]
            if (length(pct_val) == 0) pct_val <- 0
            out[[length(out) + 1]] <- data.frame(
              variable = var_data$variable[1],
              category = cat,
              xmin     = cum_x * sign,
              xmax     = (cum_x + pct_val) * sign,
              pct      = pct_val,
              stringsAsFactors = FALSE
            )
            cum_x <- cum_x + pct_val
          }
          do.call(rbind, out)
        }

        div_df <- do.call(rbind, lapply(var_order, function(v) {
          sub      <- long[long$variable == v, ]
          n_total  <- sub$n_total[1]

          neg_part  <- stack_side(sub, rev(neg_levels), -1)
          pos_part  <- stack_side(sub, pos_levels,       1)

          neut_part <- if (length(neut_level) > 0) {
            pct_n <- sub$pct[sub$category == neut_level]
            if (length(pct_n) == 0) pct_n <- 0
            data.frame(
              variable = v, category = neut_level,
              xmin = -pct_n / 2, xmax = pct_n / 2, pct = pct_n,
              stringsAsFactors = FALSE
            )
          } else NULL

          full <- rbind(neg_part, neut_part, pos_part)
          full$n_total <- n_total
          full
        }))

        div_df$variable <- factor(div_df$variable, levels = var_order)
        div_df$category <- factor(div_df$category, levels = all_levels)

        p <- ggplot2::ggplot(div_df, ggplot2::aes(
          xmin = xmin, xmax = xmax,
          ymin = as.numeric(variable) - bar_h / 2,
          ymax = as.numeric(variable) + bar_h / 2,
          fill = category
        )) +
          ggplot2::geom_rect(colour = NA) +
          ggplot2::geom_vline(xintercept = 0, colour = 'grey30', linewidth = 0.5) +
          ggplot2::scale_y_continuous(
            breaks = seq_along(var_order),
            labels = var_order,
            expand = c(0.03, 0)
          ) +
          ggplot2::scale_x_continuous(
            labels = function(x) paste0(abs(x), '%'),
            limits = c(-100, 100),
            breaks = seq(-100, 100, 25)
          )

        # Labels
        if (opts$showPct) {
          label_data <- div_df[div_df$pct >= opts$minPctLabel, ]
          label_data$xmid  <- (label_data$xmin + label_data$xmax) / 2
          label_data$label <- paste0(round(label_data$pct, 1), '%')
          p <- p + ggplot2::geom_text(
            data     = label_data,
            ggplot2::aes(x = xmid, y = (as.numeric(variable)), label = label, fill = NULL),
            size     = 3.0,
            colour   = 'white',
            fontface = 'bold'
          )
        }

        p <- p +
          ggplot2::scale_fill_manual(
            values = pal,
            name   = if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL,
            guide  = ggplot2::guide_legend(nrow = 1)
          )

        if (opts$showN) {
          n_data <- unique(div_df[, c('variable', 'n_total')])
          n_data$x_pos <- 103
          p <- p + ggplot2::geom_text(
            data   = n_data,
            ggplot2::aes(x = x_pos, y = as.numeric(variable),
                         label = paste0('n=', n_total), fill = NULL),
            hjust  = 0, size = 3, colour = 'grey40'
          ) +
            ggplot2::coord_cartesian(clip = 'off') +
            ggplot2::theme(plot.margin = ggplot2::margin(8, 55, 8, 10))
        }

        p <- private$.add_theme(p, opts)
        p
      },

      # ── Shared theme application ──────────────────────────────────────────
      .add_theme = function(p, opts) {
        base_theme <- switch(opts$themeChoice,
          minimal = ggplot2::theme_minimal(base_size = 12),
          classic = ggplot2::theme_classic(base_size = 12),
          bw      = ggplot2::theme_bw(base_size = 12),
          light   = ggplot2::theme_light(base_size = 12),
          ggplot2::theme_minimal(base_size = 12)
        )

        leg_pos <- switch(opts$legendPos,
          bottom = 'bottom', right = 'right',
          top    = 'top',    none  = 'none',
          'bottom'
        )

        title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

        p <- p +
          base_theme +
          ggplot2::theme(
            legend.position    = leg_pos,
            legend.title       = ggplot2::element_text(size = 9),
            legend.text        = ggplot2::element_text(size = 8.5),
            legend.key.size    = ggplot2::unit(0.55, 'cm'),
            plot.title         = ggplot2::element_text(size = 13, face = 'bold', hjust = 0.5, margin = ggplot2::margin(b = 8)),
            axis.text.y        = ggplot2::element_text(size = 10.5, colour = 'grey20'),
            axis.text.x        = ggplot2::element_text(size = 9.5),
            axis.title.x       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(colour = 'grey90', linewidth = 0.35),
            plot.margin        = ggplot2::margin(8, 14, 8, 10)
          ) +
          ggplot2::labs(title = title_text)

        p
      },

      # ── Palette helper ────────────────────────────────────────────────────
      .palette = function(scheme, n, diverging = FALSE) {
        n <- max(n, 1L)

        pal_map <- list(
          tableau  = 'Tableau 10',
          rdbulite = 'Blue-Red 3',
          rdylgn   = 'Red-Green',
          piyg     = 'PiYG',
          prgn     = 'PRGn',
          pastel   = 'Pastel 1',
          dark2    = 'Dark 2',
          set2     = 'Set 2',
          viridis  = 'viridis',
          plasma   = 'plasma'
        )

        hcl_name <- pal_map[[scheme]]
        if (is.null(hcl_name)) hcl_name <- 'Tableau 10'

        # Diverging palettes: go from negative pole to positive pole
        if (diverging && n >= 3) {
          cols <- tryCatch(
            grDevices::hcl.colors(n, palette = hcl_name, rev = FALSE),
            error = function(e) grDevices::hcl.colors(n, palette = 'Blue-Red 3')
          )
        } else {
          cols <- tryCatch(
            grDevices::hcl.colors(n, palette = hcl_name),
            error = function(e) grDevices::hcl.colors(n, palette = 'Tableau 10')
          )
        }
        cols
      }
    )
  )
}
