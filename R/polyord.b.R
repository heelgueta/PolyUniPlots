polyOrdClass <- R6::R6Class(
    "polyOrdClass",
    inherit = polyOrdBase,
    private = list(

        .run = function() {
            vars <- self$options$vars
            if (length(vars) == 0) return()
            data <- self$data[, vars, drop = FALSE]
            for (v in vars)
                if (!is.factor(data[[v]])) data[[v]] <- as.factor(data[[v]])
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

            all_levels <- unique(unlist(lapply(vars, function(v) levels(data[[v]]))))

            prop_list <- lapply(vars, function(v) {
                x   <- data[[v]][!is.na(data[[v]])]
                n   <- length(x)
                tbl <- table(factor(x, levels = all_levels))
                data.frame(
                    variable = v,
                    category = all_levels,
                    pct      = as.numeric(tbl) / n * 100,
                    n_total  = n,
                    stringsAsFactors = FALSE)
            })
            long <- do.call(rbind, prop_list)
            long$category <- factor(long$category, levels = all_levels)

            var_order <- switch(opts$sortVars,
                name = sort(vars),
                freq_first = {
                    fc  <- all_levels[1]
                    pct <- sapply(vars, function(v) long$pct[long$variable == v & long$category == fc][1])
                    vars[order(pct)]
                },
                freq_last = {
                    lc  <- all_levels[length(all_levels)]
                    pct <- sapply(vars, function(v) long$pct[long$variable == v & long$category == lc][1])
                    vars[order(-pct)]
                },
                vars)
            if (opts$reverseVars) var_order <- rev(var_order)
            long$variable <- factor(long$variable, levels = var_order)

            n_cats <- length(all_levels)
            pal    <- private$.palette(opts$colorScheme, n_cats)
            names(pal) <- all_levels

            if (opts$chartType == "waffle") {
                p <- private$.waffle_plot(long, var_order, all_levels, pal, opts)
            } else if (opts$chartType == "pictogram") {
                p <- private$.pictogram_plot(long, var_order, all_levels, pal, opts)
            } else if (opts$chartType == "parliament") {
                p <- private$.parliament_plot(long, var_order, all_levels, pal, opts)
            } else {
                # bars (default)
                if (opts$diverging && n_cats >= 3) {
                    p <- private$.diverging_plot(long, var_order, all_levels, pal, opts)
                } else {
                    p <- private$.stacked_plot(long, var_order, all_levels, pal, opts)
                }
            }

            print(p)
            TRUE
        },

        # ── Stacked bar chart ──────────────────────────────────────────────
        .stacked_plot = function(long, var_order, all_levels, pal, opts) {
            bar_h   <- opts$barHeight / 100
            leg_lbl <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL

            p <- ggplot2::ggplot(long, ggplot2::aes(
                    x = pct, y = variable, fill = category)) +
                ggplot2::geom_col(
                    position = ggplot2::position_stack(reverse = TRUE),
                    width    = bar_h,
                    colour   = NA)

            if (opts$showPct) {
                ld       <- long[long$pct >= opts$minPctLabel, ]
                ld$label <- paste0(round(ld$pct, 1), "%")
                p <- p + ggplot2::geom_text(
                    data     = ld,
                    ggplot2::aes(label = label),
                    position = ggplot2::position_stack(vjust=0.5, reverse=TRUE),
                    size     = 3.2, colour="white", fontface="bold")
            }

            p <- p +
                ggplot2::scale_x_continuous(
                    limits = c(0, 100), breaks=seq(0,100,25),
                    labels = function(x) paste0(x, "%"),
                    expand = c(0, 0)) +
                ggplot2::scale_fill_manual(
                    values = pal, name=leg_lbl,
                    guide  = ggplot2::guide_legend(nrow=1))

            if (opts$showN) {
                nd <- unique(long[, c("variable","n_total")])
                p <- p +
                    ggplot2::geom_text(
                        data        = nd,
                        inherit.aes = FALSE,
                        ggplot2::aes(x=103, y=variable, label=paste0("n=",n_total)),
                        hjust=0, size=3, colour="grey40") +
                    ggplot2::coord_cartesian(clip="off") +
                    ggplot2::theme(plot.margin=ggplot2::margin(8,55,8,10))
            }

            private$.add_theme(p, opts)
        },

        # ── Diverging bar chart ────────────────────────────────────────────
        .diverging_plot = function(long, var_order, all_levels, pal, opts) {
            n_cats  <- length(all_levels)
            mid_idx <- ceiling(n_cats / 2)
            has_mid <- (n_cats %% 2 == 1)
            neg_lvl <- if (mid_idx > 1) rev(all_levels[1:(mid_idx - 1)]) else character(0)
            neu_lvl <- if (has_mid) all_levels[mid_idx] else character(0)
            pos_lvl <- all_levels[(mid_idx + as.integer(has_mid)):n_cats]
            bar_h   <- opts$barHeight / 100
            leg_lbl <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL

            stack_side <- function(sub, lvls, sign) {
                cum <- 0
                rows <- lapply(lvls, function(cat) {
                    pv <- sub$pct[sub$category == cat]
                    if (length(pv) == 0) pv <- 0
                    r  <- data.frame(
                        variable = sub$variable[1],
                        category = cat,
                        xmin     = cum * sign,
                        xmax     = (cum + pv) * sign,
                        pct      = pv,
                        stringsAsFactors = FALSE)
                    cum <<- cum + pv
                    r
                })
                do.call(rbind, rows)
            }

            div_df <- do.call(rbind, lapply(var_order, function(v) {
                sub   <- long[long$variable == v, ]
                n_tot <- sub$n_total[1]
                neg   <- stack_side(sub, neg_lvl, -1)
                pos   <- stack_side(sub, pos_lvl,  1)
                neu   <- if (length(neu_lvl) > 0) {
                    pv <- sub$pct[sub$category == neu_lvl]
                    if (length(pv) == 0) pv <- 0
                    data.frame(variable=v, category=neu_lvl,
                               xmin=-pv/2, xmax=pv/2, pct=pv,
                               stringsAsFactors=FALSE)
                } else NULL
                full         <- rbind(neg, neu, pos)
                full$n_total <- n_tot
                full
            }))

            div_df$variable <- factor(div_df$variable, levels=var_order)
            div_df$category <- factor(div_df$category, levels=all_levels)

            p <- ggplot2::ggplot(div_df, ggplot2::aes(
                    xmin = xmin, xmax = xmax,
                    ymin = as.numeric(variable) - bar_h / 2,
                    ymax = as.numeric(variable) + bar_h / 2,
                    fill = category)) +
                ggplot2::geom_rect(colour=NA) +
                ggplot2::geom_vline(xintercept=0, colour="grey30", linewidth=0.5) +
                ggplot2::scale_y_continuous(
                    breaks=seq_along(var_order), labels=var_order, expand=c(0.05,0)) +
                ggplot2::scale_x_continuous(
                    labels=function(x) paste0(abs(x),"%"),
                    limits=c(-100,100), breaks=seq(-100,100,25)) +
                ggplot2::scale_fill_manual(values=pal, name=leg_lbl,
                    guide=ggplot2::guide_legend(nrow=1))

            if (opts$showPct) {
                ld       <- div_df[div_df$pct >= opts$minPctLabel, ]
                ld$xmid  <- (ld$xmin + ld$xmax) / 2
                ld$label <- paste0(round(ld$pct, 1), "%")
                p <- p + ggplot2::geom_text(
                    data        = ld,
                    inherit.aes = FALSE,
                    ggplot2::aes(x=xmid, y=as.numeric(variable), label=label),
                    size=3, colour="white", fontface="bold")
            }

            if (opts$showN) {
                nd          <- unique(div_df[, c("variable","n_total")])
                nd$var_num  <- as.numeric(nd$variable)
                p <- p +
                    ggplot2::geom_text(
                        data        = nd,
                        inherit.aes = FALSE,
                        ggplot2::aes(x=103, y=var_num, label=paste0("n=",n_total)),
                        hjust=0, size=3, colour="grey40") +
                    ggplot2::coord_cartesian(clip="off") +
                    ggplot2::theme(plot.margin=ggplot2::margin(8,55,8,10))
            }

            private$.add_theme(p, opts)
        },

        # ── Waffle chart ───────────────────────────────────────────────────
        # 10x10 grid of tiles per variable; each tile = 1 percentage point
        .waffle_plot = function(long, var_order, all_levels, pal, opts) {
            n_cells <- 100L
            grid_w  <- 10L
            gap     <- 12.5   # vertical spacing between grids

            waffle_df <- do.call(rbind, lapply(seq_along(var_order), function(vi) {
                v   <- var_order[vi]
                sub <- long[long$variable == v, ]
                # Round pcts to integers summing to exactly 100
                counts <- round(sub$pct)
                diff   <- n_cells - sum(counts)
                if (diff != 0L)
                    counts[which.max(counts)] <- counts[which.max(counts)] + diff
                counts <- pmax(counts, 0L)
                # Safety: ensure sum == 100 after pmax adjustment
                tot <- sum(counts)
                if (tot != n_cells) counts[1] <- counts[1] + (n_cells - tot)
                counts <- pmax(counts, 0L)

                cats <- factor(
                    rep(all_levels, times=counts),
                    levels=all_levels)

                data.frame(
                    variable = v,
                    var_idx  = vi,
                    cx       = (seq_len(n_cells) - 1L) %% grid_w,
                    cy       = (seq_len(n_cells) - 1L) %/% grid_w + (vi - 1L) * gap,
                    category = cats,
                    stringsAsFactors = FALSE)
            }))
            waffle_df$category <- factor(waffle_df$category, levels=all_levels)

            # Variable labels placed to the left of each grid
            label_df <- data.frame(
                variable = var_order,
                lx       = -1.2,
                ly       = (seq_along(var_order) - 1L) * gap + 4.5,
                stringsAsFactors = FALSE)

            leg_lbl    <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL
            leg_pos    <- switch(opts$legendPos,
                bottom="bottom", right="right", top="top", none="none", "bottom")
            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

            base_t <- switch(opts$themeChoice,
                minimal = ggplot2::theme_minimal(base_size=12),
                classic = ggplot2::theme_classic(base_size=12),
                bw      = ggplot2::theme_bw(base_size=12),
                light   = ggplot2::theme_light(base_size=12),
                ggplot2::theme_minimal(base_size=12))

            ggplot2::ggplot(waffle_df, ggplot2::aes(x=cx, y=cy, fill=category)) +
                ggplot2::geom_tile(
                    colour    = "white",
                    linewidth = 1.5,
                    width     = 0.92,
                    height    = 0.92) +
                ggplot2::geom_text(
                    data        = label_df,
                    inherit.aes = FALSE,
                    ggplot2::aes(x=lx, y=ly, label=variable),
                    hjust    = 1,
                    size     = 3.5,
                    colour   = "grey25",
                    fontface = "plain") +
                ggplot2::scale_fill_manual(
                    values = pal, name=leg_lbl,
                    guide  = ggplot2::guide_legend(nrow=1)) +
                ggplot2::scale_x_continuous(
                    expand = ggplot2::expansion(add=c(3.5, 0.5))) +
                ggplot2::coord_equal() +
                base_t +
                ggplot2::theme(
                    legend.position = leg_pos,
                    legend.text     = ggplot2::element_text(size=9),
                    legend.key.size = ggplot2::unit(0.5, "cm"),
                    plot.title      = ggplot2::element_text(size=13, face="bold", hjust=0.5),
                    axis.text       = ggplot2::element_blank(),
                    axis.ticks      = ggplot2::element_blank(),
                    axis.title      = ggplot2::element_blank(),
                    panel.grid      = ggplot2::element_blank(),
                    plot.margin     = ggplot2::margin(8, 14, 8, 80)) +
                ggplot2::labs(title=title_text)
        },

        # ── Pictogram chart ────────────────────────────────────────────────
        # Dot matrix (circles), each dot = 1%, 10x10 per variable
        .pictogram_plot = function(long, var_order, all_levels, pal, opts) {
            n_cells <- 100L
            grid_w  <- 10L
            gap     <- 12.5

            picto_df <- do.call(rbind, lapply(seq_along(var_order), function(vi) {
                v   <- var_order[vi]
                sub <- long[long$variable == v, ]
                counts <- round(sub$pct)
                diff   <- n_cells - sum(counts)
                if (diff != 0L)
                    counts[which.max(counts)] <- counts[which.max(counts)] + diff
                counts <- pmax(counts, 0L)
                tot <- sum(counts)
                if (tot != n_cells) counts[1] <- counts[1] + (n_cells - tot)
                counts <- pmax(counts, 0L)

                cats <- factor(
                    rep(all_levels, times=counts),
                    levels=all_levels)

                data.frame(
                    variable = v,
                    var_idx  = vi,
                    cx       = (seq_len(n_cells) - 1L) %% grid_w,
                    cy       = (seq_len(n_cells) - 1L) %/% grid_w + (vi - 1L) * gap,
                    category = cats,
                    stringsAsFactors = FALSE)
            }))
            picto_df$category <- factor(picto_df$category, levels=all_levels)

            label_df <- data.frame(
                variable = var_order,
                lx       = -1.2,
                ly       = (seq_along(var_order) - 1L) * gap + 4.5,
                stringsAsFactors = FALSE)

            leg_lbl    <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL
            leg_pos    <- switch(opts$legendPos,
                bottom="bottom", right="right", top="top", none="none", "bottom")
            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

            base_t <- switch(opts$themeChoice,
                minimal = ggplot2::theme_minimal(base_size=12),
                classic = ggplot2::theme_classic(base_size=12),
                bw      = ggplot2::theme_bw(base_size=12),
                light   = ggplot2::theme_light(base_size=12),
                ggplot2::theme_minimal(base_size=12))

            ggplot2::ggplot(picto_df, ggplot2::aes(x=cx, y=cy, colour=category)) +
                ggplot2::geom_point(shape=16, size=4.5) +
                ggplot2::geom_text(
                    data        = label_df,
                    inherit.aes = FALSE,
                    ggplot2::aes(x=lx, y=ly, label=variable),
                    hjust    = 1,
                    size     = 3.5,
                    colour   = "grey25",
                    fontface = "plain") +
                ggplot2::scale_colour_manual(
                    values = pal, name=leg_lbl,
                    guide  = ggplot2::guide_legend(
                        nrow=1,
                        override.aes=list(size=4))) +
                ggplot2::scale_x_continuous(
                    expand = ggplot2::expansion(add=c(3.5, 0.5))) +
                ggplot2::coord_equal() +
                base_t +
                ggplot2::theme(
                    legend.position = leg_pos,
                    legend.text     = ggplot2::element_text(size=9),
                    legend.key.size = ggplot2::unit(0.5, "cm"),
                    plot.title      = ggplot2::element_text(size=13, face="bold", hjust=0.5),
                    axis.text       = ggplot2::element_blank(),
                    axis.ticks      = ggplot2::element_blank(),
                    axis.title      = ggplot2::element_blank(),
                    panel.grid      = ggplot2::element_blank(),
                    plot.margin     = ggplot2::margin(8, 14, 8, 80)) +
                ggplot2::labs(title=title_text)
        },

        # ── Parliament / arc chart ─────────────────────────────────────────
        # Semicircular arrangement of dots per variable, stacked vertically
        .parliament_plot = function(long, var_order, all_levels, pal, opts) {
            n_seats <- 100L
            n_rows  <- 4L
            radii   <- seq(0.8, 1.8, length.out=n_rows)
            # Seats per row proportional to arc length
            raw_seats <- radii / sum(radii) * n_seats
            row_seats <- round(raw_seats)
            row_seats[n_rows] <- n_seats - sum(row_seats[-n_rows])
            row_seats <- pmax(row_seats, 1L)

            gap_y <- 2.4  # vertical spacing between variables

            parl_df <- do.call(rbind, lapply(seq_along(var_order), function(vi) {
                v     <- var_order[vi]
                sub   <- long[long$variable == v, ]
                y_off <- (vi - 1L) * gap_y

                # Generate seat positions for all rows
                pts <- do.call(rbind, lapply(seq_len(n_rows), function(ri) {
                    r      <- radii[ri]
                    na_pts <- row_seats[ri]
                    angles <- seq(pi * 0.08, pi * 0.92, length.out=na_pts)
                    data.frame(px=r * cos(angles), py=r * sin(angles) + y_off,
                               stringsAsFactors=FALSE)
                }))

                # Assign categories
                counts <- round(sub$pct)
                diff   <- n_seats - sum(counts)
                if (diff != 0L)
                    counts[which.max(counts)] <- counts[which.max(counts)] + diff
                counts <- pmax(counts, 0L)
                tot <- sum(counts)
                if (tot != n_seats) counts[1] <- counts[1] + (n_seats - tot)
                counts <- pmax(counts, 0L)

                cats <- rep(all_levels, times=counts)
                # Trim or pad to exactly n_seats (safety)
                if (length(cats) > n_seats) cats <- cats[seq_len(n_seats)]
                if (length(cats) < n_seats) cats <- c(cats, rep(all_levels[1], n_seats - length(cats)))

                pts$category <- factor(cats, levels=all_levels)
                pts$variable <- v
                pts$var_idx  <- vi
                pts
            }))
            parl_df$category <- factor(parl_df$category, levels=all_levels)

            # Variable labels to the left of each arc
            label_df <- data.frame(
                variable = var_order,
                lx       = -2.1,
                ly       = (seq_along(var_order) - 1L) * gap_y + 0.7,
                stringsAsFactors = FALSE)

            leg_lbl    <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL
            leg_pos    <- switch(opts$legendPos,
                bottom="bottom", right="right", top="top", none="none", "bottom")
            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

            base_t <- switch(opts$themeChoice,
                minimal = ggplot2::theme_minimal(base_size=12),
                classic = ggplot2::theme_classic(base_size=12),
                bw      = ggplot2::theme_bw(base_size=12),
                light   = ggplot2::theme_light(base_size=12),
                ggplot2::theme_minimal(base_size=12))

            ggplot2::ggplot(parl_df, ggplot2::aes(x=px, y=py, colour=category)) +
                ggplot2::geom_point(size=3.2, shape=16, alpha=0.92) +
                ggplot2::geom_text(
                    data        = label_df,
                    inherit.aes = FALSE,
                    ggplot2::aes(x=lx, y=ly, label=variable),
                    hjust    = 1,
                    size     = 3.5,
                    colour   = "grey20") +
                ggplot2::scale_colour_manual(
                    values = pal, name=leg_lbl,
                    guide  = ggplot2::guide_legend(
                        nrow=1,
                        override.aes=list(size=4))) +
                ggplot2::coord_equal() +
                ggplot2::xlim(-2.4, 2.0) +
                base_t +
                ggplot2::theme(
                    legend.position = leg_pos,
                    legend.text     = ggplot2::element_text(size=9),
                    legend.key.size = ggplot2::unit(0.5, "cm"),
                    plot.title      = ggplot2::element_text(size=13, face="bold", hjust=0.5),
                    axis.text       = ggplot2::element_blank(),
                    axis.ticks      = ggplot2::element_blank(),
                    axis.title      = ggplot2::element_blank(),
                    panel.grid      = ggplot2::element_blank(),
                    plot.margin     = ggplot2::margin(8, 14, 8, 80)) +
                ggplot2::labs(title=title_text)
        },

        # ── Shared theme helper ────────────────────────────────────────────
        .add_theme = function(p, opts) {
            base_t <- switch(opts$themeChoice,
                minimal = ggplot2::theme_minimal(base_size=12),
                classic = ggplot2::theme_classic(base_size=12),
                bw      = ggplot2::theme_bw(base_size=12),
                light   = ggplot2::theme_light(base_size=12),
                ggplot2::theme_minimal(base_size=12))

            leg_pos <- switch(opts$legendPos,
                bottom="bottom", right="right", top="top", none="none", "bottom")

            title_text <- if (nchar(trimws(opts$title)) > 0) opts$title else NULL

            p + base_t +
                ggplot2::theme(
                    legend.position    = leg_pos,
                    legend.text        = ggplot2::element_text(size=9),
                    legend.key.size    = ggplot2::unit(0.5,"cm"),
                    plot.title         = ggplot2::element_text(size=13, face="bold", hjust=0.5),
                    axis.text.y        = ggplot2::element_text(size=10.5, colour="grey20"),
                    axis.text.x        = ggplot2::element_text(size=9.5),
                    axis.title         = ggplot2::element_blank(),
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor   = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_line(colour="grey90", linewidth=0.35),
                    plot.margin        = ggplot2::margin(8, 14, 8, 10)) +
                ggplot2::labs(title=title_text)
        },

        .palette = function(scheme, n) {
            n <- max(n, 1L)
            hcl_map <- list(
                rdbulite = "Blue-Red 3",
                rdylgn   = "Red-Green",
                piyg     = "PiYG",
                prgn     = "PRGn",
                pastel   = "Pastel 1",
                dark2    = "Dark 2",
                set2     = "Set 2",
                viridis  = "viridis",
                plasma   = "plasma")
            nm <- hcl_map[[scheme]]
            if (is.null(nm)) nm <- "Set 2"
            tryCatch(
                grDevices::hcl.colors(n, palette=nm),
                error=function(e) grDevices::hcl.colors(n, palette="Set 2"))
        }
    )
)
