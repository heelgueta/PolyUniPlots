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

            if (opts$diverging && n_cats >= 3) {
                p <- private$.diverging_plot(long, var_order, all_levels, pal, opts)
            } else {
                p <- private$.stacked_plot(long, var_order, all_levels, pal, opts)
            }

            print(p)
            TRUE
        },

        .stacked_plot = function(long, var_order, all_levels, pal, opts) {
            bar_h <- opts$barHeight / 100
            leg_lbl <- if (nchar(trimws(opts$legendTitle)) > 0) opts$legendTitle else NULL

            p <- ggplot2::ggplot(long, ggplot2::aes(
                    x = pct, y = variable, fill = category)) +
                ggplot2::geom_col(
                    position = ggplot2::position_stack(reverse = TRUE),
                    width = bar_h, colour = NA)

            if (opts$showPct) {
                ld <- long[long$pct >= opts$minPctLabel, ]
                ld$label <- paste0(round(ld$pct, 1), "%")
                p <- p + ggplot2::geom_text(
                    data = ld,
                    ggplot2::aes(label = label),
                    position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
                    size = 3.2, colour = "white", fontface = "bold")
            }

            p <- p +
                ggplot2::scale_x_continuous(
                    limits = c(0, 100), breaks = seq(0, 100, 25),
                    labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
                ggplot2::scale_fill_manual(
                    values = pal, name = leg_lbl,
                    guide  = ggplot2::guide_legend(nrow = 1))

            if (opts$showN) {
                nd <- unique(long[, c("variable","n_total")])
                p <- p +
                    ggplot2::geom_text(
                        data = nd, inherit.aes = FALSE,
                        ggplot2::aes(x = 103, y = variable,
                                     label = paste0("n=", n_total)),
                        hjust = 0, size = 3, colour = "grey40") +
                    ggplot2::coord_cartesian(clip = "off") +
                    ggplot2::theme(plot.margin = ggplot2::margin(8, 55, 8, 10))
            }

            private$.add_theme(p, opts)
        },

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
                    r  <- data.frame(variable=sub$variable[1], category=cat,
                                     xmin=cum*sign, xmax=(cum+pv)*sign, pct=pv,
                                     stringsAsFactors=FALSE)
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
                # neutral: split evenly 50/50 around zero
                neu   <- if (length(neu_lvl) > 0) {
                    pv <- sub$pct[sub$category == neu_lvl]
                    if (length(pv) == 0) pv <- 0
                    data.frame(variable=v, category=neu_lvl,
                               xmin=-pv/2, xmax=pv/2, pct=pv,
                               stringsAsFactors=FALSE)
                } else NULL
                full <- rbind(neg, neu, pos)
                full$n_total <- n_tot
                full
            }))

            div_df$variable <- factor(div_df$variable, levels=var_order)
            div_df$category <- factor(div_df$category, levels=all_levels)

            p <- ggplot2::ggplot(div_df, ggplot2::aes(
                    xmin=xmin, xmax=xmax,
                    ymin=as.numeric(variable)-bar_h/2,
                    ymax=as.numeric(variable)+bar_h/2,
                    fill=category)) +
                ggplot2::geom_rect(colour=NA) +
                ggplot2::geom_vline(xintercept=0, colour="grey30", linewidth=0.5) +
                ggplot2::scale_y_continuous(
                    breaks=seq_along(var_order), labels=var_order,
                    expand=c(0.05,0)) +
                ggplot2::scale_x_continuous(
                    labels=function(x) paste0(abs(x),"%"),
                    limits=c(-100,100), breaks=seq(-100,100,25)) +
                ggplot2::scale_fill_manual(values=pal, name=leg_lbl,
                    guide=ggplot2::guide_legend(nrow=1))

            if (opts$showPct) {
                ld       <- div_df[div_df$pct >= opts$minPctLabel, ]
                ld$xmid  <- (ld$xmin + ld$xmax) / 2
                ld$label <- paste0(round(ld$pct,1),"%")
                p <- p + ggplot2::geom_text(
                    data=ld, inherit.aes=FALSE,
                    ggplot2::aes(x=xmid, y=as.numeric(variable), label=label),
                    size=3, colour="white", fontface="bold")
            }

            if (opts$showN) {
                nd <- unique(div_df[, c("variable","n_total")])
                nd$var_num <- as.numeric(nd$variable)
                p <- p +
                    ggplot2::geom_text(
                        data=nd, inherit.aes=FALSE,
                        ggplot2::aes(x=103, y=var_num,
                                     label=paste0("n=",n_total)),
                        hjust=0, size=3, colour="grey40") +
                    ggplot2::coord_cartesian(clip="off") +
                    ggplot2::theme(plot.margin=ggplot2::margin(8,55,8,10))
            }

            private$.add_theme(p, opts)
        },

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
                    plot.margin        = ggplot2::margin(8,14,8,10)) +
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
