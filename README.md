# PolyUniPlots

A jamovi module for plotting **multiple variables side by side on a shared scale**. Stop flipping between outputs — drop all your variables in at once and compare them in one chart.

Two analyses are included: one for numeric variables and one for ordinal/nominal variables.

---

## Installation

Download `PolyUniPlots.jmo` from this repo, then in jamovi:

**Modules → Install from file → select PolyUniPlots.jmo**

Requires jamovi ≥ 2.3 (tested on 2.4.8).

---

## PolyNum — Multiple Numeric Plots

Plot any number of continuous variables on the same axis. Layer geoms freely:

| Geom | What it shows |
|---|---|
| Box plot | Median, IQR, whiskers, outliers |
| Violin | Full distribution shape |
| Jitter | Raw data points |
| Mean diamond | Mean as ◆ |
| Rug | Tick marks along the axis |

**Options:** horizontal or vertical orientation · 10 colour palettes · 4 themes · transparency, box width, jitter spread · custom title · manual plot size

**Typical use:** comparing subscales, pre/post measures, physiological variables, any set of numeric items sharing a meaningful common scale.

---

## PolyOrd — Multiple Ordinal/Nominal Plots

Display several categorical variables as horizontal proportional bars, all on the same 0–100% scale. Two layout modes:

| Mode | Description |
|---|---|
| Stacked (default) | 100% bars, categories left to right |
| Diverging (Likert) | Centred at the neutral midpoint; negative categories extend left, positive right |

**Options:** 10 palettes including diverging schemes · percentage labels with minimum threshold · N per variable · sort variables (as entered / alphabetically / by first or last category) · reverse order · legend position · custom title · manual plot size

**Typical use:** Likert-scale batteries, multiple ordinal items on a shared response scale, any set of categorical variables you want to compare at a glance.

---

## Repository structure

```
analyses/          # YAML option and result definitions
R/                 # R source (implementation + base classes)
ui/                # Compiled JS UI panels
icons/             # Per-analysis SVG icons
jamovi.yaml        # Module manifest
DESCRIPTION        # R package metadata
NAMESPACE
PolyUniPlots.jmo   # Packaged module (install directly in jamovi)
```

---

## Roadmap

- [ ] Grouping variable for split distributions
- [ ] Histogram / density option for numeric vars
- [ ] Ridge plot (ggridges)
- [ ] Custom colour per variable

---

## License

GPL (≥ 2)
