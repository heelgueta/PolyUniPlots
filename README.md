# PolyUniPlots — Multiple Univariate Plots for jamovi

A jamovi module for plotting **multiple variables simultaneously** in a single, shared-scale chart. Compare distributions at a glance without flipping between outputs.

---

## Analyses

### 1. Multiple Numeric Plots (`PolyNum`)

Stack as many numeric variables as you want on the same axis. Mix and match geom layers:

| Layer | What it shows |
|---|---|
| **Violin** | Full distribution shape |
| **Box plot** | Median, IQR, whiskers |
| **Jitter** | Raw data points (with spread control) |
| **Mean diamond** | Mean marked as a ◆ |
| **Rug** | Marginal tick marks along the axis |

**Options:**
- Horizontal or vertical orientation
- 10 colour palettes (Viridis, Plasma, Mako, Rocket, Turbo, Tableau 10, Dark 2, Pastel 1, Warm, Cold)
- 4 themes (Minimal, Classic, B&W, Light)
- Independent control over transparency, box width, jitter spread, point size, violin scaling
- Toggle outliers, grid lines
- Custom title and axis labels
- Manual plot size (px)
- Automatic `n=` caption per variable

**Typical use:** compare pre/post scores, multiple subscales, physiological measures.

---

### 2. Multiple Ordinal/Nominal Plots (`PolyOrd`)

Display several Likert-scale items (or any categorical variables) as horizontal proportional bars, all on the same 0–100% scale.

**Two layout modes:**

| Mode | Description |
|---|---|
| **Stacked (default)** | 100% horizontal bars, categories left-to-right |
| **Diverging (Likert)** | Centred at neutral; disagree extends left, agree right |

**Options:**
- 10 colour palettes including diverging schemes (Red–Blue, Red–Yellow–Green, PiYG, Purple–Green)
- Percentage labels with a minimum-display threshold (e.g. hide labels < 5%)
- Optional `n=` count per variable
- Sort variables: as entered, alphabetically, by first/last category frequency
- Reverse variable order
- Legend position (bottom / right / top / none) and custom legend title
- Custom plot title and manual size

**Typical use:** Likert-scale survey batteries, multiple ordinal items sharing the same response scale.

---

## Installation

### Requirements

- [jamovi](https://www.jamovi.org/) ≥ 2.3
- R ≥ 4.1 (bundled with jamovi)
- R packages: `ggplot2 ≥ 3.3`, `scales ≥ 1.1` (installed automatically)

### Option A — Install from source (recommended during development)

```r
# In R (not the jamovi R):
install.packages('jmvtools',
  repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))

# Install the module into jamovi
jmvtools::install('path/to/PolyUniPlots')
```

Then restart jamovi. The module appears under **PolyUniPlots** in the Analyses menu.

### Option B — Developer mode (live reload)

```r
# From the PolyUniPlots directory:
jmvtools::prepare()   # generates base R classes from YAML
jmvtools::install()   # installs into jamovi's module library
```

To iterate quickly, use `jmvtools::install()` after each change and restart jamovi.

---

## Development notes

```
PolyUniPlots/
├── DESCRIPTION            # R package metadata + dependencies
├── NAMESPACE              # R namespace declarations
├── R/
│   ├── polynum.b.R        # PolyNum analysis implementation
│   └── polyord.b.R        # PolyOrd analysis implementation
└── jamovi/
    ├── polynum.a.yaml     # Analysis options definition
    ├── polynum.r.yaml     # Results (image output) definition
    ├── polynum.u.yaml     # UI layout definition
    ├── polyord.a.yaml
    ├── polyord.r.yaml
    └── polyord.u.yaml
```

The `.b.R` files contain hand-written R6 classes that extend auto-generated base classes (created by `jmvtools::prepare()` from the YAML definitions). Do not edit files named `*.base.R` — they are regenerated.

---

## Roadmap

- [ ] Grouping variable for split distributions
- [ ] Histogram / density panel for numeric vars
- [ ] Export plot as PNG/SVG from within jamovi
- [ ] Ridge plot option (ggridges)
- [ ] Custom colour picker per variable

---

## License

GPL (≥ 2)
