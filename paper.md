---
title: 'PolyUniPlots: A jamovi module for comparative univariate visualization'
tags:
  - R
  - jamovi
  - visualization
  - exploratory data analysis
  - psychology
  - social sciences
authors:
  - name: Herman Elgueta
    orcid: 0000-XXXX-XXXX-XXXX
    affiliation: 1
affiliations:
  - name: Universidad de Magallanes, Punta Arenas, Chile
    index: 1
date: 31 May 2026
bibliography: paper.bib
---

# Summary

PolyUniPlots is a module for jamovi [@jamovi] that provides point-and-click access to a wide range of univariate visualization techniques applied simultaneously across multiple variables. It includes two analyses: **polyNum**, for continuous variables, and **polyOrd**, for ordinal and nominal (factor) variables. Rather than plotting one variable at a time, both analyses render all selected variables in a single, unified graphic, enabling direct visual comparison of distributions and response patterns across items, scales, or measurement occasions. The module is implemented entirely in R [@R] using ggplot2 [@ggplot2], relying only on packages already bundled with jamovi, and requires no additional installation or coding by the end user.

# Statement of Need

Exploratory visualization of multiple variables is a routine task in social and behavioral research. Researchers routinely administer batteries of Likert-type items, psychological scales, or repeated measurements and need to inspect distributions side by side before conducting further analysis. Despite this, most point-and-click statistical software treats univariate plots as single-variable operations. Producing a multi-variable comparison therefore typically requires scripting, manual arrangement of separate figures, or switching to specialized tools—an unnecessary barrier for researchers who rely on graphical interfaces.

Existing jamovi modules cover specific visualization needs well. The `jjstatsplot` module [@jjstatsplot] provides annotated statistical graphics; `scatr` focuses on scatterplots and regression; `ggplot2` itself [@ggplot2] is accessible via the `Rj` editor within jamovi but requires writing code. No existing module offers a broad, menu-driven toolkit dedicated specifically to the problem of visualizing many variables at once with a common coordinate system and a shared color legend. PolyUniPlots fills this gap.

The target audience is researchers in psychology, education, sociology, and adjacent fields who use jamovi as their primary analysis environment and who need to rapidly characterize distributions of multiple variables—whether to check normality assumptions, inspect item-level response distributions in survey data, or generate publication-ready figures without leaving the graphical interface.

# Functionality

## polyNum: Numeric variable visualization

polyNum accepts any number of continuous variables and renders them together in a single panel. Seven plot types are available, selectable via radio buttons in the interface:

- **Box plot**: standard five-number summary with optional outlier display.
- **Violin**: kernel density mirrored around a central axis, with an optional embedded box, with configurable scale normalization (area, count, or width).
- **Strip**: individual data points jittered along a categorical axis.
- **Histogram**: per-variable histograms arranged in a faceted grid.
- **Ridge**: overlapping density ribbons stacked vertically, inspired by the ridgeline (or "joyplot") style [@ggridges], with configurable overlap percentage so that adjacent distributions can interpenetrate for a more compact display.
- **Barcode**: a tick-mark representation in which each observation is drawn as a short perpendicular segment, analogous to a one-dimensional rug without the marginal axis context; useful for small samples.
- **Raincloud**: a combination of a half-violin density estimate, an individual-point jitter layer, and a compact boxplot [@allen2021], always rendered horizontally for readability; particularly suited to comparing skewed or multimodal distributions.

For box, violin, and strip plots, the user can choose between vertical and horizontal orientations. Across relevant plot types, optional overlays include a mean marker with optional confidence interval (width configurable from 50% to 99%), a rug, and individual jitter. Aesthetic controls include nine color palettes (discrete colorblind-friendly and perceptually uniform sequential schemes from viridis [@viridis]), four ggplot2 themes, transparency, box width, jitter spread, ridge overlap, and line thickness.

## polyOrd: Ordinal and nominal variable visualization

polyOrd accepts factor variables and renders response distributions across all selected variables in a single figure. Four chart types are available:

- **Stacked bars** (default): 100% stacked horizontal bars, one per variable, sharing a common x-axis from 0% to 100%. A diverging mode centers the chart on a neutral midpoint for Likert-type items, splitting the middle category evenly left and right and anchoring positive and negative poles on opposite sides [@likert; @hartigan]. Variable ordering can be fixed (as entered), alphabetical, or sorted by frequency of the first or last category.
- **Waffle chart**: a 10 × 10 grid of tiles per variable in which each cell represents one percentage point; categories fill the grid sequentially and are colored by category. Multiple variables stack vertically with labeled rows, making proportional comparisons across items visually immediate.
- **Pictogram**: the same 10 × 10 grid rendered as filled circles rather than tiles, approximating the dot-matrix or isotype style [@neurath] often preferred in communication contexts.
- **Parliament/arc chart**: seats arranged in concentric semicircular arcs (inspired by hemisphere or parliament diagrams), where the total arc length is divided among categories in proportion to their frequency. Each variable is rendered as a separate panel stacked vertically, with variable labels to the left of each arc.

Across all chart types, optional labels include percentage values (with a configurable minimum threshold to suppress labels on very small segments) and per-variable sample sizes. The legend title, position, color palette, and theme are fully configurable.

## Implementation

PolyUniPlots is implemented as a standard jamovi module. The R source defines two `jmvcore::Analysis` subclasses (`polyNumClass` and `polyOrdClass`) following the jmvcore [@jmvcore] R6 class pattern. All plotting is performed with ggplot2 [@ggplot2] and the scales package [@scales], both of which are bundled within jamovi's base module and therefore require no separate installation. Ridge density estimates use `stats::density()` rather than the ggridges package [@ggridges] to avoid external dependencies; raincloud half-violins are similarly computed with base R density estimation. Waffle, pictogram, and parliament layouts use manual coordinate arithmetic passed to standard ggplot2 geoms (`geom_tile`, `geom_point`, `geom_arc` approximated via `geom_polygon`), keeping the dependency footprint minimal.

The jamovi GUI is defined in a Browserify UMD JavaScript bundle following jamovi's `jus 2.0` UI specification. Radio buttons control discrete plot type selection; collapse boxes organize options by logical group. The module is distributed as a `.jmo` archive installable directly from within jamovi via *Modules → Install from file*.

# Acknowledgements

The author thanks the jamovi development team for their open platform and documentation.

# References
