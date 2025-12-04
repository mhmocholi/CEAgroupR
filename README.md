# CEAgroupR

**CEAgroupR** is an R package for performing multidimensional cost-effectiveness
and cost-utility analyses, with explicit support for multiple strategies,
multiple datasets (e.g., base case and discounted scenario), and subgroup-based
evaluations.

The package integrates a unified analytical engine for non-parametric bootstrap
estimation and a unified graphical engine for consistent, publication-ready
visualizations across all cost-effectiveness outputs.

## Key features

- Multi-dataset (multigroup) analysis.
- Multi-strategy comparison with automatic reference vs. alternatives.
- Subgroup stratification with automatic factor normalization.
- Non-parametric bootstrap for incremental costs, incremental effects, ICERs, NMB.
- Unified graphical engine (`ce_plot_base`) for:
  - ICER planes  
  - CEACs  
  - EVPIs  
  - Marginal distributions
- Interactive Shiny dashboard (`CEAgroupRui()`) for applied analyses.
- CRAN-friendly modular design with reproducible workflows.

---

# Installation

## Development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("TU_USUARIO/CEAgroupR")
```

## Local development version

```r
devtools::load_all()
```

---

# Quick example

Below is a minimal example using the bundled datasets `cua_multi` and
`cua_multi_discounted`.

```r
library(CEAgroupR)

data_list <- list(
  Base       = cua_multi,
  Discounted = cua_multi_discounted
)

res <- compute_icers(
  data          = data_list,
  group         = "group",
  cost          = "cost_total",
  effect        = "effect",
  R             = 500,
  lambda        = c(20000, 30000),
  subgroup_vars = c("diabetes", "HTA"),
  ref_group     = "usual_care"
)
```

Summary statistics:

```r
res$summary_stats
```

---

# ICER plane

```r
plot.icers(
  res,
  mode     = "Overall",
  color_by = "comparison",
  shape_by = "dataset",
  facet_by = "dataset"
)
```

---

# CEAC (Cost-Effectiveness Acceptability Curve)

```r
plot.ceacs(
  res,
  mode         = "Overall",
  color_by     = "comparison",
  shape_by     = "dataset",
  lambda_steps = 80
)
```

---

# EVPI

```r
plot.evpis(
  res,
  mode         = "Overall",
  color_by     = "comparison",
  shape_by     = "dataset",
  lambda_steps = 80
)
```

---

# Marginal distributions

Incremental cost:

```r
plot.marginals(
  res,
  variable  = "cost",
  geom_type = "density",
  mode      = "Overall"
)
```

Incremental effect:

```r
plot.marginals(
  res,
  variable  = "effect",
  geom_type = "histogram",
  mode      = "Overall",
  bins      = 30
)
```

---

# Subgroup visualizations

```r
plot.icers(
  res,
  mode     = "Subgroups",
  color_by = "comparison",
  facet_by = "subgroup"
)
```

---

# Interactive dashboard

CEAgroupR includes an interactive Shiny interface:

```r
CEAgroupRui()
```

This dashboard allows users to upload datasets, configure bootstrap settings,
view summary statistics, generate ICER/CEAC/EVPI/Marginal plots interactively,
and export graphics in PNG and PDF formats.

---

# Documentation

Access the full vignette:

```r
browseVignettes("CEAgroupR")
```

---

# Citation

If you use **CEAgroupR** in publications, please cite:

```
Hernández-Mocholí, M. A. (2025). CEAgroupR: Multigroup Cost-Effectiveness Analysis in R.
```

---

# License

MIT License (see LICENSE file)
