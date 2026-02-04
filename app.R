# app.R — CLT / Sampling Intuition (CLT kept; updated panels 2/3/4; responsive; fixed bugs)
library(shiny)
library(bslib)
library(ggplot2)

'
unlink("docs", recursive = TRUE, force = TRUE)
shinylive::export(appdir = ".", destdir = "docs")
httpuv::runStaticServer("docs", port = 8000)
'

if (FALSE) {
  library(munsell)
  library(tibble)
}

CLT_VH <- "calc(100vh - 80px)"
`%||%` <- function(a, b) if (is.null(a)) b else a

# =========================
# CSS as a single variable
# =========================
APP_CSS <- HTML("
  .card, .card-body, .bslib-card { min-height: 0 !important; }

  .plot-stack {
    height: 100%;
    min-height: 0;
    display: flex;
    flex-direction: column;
    gap: .75rem;
  }

  .plot-box {
    flex: 1 1 auto;
    min-height: 230px;
  }

  #clt_stat { display: none; }

  .scroll-y { overflow-y: auto; min-height: 0; }

  @media (max-width: 992px) {
    .plot-box { min-height: 210px; }
  }
  @media (max-width: 768px) {
    .plot-box { min-height: 190px; }
  }

  .navbar .jb-logo-item > a {
    padding-left: 0.1rem !important;
    padding-right: 0.1rem !important;
    padding-top: 0.1rem !important;
    padding-bottom: 0.4rem !important;
  }
  .jb-logo { height: 38px; display: block; margin: 0; }
")

# Helper for fillable plot cards
plot_card <- function(id, flex = 1, min_h = 230, title = NULL) {
  card(
    fill = TRUE,
    class = "plot-box",
    style = sprintf("flex:%s 1 0; min-height:%spx;", flex, min_h),
    if (!is.null(title)) card_header(title),
    card_body_fill(plotOutput(id, height = "100%"))
  )
}

# =========================
# Color system (edit here)
# =========================
COL_BG       <- "#f7f6f3"
COL_BARS     <- "#4A4E69"
COL_OUTLINE  <- COL_BG
COL_EXPECTED <- "#E69F00"
COL_CENTER   <- "#2f6f8f"

FIXED_SEED <- 91L

# =========================
# Plot theme
# =========================
Plot_theme <- function(
    base_font    = c("Avenir Next", "Verdana", "sans-serif"),
    heading_font = c("Trebuchet MS", "Avenir", "Verdana", "sans-serif"),
    base_size = 12
) {
  base_family    <- base_font[[1]]
  heading_family <- heading_font[[1]]

  theme_minimal(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.background  = element_rect(fill = COL_BG, color = NA),
    panel.background = element_rect(fill = COL_BG, color = NA),
    text = element_text(color = "#343A40"),
    panel.grid.major = element_line(color = "#cfdceb", linewidth = 0.45),
    panel.grid.minor = element_line(color = "#e3edf7", linewidth = 0.25),
    axis.line = element_line(color = "#9fb3c1"),
    axis.text.x  = element_text(color = "#343A40", family = base_family, size = 13),
    axis.text.y  = element_text(color = "#343A40", family = base_family, size = 13),
    axis.title.x = element_text(color = "#343A40", family = heading_family, face = "bold", size = 14),
    axis.title.y = element_text(color = "#343A40", family = heading_family, face = "bold", size = 14, angle = 90, margin = margin(r = 3)),
    plot.title = element_text(
      family = heading_family, size = 16, face = "bold",
      color = "#2f3e46", margin = margin(b = 8, unit = "pt")
    ),
    plot.subtitle = element_text(family = base_family, size = 14, color = "#3b4a55", margin = margin(b = 3)),
    plot.margin = margin(t = 5, r = 6, b = 4, l = 6, unit = "pt")
  )
}

# ============================================================
# =========================  PANEL 1: CLT (AS-IS)  =========================
# ============================================================

dist_specs <- function(dist, params) {
  if (dist == "Normal") {
    mu <- params$mu; sd <- params$sd
    gen <- function(n) rnorm(n, mean = mu, sd = sd)
    return(list(name = "Normal", pop_center = mu, pop_sd = sd, gen = gen))
  }
  if (dist == "Uniform") {
    a <- params$a; b <- params$b
    gen <- function(n) runif(n, min = a, max = b)
    pop_center <- (a + b) / 2
    pop_sd <- (b - a) / sqrt(12)
    return(list(name = "Uniform", pop_center = pop_center, pop_sd = pop_sd, gen = gen))
  }
  if (dist == "Exponential") {
    mean <- params$exp_mean
    rate <- 1 / mean
    gen <- function(n) rexp(n, rate = rate)
    pop_center <- mean
    pop_sd <- mean
    return(list(name = "Exponential", pop_center = pop_center, pop_sd = pop_sd, gen = gen))
  }
  if (dist == "Lognormal") {
    median <- params$ln_median
    sigma  <- params$ln_sigma
    mu <- log(median)
    gen <- function(n) rlnorm(n, meanlog = mu, sdlog = sigma)
    pop_center <- exp(mu + 0.5 * sigma^2)
    pop_sd <- sqrt((exp(sigma^2) - 1) * exp(2 * mu + sigma^2))
    return(list(name = "Lognormal", pop_center = pop_center, pop_sd = pop_sd, gen = gen))
  }
  if (dist == "Bernoulli") {
    p <- params$p
    gen <- function(n) rbinom(n, size = 1, prob = p)
    pop_center <- p
    pop_sd <- sqrt(p * (1 - p))
    return(list(name = "Bernoulli (0/1)", pop_center = pop_center, pop_sd = pop_sd, gen = gen))
  }
  if (dist == "Poisson") {
    lambda <- params$lambda
    gen <- function(n) rpois(n, lambda = lambda)
    pop_center <- lambda
    pop_sd <- sqrt(lambda)
    return(list(name = "Poisson", pop_center = pop_center, pop_sd = pop_sd, gen = gen))
  }
  stop("Unknown distribution")
}

calc_stat <- function(x, stat) if (stat == "mean") mean(x) else median(x)
pretty_stat <- function(stat) if (stat == "mean") "Average" else "Median"

expected_curve_counts_continuous <- function(ref, N, binwidth, from, to) {
  ref <- ref[is.finite(ref)]
  ref <- ref[ref >= 0]

  if (length(ref) < 50 || !is.finite(N) || N <= 0 || !is.finite(binwidth) || binwidth <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  d <- density(ref, from = from, to = to, n = 512)
  y <- d$y * N * binwidth

  if (!any(is.finite(y)) || max(y, na.rm = TRUE) <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  data.frame(x = d$x, y = y)
}

expected_curve_counts_discrete <- function(dist, params, N, lim) {
  if (!is.finite(N) || N <= 0) return(data.frame(x = numeric(0), y = numeric(0)))

  if (dist == "Poisson") {
    lam <- params$lambda
    xs <- 0:floor(lim[2])
    ys <- dpois(xs, lam) * N
    return(data.frame(x = xs, y = ys))
  }

  if (dist == "Bernoulli") {
    p <- params$p
    xs <- c(0, 1)
    ys <- dbinom(xs, 1, p) * N
    return(data.frame(x = xs, y = ys))
  }

  data.frame(x = numeric(0), y = numeric(0))
}

# ============================================================
# =========================  PANEL 2: REPRESENTATION  ======================
# ============================================================

strata_palette <- c("#2f6f8f", "#E69F00", "#4A4E69", "#6c757d", "#94b0c2")

make_grid <- function(n_total, n_cols = NULL) {

  n_total <- as.integer(n_total)
  if (is.null(n_cols))
    n_cols <- max(10L, as.integer(round(sqrt(n_total))))
  id <- seq_len(n_total)
  x <- ((id - 1L) %% n_cols) + 1L
  y <- ((id - 1L) %/% n_cols) + 1L
  data.frame(id = id, x = x, y = y)
  }

# population tiling: wide; keep your existing intent
ncols_for_pop <- function(N) {
  N <- as.integer(N)

  allowed <- c(1L, 2L, 4L, 5L, 10L, 20L, 25L, 50L, 100L)

  # Choose a default "nice" width; 50 is a good wide layout
  preferred <- 50L

  # Snap to nearest allowed (stable, never errors)
  allowed[which.max(abs(allowed - preferred))]
}

# sample tiling: "as wide as possible" up to a cap; INTERNAL, no UI
SAMP_NCOL_MAX <- 95L
ncols_for_sample <- function(n) {
  n <- as.integer(n)

  allowed <- c(1L, 2L, 4L, 5L, 10L, 20L, 25L, 50L, 100L)

  # Prefer wide, but cap so the plot doesn't get absurdly wide
  preferred <- min(50L, max(10L, n))     # tends toward 50 as n grows
  preferred <- min(preferred, SAMP_NCOL_MAX)

  allowed[which.max(abs(allowed - preferred))]
}

build_population <- function(N = 2000, k = 3, props = NULL, rates = NULL, seed = 91) {
  set.seed(seed)
  k <- as.integer(k)
  grid <- make_grid(N, n_cols = ncols_for_pop(N))

  if (is.null(props)) props <- rep(1 / k, k)
  props <- pmax(props, 0)
  props <- props / sum(props)

  if (is.null(rates)) rates <- seq(0.08, 0.30, length.out = k)

  n_by <- floor(N * props)
  while (sum(n_by) < N) n_by[which.max(props)] <- n_by[which.max(props)] + 1
  while (sum(n_by) > N) n_by[which.max(n_by)] <- n_by[which.max(n_by)] - 1

  strata <- rep(seq_len(k), times = n_by)
  strata <- sample(strata, length(strata), replace = FALSE)

  p_i <- rates[strata]
  y <- rbinom(N, 1, p_i)

  cbind(grid, stratum = factor(strata, levels = seq_len(k)), outcome = y)
}

draw_sample <- function(pop_df, n, mode = c("srs", "stratified"), seed = 91L) {
  mode <- match.arg(mode)
  n <- as.integer(n)
  n <- max(1L, min(n, nrow(pop_df)))

  set.seed(as.integer(seed))

  if (mode == "srs") {
    idx <- sample(pop_df$id, n, replace = FALSE)
    return(pop_df[pop_df$id %in% idx, , drop = FALSE])
  }

  tab <- table(pop_df$stratum)
  props <- as.numeric(tab) / sum(tab)
  n_by <- floor(n * props)
  while (sum(n_by) < n) n_by[which.max(props)] <- n_by[which.max(props)] + 1
  while (sum(n_by) > n) n_by[which.max(n_by)] <- n_by[which.max(n_by)] - 1

  samp <- do.call(rbind, lapply(seq_along(n_by), function(i) {
    ids <- pop_df$id[pop_df$stratum == levels(pop_df$stratum)[i]]
    if (length(ids) == 0) return(pop_df[0, ])
    take <- min(n_by[i], length(ids))
    pop_df[pop_df$id %in% sample(ids, take, replace = FALSE), , drop = FALSE]
  }))
  samp
}

# grouped sample layout: reverse strata so higher number lands toward bottom;
# then lay out with very wide columns (internal policy)
grouped_sample_layout <- function(samp_df, n_cols) {
  levs <- levels(samp_df$stratum)
  samp_df$stratum <- factor(samp_df$stratum, levels = rev(levs))
  samp_df <- samp_df[order(samp_df$stratum, samp_df$id), , drop = FALSE]

  n_cols <- as.integer(max(10L, n_cols))
  grid <- make_grid(nrow(samp_df), n_cols = n_cols)

  out <- samp_df
  out$x <- grid$x
  out$y <- grid$y
  out
}

# tiles: keep borders = background (no random outline); keep fill = stratum
# ADD: overlay outcome indicator (circle) where outcome == 1, if outcome exists
plot_tiles <- function(df, title, subtitle, show_legend = TRUE) {
  p <- ggplot(df, aes(x, y, fill = stratum)) +
    geom_tile(color = COL_BG, linewidth = 0.6) +
    scale_fill_manual(values = strata_palette[seq_along(levels(df$stratum))]) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    Plot_theme() +
    theme(panel.grid = element_blank(), axis.line = element_blank())

  if ("outcome" %in% names(df)) {
    df_case <- df[is.finite(df$outcome) & df$outcome == 1, , drop = FALSE]
    if (nrow(df_case) > 0) {
      p <- p +
        geom_point(
          data = df_case,
          aes(x, y),
          inherit.aes = FALSE,
          shape = 21,           # circle
          fill  = NA,           # hollow
          color = "#2f3e46",    # readable on all strata fills
          stroke = 0.9,
          size = 1.5
        )
    }
  }

  if (!show_legend) p <- p + theme(legend.position = "none")
  p
}

plot_comp_bars <- function(pop_df, samp_df) {
  pop_tab  <- as.data.frame(prop.table(table(pop_df$stratum)))
  samp_tab <- as.data.frame(prop.table(table(samp_df$stratum)))
  names(pop_tab)  <- c("stratum", "prop")
  names(samp_tab) <- c("stratum", "prop")
  pop_tab$group <- "Population"
  samp_tab$group <- "Sample"

  df <- rbind(pop_tab, samp_tab)
  df$stratum <- factor(df$stratum, levels = levels(pop_df$stratum))
  df <- df[order(df$group, df$stratum), ]
  df$label <- paste0(round(100 * df$prop), "%")

  ggplot(df, aes(x = group, y = prop, fill = stratum)) +
    geom_col(width = 0.62, color = COL_BG, linewidth = 1.0) +
    geom_text(
      data = df[df$prop > 0.06, ],
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "#343A40",
      family = "Avenir Next"
    ) +
    scale_fill_manual(values = strata_palette[seq_along(levels(pop_df$stratum))]) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    labs(
      title = "Sample composition (by Group)",
      x = NULL, y = "Proportion"
    ) +
    Plot_theme() +
    theme(legend.position = "none")
}

# NEW: outcome plot by Group (population vs sample)
plot_outcome_rates <- function(pop_df, samp_df) {
  pop <- aggregate(outcome ~ stratum, data = pop_df, FUN = mean)
  samp <- aggregate(outcome ~ stratum, data = samp_df, FUN = mean)
  pop$group <- "Population"
  samp$group <- "Sample"
  df <- rbind(pop, samp)
  df$stratum <- factor(df$stratum, levels = levels(pop_df$stratum))
  df$label <- paste0(round(100 * df$outcome), "%")

  ggplot(df, aes(x = stratum, y = outcome, fill = group)) +
    geom_col(position = position_dodge(width = 0.70), width = 0.62, color = COL_BG, linewidth = 1.0) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.70),
      vjust = -0.35,
      size = 4,
      family = "Avenir Next",
      color = "#343A40"
    ) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, max(df$outcome) * 1.15)) +
    scale_fill_manual(values = c("Population" = "#2f3e46", "Sample" = COL_CENTER)) +
    labs(
      title = "Outcome rate (within each Group)",
      x = "Group", y = "Outcome rate"
    ) +
    Plot_theme() +
    theme(legend.position = "top", legend.title = element_blank())
}

# ============================================================
# =========================  PANEL 3: REGRESSION TO MEAN  ===================
# ============================================================

sim_rtm <- function(N = 1200, mu = 0, sd_true = 12, sd_noise = 8, top_pct = 10, seed = 91L) {
  set.seed(as.integer(seed))
  true <- rnorm(N, mu, sd_true)
  y1 <- true + rnorm(N, 0, sd_noise)
  cutoff <- as.numeric(stats::quantile(y1, probs = 1 - top_pct / 100, names = FALSE))
  sel <- y1 >= cutoff
  y2 <- true + rnorm(N, 0, sd_noise)
  data.frame(id = seq_len(N), true = true, y1 = y1, y2 = y2, selected = sel, cutoff = cutoff)
}

plot_rtm_selection <- function(df) {
  ggplot(df, aes(y1)) +
    geom_histogram(bins = 40, fill = COL_BARS, color = COL_OUTLINE, alpha = 0.55) +
    geom_vline(xintercept = unique(df$cutoff), color = COL_CENTER, linewidth = 1.2, linetype = "dashed") +
    annotate("text", x = unique(df$cutoff), y = Inf,
             label = "Selection threshold\n(based on observed y1)",
             vjust = 1.2, hjust = -0.05, color = COL_CENTER, family = "Avenir Next", size = 4.2) +
    labs(
      title = "Step 1: Selection based on an extreme noisy measurement",
      subtitle = "Selection is on observed y1 (true values do not change).",
      x = "Observed first measurement (y1)",
      y = "Count"
    ) +
    Plot_theme()
}

# Revert to your “old” look (segments + points), but SORTED:
# - we order selected individuals by y1, and use that order as x
plot_rtm_pairs <- function(df) {
  sub <- df[df$selected, , drop = FALSE]
  validate(need(nrow(sub) > 2, "Not enough selected points to show the re-measure plot."))

  # Keep it legible
  if (nrow(sub) > 300) sub <- sub[sample(seq_len(nrow(sub)), 300), , drop = FALSE]

  # Sort by y1 so the bundle has structure (less spaghetti)
  sub <- sub[order(sub$y1, decreasing = TRUE), , drop = FALSE]
  sub$idx <- seq_len(nrow(sub))

  # Long form: two time points
  long <- rbind(
    data.frame(idx = sub$idx, time = "Observed y1 (selected)", value = sub$y1),
    data.frame(idx = sub$idx, time = "Observed y2 (new noise)", value = sub$y2)
  )
  long$time <- factor(long$time, levels = c("Observed y1 (selected)", "Observed y2 (new noise)"))

  # Summary anchors (means) to help the eye
  means <- aggregate(value ~ time, data = long, FUN = mean)

  means$hjust_lab <- ifelse(
    as.character(means$time) == "Observed y1 (selected)",
    1.4,
    -0.8
  )


  ggplot(long, aes(x = time, y = value, group = idx)) +
    # Individual trajectories
    geom_line(color = COL_BARS, alpha = 0.15, linewidth = 0.6) +
    # Points (subtle but visible)
    geom_point(aes(color = time), size = 1.5, alpha = 0.7) +

    # Mean markers (strong anchors)
    geom_point(
      data = means,
      aes(x = time, y = value),
      inherit.aes = FALSE,
      size = 3.2,
      color = "#E69F00"
    ) +
    geom_line(
      data = means,
      aes(x = time, y = value, group = 1),
      inherit.aes = FALSE,
      linewidth = 1.1,
      color = "#E69F00",
      alpha = 0.9
    ) +
    geom_text(
      data = means,
      aes(x = time, y = value, label = sprintf("mean\n%.2f", value), hjust = hjust_lab),
      inherit.aes = FALSE,
      size = 6,
      family = "Avenir Next",
      color = COL_CENTER
    ) +

    scale_color_manual(values = c(
      "Observed y1 (selected)" = COL_BARS,
      "Observed y2 (new noise)" = COL_CENTER
    )) +
    labs(
      title = "Step 2: Re-measure the same selected individuals",
      subtitle = "Same people, new noise. Most y2 values are less extreme than the y1 values used for selection.",
      x = NULL,
      y = "Observed value"
    ) +
    Plot_theme() +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    )
}

plot_rtm_expectations <- function(df) {
  sub <- df[df$selected, , drop = FALSE]
  e_true_sel <- mean(sub$true)
  e_y1_sel   <- mean(sub$y1)
  e_y2_sel   <- mean(sub$y2)
  pop_mean   <- mean(df$true)

  d <- data.frame(
    label = factor(
      c("Expected true value | selected on extreme y1",
        "Expected observed value on selection | selected on extreme y1",
        "Expected observed value on re-measurement (new noise) | selected on extreme y1"),
      levels = c("Expected true value | selected on extreme y1",
                 "Expected observed value on selection | selected on extreme y1",
                 "Expected observed value on re-measurement (new noise) | selected on extreme y1")
    ),
    value = c(e_true_sel, e_y1_sel, e_y2_sel)
  )

  ggplot(d, aes(label, value)) +
    geom_col(width = 0.70, fill = COL_EXPECTED, alpha = 0.85, color = COL_BG, linewidth = 1.0) +
    geom_hline(yintercept = pop_mean, linetype = "dashed", color = "#2f3e46", linewidth = 1.0, alpha = 0.7) +
    geom_text(aes(label = sprintf("%.2f", value)), vjust = 1, family = "Avenir Next", size = 5, color = "#343A40") +
    annotate(
      "text", x = 2, y = min(d$value) - 0.05 * diff(range(d$value)),
      label = "Apparent change comes from selecting extreme noisy values, then re-measuring.\nNo true values change.",
      hjust = 0.5, vjust = 0, family = "Avenir Next", size = 4.4, color = "#343A40"
    ) +
    labs(
      title = "What changes in expectation (and what does not)",
      x = NULL,
      y = "Expected value"
    ) +
    Plot_theme() +
    theme(axis.text.x = element_text(size = 13))
}

# ============================================================
# =========================  PANEL 4: SPREAD / MOE / CI  ====================
# ============================================================

z_from_level <- function(level) {
  if (abs(level - 0.90) < 1e-9) return(1.645)
  if (abs(level - 0.95) < 1e-9) return(1.96)
  if (abs(level - 0.99) < 1e-9) return(2.576)
  1.96
}

# Relevant x-range: tie to CURRENT sd (not max sd) so it doesn’t waste space.
# Still fixed with respect to sampling noise; it updates as sliders change.
ci_xlims <- function(mu, sd) {
  k <- 3
  c(mu - k * sd, mu + k * sd)
}

# ============================================================
# UI
# ============================================================

ui <- tagList(
  tags$head(
    tags$title("Intuition Lab - Central Limit"),
    tags$style(APP_CSS)
  ),

  page_navbar(
    title = "Intuition Lab",
    theme = bs_theme(
      version = 5,
      bg = "#f7f6f3",
      fg = "#343A40",
      primary = "#2f6f8f",
      secondary = "#495057",
      info = "#2f6f8f",
      code_bg = "#f7f6f3",
      code_fg = "#3182ce",
      base_font = c("Avenir Next", "Verdana", "sans-serif"),
      heading_font = c("Trebuchet MS", "Avenir", "Verdana", "sans-serif"),
      link_color_hover = "#137288",
      link_visited     = "#137288"
    ),
    fillable = TRUE,
    fluid = TRUE,

    # ============================================================
    # Panel — Sample representation (3 rows on the right)
    # ============================================================
    nav_panel(
      "Random, but Representative",
      layout_sidebar(
        sidebar = sidebar(
          title = "Random Sampling & Representation",
          open = "open",
          width = 550,
          p(strong("What it is")),
          p("Representativeness means the sample reflects key features of the population, especially when the population is not homogeneous. Random sampling is unbiased in expectation, but any single draw can still be uneven; stratified sampling is a design that intentionally preserves coverage across groups."),

          p(strong("Why it matters")),
          p("Most real populations have structure—geography, age, risk groups, access—so estimates can be distorted if sampling misses or underweights parts of that structure. This panel trains the habit of asking: ‘Did we sample the population’s shape, or just sample a lot of people?’"),

          p(strong("What this visualization shows")),
          p("A structured population is shown first, then samples are drawn to reveal how representativeness is about capturing structure, not just drawing a lot of points. Compare the population composition to the sample composition, then look at how subgroup outcome rates can shift when some groups are under-sampled—this is the intuition behind why design (like stratification) can matter as much as n."),

          p(strong("Common pitfalls")),
          p("Assuming randomness guarantees representativeness in a single sample, ignoring subgroup coverage, or thinking a global estimate is safe even when Group-level estimates are unstable due to small within-Group n.")

          ),

        layout_columns(
          col_widths = breakpoints(sm = c(12, 12), md = c(5, 7), lg = c(3, 9)),
          fill = TRUE,

          card(
            height = CLT_VH,
            card_body(
              uiOutput("rep_summary"),
              p(class = "small text-muted",
                "Population tiles show groups. Sample tiles are grouped and reversed to match the bar ordering."),
              sliderInput("rep_N", "Population size (N)", min = 1000, max = 5000, value = 2000, step = 1000, width = "100%"),
              sliderInput("rep_k", "Number of groups (k)", min = 2, max = 5, value = 3, step = 1, width = "100%"),
              hr(),
              radioButtons(
                "rep_mode", "Sampling mode",
                choices = c("Simple random sample" = "srs", "Stratified sample" = "stratified"),
                selected = "srs", inline = FALSE
              ),
              sliderInput("rep_n", "Sample size (n)", min = 50, max = 500, value = 200, step = 50, width = "100%"),
              actionButton("rep_draw", "Draw sample", class = "btn-primary w-100"),
              hr(),
              uiOutput("rep_props_ui"),
              uiOutput("rep_rates_ui")
            )
          ),

          card(
            height = CLT_VH,
            full_screen = TRUE,
            fill = TRUE,
            card_body_fill(
              div(
                class = "plot-stack scroll-y",
                # Row 1: big population
                plot_card("rep_pop_plot", flex = 1.3, min_h = 240),
                # Row 2: sample — give it more space; ncol is internally chosen to be very wide
                plot_card("rep_samp_plot", flex = .6, min_h = 200),
                # Row 3: two small cards side-by-side (composition + outcomes)
                card(
                  fill = TRUE,
                  class = "plot-box",
                  style = "flex: 1.0 1 0; min-height: 270px;",
                  card_body_fill(
                    layout_columns(
                      col_widths = c(6, 6),
                      fill = TRUE,
                      plotOutput("rep_comp_plot", height = "100%"),
                      plotOutput("rep_outcome_plot", height = "100%")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    # ============================================================
    # Panel — CLT Visualizer (structure as you provided)
    # ============================================================
    nav_panel(
      "Averages Settle",
      layout_sidebar(
        sidebar = sidebar(
          title = "Central Limit Theorem (CLT)",
          open = "open",
          width = 550,
          p(strong("What it is")),
          p("The central limit theorem describes how averages behave across repeated sampling: even when raw data are irregular or skewed, sample averages tend to cluster around the true center and their distribution becomes approximately bell-shaped once n is large enough. The key is that this is about a statistic across repeated samples, not the raw observations themselves."),

          p(strong("Why it matters")),
          p("In practice, we reason with averages—average risk, average cost, average change—and we attach uncertainty to those averages. CLT intuition explains why those summaries become reliable as information accumulates and why normal-based approximations often work surprisingly well even when the underlying data are not normal."),

          p(strong("What this visualization shows")),
          p("The top row shows two independent samples from the same process, the middle plot pools raw points to reveal the source shape, and the bottom plot pools the averages (or medians) to show how the summary tightens and becomes more bell-shaped with repeated sampling and larger n. The intended ‘aha’ is that the raw data can stay skewed while the *distribution of the summary* becomes regular and concentrated."),

          p(strong("Common pitfalls")),
          p("Treating one sample as the truth, expecting the raw data to look normal, or leaning on rules of thumb like n ≈ 30 without considering skew, tail heaviness, and the specific statistic being used (mean vs median).")
        ),

        layout_columns(
          col_widths = breakpoints(sm = c(12, 12), md = c(5, 7), lg = c(3, 9)),
          fill = TRUE,

          card(
            height = CLT_VH,
            card_body(
              selectInput("clt_dist", "Choose a source distribution",
                          choices = c("Normal", "Uniform", "Exponential", "Lognormal", "Bernoulli", "Poisson"),
                          selected = "Normal", width = "100%"),
              uiOutput("clt_param_ui"),
              sliderInput("clt_n", "Sample size per draw", min = 5, max = 500, value = 30, step = 5, width = "100%"),
              radioButtons("clt_stat", "Statistic to collect (bottom plot)",
                           choices = c("Average" = "mean", "Median" = "median"),
                           selected = "mean", inline = TRUE, width = "100%"),
              hr(),
              div(style = "display:flex; gap:10px; flex-wrap:wrap;",
                  actionButton("clt_draw_once", "Draw once", class = "btn-primary"),
                  actionButton("clt_reset", "Reset collectors", class = "btn-outline-secondary")),
              sliderInput("clt_runs", "Auto-run (number of draws)", min = 25, max = 2000, value = 250, step = 25, width = "100%"),
              actionButton("clt_autorun", "Auto-run", class = "btn-outline-primary", width = "100%")
            )
          ),

          card(
            height = CLT_VH,
            full_screen = TRUE,
            fill = TRUE,
            card_body_fill(
              div(
                class = "plot-stack scroll-y",
                card(
                  fill = TRUE,
                  class = "plot-box",
                  style = "flex: 1.0 1 0; min-height: 280px;",
                  card_body_fill(
                    layout_columns(
                      col_widths = c(6, 6),
                      fill = TRUE,
                      plotOutput("clt_plot_A", height = "100%"),
                      plotOutput("clt_plot_B", height = "100%")
                    )
                  )
                ),
                plot_card("clt_plot_points", flex = 0.95, min_h = 240),
                plot_card("clt_plot_stats",  flex = 0.95, min_h = 240)
              )
            )
          )
        )
      )
    ),

    # ============================================================
    # Panel — Regression to the mean (reverted plot style; sorted)
    # ============================================================
    nav_panel(
      "Back to Earth",
      layout_sidebar(
        sidebar = sidebar(
          title = "Regression To The Mean",
          open = "open",
          width = 550,
          p(strong("What it is")),
          p("Regression to the mean is the statistical tendency for extreme observed measurements to be followed by less extreme measurements when selection is based on noisy observations. The mechanism is selection plus measurement error: you condition on a high observed value, which partially selects on noise, and the noise component does not repeat the same way."),

          p(strong("Why it matters")),
          p("This pattern is routinely mistaken for real improvement or decline in before–after comparisons, interventions, clinical follow-up, and performance tracking. This panel is meant to build the reflex: when you see ‘extremes improved,’ ask whether you selected on an extreme noisy measure and then re-measured."),

          p(strong("What this visualization shows")),
          p("First you select an extreme group based on a noisy first measurement, then you re-measure the same individuals and watch many values move closer to typical levels. The point to focus on is that ‘extreme’ at time 1 often means ‘extreme + lucky noise,’ so when the noise changes at time 2, the group looks less extreme even though the underlying individuals did not change."),

          p(strong("Common pitfalls")),
          p("Attributing the change to a treatment or behavior without a proper comparison, imagining the mean as a force pulling values back, or forgetting that selecting the top (or bottom) group is a form of conditioning that changes expectations even if nothing causal happened.")
        ),

        layout_columns(
          col_widths = breakpoints(sm = c(12, 12), md = c(5, 7), lg = c(3, 9)),
          fill = TRUE,

          card(
            height = CLT_VH,
            card_body(
              sliderInput("rtm_N", "Cohort size (N)", min = 500, max = 8000, value = 2000, step = 250, width = "100%"),
              sliderInput("rtm_mu", "Population true mean", min = -10, max = 10, value = 0, step = 0.5, width = "100%"),
              sliderInput("rtm_sd_true", "True SD (real differences)", min = 2, max = 30, value = 12, step = 0.5, width = "100%"),
              sliderInput("rtm_sd_noise", "Measurement noise SD", min = 1, max = 30, value = 8, step = 0.5, width = "100%"),
              sliderInput("rtm_top", "Select top X% based on observed y1 (y1 = true + noise)", min = 2, max = 25, value = 10, step = 1, width = "100%"),
              actionButton("rtm_run", "Run simulation", class = "btn-primary w-100"),
              hr(),
              uiOutput("rtm_summary")
            )
          ),

          card(
            height = CLT_VH,
            full_screen = TRUE,
            fill = TRUE,
            card_body_fill(
              div(
                class = "plot-stack scroll-y",
                plot_card("rtm_select", flex = 0.9, min_h = 240),
                plot_card("rtm_pairs",  flex = 1.1, min_h = 280),
                plot_card("rtm_expect", flex = 1.0, min_h = 240)
              )
            )
          )
        )
      )
    ),

    # ============================================================
    # Panel — Spread & CI (x-axis relevant; projected p-value label)
    # ============================================================
    nav_panel(
      "Understanding Intervals",
      layout_sidebar(
        sidebar = sidebar(
          title = "Data Spread, Variance, and Deviation",
          open = "open",
          width = 550,
          p(strong("What it is")),
          p("A confidence interval is a data-supported range for the population mean, built from the sample mean and its standard error, which shrinks as sample size increases. SD describes variability among individuals; SE describes variability of the mean across hypothetical repeated samples; the CI is the interpretable object that turns SE into a plausible range."),

          p(strong("Why it matters")),
          p("Confidence intervals shift thinking from ‘is it significant?’ to ‘what values of the mean are consistent with the data and how precise is the estimate?’ This is the practical link between sampling variation and decision-making: the interval tells you what you can reasonably claim about the population mean given the sample."),

          p(strong("What this visualization shows")),
          p("The dot plot shows individual spread, while the confidence interval overlays uncertainty about the population mean, making clear that ‘people vary’ and ‘we are uncertain about the mean’ are different ideas. The comparison line is there to connect the CI to inference: it helps you see whether a hypothesized or reference mean is compatible with the observed data at the chosen confidence level."),


          p(strong("Common pitfalls")),
          p("Interpreting a CI as a range containing most individuals, confusing SD with uncertainty in the mean, or treating the CI as a literal probability statement about the true mean rather than a procedure with long-run coverage properties.")
        ),

        layout_columns(
          col_widths = breakpoints(sm = c(12, 12), md = c(5, 7), lg = c(3, 9)),
          fill = TRUE,

          card(
            height = CLT_VH,
            card_body(
              sliderInput("ci_mu", "Typical mean (e.g., systolic BP)", min = 80, max = 180, value = 120, step = 1, width = "100%"),
              sliderInput("ci_sd", "Individual SD", min = 5, max = 40, value = 15, step = 0.5, width = "100%"),
              sliderInput("ci_n", "Sample size (N)", min = 10, max = 500, value = 100, step = 5, width = "100%"),
              selectInput("ci_level", "Confidence level",
                          choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95),
              sliderInput("ci_comp", "Comparison mean (reference value)", min = 40, max = 220, value = 120, step = 1, width = "100%"),
              actionButton("ci_redraw", "Redraw sample", class = "btn-outline-primary w-100"),
              hr(),
              uiOutput("ci_summary")
            )
          ),

          card(
            height = CLT_VH,
            full_screen = TRUE,
            fill = TRUE,
            card_body_fill(
              div(
                class = "plot-stack scroll-y",
                plot_card("ci_dots",    flex = 1.0, min_h = 260),
                plot_card("ci_density", flex = 1.0, min_h = 260)
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "Acknowledgments",
      card(
        card_body(
          p(strong("Funding and Disclaimer")),
          p(
            "This project is supported by the Assistant Secretary for Technology Policy (ASTP) of the US Department of Health and Human Services (HHS) under grant number 90PH0005/01-13, the Public Health Informatics & Technology Workforce Development Program (the PHIT Workforce Development Program) for $8.1 million."
          ),
          p(
            "This information or content and conclusions are those of the authors and should not be construed as the official position or policy of, nor should any endorsements be inferred by ASTP, HHS or the U.S. Government."
          ),
          hr(),
          p(strong("Design Influence & inspirations")),
          p(
            "This app was developed independently, with design and pedagogical inspiration drawn from ",
            tags$a("Seeing Theory", href = "https://seeing-theory.brown.edu/", target = "_blank"), ", ",
            tags$a("PhET Interactive Simulations", href = "https://phet.colorado.edu/", target = "_blank"), ", and ",
            tags$a("Stapplet", href = "https://stapplet.com/", target = "_blank"), "."
          )

        )
      )
    ),
    nav_spacer(),
    nav_item(
      class = "jb-logo-item",
      tags$a(
        href   = "https://jonathanmbarnes.github.io",
        target = "_blank",
        tags$img(src = "HexlogoJB.png", alt = "Jonathan M. Barnes", class = "jb-logo")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ------------------------------------------------------------
  # CLT
  # ------------------------------------------------------------
  output$clt_param_ui <- renderUI({
    dist <- input$clt_dist

    if (dist == "Normal") {
      tagList(
        sliderInput("clt_mu", "Center (mean)", min = 0, max = 50, value = 25, step = 1, width = "100%"),
        sliderInput("clt_sd", "Spread (sd)",   min = 0.5, max = 25, value = 8, step = 0.5, width = "100%")
      )
    } else if (dist == "Uniform") {
      tagList(
        sliderInput("clt_a", "Minimum", min = 0, max = 49, value = 0, step = 1, width = "100%"),
        sliderInput("clt_b", "Maximum", min = 1, max = 100, value = 50, step = 1, width = "100%")
      )
    } else if (dist == "Exponential") {
      sliderInput("clt_exp_mean", "Typical size (mean)", min = 0.2, max = 30, value = 10, step = 0.2, width = "100%")
    } else if (dist == "Lognormal") {
      tagList(
        sliderInput("clt_ln_median", "Typical value (median)", min = 0.2, max = 30, value = 8, step = 0.2, width = "100%"),
        sliderInput("clt_ln_sigma",  "Skew / spread",          min = 0.1, max = 2.0, value = 0.8, step = 0.05, width = "100%")
      )
    } else if (dist == "Bernoulli") {
      sliderInput("clt_p", "Chance of 1", min = 0.01, max = 0.99, value = 0.30, step = 0.01, width = "100%")
    } else if (dist == "Poisson") {
      sliderInput("clt_lambda", "Average rate (lambda)", min = 0.5, max = 50, value = 6, step = 0.5, width = "100%")
    }
  })

  params <- reactive({
    list(
      mu = input$clt_mu %||% 25,
      sd = input$clt_sd %||% 8,
      a  = input$clt_a  %||% 0,
      b  = input$clt_b  %||% 50,
      exp_mean  = input$clt_exp_mean %||% 10,
      ln_median = input$clt_ln_median %||% 8,
      ln_sigma  = input$clt_ln_sigma  %||% 0.8,
      p = input$clt_p %||% 0.3,
      lambda = input$clt_lambda %||% 6
    )
  })

  clt_spec <- reactive({
    d <- input$clt_dist
    pr <- params()
    if (d == "Uniform" && pr$a >= pr$b) pr$b <- pr$a + 1
    dist_specs(d, pr)
  })

  ref_draw <- reactive({
    spec <- clt_spec()
    spec$gen(10000)
  })

  x_limits <- reactive({
    dist <- input$clt_dist
    pr   <- params()

    pad <- function(lo, hi, p = 0.04) {
      d <- hi - lo
      c(lo - p*d, hi + p*d)
    }

    if (dist %in% c("Exponential", "Lognormal", "Poisson", "Bernoulli")) {
      x <- ref_draw()
      x <- x[is.finite(x) & x >= 0]
      if (length(x) < 50) return(c(0, 1))

      hi <- as.numeric(stats::quantile(x, 0.999, names = FALSE))
      hi <- max(hi, 1)

      if (dist %in% c("Exponential", "Lognormal")) hi <- hi * 1.15
      if (dist == "Poisson")                       hi <- hi + 3

      lim <- pad(0, hi)
      lim[1] <- 0
      return(lim)
    }

    if (dist == "Normal") {
      mu <- pr$mu; sd <- pr$sd
      lo <- mu - 4*sd
      hi <- mu + 4*sd
      return(pad(lo, hi))
    }

    if (dist == "Uniform") {
      lo <- pr$a
      hi <- pr$b
      return(pad(lo, hi))
    }

    c(0, 50)
  })

  rv <- reactiveValues(
    sampA = NULL,
    sampB = NULL,
    pooled_points = numeric(0),
    pooled_stats  = numeric(0)
  )

  draw_once <- function() {
    spec <- clt_spec()
    n <- input$clt_n/2
    stat <- input$clt_stat

    a <- spec$gen(n)
    b <- spec$gen(n)

    rv$sampA <- a
    rv$sampB <- b

    rv$pooled_points <- c(rv$pooled_points, a, b)
    rv$pooled_stats  <- c(rv$pooled_stats, calc_stat(a, stat), calc_stat(b, stat))
  }

  observeEvent(input$clt_draw_once, { draw_once() })

  observeEvent(input$clt_autorun, {
    spec <- clt_spec()
    n <- input$clt_n/2
    stat <- input$clt_stat
    runs <- input$clt_runs

    new_points <- numeric(0)
    new_stats  <- numeric(2 * runs)

    for (i in seq_len(runs)) {
      a <- spec$gen(n)
      b <- spec$gen(n)

      new_points <- c(new_points, a, b)
      new_stats[2*i - 1] <- calc_stat(a, stat)
      new_stats[2*i]     <- calc_stat(b, stat)

      if (i == runs) {
        rv$sampA <- a
        rv$sampB <- b
      }
    }

    rv$pooled_points <- c(rv$pooled_points, new_points)
    rv$pooled_stats  <- c(rv$pooled_stats, new_stats)
  })

  observeEvent(input$clt_reset, {
    rv$sampA <- NULL
    rv$sampB <- NULL
    rv$pooled_points <- numeric(0)
    rv$pooled_stats  <- numeric(0)
  })

  build_expected_df <- function(N_in, binwidth, lim) {
    d <- input$clt_dist
    pr <- params()

    if (d %in% c("Poisson", "Bernoulli")) {
      expected_curve_counts_discrete(d, pr, N_in, lim)
    } else {
      expected_curve_counts_continuous(ref_draw(), N_in, binwidth, lim[1], lim[2])
    }
  }

  sample_plot <- function(x, label) {
    validate(need(!is.null(x), "Click “Draw once” to generate Sample A and Sample B."))

    spec <- clt_spec()
    lim  <- x_limits()

    stat     <- input$clt_stat
    stat_val <- calc_stat(x, stat)

    # FIX: filter using x (your earlier error was caused by referencing 'vals' before it existed)
    vals <- x[is.finite(x) & x >= lim[1] & x <= lim[2]]
    df   <- data.frame(value = vals)

    bins     <- 20
    binwidth <- (lim[2] - lim[1]) / bins
    N_in     <- nrow(df)

    p_base <- ggplot(df, aes(value)) +
      geom_dotplot(
        binaxis    = "x",
        stackdir   = "up",
        method     = "histodot",
        binwidth   = binwidth,
        dotsize    = 0.7,
        stackratio = 1,
        fill  = COL_BARS,
        color = COL_OUTLINE,
        alpha = 0.6
      ) +
      scale_x_continuous(limits = lim) +
      scale_y_continuous(NULL, breaks = NULL) +
      Plot_theme() +
      theme(axis.ticks.y = element_blank(),
            axis.text.y  = element_blank())

    exp_raw <- build_expected_df(N_in, binwidth, lim)

    dot_data <- ggplot_build(p_base)$data[[1]]
    max_stack_y <- suppressWarnings(max(dot_data$y, na.rm = TRUE))
    max_exp_y   <- suppressWarnings(max(exp_raw$y,  na.rm = TRUE))

    exp_df <- if (!is.finite(max_stack_y) || max_stack_y <= 0 || !is.finite(max_exp_y) || max_exp_y <= 0) {
      data.frame(x = numeric(0), y = numeric(0))
    } else {
      transform(exp_raw, y = y / max_exp_y * max_stack_y)
    }

    p_base +
      geom_line(
        data = exp_df, aes(x, y), inherit.aes = FALSE,
        linetype = "dotted", linewidth = 1.6, color = COL_EXPECTED
      ) +
      geom_vline(xintercept = spec$pop_center, linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      geom_vline(xintercept = stat_val, linewidth = 1.0) +
      labs(title = label, x = "Value", y = NULL)
  }

  output$clt_plot_A <- renderPlot({ sample_plot(rv$sampA, "Sample A") }, bg = COL_BG)
  output$clt_plot_B <- renderPlot({ sample_plot(rv$sampB, "Sample B") }, bg = COL_BG)

  output$clt_plot_points <- renderPlot({
    validate(need(length(rv$pooled_points) > 0, "Draw some samples to pool raw points."))

    spec <- clt_spec()
    lim <- x_limits()

    vals <- rv$pooled_points
    vals <- vals[is.finite(vals) & vals >= lim[1] & vals <= lim[2]]
    df <- data.frame(value = vals)

    bins <- 45
    binwidth <- (lim[2] - lim[1]) / bins
    N_in <- nrow(df)

    exp_df <- build_expected_df(N_in, binwidth, lim)

    ggplot(df, aes(value)) +
      geom_histogram(bins = bins, fill = COL_BARS, color = COL_OUTLINE, alpha = 0.6) +
      geom_line(data = exp_df, aes(x, y), inherit.aes = FALSE,
                linetype = "dotted", linewidth = 1.5, color = COL_EXPECTED) +
      geom_vline(xintercept = spec$pop_center, linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      scale_x_continuous(limits = lim) +
      labs(
        title = sprintf("Pooled raw points (in-range points = %d)", N_in),
        subtitle = "Orange dotted = expected shape; blue dashed = typical center.",
        x = "Value", y = "Count"
      ) +
      Plot_theme()
  }, bg = COL_BG)

  output$clt_plot_stats <- renderPlot({
    validate(need(length(rv$pooled_stats) > 0, "Draw some samples to pool the statistic."))

    spec <- clt_spec()
    lim <- x_limits()
    stat <- input$clt_stat

    vals <- rv$pooled_stats
    vals <- vals[is.finite(vals) & vals >= lim[1] & vals <= lim[2]]
    df <- data.frame(stat = vals)

    bins <- 45
    binwidth <- (lim[2] - lim[1]) / bins
    N_in <- nrow(df)

    p <- ggplot(df, aes(stat)) +
      geom_histogram(bins = bins, fill = COL_BARS, color = COL_OUTLINE, alpha = 0.6) +
      geom_vline(xintercept = spec$pop_center, linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      scale_x_continuous(limits = lim) +
      labs(
        title = sprintf("Pooled %s values (in-range total = %d)", tolower(pretty_stat(stat)), N_in),
        subtitle = "Same x-axis as raw data. Tightening reflects reduced uncertainty in the statistic.",
        x = pretty_stat(stat), y = "Count"
      ) +
      Plot_theme()

    if (stat == "mean" && is.finite(spec$pop_sd) && spec$pop_sd > 0 && N_in > 0) {
      se <- spec$pop_sd / sqrt(input$clt_n/2)
      xs <- seq(lim[1], lim[2], length.out = 512)
      ys <- dnorm(xs, mean = spec$pop_center, sd = se) * N_in * binwidth
      ref <- data.frame(x = xs, y = ys)

      p <- p + geom_line(
        data = ref, aes(x, y), inherit.aes = FALSE,
        linetype = "dotted", linewidth = 1.5, color = COL_EXPECTED
      )
    }
    p
  }, bg = COL_BG)

  # ------------------------------------------------------------
  # Sample representation
  # ------------------------------------------------------------
  output$rep_props_ui <- renderUI({
    k <- as.integer(input$rep_k %||% 3)
    tagList(
      h6("Group proportions"),
      lapply(seq_len(k), function(i) {
        sliderInput(
          paste0("rep_prop_", i),
          paste0("Group ", i),
          min = 0.05, max = 0.95,
          value = c(0.40, 0.35, 0.25, 0.20, 0.20)[i],
          step = 0.01, width = "100%"
        )
      })
    )
  })

  output$rep_rates_ui <- renderUI({
    k <- as.integer(input$rep_k %||% 3)
    tagList(
      h6("Within-Group outcome rates"),
      lapply(seq_len(k), function(i) {
        sliderInput(
          paste0("rep_rate_", i),
          paste0("Group ", i),
          min = 0.01, max = 0.60,
          value = c(0.08, 0.15, 0.25, 0.06, 0.30)[i],
          step = 0.01, width = "100%"
        )
      })
    )
  })

  rep_pop <- reactive({
    k <- as.integer(input$rep_k %||% 3)
    props <- vapply(seq_len(k), function(i) input[[paste0("rep_prop_", i)]] %||% (1/k), numeric(1))
    rates <- vapply(seq_len(k), function(i) input[[paste0("rep_rate_", i)]] %||% 0.15, numeric(1))
    build_population(
      N = as.integer(input$rep_N %||% 2000),
      k = k,
      props = props,
      rates = rates,
      seed = FIXED_SEED
    )
  })

  rep_samp <- reactiveVal(NULL)

  observeEvent(input$rep_draw, {
    pop <- rep_pop()
    samp <- draw_sample(
      pop_df = pop,
      n = as.integer(input$rep_n %||% 200),
      mode = input$rep_mode %||% "srs",
      seed = FIXED_SEED
    )
    rep_samp(samp)
  }, ignoreInit = TRUE)

  observeEvent(TRUE, {
    pop <- rep_pop()
    rep_samp(draw_sample(pop, n = as.integer(input$rep_n %||% 200), mode = "srs", seed = FIXED_SEED))
  }, once = TRUE)

  output$rep_summary <- renderUI({
    pop <- rep_pop()
    samp <- rep_samp()
    if (is.null(samp)) return(NULL)

    pop_rate  <- mean(pop$outcome)
    samp_rate <- mean(samp$outcome)

    pop_p <- as.numeric(prop.table(table(pop$stratum)))
    samp_p <- as.numeric(prop.table(table(samp$stratum)))
    L <- max(length(pop_p), length(samp_p))
    pop_p <- c(pop_p, rep(0, L - length(pop_p)))
    samp_p <- c(samp_p, rep(0, L - length(samp_p)))
    comp_gap <- 0.5 * sum(abs(pop_p - samp_p))

    div(
      p(strong(sprintf("Outcome rate: population %.1f%% vs sample %.1f%%", 100 * pop_rate, 100 * samp_rate))),
      p(class = "small text-muted", sprintf("Composition gap (TV distance): %.3f (0 = perfect match).", comp_gap))
    )
  })

  output$rep_pop_plot <- renderPlot({
    pop <- rep_pop()
    plot_tiles(
      pop,
      title = "Population (○ = outcome)",
      subtitle = sprintf("N = %d. Fill = Group.", nrow(pop)),
      show_legend = FALSE
    )
  }, bg = COL_BG)

  output$rep_samp_plot <- renderPlot({
    samp <- rep_samp()
    validate(need(!is.null(samp), "Click “Draw sample”."))
    ncol <- ncols_for_sample(nrow(samp))  # INTERNAL choice (wide layout)
    g <- grouped_sample_layout(samp, n_cols = ncol)
    plot_tiles(
      g,
      title = "Sample (○ = outcome)",
      subtitle = sprintf("N = %d. Fill = Group.", nrow(samp)),
      show_legend = FALSE
    )
  }, bg = COL_BG)

  output$rep_comp_plot <- renderPlot({
    pop <- rep_pop()
    samp <- rep_samp()
    validate(need(!is.null(samp), "Click “Draw sample”."))
    plot_comp_bars(pop, samp)
  }, bg = COL_BG)

  output$rep_outcome_plot <- renderPlot({
    pop <- rep_pop()
    samp <- rep_samp()
    validate(need(!is.null(samp), "Click “Draw sample”."))
    plot_outcome_rates(pop, samp)
  }, bg = COL_BG)

  # ------------------------------------------------------------
  # RTTM
  # ------------------------------------------------------------
  rtm_df <- reactiveVal(NULL)

  observeEvent(input$rtm_run, {
    rtm_df(sim_rtm(
      N = as.integer(input$rtm_N %||% 2000),
      mu = input$rtm_mu %||% 0,
      sd_true = input$rtm_sd_true %||% 12,
      sd_noise = input$rtm_sd_noise %||% 8,
      top_pct = input$rtm_top %||% 10,
      seed = FIXED_SEED
    ))
  }, ignoreInit = TRUE)

  observeEvent(TRUE, {
    rtm_df(sim_rtm(N = 2000, mu = 0, sd_true = 12, sd_noise = 8, top_pct = 10, seed = FIXED_SEED))
  }, once = TRUE)

  output$rtm_summary <- renderUI({
    df <- rtm_df()
    if (is.null(df)) return(NULL)
    sub <- df[df$selected, , drop = FALSE]

    div(
      p(strong(sprintf("Selected group size: %d (%.1f%%)", nrow(sub), 100 * nrow(sub) / nrow(df)))),
      p(class = "small text-muted", sprintf("Mean(true | selected): %.2f", mean(sub$true))),
      p(class = "small text-muted", sprintf("Mean(y1 | selected): %.2f", mean(sub$y1))),
      p(class = "small text-muted", sprintf("Mean(y2 | selected): %.2f", mean(sub$y2)))
    )
  })

  output$rtm_select <- renderPlot({
    df <- rtm_df()
    validate(need(!is.null(df), "Run simulation."))
    plot_rtm_selection(df)
  }, bg = COL_BG)

  output$rtm_pairs <- renderPlot({
    df <- rtm_df()
    validate(need(!is.null(df), "Run simulation."))
    plot_rtm_pairs(df)
  }, bg = COL_BG)

  output$rtm_expect <- renderPlot({
    df <- rtm_df()
    validate(need(!is.null(df), "Run simulation."))
    plot_rtm_expectations(df)
  }, bg = COL_BG)

  # ------------------------------------------------------------
  # Spread & CI
  # ------------------------------------------------------------
  ci_bump <- reactiveVal(0L)
  observeEvent(input$ci_redraw, { ci_bump(ci_bump() + 1L) }, ignoreInit = TRUE)

  ci_sample <- reactive({
    mu <- input$ci_mu %||% 120
    sd <- input$ci_sd %||% 15
    n  <- as.integer(input$ci_n %||% 100)
    lvl <- as.numeric(input$ci_level %||% 0.95)
    z <- z_from_level(lvl)

    set.seed(FIXED_SEED + ci_bump())
    x <- rnorm(n, mean = mu, sd = sd)

    xbar <- mean(x)
    s <- sd(x)
    se <- s / sqrt(n)
    moe <- z * se
    ci <- c(xbar - moe, xbar + moe)

    comp <- input$ci_comp %||% mu
    inside <- (comp >= ci[1] && comp <= ci[2])

    z_comp <- (xbar - comp) / se
    p_comp <- 2 * (1 - pnorm(abs(z_comp)))

    list(x = x, mu = mu, sd = sd, n = n, level = lvl, z = z,
         xbar = xbar, s = s, se = se, moe = moe, ci = ci,
         comp = comp, inside = inside, z_comp = z_comp, p_comp = p_comp)
  })

  output$ci_summary <- renderUI({
    d <- ci_sample()
    msg <- if (d$inside) "Comparison value is inside the CI (not distinguished at this level)." else "Comparison value is outside the CI (distinguished at this level)."
    div(
      p(strong(sprintf("Sample mean = %.2f", d$xbar))),
      p(strong(sprintf("SD ≈ %.2f  |  SE ≈ %.2f", d$s, d$se))),
      p(strong(sprintf("%.0f%% CI for mean: [%.2f, %.2f] (MOE ±%.2f)", 100 * d$level, d$ci[1], d$ci[2], d$moe))),
      p(strong(sprintf("Projected two-sided p-value vs comparison: p ≈ %.3f", d$p_comp))),
      p(class = "small text-muted", msg)
    )
  })

  output$ci_dots <- renderPlot({
    d <- ci_sample()
    xlim <- ci_xlims(d$mu, d$sd)
    df <- data.frame(value = d$x)

    ggplot(df, aes(value)) +
      geom_dotplot(
        binaxis = "x",
        stackdir = "up",
        method = "histodot",
        binwidth = (xlim[2] - xlim[1]) / 40,
        dotsize = 0.60,
        stackratio = 0.9,
        fill = COL_BARS,
        color = COL_OUTLINE,
        alpha = 0.65
      ) +
      geom_vline(xintercept = d$xbar, color = COL_CENTER, linewidth = 1.2) +
      annotate("text", x = d$xbar, y = Inf, label = "sample mean", vjust = 1.4, hjust = -0.05,
               color = COL_CENTER, family = "Avenir Next", size = 4.2) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Individuals (SD)",
        subtitle = "X-range updates with μ and SD so the spread uses the available space.",
        x = NULL, y = NULL
      ) +
      Plot_theme() +
      theme(panel.grid = element_blank(), axis.line = element_blank())
  }, bg = COL_BG)

  output$ci_density <- renderPlot({
    d <- ci_sample()
    xlim <- ci_xlims(d$mu, d$sd)

    xs <- seq(xlim[1], xlim[2], length.out = 700)
    ys <- dnorm(xs, mean = d$mu, sd = d$sd)
    den <- data.frame(x = xs, y = ys)

    shade <- den[den$x >= (d$mu - d$sd) & den$x <= (d$mu + d$sd), , drop = FALSE]
    y_ci <- max(den$y) * 0.12

    ggplot(den, aes(x, y)) +
      geom_line(linewidth = 1.2, color = "#2f3e46", alpha = 0.65) +
      geom_area(data = shade, aes(x, y), fill = COL_EXPECTED, alpha = 0.35) +


      annotate("segment", x = d$ci[1], xend = d$ci[2], y = y_ci, yend = y_ci,
               color = COL_CENTER, linewidth = 5, alpha = 0.9) +
      annotate("point", x = d$ci[1], y = y_ci, size = 3, color = COL_CENTER) +
      annotate("point", x = d$ci[2], y = y_ci, size = 3, color = COL_CENTER) +
      annotate("text", x = (d$ci[1] + d$ci[2]) / 2, y = y_ci, label = "Confidence Interval",
               vjust = -1.2, family = "Avenir Next", size = 4.6, color = COL_CENTER, fontface = "bold") +


      geom_vline(xintercept = d$mu, color = "#2f3e46", linetype = "dashed", linewidth = .8, alpha = 0.6) +
      annotate("text", x = d$mu, y = Inf, label = "μ", vjust = 1.4, hjust = -0.05,
               color = "#2f3e46", family = "Avenir Next", size = 5) +

      geom_vline(xintercept = d$comp, color = if (d$inside) "#2f3e46" else COL_EXPECTED,
                 linewidth = .9, alpha = 0.6) +
      annotate("text", x = d$comp, y = max(den$y) * 0.75,
               label = sprintf("comparison\np ≈ %.3f", d$p_comp),
               hjust = -0.05, vjust = 0.5,
               family = "Avenir Next", size = 5,
               color = if (d$inside) "#2f3e46" else COL_CENTER) +




      scale_x_continuous(limits = xlim) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Expected values N(μ, SD) + CI for the mean",
        subtitle = "Orange shading: within ±1 SD. Blue bar: CI for the mean. Comparison line labeled with projected p-value.",
        x = NULL, y = NULL
      ) +
      Plot_theme() +
      theme(panel.grid = element_blank(), axis.line = element_blank())
  }, bg = COL_BG)
}

shinyApp(ui, server)
