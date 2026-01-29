# app.R — CLT / LLN Intuition Simulator (single-file; Shiny + bslib v5)
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
  #library(tidyverse)
}



CLT_VH <- "calc(100vh - 120px)"
`%||%` <- function(a, b) if (is.null(a)) b else a

# =========================
# Color system (edit here)
# =========================
COL_BG       <- "#f7f6f3"
COL_BARS     <- "#4A4E69"   # dot/hist fill color
COL_OUTLINE  <- COL_BG      # minimal outlines; set darker if desired
COL_EXPECTED <- "#E69F00"   # expected distribution curve (orange)
COL_CENTER   <- "#2f6f8f"   # center vertical line (cyan-leaning blue; high contrast vs COL_BARS)

# =========================
# Plot theme
# =========================
Plot_theme <- function(
    base_font    = c("Avenir Next", "Verdana", "sans-serif"),
    heading_font = c("Trebuchet MS", "Avenir", "Verdana", "sans-serif"),
    base_size = 14
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
    plot.subtitle = element_text(family = base_family, size = 12, color = "#3b4a55"),

    plot.margin = margin(t = 5, r = 6, b = 4, l = 6, unit = "pt")
  )
}

# =========================
# Distributions
# =========================
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
    mu <- log(median) # median = exp(mu)
    gen <- function(n) rlnorm(n, meanlog = mu, sdlog = sigma)
    pop_center <- exp(mu + 0.5 * sigma^2) # mean for a consistent "center" anchor
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

# =========================
# Helpers: dot stacking + expected curve in counts
# =========================

# Compute stacked dot positions so y-axis truly represents Count
stack_dots <- function(values, lim, bins) {
  bw <- (lim[2] - lim[1]) / bins
  if (!is.finite(bw) || bw <= 0) return(data.frame(x = numeric(0), y = integer(0)))

  # Keep only finite values in-range
  v <- values[is.finite(values) & values >= lim[1] & values <= lim[2]]
  if (!length(v)) return(data.frame(x = numeric(0), y = integer(0)))

  # Integer bin id: 0...(bins-1)
  bin <- floor((v - lim[1]) / bw)
  bin <- pmin(pmax(bin, 0), bins - 1)

  # Bin center x-position
  x_center <- lim[1] + (bin + 0.5) * bw

  d <- data.frame(bin = bin, x = x_center)
  d <- d[order(d$bin), , drop = FALSE]

  # Stack within each bin (start at 0 so bottom dot sits on baseline)
  d$y <- ave(d$bin, d$bin, FUN = function(z) seq_along(z) - 1)

  d[, c("x", "y")]
}

# Expected curve scaled to counts for continuous-ish distributions using a reference draw
expected_curve_counts_continuous <- function(ref, N, binwidth, from, to) {
  ref <- ref[is.finite(ref)]
  ref <- ref[ref >= 0]

  # If nothing to estimate from, bail safely
  if (length(ref) < 50 || !is.finite(N) || N <= 0 || !is.finite(binwidth) || binwidth <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  # density always returns n points, but guard anyway
  d <- density(ref, from = from, to = to, n = 512)
  y <- d$y * N * binwidth

  # If the curve is effectively zero everywhere, return empty (prevents invisible lines)
  if (!any(is.finite(y)) || max(y, na.rm = TRUE) <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  data.frame(x = d$x, y = y)
}

# Expected curve for discrete distributions (Poisson / Bernoulli) scaled to counts
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

# =========================
# UI
# =========================
ui <- page_navbar(
  title = "Intuition Lab",
  theme = bs_theme(
    version = 5,
    bg        = COL_BG,
    fg        = "#343A40",
    primary   = "#2f6f8f",
    secondary = "#495057",
    info      = "#2f6f8f"
  ),

  nav_panel(
    "CLT Visualizer",
    layout_sidebar(
      sidebar = sidebar(
        title = "Central Limit Theorem (CLT)",
        open = "open",
        width = 520,

        p(strong("What this visualization shows")),
        p("The top row shows two independent samples drawn from the same process. The middle plot pools all raw points to reveal the underlying shape. The bottom plot pools the averages (or medians) from each sample—this is the CLT view, showing how the summary tightens and becomes more bell-shaped as sample size grows."),

        p(strong("What it is")),
        p("The central limit theorem describes how averages behave across repeated sampling. Even when the raw data are skewed or irregular, the distribution of sample averages tends to become approximately bell-shaped when the sample size is large enough, and those averages cluster around the true center."),

        p(strong("Why it matters")),
        p("In practice, people summarize data using averages: average risk, average cost, average change, and average differences between groups. The CLT explains why these averages become reliable as data accumulate and why uncertainty around them often follows predictable patterns, even when the raw data are not bell-shaped."),

        p(strong("Common pitfalls")),
        p("Small samples can look unusual by chance, so one sample should not be treated as the full truth. The CLT is about averages (or other summaries), not raw values becoming bell-shaped. A common guideline is that n ≈ 30 is often 'enough,' but it is not definitive—the required sample size depends on skew, variability, and tail heaviness.")
      ),

      layout_columns(
        col_widths = c(4, 8),

        # Controls
        card(
          height = CLT_VH,
          card_header("Controls"),
          card_body(
            selectInput(
              "clt_dist",
              "Choose a source distribution",
              choices = c("Normal", "Uniform", "Exponential", "Lognormal", "Bernoulli", "Poisson"),
              selected = "Normal"
            ),
            uiOutput("clt_param_ui"),

            sliderInput("clt_n", "Sample size per draw", min = 5, max = 500, value = 30, step = 5),

            radioButtons(
              "clt_stat",
              "Statistic to collect (bottom plot)",
              choices = c("Average" = "mean", "Median" = "median"),
              selected = "mean",
              inline = TRUE
            ),

            hr(),

            div(
              style = "display:flex; gap:10px; flex-wrap:wrap;",
              actionButton("clt_draw_once", "Draw once", class = "btn-primary"),
              actionButton("clt_reset", "Reset collectors", class = "btn-outline-secondary")
            ),

            br(),

            sliderInput("clt_runs", "Auto-run (number of draws)", min = 10, max = 2000, value = 200, step = 25),
            actionButton("clt_autorun", "Auto-run", class = "btn-outline-primary"),

            p(strong("Tip")),
            p("Try Exponential or Lognormal, then increase sample size and watch the bottom plot tighten even though the raw data stay skewed.")
          )
        ),

        # Plots
        card(
          height = CLT_VH,
          full_screen = TRUE,
          card_header("Plots"),
          card_body(
            card(
              card_header("Two independent samples (A and B)"),
              card_body(
                layout_columns(
                  col_widths = c(6, 6),
                  plotOutput("clt_plot_A", height = "280px"),
                  plotOutput("clt_plot_B", height = "280px")
                )
              )
            ),

            card(
              card_header("Collector 1: all raw sample points pooled"),
              card_body(plotOutput("clt_plot_points", height = "240px"))
            ),

            card(
              full_screen = TRUE,
              card_header("Collector 2: distribution of sample averages (CLT view)"),
              card_body(plotOutput("clt_plot_stats", height = "240px"))
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
        p("This project is supported by the Assistant Secretary for Technology Policy (ASTP) of the US Department of Health and Human Services (HHS) under grant number 90PH0005/01-13."),
        p("This information or content and conclusions are those of the authors and should not be construed as the official position or policy of, nor should any endorsements be inferred by ASTP, HHS or the U.S. Government.")
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {

  output$clt_param_ui <- renderUI({
    dist <- input$clt_dist

    if (dist == "Normal") {
      tagList(
        sliderInput("clt_mu", "Center (mean)", min = 0, max = 50, value = 25, step = 1),
        sliderInput("clt_sd", "Spread (sd)",   min = 0.5, max = 25, value = 8, step = 0.5)
      )
    } else if (dist == "Uniform") {
      tagList(
        sliderInput("clt_a", "Minimum", min = 0, max = 49, value = 0, step = 1),
        sliderInput("clt_b", "Maximum", min = 1, max = 100, value = 50, step = 1)
      )
    } else if (dist == "Exponential") {
      sliderInput("clt_exp_mean", "Typical size (mean)", min = 0.2, max = 30, value = 10, step = 0.2)
    } else if (dist == "Lognormal") {
      tagList(
        sliderInput("clt_ln_median", "Typical value (median)", min = 0.2, max = 30, value = 8, step = 0.2),
        sliderInput("clt_ln_sigma",  "Skew / spread",          min = 0.1, max = 2.0, value = 0.8, step = 0.05)
      )
    } else if (dist == "Bernoulli") {
      sliderInput("clt_p", "Chance of 1", min = 0.01, max = 0.99, value = 0.30, step = 0.01)
    } else if (dist == "Poisson") {
      sliderInput("clt_lambda", "Average rate (lambda)", min = 0.5, max = 50, value = 6, step = 0.5)
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

  # Reference draw for expected curves (kept modest for ShinyLive performance)
  ref_draw <- reactive({
    spec <- clt_spec()
    spec$gen(20000)
  })

  x_limits <- reactive({
    x <- ref_draw()
    x <- x[is.finite(x)]
    x <- x[x >= 0]

    base <- as.numeric(quantile(x, probs = 0.999, names = FALSE))

    dist <- input$clt_dist

    # distribution-specific widening
    if (dist == "Lognormal" || dist == "Exponential") {
      xmax <- base * 2.5
    } else if (dist == "Poisson") {
      xmax <- base + 10
    } else {
      xmax <- base * 1.6
    }

    xmax <- max(xmax, 50)
    c(0, xmax)
  })


  rv <- reactiveValues(
    sampA = NULL,
    sampB = NULL,
    pooled_points = numeric(0),
    pooled_stats  = numeric(0)
  )

  draw_once <- function() {
    spec <- clt_spec()
    n <- input$clt_n
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
    n <- input$clt_n
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

  # ---------- Plot builders ----------

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

    vals <- x[is.finite(x) & x >= lim[1] & x <= lim[2]]
    df   <- data.frame(value = vals)

    bins     <- 20
    binwidth <- (lim[2] - lim[1]) / bins
    N_in     <- nrow(df)
    if (N_in < 2) exp_df <- data.frame(x=numeric(0), y=numeric(0))


    # Base dot plot
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
        alpha = 0.8
      ) +
      scale_x_continuous(limits = lim) +
      scale_y_continuous(NULL, breaks = NULL) +
      Plot_theme() +
      theme(axis.ticks.y = element_blank(),
            axis.text.y  = element_blank())

    # If no points in range, just return the dotplot + lines
    if (N_in <= 0) {
      return(
        p_base +
          geom_vline(xintercept = spec$pop_center, linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
          geom_vline(xintercept = stat_val, linewidth = 1.0) +
          labs(
            title = label,
            subtitle = "Orange dotted = expected shape; blue dashed = typical center; solid = this sample’s statistic",
            x = "Value", y = NULL
          )
      )
    }

    # Expected curve in COUNT units (this is the one you already trust)
    exp_raw <- build_expected_df(N_in, binwidth, lim)

    # Rescale expected curve to dotplot's internal y-units (stack height)
    dot_data <- ggplot_build(p_base)$data[[1]]
    max_stack_y <- suppressWarnings(max(dot_data$y, na.rm = TRUE))
    max_exp_y   <- suppressWarnings(max(exp_raw$y,  na.rm = TRUE))

    if (!is.finite(max_stack_y) || max_stack_y <= 0 || !is.finite(max_exp_y) || max_exp_y <= 0) {
      exp_df <- data.frame(x = numeric(0), y = numeric(0))
    } else {
      exp_df <- transform(exp_raw, y = y / max_exp_y * max_stack_y)
    }

    p_base +
      geom_line(
        data = exp_df, aes(x, y), inherit.aes = FALSE,
        linetype = "dotted", linewidth = 1.6, color = COL_EXPECTED
      ) +
      geom_vline(xintercept = spec$pop_center, linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      geom_vline(xintercept = stat_val, linewidth = 1.0) +
      labs(
        title = label,
        x = "Value", y = NULL
      )
  }

  output$clt_plot_A <- renderPlot({ sample_plot(rv$sampA, "Sample A") })
  output$clt_plot_B <- renderPlot({ sample_plot(rv$sampB, "Sample B") })

  output$clt_plot_points <- renderPlot({
    validate(need(length(rv$pooled_points) > 0, "Draw some samples to pool raw points."))

    spec <- clt_spec()
    lim <- x_limits()

    # Filter to plotted range BEFORE scaling expected curve
    vals <- rv$pooled_points
    vals <- vals[is.finite(vals) & vals >= lim[1] & vals <= lim[2]]
    df <- data.frame(value = vals)

    bins <- 45
    binwidth <- (lim[2] - lim[1]) / bins
    N_in <- nrow(df)

    exp_df <- build_expected_df(N_in, binwidth, lim)

    ggplot(df, aes(value)) +
      geom_histogram(
        bins = bins,
        fill = COL_BARS,
        color = COL_OUTLINE,
        alpha = 0.6
      ) +
      geom_line(data = exp_df, aes(x, y), inherit.aes = FALSE,
                linetype = "dotted", linewidth = 1.5, color = COL_EXPECTED) +
      geom_vline(xintercept = spec$pop_center,
                 linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      scale_x_continuous(limits = lim) +
      labs(
        title = sprintf("Pooled raw points (in-range points = %d)", N_in),
        subtitle = "Orange dotted = expected shape; blue dashed = typical center; solid = this sample’s statistic",
        x = "Value", y = "Count"
      ) +
      Plot_theme()
  })

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
      geom_histogram(
        bins = bins,
        fill = COL_BARS,
        color = COL_OUTLINE,
        alpha = 0.6
      ) +
      geom_vline(xintercept = spec$pop_center,
                 linetype = "dashed", linewidth = 1.5, color = COL_CENTER) +
      scale_x_continuous(limits = lim) +
      labs(
        title = sprintf("Pooled %s values (in-range total = %d)", tolower(pretty_stat(stat)), N_in),
        subtitle = "Same x-axis as raw data. The distribution tightens around the typical center as sample size increases.",
        x = pretty_stat(stat), y = "Count"
      ) +
      Plot_theme()

    # CLT reference curve overlay for mean only (scaled to counts)
    if (stat == "mean" && is.finite(spec$pop_sd) && spec$pop_sd > 0 && N_in > 0) {
      se <- spec$pop_sd / sqrt(input$clt_n)
      xs <- seq(lim[1], lim[2], length.out = 512)
      ys <- dnorm(xs, mean = spec$pop_center, sd = se) * N_in * binwidth
      ref <- data.frame(x = xs, y = ys)

      p <- p + geom_line(
        data = ref, aes(x, y), inherit.aes = FALSE,
        linetype = "dotted", linewidth = 1.5, color = COL_EXPECTED
      )
    }

    p
  })
}

shinyApp(ui, server)
