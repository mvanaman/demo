# Function that tells you useful functions
#use.functions <-
tibble(
  "Task" = c("Wrangling",
             "Exploring",
             "T-test (unpaired",
             "Regression",
             "Fisher's Exact (use for chi-squared)",
             "Correlation"),
  "Function" = c("row_to_names",
                 "skim",
                 "my.t",
                 "multivariate.regressions",
                 "my.fisher",
                 "apa.cor.table"),
  "Package" = c("janitor",
                "skimr",
                "None",
                "None",
                "None",
                "apaTables"),
  "What Does" = c("Makes any row into colnames",
                  "Descriptives statistics",
                  "Tidy t-test with cohen's d",
                  "Tidy regressions when > 1 DV",
                  "Tidy Exact test (returns odds ratio when < 3 vars)",
                  "APA-style correlation matrix with M and SD")
)
# Organizaing and Presenting Data
# Statistical Analysis ----
### Runs regressions on list of DVs given list of IVs ----
multivariate.regressions <- function(data, IVs, DVs, ...) {
  IVs <- paste("~", paste(IVs, sep = "", collapse = " + "))
  models <- list()
  for (i in DVs) {
    models[[i]] <-
        lm(paste(i[[1]], IVs), data = data) %>%
      tidy(...)
  }
  return(models)
}
## conducts t-test, returns bunch of stuff in tidy format, long format optional
my.t <-
    function(data, IV, DV, long = FALSE, ...) {
      require(dplyr)
      IV.1 <- enquo(IV)
      DV.1 <- enquo(DV)
      means <- suppressMessages(
        data %>%
        group_by(!!IV.1) %>%
        summarise(mean = mean(!!DV.1, na.rm = TRUE)) %>%
          pivot_wider(names_from = !!IV.1, values_from =  "mean")
        )
      names(means) <- paste(names(means), "mean", sep = ".")
      SDs <- suppressMessages(
        data %>%
          group_by(!!IV.1) %>%
          summarise(sd = sd(!!DV.1, na.rm = TRUE)) %>%
          pivot_wider(names_from = !!IV.1, values_from =  "sd")
      )
      names(SDs) <- paste(names(SDs), "sd", sep = ".")

      IV.2 <- rlang::sym(rlang::as_label(rlang::enquo(IV)))
      DV.2 <- rlang::sym(rlang::as_label(rlang::enquo(DV)))
      form <- expr(!! DV.2 ~ !! IV.2)
      t.tests <- t.test(eval(form), data, var.equal = FALSE)
      stats <- t.tests %>% broom::glance(x)
      std.err <- t.tests$stderr
      Ds <-
        cohen.d(x = data %>% select(!!DV.1, !!IV.1),
                group = deparse(substitute(IV)))$cohen.d %>%
        as.data.frame %>%
        select(effect) %>%
        round(2) %>%
        format(nsmall = 2)
      cols <- c("conf.low", "conf.high", "statistic", "parameter", "p.value")
      means.table <-
        cbind(means, SDs, stats[, "estimate"], std.err, stats[, cols], Ds)
      means.table <- means.table %>%
        rename(mean.difference = estimate, cohens.d = effect, t = statistic, df = parameter)
      rownames(means.table) <- NULL
      if (long == TRUE) {
        means.table.long <-
          means.table %>% t %>% data.frame("Value" = .) %>% rownames_to_column("Statistic")
        return(means.table.long)
      } else{
        return(means.table)
      }
    }
## fisher's exact tests, returns pretty table and tidy stats (OR if both variables are binary)
my.fisher <-
    function(data, ...) {
      require(janitor)
      require(dplyr)
      require(broom)
      my.tab <- tabyl(data,
                      ...,
              show_na = FALSE,
              show_missing_levels = FALSE)
      my.tab.pretty <-
        my.tab %>%
        adorn_totals(where = c("row", "col")) %>%
        adorn_percentages(denominator = "all") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns %>%
        adorn_title()
      fish <- tabyl(data,
                    ...,
                    show_na = FALSE,
                    show_missing_levels = FALSE) %>%
        fisher.test() %>%
        broom::glance()
      if ("estimate" %in% names(fish)) {
        fish <- fish %>% rename(odds.ratio = estimate)
      }
      return(list(my.tab.pretty = my.tab.pretty, fish = fish))
    }
# Reporting Statistical Models ----
##  returns tidy table of fixed effects from mixed effects models ----
fixed.effects <- function(mod, ci.method = "Wald", ...) {
  require(broom.mixed)
  require(tidyverse)
  require(reghelper)
  # fixed effects table
  fixed.results <-
    as.data.frame(summary(mod)$coefficients) %>% rownames_to_column("Predictor")
  CIs <- mod %>%
    confint.merMod(method = ci.method, ...) %>%
    as.data.frame %>%
    rownames_to_column("Predictor")
  fixed.results <- left_join(fixed.results, CIs) %>%
    rename(p = `Pr(>|t|)`) %>%
    mutate(OR = format(round(exp(fixed.results$Estimate), 2), nsmall = 2),
           p = pvalue(p)) %>%
    mutate(across(where(is.numeric), round, 2))
  return(fixed.results)
}
## Make mediation latex table ----
mediation.table <- function(table, caption, ...) {
  addtorow <- list()
  addtorow$pos <- list(-1, 0, nrow(table), nrow(table))
  addtorow$command <- c(
    "\\toprule \\textit{Dependent Variable:}  &  \\multicolumn{2}{c}{\\underline{Policy Support}} &  \\multicolumn{2}{c}{\\underline{Wrongness}} \\\\",

    "Effect  & \\multicolumn{1}{c}{\\textit{b}} &  \\multicolumn{1}{c}{$SE$}   &
  \\multicolumn{1}{c}{\\textit{b}} &  \\multicolumn{1}{c}{$SE$}   \\\\",

    '\\bottomrule \\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Note.} $SE$ = Standard Error, calculated with ordinary bootstrap with 10,000 replications.}}
   \\\\',

    "\\multicolumn{5}{l} {\\parbox[t]{10cm}{$^{\\text{***}} p < 0.001; \\ ^{\\text{**}} p < 0.01; \\ ^{text{*}} p < 0.05 $ (two-tailed)}}
  \\\\"
  )

  X.Table <- xtable(table, caption = caption, ...)

  xtable::align(X.Table) <- "lllclclc"

  print(
    X.Table,
    include.rownames = FALSE,
    include.colnames = FALSE,
    add.to.row = addtorow,
    caption.placement = "top",
    latex.environments = "flushleft",
    floating = TRUE,
    table.placement = "H",
    escape = FALSE,
    hline.after = c(0),
    booktabs = TRUE,
    sanitize.text.function = identity,
    sanitize.colnames.function = function(x) {
      x
    }
  )
}
## gets paths coefficients for plotting mediation figs ----
get.path <- function(table) {
  table %>%
    t %>%
    as.data.frame %>%
    header.true %>%
    slice(1) %>%
    rename(indirect = ab, direct = cp) %>%
    mutate(a = as.character(a),
           b = as.character(b),
           indirect = as.character(indirect),
           direct = as.character(direct),
           total = as.character(total))
}
## for parallel mediation ----
get.path.parallel <- function(table) {
  require(tidyverse)
  table %>%
    t %>%
    as.data.frame %>%
    header.true %>%
    slice(1) %>%
    rename(indirect = ab, direct = cp) %>%
    mutate(a = as.character(a),
           b = as.character(b),
           indirect = as.character(indirect),
           direct = as.character(direct),
           total = as.character(total))
}
### CFI function for EFA ----
fa.CFI<-function(x){
  nombre<-paste(x,"CFI",sep = ".")
  nombre<-
    ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  return(nombre)
}

### Get loadings from EFA ----
### Latex Tables for Loadings from EFA
loadings.table <-
  function(solution, cutoff, ...){
    require(dplyr)
    require(xtable)
    get.loadings <-
      function(x) {
        if(ncol(x$loadings) == 1){
          x$loadings[, 1:ncol(x$loadings)] %>%
            data.frame(Loading = .) %>%
            rownames_to_column("Item") %>%
            mutate_all(function(x)
              ifelse(x >= abs(0.5), x, NA)) %>%
            rename(Factor = 2)
        } else {
          x$loadings[, 1:ncol(x$loadings)] %>%
            data.frame() %>%
            rownames_to_column("Item") %>%
            mutate(across(where(is.numeric), round, 3)) %>%
            mutate_all(function(x)
              ifelse(x >= abs(0.5), x, "")) %>%
            select(Item, Factor = names(select(., -Item)))
        }
      }
    loadings <- get.loadings(solution)
    # Latex Table
    if (ncol(loadings) > 2){
      addtorow <- list()
      addtorow$pos <- list(0,-1, nrow(loadings))
      addtorow$command <- c(
        paste(
          "\\hline \n",
          "\\endhead \n",
          "{\\footnotesize Continued on next page} \n",
          "\\endfoot \n",
          "\\endlastfoot \n",
          sep = ""
        ),
        paste(c("\\toprule  Item & ", paste("Factor", 1:c(ncol(loadings)-2), "&"),
                paste("Factor", ncol(loadings)-1), " \\\\"), collapse = " "),
        paste(c("\\midrule",  rep("&", ncol(loadings)-1), "\\\\"), collapse = " ")
      )
    } else {
      addtorow <- list()
      addtorow$pos <- list(0,-1, nrow(loadings))
      addtorow$command <- c(
        paste(
          "\\hline \n",
          "\\endhead \n",
          "{\\footnotesize Continued on next page} \n",
          "\\endfoot \n",
          "\\endlastfoot \n",
          sep = ""
        ),
        "\\toprule  Item & Loading \\\\",
        "\\midrule  &  \\\\"
      )
    }
    latex.table <- xtable(loadings,
           caption = paste("Loadings: ", ncol(loadings)-1, "Factors", sep = " "),
           align = paste(c("ll", rep("c", c(ncol(loadings)-1))), collapse = ""),
           ...)
    latex.table <- print.xtable(
        ...,
        latex.table,
        comment = FALSE,
        print.results = FALSE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row = addtorow,
        caption.placement = "top",
        latex.environments = "flushleft",
        booktabs = TRUE,
        floating = FALSE,
        hline.after = NULL,
        table.placement = "!ht",
        tabular.environment = "longtable",
        sanitize.colnames.function = function(x) {x})
    note <-
      c("Note: wrap in cat when rendering latex table in R Markdown: cat(x$latex.table). And add chunk option: results='TRUE'")
    tables <- list(loadings = loadings, latex.table = latex.table, note = note)
    return(tables)
  }

# Plots
### Raincloud Plot Function ----
raincloud <-
  # Define the arguments of the function
  function(df, xvar, yvar, ...) {
    # Flat violin function
    "%||%" <- function(a, b) {
      if (!is.null(a)) a else b
    }
    geom_flat_violin <-
      function(
        mapping = NULL,
        data = NULL,
        stat = "ydensity",
        position = "dodge",
        trim = TRUE,
        scale = "area",
        show.legend = NA,
        inherit.aes = TRUE,
        ...) {
      layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomFlatViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          trim = trim,
          scale = scale,
          ...))
    }
    GeomFlatViolin <-
      ggproto("GeomFlatViolin", Geom,
        setup_data = function(data, params) {
          data$width <- data$width %||%
            params$width %||% (resolution(data$x, FALSE) * 0.9)
          # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
          data %>%
            group_by(group) %>%
            mutate(
              ymin = min(y),
              ymax = max(y),
              xmin = x,
              xmax = x + width / 2)
        },
        draw_group = function(data, panel_scales, coord) {
          # Find the points for the line to go all the way around
          data <- transform(data,
            xminv = x,
            xmaxv = x + violinwidth * (xmax - x))
          # Make sure it's sorted properly to draw the outline
          newdata <- rbind(
            plyr::arrange(transform(data, x = xminv), y),
            plyr::arrange(transform(data, x = xmaxv), -y))
          # Close the polygon: set first and last point the same
          # Needed for coord_polar and such
          newdata <- rbind(newdata, newdata[1, ])
          ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
        },
        draw_key = draw_key_polygon,
        default_aes = aes(
          weight = 1, colour = "grey20", fill = "white", size = 0.5,
          alpha = NA, linetype = "solid"),
        required_aes = c("x", "y"))
    # define a theme to make the plots look real nice
    raincloud_theme <- theme(
      text = element_text(size = 10),
      axis.title.x = element_text(size = 16, vjust = -.25, family = "Times New Roman"),
      axis.title.y = element_text(size = 16, family = "Times New Roman"),
      axis.text = element_text(size = 14, family = "Times New Roman"),
      axis.text.x = element_text(size = 14, angle = 0, vjust = 0.5, family = "Times New Roman"),
      legend.title = element_text(size = 12, family = "Times New Roman"),
      legend.text = element_text(size = 12, family = "Times New Roman"),
      legend.position = "bottom",
      plot.title = element_text(
        lineheight = .8,
        face = "bold",
        size = 16),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"))
    # Make plot
    return.plot <- ggplot(df,
        aes_string(y = deparse(substitute(yvar)),
                   x = deparse(substitute(xvar)), ...)) +
      geom_flat_violin( # for density
        position = position_nudge(x = .2, y = 0),
        alpha = .5,
        show.legend = TRUE) +
      geom_point(
        aes_string(...),
        position = position_jitter(width = .15),
        size = .50,
        alpha = 0.3,
        stroke = 0,
        shape = 16,
        show.legend = FALSE
      ) +
      geom_boxplot(
        aes_string(...),
        width = .1,
        outlier.shape = NA,
        alpha = 0.25,
        show.legend = FALSE
      ) +
      coord_flip() + # to put boxplot and dots underneath (otherwise are to the side of density)
      theme_bw() +
      raincloud_theme # add the theme we made earlier
    return.plot
  }

# Functions for Plotting ----
## Univariate ----
get.histograms.uni <-
  function(data, x) {
    ggplot(data, aes_string(x = x)) +
      geom_histogram() +
      labs(x = x) +
      theme_classic() +
      theme(text = element_text(family = "Times"),
            legend.position = "none",
            plot.title = element_text(size = 11))
  }
get.densities.uni <-
  function(data, x) {
    ggplot(data, aes_string(x = x)) +
      geom_density() +
      labs(x = x) +
      theme_classic() +
      theme(text = element_text(family = "Times"),
            legend.position = "none",
            plot.title = element_text(size = 11))
  }
get.bar.uni <-
  function(data, x) {
    ggplot(data, aes_string(x = x)) +
      geom_bar(stat = "count") +
      labs(x = x) +
      theme_classic() +
      theme(text = element_text(family = "Times"),
            legend.position = "none",
            plot.title = element_text(size = 11))
  }
get.violin.uni <-
  function(data, x) {
    ggplot(data, aes(x = x, y = !!sym(x))) +
      geom_violin(width = 1.4) +
      geom_boxplot(width = 0.1,
                   color = "grey",
                   alpha = 0.2) +
      labs(x = NULL) +
      theme_classic() +
      theme(text = element_text(family = "Times"),
            legend.position = "none",
            plot.title = element_text(size = 11))
  }
## Multivariate ----
get.histograms.multi <-
  function(data, x, fill) {
    if(!(is.factor(data[, fill]))) {
      data[, fill] <- as_factor(data[, fill])
    }
    ggplot(data, aes_string(x = x, fill = fill)) +
      geom_histogram(color = "#e9ecef",
                     alpha = 0.6,
                     position = 'identity') +
      labs(fill = "") +
      theme_classic()
  }
get.densities.multi <-
  function(data, x, fill) {
    if(!(is.factor(data[, fill]))) {
      data[, fill] <- as_factor(data[, fill])
    }
    ggplot(data, aes_string(x = x, fill = fill)) +
      geom_density(
        color = "#e9ecef",
        alpha = 0.6,
        position = 'identity') +
      theme_classic()
  }
get.bar.multi <-
  function(data, x, y) {
    if(!(is.factor(data[, y]))) {
      data[, x] <- as_factor(data[, x])
    }
    ggplot(data, aes_string(x = x, y = y, fill = x)) +
      stat_summary(fun = mean, geom = "bar", show.legend = FALSE) +
      stat_summary(fun.data = mean_cl_normal,
                   geom = "errorbar",
                   fun.args = list(mult = 1)) +
      theme_classic()
  }
get.violin.multi <-
  function(data, x, y) {
    if(!(is.factor(data[, y]))) {
      data[, x] <- as_factor(data[, x])
    }
    ggplot(data, aes_string(x = x, y = y, fill = x)) +
      geom_violin(width = 1.4, show.legend = FALSE) +
      geom_boxplot(width = 0.1, alpha = 0.2, show.legend = FALSE) +
      theme_classic()
  }
get.ridges <-
  function(data, x, y) {
    if(!(is.factor(data[, y]))) {
      data[, y] <- as_factor(data[, y])
    }
    ggplot(data, aes_string(x = x, y = y, fill = y)) +
      ggridges::geom_density_ridges() +
      theme_classic() +
      theme(text = element_text(family = "Times"),
            legend.position = "none",
            plot.title = element_text(size = 11))
  }


# Functions for defining what goes on a panel in correlogram  ----
## Correlation with asterisks
cor_fun <-
  function(data,
           mapping,
           method = "pearson",
           ndp = 2,
           sz = 5,
           stars = TRUE,
           label,
           ...) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    corr <-
      cor.test(x, y, method = method, use = "pairwise.complete.obs")
    est <- corr$estimate
    # lb.size <- sz* abs(est)

    if (stars) {
      stars <-
        c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
      lbl <-
        ifelse(
          missing(label),
          paste0(format(round(est, ndp), nsmall = 2), stars),
          paste0(label, format(round(est, ndp), nsmall = 2), stars, sep = " ")
        )
    } else{
      lbl <- round(est, ndp)
    }
    lbl <- gsub("(^|[^0-9])0+", "\\1", lbl, perl = TRUE)

    ggplot(data = data, mapping = mapping) +
      annotate(
        "text",
        x = mean(x, na.rm = TRUE),
        y = mean(y, na.rm = TRUE),
        label = lbl,
        family = "Times",
        # size=lb.size,
        ...) +
      theme(panel.grid = element_blank())
  }
## Histogram with density overlay
densigram_fun <- function(data, mapping, hist = list(), ...) {
  X = eval_data_col(data, mapping$x)
  mn = mean(X, na.rm = TRUE)
  s = sd(X, na.rm = TRUE)
  bw = abs(2 * IQR(X, na.rm = TRUE) / length(X) ^ (1 / 3))

  ggplot(data, mapping)  +
    geom_histogram(binwidth = bw, alpha = 0.5) +
    stat_function(fun = function(x)
      dnorm(x, mean = mn, sd = s) * bw * nrow(data)
    )
}
## Scatter plot with regression line
scatter_fun <- function(data, mapping, method = "loess", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.5, shape = 20, size = 1, stroke = 0) +
    geom_smooth(method = method, ...)
  p
}
