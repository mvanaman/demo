# Exploratory Plotting ----
## Quick missingness ----
naniar::gg_miss_var(iris, show_pct = TRUE)
## Univariate ----
### Uni Histograms
plots <- lapply(colnames(iris[, -5]), function(x) get.histograms.uni(iris, x))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Uni Density
plots <- lapply(colnames(iris[, -5]), function(x) get.densities.uni(iris, x))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Uni Bar (returns counts for each group)
plots <- lapply(colnames(mtcars[, 8:11]), function(x) get.bar.uni(mtcars, x))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Uni Violins with Box and Whisker
plots <- lapply(colnames(iris[, -5]), function(x) get.violin.uni(iris, x))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])

## Multivariate ----
### Multi Histogram
plots <-
    lapply(colnames(iris[,-5]), function(x)
        get.histograms.multi(iris, x, fill  = "Species"))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Multi Density
plots <-
    lapply(colnames(iris[,-5]), function(x)
        get.densities.multi(iris, x, fill  = "Species"))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Multi Bar (returns means for each group)
plots <- lapply(colnames(iris[, -5]), function(y) get.bar.multi(iris, x = "Species", y))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
### Multi Violin Plots with Boxplots, With Grouping Variable ----
plots <- lapply(colnames(iris[, -5]), function(y) get.violin.multi(iris, x = "Species", y))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])
## Scatterplot Matrix (best for small-ish datasets)
psych::pairs.panels(iris, ellipses = FALSE, pch = ".", hist.col = wes_palette("FantasticFox1", n = 1))
## Scatterplot Matrix by Group (best for small-ish datasets)
psych::pairs.panels(
    iris,
    ellipses = FALSE,
    pch = 21,
    bg = wes_palette("FantasticFox1", n = 3)[iris$Species],
    hist.col = wes_palette("FantasticFox1", n = 3)
)
## Ridges Plots
plots <- lapply(colnames(iris[, -5]), function(x) get.ridges(iris, x, y = "Species"))
ggpubr::ggarrange(plotlist = plots[1:length(plots)])

# More Elaborate Visualiations
## Example correlogram ----
GGally::ggpairs(iris %>% select(Petal.Length, Petal.Width, Species),
    ## if you need to add something to aes() after the fact,
    ## you can use the "mapping =" argument
    mapping = ggplot2::aes(colour = Species, fill = Species),
    ## using wrap(), you can pass arguments to the function used for
    ## each panel. In the lower panel, geom_smooth is being used, so you
    ## can e.g. change the color of the line with "color ="
    lower = list(continuous = wrap(scatter_fun, method = "lm")),
    diag = list(continuous = wrap(densigram_fun, alpha = 1)),
    upper = list(continuous = wrap(cor_fun, label = "r = "))) +
    # You can add whatever theme customizations you want.
    # I changed the color pallete, added the classic theme, and used
    # Times font
    scale_color_manual(values = wes_palette(n = 3, name = "FantasticFox1")) +
    scale_fill_manual(values = wes_palette(n = 3, name = "FantasticFox1")) +
    theme_classic() +
    theme(text = element_text(family = "Times"),
          strip.background = element_rect(color = "gray"))
## Correlogram with marginal histograms with density ----
plots <- list()
for (i in colnames(iris)) {
  plots[[i]] <-
    ggplot(iris, aes_string(x = "Petal.Width", y = iris[, i])) +
    geom_point(
      alpha = 0.3,
      position = position_jitter(
        seed = 352,
        width = 0.05,
        height = 0.15
      ),
      shape = 20
    ) +
    geom_smooth(formula = "y ~ x",
                method = "lm",
                color = "black") +
    labs(y = i, x = "Petal Width") +
    theme_apa() +
    theme(text = element_text(family = "Times"))
  plots[[i]] <-
    ggMarginal(plots[[i]], type = "densigram", alpha = 0.3)
}

plot.list <-
  ggarrange(
    plots$Sepal.Length,
    plots$Sepal.Width,
    plots$Petal.Length,
    plots$Petal.Width,
    nrow = 2,
    ncol = 2)
plot.list <-
  annotate_figure(
    p = plot.list,
    top = text_grob(
      "Example Correlations with A Single X-Variable",
      size = 14,
      family = "Times")
  )
