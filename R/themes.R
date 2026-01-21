#' Themes and colors for ggplots
#' UChicago Brand Resources: https://creative.uchicago.edu/brand-resources/
#' UChicago Brand Identity Guidelines: https://uchicago.app.box.com/s/5th2zs7py5qizriam5z06sbpqlyqfied
#' Uchicago Color System found on Brand Identity Guidelines pages 29-33

library(ggplot2)

# 1. Define vectors of color palettes ------------------------------------------

v_uchicago_primary_palette    <- c("#800000", "#737373", "#A6A6A6", "#D9D9D9")
v_uchicago_secondary_palette1 <- c(
  "#EAAA00", "#DE7C00", "#789D4A", "#275D38", "#007396", "#59315F", "#A4343A")

v_uchicago_secondary_palette2 <- c(
  "#F3D03E", "#ECA154", "#A9C47F", "#9CAF88", "#3EB1C8", "#86647A", "#B46A55")

v_uchicago_secondary_palette3 <- c(
  "#CC8A00", "#A9431E", "#13301C", "#284734", "#002A3A", "#41273B", "#643335")

# 1. Define themes -------------------------------------------------------------

theme_uchicago <- theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title            = element_text(face   = "bold",
                                         color  = "#800000",
                                         size   = 10,
                                         margin = margin(5,5,5,5)),
    plot.subtitle         = element_text(color  = "#643335",
                                         size   = 10,
                                         face   = "italic",
                                         margin = margin(2, 5, 0, 5)),
    plot.caption          = element_text(face   = "italic",
                                         color  = "#643335",
                                         size   = 8,
                                         hjust  = 0,
                                         margin = margin(5, 5, 5, 5)),
    panel.grid.minor.y    = element_line(linetype = 3, color = "#737373", linewidth = 0.25),
    panel.grid.major.y    = element_line(linetype = 3, color = "#737373", linewidth = 0.25),
    panel.grid.minor.x    = element_line(linetype = 3, color = "#737373", linewidth = 0.25),
    panel.grid.major.x    = element_line(linetype = 3, color = "#737373", linewidth = 0.25),
    panel.spacing         = unit(0.5, "cm"),
    plot.margin           = margin(0, .5, 0, .5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size   = 9,
                                         face   = "bold",
                                         color  = "#643335",
                                         margin = margin(0,0,0,0)),
    legend.text           = element_text(size   = 8,
                                         color  = "#643335",
                                         margin = margin(0,0,0,0)),
    axis.title            = element_text(size   = 8,
                                         hjust  = .5,
                                         color  = "#643335"),
    axis.text.y           = element_text(size   = 8,
                                         hjust  = 1,
                                         vjust  = 0.5,
                                         color  = "#643335"),
    axis.text.x           = element_text(size   = 8,
                                         hjust  =.5,
                                         vjust  = 0.5,
                                         color  = "#643335"),
    strip.text.x          = element_text(size   = 9,
                                         face   = "bold",
                                         color  = "#643335",
                                         margin = margin(0,0,15,0)),
    strip.text.y          = element_text(size   = 9,
                                         face   = "bold",
                                         color  = "#643335",
                                         margin = margin(0,13,0,13)),
    strip.background      = element_rect(fill   = "white", color = NA),
    axis.ticks            = element_blank(),
    #axis.line.y.left      = element_line(color="#350E20", size = .3),
    axis.line.y = element_blank(),
    axis.line.x.bottom    = element_line(color="#643335", size = .3))

# Rename theme to keep consistency with codebase
my_theme          <- theme_uchicago
my_theme_rotated  <- theme_uchicago +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# End. -------------------------------------------------------------------------
