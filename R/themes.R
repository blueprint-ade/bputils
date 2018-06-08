
theme_pepso <- function(base_size = 12, base_family = "Segoe UI",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22) {

  theme_minimal(
    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size) %+replace%
  theme(
    legend.position    = "bottom",
    panel.spacing      = unit(5, "pt"),
    strip.text         = element_text(hjust = 0, size = 12,
                                      margin = margin(2, 2, 4, 2, unit = "pt")),
    axis.text.y        = element_text(size = 12, hjust = 1),
    panel.grid.major.x = element_line(colour = "grey50", linetype = 3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    complete = TRUE)

}

scale_colour_bo <- function(...) {

  scale_colour_manual(values = c("#FFDA6E", "#ADC5E2"), ...)

}
