# Shared look for the analysis figures.

ink   <- "#0b0b0b"
ink_2 <- "#52514e"
# Categorical slots 1-3 of the dataviz reference palette (validated all-pairs)
blue <- "#2a78d6"; orange <- "#eb6834"; aqua <- "#1baf7a"

# The three methods compared in old_vs_new.r, always in these colours
method_labels <- c(pr = "PR alone", old = "Points + PR (2025)", new = "Adjusted time (new)")
method_colours <- setNames(c(blue, orange, aqua), method_labels)

theme_sdxc <- theme_minimal(base_size = 12) +
  theme(plot.background  = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major = element_line(colour = "#e4e3df", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        text = element_text(colour = ink), axis.text = element_text(colour = ink_2),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = ink_2),
        plot.caption = element_text(colour = ink_2, hjust = 0),
        legend.position = "top", legend.justification = "left",
        strip.text = element_text(face = "bold", hjust = 0))

save_fig <- function(plot, name, w = 9, h = 5) {
  dir.create(file.path("analysis", "figures"), showWarnings = FALSE)
  ggsave(file.path("analysis", "figures", name), plot, width = w, height = h, dpi = 150, device = ragg::agg_png)
}
