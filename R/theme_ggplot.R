#' seabORD ggplot theme
#'
#' Registers the seabORD fonts (if necessary) and returns the
#' package ggplot2 theme.
#'
#' @return A ggplot2 theme object.
#' @export
theme_seabord <- function() {

  if (!"montserrat" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("Montserrat", "montserrat")
  }

  if (!"jetbrains_mono" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("JetBrains Mono", "jetbrains_mono")
  }

  showtext::showtext_auto()

  ggplot2::theme(

    # backgrounds
    plot.background = ggplot2::element_rect(
      fill = "#fff",
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "#EAEFEC",
      color = NA
    ),

    # grid
    panel.grid.major = ggplot2::element_line(
      color = "#EAEFEC"
    ),
    panel.grid.minor = ggplot2::element_line(
      color = "#EAEFEC"
    ),

    # axes
    axis.text = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),
    axis.title = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),

    # title
    plot.title = ggplot2::element_text(
      color = "#0483A4",
      family = "montserrat",
      size = 16,
      face = "bold",
      hjust = 0.5
    ),

    # caption
    plot.caption = ggplot2::element_text(
      color = "#292C2F",
      family = "jetbrains_mono"
    ),

    # legend
    legend.background = ggplot2::element_rect(
      fill = "#fff",
      color = NA
    ),
    legend.key = ggplot2::element_rect(
      fill = "#EAEFEC",
      color = NA
    ),
    legend.text = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),
    legend.title = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat",
      face = "bold"
    )
  )
}
